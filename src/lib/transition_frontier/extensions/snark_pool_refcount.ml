open Core_kernel
open Frontier_base
open Internal_tracing
module Work = Transaction_snark_work.Statement

module T = struct
  type view =
    { removed : int
    ; refcount_table : int Work.Table.t
          (** Tracks the number of blocks that have each work statement in
              their scan state.
              Work is included iff it is a member of some block scan state.
          *)
    ; best_tip_table : Work.Hash_set.t
          (** The set of all snark work statements present in the scan state
              for the last 10 blocks in the best chain.
          *)
    }
  [@@deriving sexp]

  type t =
    { refcount_table : int Work.Table.t
          (** Tracks the number of blocks that have each work statement in
              their scan state.
              Work is included iff it is a member of some block scan state.
          *)
    ; best_tip_table : Work.Hash_set.t
          (** The set of all snark work statements present in the scan state
              for the last 10 blocks in the best chain.
          *)
    }

  let get_work = Staged_ledger.Scan_state.all_work_statements_exn

  (** Returns true if this update changed which elements are in the table
      (but not if the same elements exist with a different reference count) *)
  let add_to_table ~get_work ~get_statement table t : bool =
    let res = ref false in
    Block_tracing.Processing.checkpoint_current `SRPC_get_work ;
    let work = get_work t in
    Block_tracing.Processing.checkpoint_current `SRPC_get_statements ;
    let statements = List.map ~f:get_statement work in
    Block_tracing.Processing.checkpoint_current `SRPC_hash_statements ;
    let _hashed = List.map ~f:Work.hash statements in
    Block_tracing.Processing.checkpoint_current `SRPC_update_work_table ;
    List.iter statements ~f:(fun statement ->
        Work.Table.update table statement ~f:(function
          | Some count ->
              count + 1
          | None ->
              res := true ;
              1 ) ) ;
    let metadata = Printf.sprintf "work_count=%d" (List.length work) in
    Block_tracing.Processing.checkpoint_current ~metadata
      `SRPC_update_work_table_done ;
    !res

  (** Returns true if this update changed which elements are in the table
      (but not if the same elements exist with a different reference count) *)
  let remove_from_table ~get_work ~get_statement table t : bool =
    let res = ref false in
    (* TODOX trace this *)
    List.iter (get_work t) ~f:(fun work ->
        Work.Table.change table (get_statement work) ~f:(function
          | Some 1 ->
              res := true ;
              None
          | Some count ->
              Some (count - 1)
          | None ->
              failwith "Removed a breadcrumb we didn't know about" ) ) ;
    !res

  let add_scan_state_to_ref_table table scan_state : bool =
    add_to_table ~get_work ~get_statement:Fn.id table scan_state

  let remove_scan_state_from_ref_table table scan_state : bool =
    remove_from_table ~get_work ~get_statement:Fn.id table scan_state

  let create ~logger:_ frontier =
    let t =
      { refcount_table = Work.Table.create ()
      ; best_tip_table = Work.Hash_set.create ()
      }
    in
    let () =
      let breadcrumb = Full_frontier.root frontier in
      let scan_state =
        Breadcrumb.staged_ledger breadcrumb |> Staged_ledger.scan_state
      in
      ignore (add_scan_state_to_ref_table t.refcount_table scan_state : bool)
    in
    ( t
    , { removed = 0
      ; refcount_table = t.refcount_table
      ; best_tip_table = t.best_tip_table
      } )

  type diff_update = { num_removed : int; is_added : bool }

  let handle_diffs t frontier diffs_with_mutants =
    let open Diff.Full.With_mutant in
    let { num_removed; is_added } =
      List.fold diffs_with_mutants ~init:{ num_removed = 0; is_added = false }
        ~f:(fun { num_removed; is_added } -> function
        | E (New_node (Full breadcrumb), _) ->
            Storage_tracing.Frontier_extensions.record `New_node
              `Snark_pool_refcount
            @@ fun () ->
            let scan_state =
              Breadcrumb.staged_ledger breadcrumb |> Staged_ledger.scan_state
            in
            Block_tracing.Processing.checkpoint_current
              `SRPC_add_scan_state_to_ref_table ;
            let added_scan_state =
              add_scan_state_to_ref_table t.refcount_table scan_state
            in
            Block_tracing.Processing.checkpoint_current
              `SRPC_add_scan_state_to_ref_table_done ;
            { num_removed; is_added = is_added || added_scan_state }
        | E
            ( Root_transitioned { new_root = _; garbage = Full garbage_nodes; _ }
            , _ ) ->
            Storage_tracing.Frontier_extensions.record `Root_transitioned
              `Snark_pool_refcount
            @@ fun () ->
            let open Diff.Node_list in
            let extra_num_removed =
              List.fold garbage_nodes ~init:0 ~f:(fun acc node ->
                  let delta =
                    if
                      remove_scan_state_from_ref_table t.refcount_table
                        node.scan_state
                    then 1
                    else 0
                  in
                  acc + delta )
            in
            { num_removed = num_removed + extra_num_removed; is_added }
        | E (Best_tip_changed new_best_tip_hash, _) ->
            Storage_tracing.Frontier_extensions.record `Best_tip_changed
              `Snark_pool_refcount
            @@ fun () ->
            let rec update_best_tip_table blocks_remaining state_hash =
              match Full_frontier.find frontier state_hash with
              | None ->
                  ()
              | Some breadcrumb ->
                  let statements =
                    try
                      Breadcrumb.staged_ledger breadcrumb
                      |> Staged_ledger.all_work_statements_exn
                    with _ -> []
                  in
                  List.iter ~f:(Hash_set.add t.best_tip_table) statements ;
                  if blocks_remaining > 0 then
                    update_best_tip_table (blocks_remaining - 1)
                      (Breadcrumb.parent_hash breadcrumb)
            in
            let num_blocks_to_include = 3 in
            Hash_set.clear t.best_tip_table ;
            Block_tracing.Processing.checkpoint_current
              `SRPC_update_best_tip_table ;
            update_best_tip_table num_blocks_to_include new_best_tip_hash ;
            Block_tracing.Processing.checkpoint_current
              `SRPC_update_best_tip_table_done ;
            { num_removed; is_added = true } )
    in
    if num_removed > 0 || is_added then
      Some
        { removed = num_removed
        ; refcount_table = t.refcount_table
        ; best_tip_table = t.best_tip_table
        }
    else None
end

include T

module Broadcasted = struct
  open Pipe_lib
  open Async_kernel
  include Functor.Make_broadcasted (T)

  let update t ?state_hash frontier diffs =
    let extension = extension t in
    let writer = writer t in
    Option.iter state_hash ~f:(fun state_hash ->
        let metadata = Printf.sprintf "diffs_count=%d" (List.length diffs) in
        Block_tracing.Processing.checkpoint state_hash ~metadata
          `Notify_SPRC_handle_diffs ) ;
    Block_tracing.Processing.set_current_state_hash state_hash ;
    match T.handle_diffs extension frontier diffs with
    | Some view ->
        Option.iter state_hash ~f:(fun state_hash ->
            Block_tracing.Processing.checkpoint state_hash
              `Notify_SPRC_write_view ) ;
        Deferred.map (Broadcast_pipe.Writer.write writer view) ~f:(fun () ->
            Option.iter state_hash ~f:(fun state_hash ->
                Block_tracing.Processing.checkpoint state_hash
                  `Notify_SPRC_write_view_done ) )
    | None ->
        Deferred.unit
end
