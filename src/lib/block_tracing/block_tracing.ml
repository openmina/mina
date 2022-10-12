open Core

let flatten_yojson_variant f v =
  match f v with `List [ tag ] -> tag | _ -> assert false

module Checkpoint = struct
  (* TODO: define order *)
  (* TODO: define structure *)
  (* TODO: define wait/computation *)
  (* TODO: descriptions *)
  type block_production_checkpoint =
    [ `Begin_block_production
    | `Find_best_tip
    | `Get_transactions_from_pool
    | (* Create staged ledger diff *)
      `Get_snark_work_for_pending_transactions
    | `Validate_and_apply_transactions
    | `Filter_successful_transactions
    | `Generate_staged_ledger_diff
    | (* Build breadcrumb *)
      `Produce_state_transition_proof
    | `Produce_chain_transition_proof
    | `Produce_validated_transition
    | `Build_breadcrumb
    | `Send_breadcrumb_to_transition_frontier
    | `Wait_for_confirmation
    | `Transition_accepted
    | `Transition_accept_timeout
    | `Failure ]
  [@@deriving to_yojson, enumerate, equal, hash, sexp_of, compare]

  let block_production_checkpoint_to_yojson =
    flatten_yojson_variant block_production_checkpoint_to_yojson

  type external_block_validation_checkpoint =
    [ `External_block_received
    | `Initial_validation
    | `Validate_proofs
    | `Done_validating_proofs
    | `Initial_validation_complete
    | `Validate_transition
    | `Check_transition_not_in_frontier
    | `Check_transition_not_in_process
    | `Check_transition_can_be_connected
    | `Register_transition_for_processing
    | `Validate_transition_complete
    | `Failure ]
  [@@deriving to_yojson, enumerate, equal, hash, sexp_of, compare]

  let external_block_validation_checkpoint_to_yojson =
    flatten_yojson_variant external_block_validation_checkpoint_to_yojson

  type block_processing_checkpoint =
    [ `Loaded_transition_from_storage
    | `Begin_local_block_processing
    | `Begin_external_block_processing
    | `Validate_frontier_dependencies
    | `Validate_frontier_dependencies_success
    | `Find_parent_breadcrumb
    | `Build_breadcrumb
    | `Validate_staged_ledger_diff
    | `Check_completed_works
    | `Prediff
    | `Apply_diff
    | `Update_coinbase_stack
    | `Check_for_sufficient_snark_work
    | `Check_zero_fee_excess
    | `Fill_work_and_enqueue_transactions
    | `Update_pending_coinbase_collection
    | `Verify_scan_state_after_apply
    | `Hash_new_staged_ledger
    | `Hash_scan_state
    | `Get_merkle_root
    | `Make_staged_ledger_hash
    | `Diff_applied
    | `Create_breadcrumb
    | `Add_and_finalize
    | `Add_and_finalize_done
    | `Breadcrumb_integrated
    | `Add_breadcrumb_to_frontier
    | `Calculate_diffs
    | `Apply_catchup_tree_diffs
    | `Apply_full_frontier_diffs
    | `Full_frontier_diffs_applied
    | `Synchronize_persistent_frontier
    | `Persistent_frontier_synchronized
    | `Notify_frontier_extensions
    | `Notify_frontier_extensions_done
    | `Add_breadcrumb_to_frontier_done
    | `Parent_breadcrumb_not_found
    | `Schedule_catchup
    | `Download_ancestry_state_hashes
    | `Failure ]
  [@@deriving to_yojson, enumerate, equal, hash, sexp_of, compare]

  let block_processing_checkpoint_to_yojson =
    flatten_yojson_variant block_processing_checkpoint_to_yojson

  type catchup_checkpoint =
    [ `To_download
    | `To_initial_validate
    | `To_verify
    | `Wait_for_parent
    | `To_build_breadcrumb
    | `Catchup_job_finished
    | `Failure ]
  [@@deriving to_yojson, enumerate, equal, hash, sexp_of, compare]

  let catchup_checkpoint_to_yojson =
    flatten_yojson_variant catchup_checkpoint_to_yojson

  type t =
    [ block_production_checkpoint
    | external_block_validation_checkpoint
    | catchup_checkpoint
    | block_processing_checkpoint ]
  [@@deriving to_yojson, enumerate, equal, hash, sexp_of, compare]

  let to_string (c : t) =
    match to_yojson c with `String name -> name | _ -> assert false
end

module Trace = struct
  module Entry = struct
    type t =
      { checkpoint : Checkpoint.t
      ; started_at : float
      ; duration : float
      ; metadata : string
      }
    [@@deriving to_yojson]

    let make ?(metadata = "") checkpoint =
      let started_at = Unix.gettimeofday () in
      (* Duration will be adjusted during post-processing *)
      let duration = 0.0 in
      { checkpoint; started_at; duration; metadata }
  end

  type block_source =
    [ `External | `Internal | `Catchup | `Reconstruct | `Unknown ]
  [@@deriving to_yojson]

  type status = [ `Pending | `Failure | `Success ] [@@deriving to_yojson]

  let block_source_to_yojson = flatten_yojson_variant block_source_to_yojson

  let status_to_yojson = flatten_yojson_variant status_to_yojson

  (* TODOX: add general metadata *)
  (* TODOX: add total time *)
  type t =
    { source : block_source
    ; blockchain_length : Mina_numbers.Length.t [@key "global_slot"]
    ; checkpoints : Entry.t list
    ; status : status
    }
  [@@deriving to_yojson]

  let empty ?(blockchain_length = Mina_numbers.Length.zero) source =
    { source; blockchain_length; checkpoints = []; status = `Pending }

  let to_yojson t = to_yojson { t with checkpoints = List.rev t.checkpoints }

  let push ~status ~source ?blockchain_length entry trace =
    match trace with
    | None ->
        let trace = empty ?blockchain_length source in
        { trace with checkpoints = [ entry ]; status }
    | Some ({ checkpoints = []; _ } as trace) ->
        { trace with checkpoints = [ entry ]; status }
    | Some ({ checkpoints = previous :: rest; _ } as trace) ->
        let previous =
          { previous with duration = entry.started_at -. previous.started_at }
        in
        { trace with checkpoints = entry :: previous :: rest; status }
end

module Structured_trace = struct
  module Entry = struct
    type t =
      { checkpoint : Checkpoint.t
      ; started_at : float
      ; duration : float
      ; metadata : string
      ; checkpoints : t list
      }
    [@@deriving to_yojson]

    let of_flat_entry entry =
      let { Trace.Entry.checkpoint; started_at; duration; metadata } = entry in
      { checkpoint; started_at; duration; metadata; checkpoints = [] }
  end

  type section = { title : string; checkpoints : Entry.t list }
  [@@deriving to_yojson]

  type t =
    { source : Trace.block_source
    ; blockchain_length : Mina_numbers.Length.t [@key "global_slot"]
    ; sections : section list
    ; status : Trace.status
    }
  [@@deriving to_yojson]

  let checkpoint_children (c : Checkpoint.t) : Checkpoint.t list =
    match c with
    | `Initial_validation ->
        [ `Validate_proofs; `Done_validating_proofs ]
    | `Validate_transition ->
        [ `Check_transition_not_in_frontier
        ; `Check_transition_not_in_process
        ; `Check_transition_can_be_connected
        ; `Register_transition_for_processing
        ]
    | `Build_breadcrumb ->
        [ `Validate_staged_ledger_diff; `Create_breadcrumb ]
    | `Validate_staged_ledger_diff ->
        [ `Check_completed_works; `Prediff; `Apply_diff; `Diff_applied ]
    | `Check_completed_works ->
        []
    | `Apply_diff ->
        [ `Update_coinbase_stack
        ; `Check_for_sufficient_snark_work
        ; `Check_zero_fee_excess
        ; `Fill_work_and_enqueue_transactions
        ; `Update_pending_coinbase_collection
        ; `Verify_scan_state_after_apply
        ; `Hash_new_staged_ledger
        ; `Hash_scan_state
        ; `Get_merkle_root
        ; `Make_staged_ledger_hash
        ]
    | `Add_and_finalize ->
        [ `Add_breadcrumb_to_frontier; `Add_breadcrumb_to_frontier_done ]
    | `Add_breadcrumb_to_frontier ->
        [ `Calculate_diffs
        ; `Apply_catchup_tree_diffs
        ; `Apply_full_frontier_diffs
        ; `Full_frontier_diffs_applied
        ; `Synchronize_persistent_frontier
        ; `Persistent_frontier_synchronized
        ; `Notify_frontier_extensions
        ; `Notify_frontier_extensions_done
        ]
    | _ ->
        []

  let is_parent_of parent entry =
    let children = checkpoint_children parent.Entry.checkpoint in
    List.mem children entry.Entry.checkpoint ~equal:Checkpoint.equal

  let has_children entry =
    not (List.is_empty (checkpoint_children entry.Entry.checkpoint))

  (* TODOX: instead of processing checkpoints at the same level here
     process the flat trace using information about parent/children relationship
     to skip children when finding the end *)
  let postprocess_checkpoints checkpoints =
    match checkpoints with
    | [] ->
        []
    | first :: _ as checkpoints ->
        let next_timestamp = ref first.Entry.started_at in
        List.rev_map checkpoints ~f:(fun entry ->
            let ended_at = !next_timestamp in
            next_timestamp := entry.started_at ;
            { entry with duration = ended_at -. entry.started_at } )

  let postprocess_entry_checkpoints entry =
    let checkpoints = postprocess_checkpoints entry.Entry.checkpoints in
    { entry with checkpoints }

  let merge_into_parent parent entry =
    let entry' = postprocess_entry_checkpoints entry in
    let checkpoints = entry' :: parent.Entry.checkpoints in
    { parent with checkpoints }

  let rec collapse_pending_stack_with_children entry acc stack =
    match stack with
    | parent :: _ as stack when is_parent_of parent entry ->
        (acc, entry :: stack)
    | child :: parent :: rest ->
        let parent' = merge_into_parent parent child in
        collapse_pending_stack_with_children entry acc (parent' :: rest)
    | [ sibling ] ->
        let sibling' = postprocess_entry_checkpoints sibling in
        (sibling' :: acc, [ entry ])
    | [] ->
        (acc, [ entry ])

  let rec collapse_pending_stack_simple entry acc stack =
    match stack with
    | parent :: rest when is_parent_of parent entry ->
        let parent' = merge_into_parent parent entry in
        (acc, parent' :: rest)
    | child :: parent :: rest ->
        let parent' = merge_into_parent parent child in
        collapse_pending_stack_simple entry acc (parent' :: rest)
    | [ sibling ] ->
        let sibling' = postprocess_entry_checkpoints sibling in
        (entry :: sibling' :: acc, [])
    | [] ->
        (entry :: acc, [])

  let structure_checkpoints checkpoints =
    let checkpoints = List.rev_map ~f:Entry.of_flat_entry checkpoints in
    postprocess_checkpoints @@ fst
    @@ List.fold checkpoints ~init:([], []) ~f:(fun (accum, stack) entry ->
           if has_children entry then
             collapse_pending_stack_with_children entry accum stack
           else collapse_pending_stack_simple entry accum stack )

  let of_flat_trace trace =
    let { Trace.source; blockchain_length; status; checkpoints } = trace in
    let checkpoints = structure_checkpoints checkpoints in
    (* TODOX: complete sections *)
    let section = { title = "All"; checkpoints } in
    let sections = [ section ] in
    { source; blockchain_length; sections; status }
end

module Distributions = struct
  type range_info =
    { mutable count : int
    ; mutable mean_time : float [@key "meanTime"]
    ; mutable max_time : float [@key "maxTime"]
    ; mutable total_time : float [@key "totalTime"]
    }
  [@@deriving to_yojson]

  (* TODO: add count *)
  type t =
    { checkpoint : Checkpoint.t
    ; mutable count : int
    ; mutable total_time : float [@key "totalTime"]
    ; one_to_ten_us : range_info [@key "oneToTenUs"]
    ; ten_to_one_hundred_us : range_info [@key "tenToOneHundredUs"]
    ; one_hundred_us_to_one_ms : range_info [@key "oneHundredUsToOneMs"]
    ; one_to_ten_ms : range_info [@key "oneToTenMs"]
    ; ten_to_one_hundred_ms : range_info [@key "tenToOneHundredMs"]
    ; one_hundred_ms_to_one_s : range_info [@key "oneHundredMsToOneS"]
    ; one_to_ten_s : range_info [@key "oneToTenS"]
    ; ten_to_one_hundred_s : range_info [@key "tenToOneHundredS"]
    ; one_hundred_s : range_info [@key "oneHundredS"]
    }
  [@@deriving to_yojson]

  type listing = t list [@@deriving to_yojson]

  type store = (Checkpoint.t, t) Hashtbl.t

  let all_store : store = Hashtbl.create (module Checkpoint)

  let produced_store : store = Hashtbl.create (module Checkpoint)

  let external_store : store = Hashtbl.create (module Checkpoint)

  let catchup_store : store = Hashtbl.create (module Checkpoint)

  let reconstruct_store : store = Hashtbl.create (module Checkpoint)

  let unknown_store : store = Hashtbl.create (module Checkpoint)

  let source_store = function
    | `Catchup ->
        catchup_store
    | `Internal ->
        produced_store
    | `External ->
        external_store
    | `Reconstruct ->
        reconstruct_store
    | `Unknown ->
        unknown_store

  let empty_range_info () =
    { count = 0; mean_time = 0.0; max_time = 0.0; total_time = 0.0 }

  let empty_checkpoint_entry checkpoint =
    { checkpoint
    ; count = 0
    ; total_time = 0.0
    ; one_to_ten_us = empty_range_info ()
    ; ten_to_one_hundred_us = empty_range_info ()
    ; one_hundred_us_to_one_ms = empty_range_info ()
    ; one_to_ten_ms = empty_range_info ()
    ; ten_to_one_hundred_ms = empty_range_info ()
    ; one_hundred_ms_to_one_s = empty_range_info ()
    ; one_to_ten_s = empty_range_info ()
    ; ten_to_one_hundred_s = empty_range_info ()
    ; one_hundred_s = empty_range_info ()
    }

  let ten_us = 0.00001

  let one_hundred_us = 0.0001

  let one_ms = 0.001

  let ten_ms = 0.01

  let one_hundred_ms = 0.1

  let one_s = 1.0

  let ten_s = 10.0

  let one_hundred_s = 100.0

  let range_for_duration record duration =
    let open Float in
    if duration < ten_us then record.one_to_ten_us
    else if duration < one_hundred_us then record.ten_to_one_hundred_us
    else if duration < one_ms then record.one_hundred_us_to_one_ms
    else if duration < ten_ms then record.one_to_ten_ms
    else if duration < one_hundred_ms then record.ten_to_one_hundred_ms
    else if duration < one_s then record.one_hundred_ms_to_one_s
    else if duration < ten_s then record.one_to_ten_s
    else if duration < one_hundred_s then record.ten_to_one_hundred_s
    else record.one_hundred_s

  let rec integrate_entry ~store entry =
    let { Structured_trace.Entry.checkpoint; duration; _ } = entry in
    let record =
      Hashtbl.find_or_add store checkpoint ~default:(fun () ->
          empty_checkpoint_entry checkpoint )
    in
    let range = range_for_duration record duration in
    record.count <- record.count + 1 ;
    record.total_time <- record.total_time +. duration ;
    range.count <- range.count + 1 ;
    range.total_time <- range.total_time +. duration ;
    range.max_time <- Float.max range.max_time duration ;
    let f_count = Float.of_int range.count in
    range.mean_time <-
      range.mean_time +. ((duration -. range.mean_time) /. f_count) ;
    List.iter entry.checkpoints ~f:(integrate_entry ~store) ;
    ()

  let integrate_trace (trace : Structured_trace.t) =
    let source_store = source_store trace.source in
    List.iter trace.sections ~f:(fun section ->
        List.iter section.checkpoints ~f:(integrate_entry ~store:all_store) ;
        List.iter section.checkpoints ~f:(integrate_entry ~store:source_store) )

  let all () = Hashtbl.data all_store

  let by_source source = Hashtbl.data (source_store source)
end

module Registry = struct
  type t = (Mina_base.State_hash.t, Trace.t) Hashtbl.t

  type produced_registry = (Mina_numbers.Global_slot.t, Trace.t) Hashtbl.t

  (* TODOX: add total time *)
  type trace_info =
    { source : Trace.block_source
    ; blockchain_length : Mina_numbers.Length.t
    ; state_hash : string
    ; status : Trace.status
    }
  [@@deriving to_yojson]

  type traces = { traces : trace_info list; produced_traces : trace_info list }
  [@@deriving to_yojson]

  let registry : t = Hashtbl.create (module Mina_base.State_hash)

  let catchup_registry : t = Hashtbl.create (module Mina_base.State_hash)

  let produced_registry : produced_registry =
    Hashtbl.create (module Mina_numbers.Global_slot)

  let postprocess_checkpoints trace =
    let next_timestamp = ref (List.hd_exn trace).Trace.Entry.started_at in
    List.map trace ~f:(fun entry ->
        let ended_at = !next_timestamp in
        next_timestamp := entry.started_at ;
        { entry with duration = ended_at -. entry.started_at } )

  let merge_traces regular catchup =
    let open Trace in
    (* TODO handle more cases, this assumes catchup + regular always means that the
       source was catchup, but there are race conditions *)
    (* These checkpoints add noise to the final trace, get rid of them *)
    let catchup_checkpoints =
      List.filter catchup.checkpoints ~f:(fun entry ->
          match entry.checkpoint with
          | `To_verify | `To_build_breadcrumb | `Catchup_job_finished ->
              false
          | _ ->
              true )
    in
    let checkpoints = regular.checkpoints @ catchup_checkpoints in
    let checkpoints =
      List.sort checkpoints ~compare:(fun l r ->
          (* Sorted from newest to oldest *)
          Float.compare r.started_at l.started_at )
    in
    let checkpoints = postprocess_checkpoints checkpoints in
    { source = catchup.source
    ; blockchain_length = regular.blockchain_length
    ; checkpoints
    ; status = catchup.status
    }

  let find_trace state_hash =
    match
      ( Hashtbl.find registry state_hash
      , Hashtbl.find catchup_registry state_hash )
    with
    | None, None ->
        None
    | Some regular, None ->
        Some regular
    | None, Some catchup ->
        Some catchup
    | Some regular, Some catchup ->
        let merged = merge_traces regular catchup in
        Some merged

  let find_produced_trace slot = Hashtbl.find produced_registry slot

  let find_trace_from_string key =
    match Mina_base.State_hash.of_base58_check key with
    | Ok state_hash ->
        find_trace state_hash
    | Error _ -> (
        try find_produced_trace (Mina_numbers.Global_slot.of_string key)
        with _ -> None )

  (* TODO: cleanup this and find a better way *)
  let all_traces () =
    let catchup_traces =
      Hashtbl.to_alist catchup_registry
      |> List.map ~f:(fun (key, item) ->
             let state_hash = Mina_base.State_hash.to_base58_check key in
             let Trace.{ blockchain_length; source; status; _ } = item in
             { state_hash; blockchain_length; source; status } )
    in
    let traces =
      Hashtbl.to_alist registry
      |> List.filter ~f:(fun (s, _) ->
             Option.is_none (Hashtbl.find catchup_registry s) )
      |> List.map ~f:(fun (key, item) ->
             let state_hash = Mina_base.State_hash.to_base58_check key in
             let Trace.{ blockchain_length; source; status; _ } = item in
             { state_hash; blockchain_length; source; status } )
    in
    let traces =
      traces @ catchup_traces
      |> List.sort ~compare:(fun a b ->
             Mina_numbers.Length.compare a.blockchain_length b.blockchain_length )
    in
    let produced_traces =
      Hashtbl.to_alist produced_registry
      |> List.map ~f:(fun (_, item) ->
             let state_hash = "<unknown>" in
             let Trace.{ blockchain_length; source; status; _ } = item in
             { state_hash; blockchain_length; source; status } )
      |> List.sort ~compare:(fun a b ->
             Mina_numbers.Global_slot.compare a.blockchain_length
               b.blockchain_length )
    in
    { traces; produced_traces }

  let push_entry ~status ~source ?blockchain_length block_id entry =
    Hashtbl.update registry block_id
      ~f:(Trace.push ~status ~source ?blockchain_length entry)

  let checkpoint ?(status = `Pending) ?metadata ~source ?blockchain_length
      block_id checkpoint =
    push_entry ~status ~source ?blockchain_length block_id
      (Trace.Entry.make ?metadata checkpoint)

  let push_catchup_entry ~status ~source ?blockchain_length block_id entry =
    Hashtbl.update catchup_registry block_id
      ~f:(Trace.push ~status ~source ?blockchain_length entry)

  let catchup_checkpoint ?(status = `Pending) ?metadata ~source
      ?blockchain_length block_id checkpoint =
    push_catchup_entry ~status ~source ?blockchain_length block_id
      (Trace.Entry.make ?metadata checkpoint)

  let push_produced_entry ~status slot entry =
    Hashtbl.update produced_registry slot
      ~f:(Trace.push ~status ~blockchain_length:slot ~source:`Internal entry)

  let produced_checkpoint ?(status = `Pending) ?metadata slot checkpoint =
    push_produced_entry ~status slot (Trace.Entry.make ?metadata checkpoint)
end

module Production = struct
  let current_producer_block_id = ref Mina_numbers.Global_slot.zero

  let checkpoint ?metadata (checkpoint : Checkpoint.block_production_checkpoint)
      =
    Registry.produced_checkpoint ?metadata !current_producer_block_id
      (checkpoint :> Checkpoint.t)

  let begin_block_production slot =
    current_producer_block_id := slot ;
    checkpoint `Begin_block_production

  let end_block_production ?state_hash ~blockchain_length at_checkpoint =
    checkpoint at_checkpoint ;
    let id = !current_producer_block_id in
    let trace =
      Option.value ~default:(Trace.empty `Internal)
      @@ Registry.find_produced_trace id
    in
    (* At this point we may know the hash of the produced block, so we get rid of the
       produced trace and register a trace for the state hash so that the next
       pipeline can continue it *)
    match state_hash with
    | None ->
        Hashtbl.set Registry.produced_registry ~key:id
          ~data:{ trace with blockchain_length; status = `Success }
    | Some state_hash ->
        Hashtbl.remove Registry.produced_registry id ;
        let trace = { trace with blockchain_length; status = `Success } in
        Hashtbl.update Registry.registry state_hash ~f:(fun _ -> trace)
end

module External = struct
  let checkpoint = Registry.checkpoint ~source:`External

  let failure ~reason state_hash =
    checkpoint ~metadata:reason ~status:`Failure state_hash `Failure

  let complete state_hash =
    checkpoint ~status:`Success state_hash `Validate_transition_complete
end

module Processing = struct
  let parent_registry = Hashtbl.create (module Mina_base.State_hash)

  let register_parent ~state_hash ~parent_hash =
    assert (not (Mina_base.State_hash.equal state_hash parent_hash)) ;
    ignore @@ Hashtbl.add parent_registry ~data:state_hash ~key:parent_hash

  let get_registered_child parent_hash =
    Hashtbl.find parent_registry parent_hash

  let checkpoint ?(source = `Unknown) = Registry.checkpoint ~source

  let failure ~reason state_hash =
    (* TODOX: compute structured version and save it *)
    checkpoint ~metadata:reason ~status:`Failure state_hash `Failure

  let complete state_hash =
    (* TODOX: compute structured version and save it *)
    checkpoint ~status:`Success state_hash `Breadcrumb_integrated ;
    let trace_rev = Registry.find_trace state_hash |> Option.value_exn in
    let structured_trace = Structured_trace.of_flat_trace trace_rev in
    Distributions.integrate_trace structured_trace
end

module Catchup = struct
  let checkpoint = Registry.catchup_checkpoint ~source:`Catchup

  let failure ?blockchain_length state_hash =
    checkpoint ~status:`Failure ?blockchain_length state_hash `Failure

  let complete ?blockchain_length state_hash =
    checkpoint ~status:`Success ?blockchain_length state_hash
      `Catchup_job_finished
end

module Reconstruct = struct
  let checkpoint = Registry.checkpoint ~source:`Reconstruct

  let failure = Processing.failure

  let complete = Processing.complete
end
