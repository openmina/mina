open Core

let flatten_yojson_variant f v =
  match f v with `List [ tag ] -> tag | _ -> assert false

module Checkpoint = struct
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
    | (* TODO: replace with specific failures? *)
      `Failure ]
  [@@deriving to_yojson, enumerate]

  let block_production_checkpoint_to_yojson =
    flatten_yojson_variant block_production_checkpoint_to_yojson

  type external_block_validation_checkpoint =
    [ `External_block_received
    | `Begin_initial_validation
    | `Validate_proofs
    | `Done_validating_proofs
    | `Initial_validation_complete
    | `Begin_external_block_validation
    | `Check_transition_not_in_frontier
    | `Check_transition_not_in_process
    | `Check_transition_can_be_connected
    | `Register_transition_for_processing
    | `Complete_external_block_validation
    | (* TODO: replace with specific failures? *)
      `Failure ]
  [@@deriving to_yojson, enumerate]

  let external_block_validation_checkpoint_to_yojson =
    flatten_yojson_variant external_block_validation_checkpoint_to_yojson

  type block_processing_checkpoint =
    [ `Begin_local_block_processing
    | `Begin_external_block_processing
    | `Validate_frontier_dependencies
    | `Validate_frontier_dependencies_success
    | `Find_parent_breadcrumb
    | `Build_breadcrumb
    | `Validate_staged_ledger_diff
    | `Create_breadcrumb
    | `Add_and_finalize
    | `Breadcrumb_integrated
    | `Add_breadcrumb_to_frontier
    | `Calculate_diffs
    | `Apply_catchup_tree_diffs
    | `Apply_full_frontier_diffs
    | `Full_frontier_diffs_applied
    | `Synchronize_persistent_frontier
    | `Persistent_frontier_synchronized
    | `Parent_breadcrumb_not_found
    | `Schedule_catchup
    | `Download_ancestry_state_hashes
    | (* TODO: replace with specific failures? *)
      `Failure ]
  [@@deriving to_yojson, enumerate]

  let block_processing_checkpoint_to_yojson =
    flatten_yojson_variant block_processing_checkpoint_to_yojson

  type catchup_checkpoint =
    [ `To_download
    | `To_initial_validate
    | `To_verify
    | `Wait_for_parent
    | `To_build_breadcrumb
    | `Catchup_job_finished
    | (* TODO: replace with specific failures? *)
      `Failure ]
  [@@deriving to_yojson, enumerate]

  let catchup_checkpoint_to_yojson =
    flatten_yojson_variant catchup_checkpoint_to_yojson

  type t =
    [ block_production_checkpoint
    | external_block_validation_checkpoint
    | block_processing_checkpoint
    | catchup_checkpoint ]
  [@@deriving to_yojson, enumerate]

  let to_string (c : t) =
    match to_yojson c with `String name -> name | _ -> assert false
end

module Entry = struct
  (* TODO: add checkpoint metadata *)
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

module Trace = struct
  type block_source = [ `External | `Internal | `Catchup | `Unknown ]
  [@@deriving to_yojson]

  type status = [ `Pending | `Failure | `Success ] [@@deriving to_yojson]

  let block_source_to_yojson = flatten_yojson_variant block_source_to_yojson

  let status_to_yojson = flatten_yojson_variant status_to_yojson

  (* TODO: add general metadata *)
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

module Registry = struct
  type t = (Mina_base.State_hash.t, Trace.t) Hashtbl.t

  type produced_registry = (Mina_numbers.Global_slot.t, Trace.t) Hashtbl.t

  type trace_info =
    { source : Trace.block_source
    ; blockchain_length : Mina_numbers.Length.t [@key "global_slot"]
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
    let next_timestamp = ref (List.hd_exn trace).Entry.started_at in
    List.map trace ~f:(fun entry ->
        let ended_at = !next_timestamp in
        next_timestamp := entry.started_at ;
        { entry with duration = ended_at -. entry.started_at } )

  let merge_traces regular catchup =
    let open Trace in
    (* TODO handle more cases, this assumes catchup + regular always means that the
       source was catchup, but there are race conditions *)
    let checkpoints = regular.checkpoints @ catchup.checkpoints in
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
      (Entry.make ?metadata checkpoint)

  let push_catchup_entry ~status ~source ?blockchain_length block_id entry =
    Hashtbl.update catchup_registry block_id
      ~f:(Trace.push ~status ~source ?blockchain_length entry)

  let catchup_checkpoint ?(status = `Pending) ?metadata ~source
      ?blockchain_length block_id checkpoint =
    push_catchup_entry ~status ~source ?blockchain_length block_id
      (Entry.make ?metadata checkpoint)

  let push_produced_entry ~status slot entry =
    Hashtbl.update produced_registry slot
      ~f:(Trace.push ~status ~blockchain_length:slot ~source:`Internal entry)

  let produced_checkpoint ?(status = `Pending) ?metadata slot checkpoint =
    push_produced_entry ~status slot (Entry.make ?metadata checkpoint)
end

module Production = struct
  let current_producer_block_id = ref Mina_numbers.Global_slot.zero

  let checkpoint (checkpoint : Checkpoint.block_production_checkpoint) =
    Registry.produced_checkpoint !current_producer_block_id
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
    (* At this point we know the hash of the produced block, so we get rid of the
       produced trace and register a trace for the state hash so that the next
       pipeline can continue it *)
    Option.iter state_hash ~f:(fun state_hash ->
        Hashtbl.remove Registry.produced_registry id ;
        let trace = { trace with blockchain_length; status = `Success } in
        Hashtbl.update Registry.registry state_hash ~f:(fun _ -> trace) ) ;
    ()
end

module External = struct
  let checkpoint = Registry.checkpoint ~source:`External

  let failure ~reason state_hash =
    checkpoint ~metadata:reason ~status:`Failure state_hash `Failure

  let complete state_hash =
    checkpoint ~status:`Success state_hash `Complete_external_block_validation
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
    checkpoint ~metadata:reason ~status:`Failure state_hash `Failure

  let complete state_hash =
    checkpoint ~status:`Success state_hash `Breadcrumb_integrated
end

module Catchup = struct
  let checkpoint = Registry.catchup_checkpoint ~source:`Catchup

  let failure ?blockchain_length state_hash =
    checkpoint ~status:`Failure ?blockchain_length state_hash `Failure

  let complete ?blockchain_length state_hash =
    checkpoint ~status:`Success ?blockchain_length state_hash
      `Catchup_job_finished
end
