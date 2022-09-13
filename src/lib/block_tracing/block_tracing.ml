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
    | `Add_and_finalize
    | `Breadcrumb_integrated
    | `Add_breadcrumb_to_frontier
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
  type t = { checkpoint : Checkpoint.t; started_at : float; duration : float }
  [@@deriving to_yojson]

  let make checkpoint =
    let started_at = Unix.gettimeofday () in
    (* Duration will be adjusted during post-processing *)
    let duration = 0.0 in
    { checkpoint; started_at; duration }
end

module Trace = struct
  type block_source = [ `External | `Internal | `Catchup | `Unknown ]
  [@@deriving to_yojson]

  let block_source_to_yojson = flatten_yojson_variant block_source_to_yojson

  (* TODO: add general metadata *)
  type t = { source : block_source; checkpoints : Entry.t list }
  [@@deriving to_yojson]

  let empty source = { source; checkpoints = [] }

  let to_yojson t = to_yojson { t with checkpoints = List.rev t.checkpoints }

  let push ~source entry trace =
    match trace with
    | None ->
        { source; checkpoints = [ entry ] }
    | Some { source; checkpoints = [] } ->
        { source; checkpoints = [ entry ] }
    | Some { source; checkpoints = previous :: rest } ->
        let previous =
          { previous with duration = entry.started_at -. previous.started_at }
        in
        { source; checkpoints = entry :: previous :: rest }
end

module Registry = struct
  type t = (Mina_base.State_hash.t, Trace.t) Hashtbl.t

  type produced_registry = (Mina_numbers.Global_slot.t, Trace.t) Hashtbl.t

  type traces = { traces : string list; produced_traces : string list }
  [@@deriving to_yojson]

  let registry : t = Hashtbl.create (module Mina_base.State_hash)

  let produced_registry : produced_registry =
    Hashtbl.create (module Mina_numbers.Global_slot)

  let find_trace state_hash = Hashtbl.find registry state_hash

  let find_produced_trace slot = Hashtbl.find produced_registry slot

  let find_trace_from_string key =
    match Mina_base.State_hash.of_base58_check key with
    | Ok state_hash ->
        find_trace state_hash
    | Error _ -> (
        try find_produced_trace (Mina_numbers.Global_slot.of_string key)
        with _ -> None )

  let all_traces () =
    let traces =
      Hashtbl.keys registry |> List.map ~f:Mina_base.State_hash.to_base58_check
    in
    let produced_traces =
      Hashtbl.keys produced_registry
      |> List.map ~f:Mina_numbers.Global_slot.to_string
    in
    { traces; produced_traces }

  let push_entry ~source block_id entry =
    Hashtbl.update registry block_id ~f:(Trace.push ~source entry)

  let checkpoint ~source block_id checkpoint =
    push_entry ~source block_id (Entry.make checkpoint)

  let push_produced_entry block_id entry =
    Hashtbl.update produced_registry block_id
      ~f:(Trace.push ~source:`Internal entry)

  let produced_checkpoint slot checkpoint =
    push_produced_entry slot (Entry.make checkpoint)
end

module Production = struct
  let current_producer_block_id = ref Mina_numbers.Global_slot.zero

  let checkpoint (checkpoint : Checkpoint.block_production_checkpoint) =
    Registry.produced_checkpoint !current_producer_block_id
      (checkpoint :> Checkpoint.t)

  let begin_block_production slot =
    current_producer_block_id := slot ;
    checkpoint `Begin_block_production

  let end_block_production ?state_hash at_checkpoint =
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
        Hashtbl.update Registry.registry state_hash ~f:(fun _ -> trace) ) ;
    ()
end

module External = struct
  let checkpoint = Registry.checkpoint ~source:`External

  let failure state_hash = checkpoint state_hash `Failure

  let complete state_hash =
    checkpoint state_hash `Complete_external_block_validation
end

module Processing = struct
  let checkpoint = Registry.checkpoint ~source:`Unknown

  let failure state_hash = checkpoint state_hash `Failure

  let complete state_hash = checkpoint state_hash `Breadcrumb_integrated
end

module Catchup = struct
  let checkpoint = Registry.checkpoint ~source:`Catchup

  let failure state_hash = checkpoint state_hash `Failure

  let complete state_hash = checkpoint state_hash `Breadcrumb_integrated
end
