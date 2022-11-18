open Core
module Checkpoint = Block_checkpoint
module Trace = Block_trace
module Structured_trace = Block_structured_trace

module Distributions = struct
  module D = Distribution.Make (struct
    type identity = Checkpoint.t [@@deriving to_yojson]
  end)

  include D

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

  let rec integrate_entry ~store entry =
    let { Structured_trace.Entry.checkpoint; duration; _ } = entry in
    record ~store checkpoint duration ;
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

  type trace_info =
    { source : Trace.block_source
    ; blockchain_length : Mina_numbers.Length.t
    ; state_hash : string
    ; status : Trace.status
    ; total_time : float
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
    let filter_cachup_noise cp =
      List.filter cp ~f:(fun entry ->
          match entry.Entry.checkpoint with
          | `To_verify | `To_build_breadcrumb | `Catchup_job_finished ->
              false
          | _ ->
              true )
    in
    let catchup_checkpoints = filter_cachup_noise catchup.checkpoints in
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
    ; other_checkpoints =
        regular.other_checkpoints (* Catchup will not happen more than once *)
    ; status = catchup.status
    ; total_time = regular.total_time
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
             let Trace.{ blockchain_length; source; status; total_time; _ } =
               item
             in
             { state_hash; blockchain_length; source; status; total_time } )
    in
    let traces =
      Hashtbl.to_alist registry
      |> List.filter ~f:(fun (s, _) ->
             Option.is_none (Hashtbl.find catchup_registry s) )
      |> List.map ~f:(fun (key, item) ->
             let state_hash = Mina_base.State_hash.to_base58_check key in
             let Trace.{ blockchain_length; source; status; total_time; _ } =
               item
             in
             { state_hash; blockchain_length; source; status; total_time } )
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
             let Trace.{ blockchain_length; source; status; total_time; _ } =
               item
             in
             { state_hash; blockchain_length; source; status; total_time } )
      |> List.sort ~compare:(fun a b ->
             Mina_numbers.Length.compare a.blockchain_length b.blockchain_length )
    in
    { traces; produced_traces }

  let push_entry ~status ~source ?blockchain_length block_id entry =
    Hashtbl.update registry block_id
      ~f:(Trace.push ~status ~source ?blockchain_length entry)

  let checkpoint ?(status = `Pending) ?metadata ~source ?blockchain_length
      (block_id : Mina_base.State_hash.t) checkpoint =
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
    (* FIXME: not a valid conversion *)
    let blockchain_length =
      Mina_numbers.Length.of_int @@ Mina_numbers.Global_slot.to_int slot
    in
    Hashtbl.update produced_registry slot
      ~f:(Trace.push ~status ~blockchain_length ~source:`Internal entry)

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
