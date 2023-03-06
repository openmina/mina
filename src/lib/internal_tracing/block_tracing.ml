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
  type t = (string, Trace.t) Hashtbl.t

  type produced_registry = (int, Trace.t) Hashtbl.t

  type trace_info =
    { source : Trace.block_source
    ; blockchain_length : int
    ; state_hash : string
    ; status : Trace.status
    ; started_at : float
    ; total_time : float
    ; metadata : Yojson.Safe.t
    }
  [@@deriving to_yojson]

  let trace_info_to_yojson t =
    let blockchain_length_int = t.blockchain_length in
    match trace_info_to_yojson t with
    | `Assoc fields ->
        `Assoc (("blockchain_length_int", `Int blockchain_length_int) :: fields)
    | other ->
        other

  type traces = { traces : trace_info list; produced_traces : trace_info list }
  [@@deriving to_yojson]

  let registry : t = Hashtbl.create (module String)

  let catchup_registry : t = Hashtbl.create (module String)

  let produced_registry : produced_registry = Hashtbl.create (module Int)

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
    ; metadata = regular.metadata (* TODO: igoring catchup metadata *)
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
    if String.length key > 50 then find_trace key
    else try find_produced_trace (Int.of_string key) with _ -> None

  (* TODO: cleanup this and find a better way *)
  let all_traces ?max_length ?height () =
    let matches_height blockchain_length =
      Option.value_map ~default:true ~f:(( = ) blockchain_length) height
    in
    let catchup_traces =
      Hashtbl.to_alist catchup_registry
      |> List.filter_map ~f:(fun (state_hash, item) ->
             let Trace.
                   { blockchain_length
                   ; source
                   ; status
                   ; total_time
                   ; metadata
                   ; _
                   } =
               item
             in
             if matches_height blockchain_length then
               Some
                 { state_hash
                 ; blockchain_length
                 ; source
                 ; status
                 ; started_at = Trace.started_at item
                 ; total_time
                 ; metadata
                 }
             else None )
    in
    let traces =
      Hashtbl.to_alist registry
      |> List.filter ~f:(fun (s, _) ->
             Option.is_none (Hashtbl.find catchup_registry s) )
      |> List.filter_map ~f:(fun (state_hash, item) ->
             let Trace.
                   { blockchain_length
                   ; source
                   ; status
                   ; total_time
                   ; metadata
                   ; _
                   } =
               item
             in
             if matches_height blockchain_length then
               Some
                 { state_hash
                 ; blockchain_length
                 ; source
                 ; status
                 ; started_at = Trace.started_at item
                 ; total_time
                 ; metadata
                 }
             else None )
    in
    let traces =
      traces @ catchup_traces
      |> List.sort ~compare:(fun a b ->
             Int.compare a.blockchain_length b.blockchain_length )
    in
    let produced_traces =
      Hashtbl.to_alist produced_registry
      |> List.map ~f:(fun (_, item) ->
             let state_hash = "<unknown>" in
             let Trace.
                   { blockchain_length
                   ; source
                   ; status
                   ; total_time
                   ; metadata
                   ; _
                   } =
               item
             in
             { state_hash
             ; blockchain_length
             ; source
             ; status
             ; started_at = Trace.started_at item
             ; total_time
             ; metadata
             } )
      |> List.sort ~compare:(fun a b ->
             Int.compare a.blockchain_length b.blockchain_length )
    in
    match max_length with
    | None ->
        { traces; produced_traces }
    | Some max_length ->
        let traces_count = List.length traces in
        let produced_traces_count = List.length produced_traces in
        let traces = List.drop traces (traces_count - max_length) in
        let produced_traces =
          List.drop produced_traces (produced_traces_count - max_length)
        in
        { traces; produced_traces }

  let push_entry ~status ~source ?blockchain_length block_id entry =
    Hashtbl.update registry block_id
      ~f:(Trace.push ~status ~source ?blockchain_length entry)

  let push_metadata ~metadata block_id =
    Hashtbl.change registry block_id ~f:(Trace.push_metadata ~metadata)

  let push_global_metadata ~metadata block_id =
    Hashtbl.change registry block_id ~f:(Trace.push_global_metadata ~metadata)

  let checkpoint ?started_at ?(status = `Pending) ?metadata ~source
      ?blockchain_length block_id checkpoint =
    push_entry ~status ~source ?blockchain_length block_id
      (Trace.Entry.make ?started_at ?metadata checkpoint)

  let push_catchup_entry ~status ~source ?blockchain_length block_id entry =
    Hashtbl.update catchup_registry block_id
      ~f:(Trace.push ~status ~source ?blockchain_length entry)

  let catchup_checkpoint ?(status = `Pending) ?metadata ~source
      ?blockchain_length block_id checkpoint =
    push_catchup_entry ~status ~source ?blockchain_length block_id
      (Trace.Entry.make ?metadata checkpoint)

  let push_produced_entry ~status slot entry =
    (* FIXME: not a valid conversion *)
    let blockchain_length = slot in
    Hashtbl.update produced_registry slot
      ~f:(Trace.push ~status ~blockchain_length ~source:`Internal entry)

  let produced_checkpoint ?(status = `Pending) ?started_at ?metadata slot
      checkpoint =
    push_produced_entry ~status slot
      (Trace.Entry.make ?started_at ?metadata checkpoint)
end

module Production = struct
  let current_slot_key = Univ_map.Key.create ~name:"current_slot" Int.sexp_of_t

  let with_slot slot f =
    Async_kernel.Async_kernel_scheduler.with_local current_slot_key slot ~f

  let get_current_slot () =
    Async_kernel.Async_kernel_scheduler.find_local current_slot_key

  (* FIXME: only production and processing checkpoints should be allowed *)
  let checkpoint ?started_at ?metadata (checkpoint : Checkpoint.t) =
    Option.iter (get_current_slot ()) ~f:(fun slot ->
        Registry.produced_checkpoint ?started_at ?metadata slot
          (checkpoint :> Checkpoint.t) )

  let push_entry ?(status = `Pending) entry =
    Option.iter (get_current_slot ()) ~f:(fun slot ->
        Registry.push_produced_entry ~status slot entry )

  let begin_block_production () = checkpoint `Begin_block_production

  let end_block_production ?state_hash ~blockchain_length at_checkpoint =
    checkpoint at_checkpoint ;
    Option.iter (get_current_slot ()) ~f:(fun slot ->
        let trace =
          Option.value ~default:(Trace.empty `Internal)
          @@ Registry.find_produced_trace slot
        in
        (* At this point we may know the hash of the produced block, so we get rid of the
           produced trace and register a trace for the state hash so that the next
           pipeline can continue it *)
        match state_hash with
        | None ->
            Hashtbl.set Registry.produced_registry ~key:slot
              ~data:{ trace with blockchain_length; status = `Success }
        | Some state_hash ->
            Hashtbl.remove Registry.produced_registry slot ;
            let trace = { trace with blockchain_length; status = `Success } in
            Hashtbl.update Registry.registry state_hash ~f:(fun _ -> trace) )

  let push_metadata metadata =
    Option.iter (get_current_slot ())
      ~f:
        (Hashtbl.change Registry.produced_registry
           ~f:(Trace.push_metadata ~metadata) )

  let push_global_metadata metadata =
    Option.iter (get_current_slot ())
      ~f:
        (Hashtbl.change Registry.produced_registry
           ~f:(Trace.push_global_metadata ~metadata) )

  module Proof_timings = struct
    let ( ! ) = Stdlib.( ! )

    let ( := ) = Stdlib.( := )

    let ref = Stdlib.ref

    type entry =
      { started_at : float; checkpoint : Checkpoint.t; metadata : string }
    [@@deriving bin_io, yojson]

    type t = entry list [@@deriving bin_io, yojson]

    let apply_to_tracing t =
      List.iter t ~f:(fun v ->
          let metadata =
            match Yojson.Safe.from_string v.metadata with
            | `Assoc v ->
                v
            | _ ->
                assert false
          in
          Trace.Entry.make ~started_at:v.started_at ~metadata v.checkpoint
          |> push_entry )

    let global = ref []

    let push ?metadata ?time t checkpoint =
      let started_at = Option.value time ~default:(Unix.gettimeofday ()) in
      List.append t
        [ { started_at
          ; checkpoint
          ; metadata = Option.value metadata ~default:"{}"
          }
        ]

    let push_global ?metadata ?time checkpoint =
      global := push !global checkpoint ?metadata ?time

    let reset_global () = global := []

    let take_global () =
      let timings = !global in
      reset_global () ; timings
  end
end

module External = struct
  let current_state_hash_key =
    Univ_map.Key.create ~name:"current_state_hash" String.sexp_of_t

  let with_state_hash state_hash f =
    Async_kernel.Async_kernel_scheduler.with_local current_state_hash_key
      state_hash ~f

  let get_current_state_hash () =
    Async_kernel.Async_kernel_scheduler.find_local current_state_hash_key

  let checkpoint = Registry.checkpoint ~source:`External

  let checkpoint_current ?status ?metadata ?blockchain_length checkpoint_name =
    match get_current_state_hash () with
    | None ->
        ()
    | Some state_hash ->
        checkpoint ?status ?metadata ?blockchain_length state_hash
          checkpoint_name

  let failure ~reason state_hash =
    checkpoint
      ~metadata:[ ("reason", `String reason) ]
      ~status:`Failure state_hash `Failure

  let failure_current ~reason =
    checkpoint_current
      ~metadata:[ ("reason", `String reason) ]
      ~status:`Failure `Failure

  let complete state_hash =
    checkpoint ~status:`Success state_hash `Validate_transition_complete
end

(* TODO: some processing can happen during block production, account for that *)
module Processing = struct
  let current_state_hash_key =
    Univ_map.Key.create ~name:"current_state_hash" String.sexp_of_t

  let with_state_hash state_hash f =
    Async_kernel.Async_kernel_scheduler.with_local current_state_hash_key
      state_hash ~f

  let get_current_state_hash () =
    Async_kernel.Async_kernel_scheduler.find_local current_state_hash_key

  let parent_registry = Hashtbl.create (module String)

  let register_parent ~state_hash ~parent_hash =
    assert (not (String.equal state_hash parent_hash)) ;
    ignore @@ Hashtbl.add parent_registry ~data:state_hash ~key:parent_hash

  let get_registered_child parent_hash =
    Hashtbl.find parent_registry parent_hash

  let checkpoint ?(source = `Unknown) = Registry.checkpoint ~source

  (* FIXME: should only allow processing checkpoints *)
  let checkpoint_current ?status ?metadata ?source ?blockchain_length
      checkpoint_name =
    match (Production.get_current_slot (), get_current_state_hash ()) with
    | Some _, _ ->
        (* TODO: verify that this is safe to do, also find a better alternative *)
        Production.checkpoint ?metadata checkpoint_name
    | None, None ->
        ()
    | None, Some block_id ->
        checkpoint ?status ?metadata ?source ?blockchain_length block_id
          checkpoint_name

  let push_metadata metadata =
    match (Production.get_current_slot (), get_current_state_hash ()) with
    | Some _, _ ->
        (* TODO: verify that this is safe to do, also find a better alternative *)
        Production.push_metadata metadata
    | None, None ->
        ()
    | None, Some block_id ->
        Registry.push_metadata ~metadata block_id

  let push_global_metadata metadata =
    match (Production.get_current_slot (), get_current_state_hash ()) with
    | Some _, _ ->
        (* TODO: verify that this is safe to do, also find a better alternative *)
        Production.push_global_metadata metadata
    | None, None ->
        ()
    | None, Some block_id ->
        Registry.push_global_metadata ~metadata block_id

  let failure ~reason state_hash =
    (* TODOX: compute structured version and save it *)
    checkpoint
      ~metadata:[ ("reason", `String reason) ]
      ~status:`Failure state_hash `Failure

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
  let with_state_hash = Processing.with_state_hash

  let checkpoint = Registry.checkpoint ~source:`Reconstruct

  let checkpoint_current ?status ?metadata ?blockchain_length checkpoint_name =
    match Processing.get_current_state_hash () with
    | None ->
        ()
    | Some block_id ->
        checkpoint ?status ?metadata ?blockchain_length block_id checkpoint_name

  let failure = Processing.failure

  let complete = Processing.complete
end
