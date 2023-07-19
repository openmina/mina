open Core
open Async
open Mina_base
open Signature_lib
module Rust = Mina_tree.Rust
module Fp = Kimchi_pasta.Basic.Fp
module Ledger = Mina_ledger.Ledger
module Length = Mina_numbers.Length
module Global_slot = Mina_numbers.Global_slot
module Real_staged_ledger = Staged_ledger

let ledger = ref (Mina_ledger.Ledger.create_ephemeral ~depth:10 ())

let constraint_constants : Genesis_constants.Constraint_constants.t ref =
  ref Genesis_constants.Constraint_constants.compiled

let staged_ledger = ref None

module Staged_ledger = struct
  type t = Mina_ledger.Ledger.t [@@deriving sexp]

  let ledger = Fn.id
end

module Mock_transition_frontier = struct
  open Pipe_lib

  module Breadcrumb = struct
    type t = Staged_ledger.t

    let staged_ledger = Fn.id
  end

  type best_tip_diff =
    { new_commands : User_command.Valid.t With_status.t list
    ; removed_commands : User_command.Valid.t With_status.t list
    ; reorg_best_tip : bool
    }

  type t = best_tip_diff Broadcast_pipe.Reader.t * Breadcrumb.t ref

  let create : unit -> t * best_tip_diff Broadcast_pipe.Writer.t =
   fun () ->
    let pipe_r, pipe_w =
      Broadcast_pipe.create
        { new_commands = []; removed_commands = []; reorg_best_tip = false }
    in
    ((pipe_r, ledger), pipe_w)

  let best_tip (_, best_tip) = !best_tip

  let best_tip_diff_pipe (pipe, _) = pipe
end

let transaction_pool = ref None

let deserialize_constants constants_bytes =
  Bin_prot.Reader.of_bytes
    [%bin_reader: Genesis_constants.Constraint_constants.t] constants_bytes

let verifier = ref None

let set_constraint_constants constants_bytes =
  try
    constraint_constants := deserialize_constants constants_bytes ;
    (* TODO move elsewhere *)
    Parallel.init_master () ;
    let proof_level = Genesis_constants.Proof_level.None in
    let logger = Logger.null () in
    let v =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Verifier.create ~logger ~proof_level
            ~constraint_constants:!constraint_constants ~conf_dir:(Some "/tmp")
            ~pids:(Child_processes.Termination.create_pid_table ()) )
    in
    verifier := Some v
  with e ->
    let bt = Printexc.get_backtrace () in
    let msg = Exn.to_string e in
    Core_kernel.printf !"except: %s\n%s\n%!" msg bt ;
    raise e

let create_initial_accounts accounts =
  let constraint_constants = !constraint_constants in
  let packed =
    Genesis_ledger_helper.Ledger.packed_genesis_ledger_of_accounts
      ~depth:constraint_constants.ledger_depth
      (lazy (List.map ~f:(fun a -> (None, a)) accounts))
  in
  Lazy.force (Genesis_ledger.Packed.t packed)

let deserialize_accounts accounts_bytes =
  Bin_prot.Reader.of_bytes [%bin_reader: Account.Stable.Latest.t list]
    accounts_bytes

let set_initial_accounts accounts_bytes =
  let ledger_ = create_initial_accounts (deserialize_accounts accounts_bytes) in
  ledger := ledger_ ;
  staged_ledger :=
    Some
      (Real_staged_ledger.create_exn ~constraint_constants:!constraint_constants
         ~ledger:ledger_ ) ;
  let ledger_hash = Ledger.merkle_root ledger_ in
  Bin_prot.Writer.to_bytes [%bin_writer: Fp.t] ledger_hash

module CreateDiffArgs = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t =
        { txns : User_command.Stable.V2.t list
        ; global_slot : Mina_numbers.Global_slot.Stable.V1.t
        ; prover : Public_key.Compressed.Stable.V1.t
        ; coinbase_receiver : Public_key.Compressed.Stable.V1.t
        ; current_state : Mina_state.Protocol_state.Value.Stable.V2.t
        }

      let to_latest = Fn.id
    end
  end]
end

module CreateDiffResult = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t =
        ( Staged_ledger_diff.Stable.V2.t
        * (User_command.Stable.V2.t * string) list )
        option
        * string option
      [@@ocaml.warning "-34"]

      let to_latest = Fn.id
    end
  end]
end

let create_diff args_serialized =
  try
    let CreateDiffArgs.
          { txns; global_slot; prover; coinbase_receiver; current_state } =
      Bin_prot.Reader.of_bytes [%bin_reader: CreateDiffArgs.Stable.Latest.t]
        args_serialized
    in
    let logger = Logger.null () in
    let current_state_view =
      Mina_state.Protocol_state.Body.view current_state.body
    in
    let transactions_by_fee =
      Sequence.of_list
        (List.map txns ~f:(fun tx ->
             match User_command.to_valid_unsafe tx with
             | `If_this_is_used_it_should_have_a_comment_justifying_it tx ->
                 tx ) )
    in
    let Genesis_constants.Constraint_constants.{ account_creation_fee = fee; _ }
        =
      !constraint_constants
    in
    let stmt_to_work_random_prover stmt =
      let proofs =
        One_or_two.map stmt ~f:(fun statement ->
            Ledger_proof.create ~statement
              ~sok_digest:Sok_message.Digest.default
              ~proof:Proof.transaction_dummy )
      in
      Option.some
        (Transaction_snark_work.Checked.create_unsafe { fee; proofs; prover })
    in
    let staged_ledger =
      match !staged_ledger with
      | Some ledger ->
          ledger
      | None ->
          failwith "staged ledger not initialized"
    in
    let diff_result =
      Real_staged_ledger.create_diff ~constraint_constants:!constraint_constants
        ~global_slot staged_ledger ~log_block_creation:false ~coinbase_receiver
        ~logger ~current_state_view ~transactions_by_fee
        ~get_completed_work:stmt_to_work_random_prover
        ~supercharge_coinbase:false
    in
    let res =
      match diff_result with
      | Ok (diff, invalid_txns) ->
          let invalid_cmds =
            List.map invalid_txns ~f:(fun (tx, e) ->
                (User_command.forget_check tx, Error.to_string_hum e) )
          in
          let diff = Staged_ledger_diff.forget diff in
          (Some (diff, invalid_cmds), None)
      | Error e ->
          (None, Some (Real_staged_ledger.Pre_diff_info.Error.to_string e))
    in
    Bin_prot.Writer.to_bytes [%bin_writer: CreateDiffResult.Stable.Latest.t] res
  with e ->
    let bt = Printexc.get_backtrace () in
    let msg = Exn.to_string e in
    Core_kernel.printf !"except: %s\n%s\n%!" msg bt ;
    raise e

module ApplyDiffArgs = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t =
        { diff : Staged_ledger_diff.Stable.V2.t
        ; global_slot : Mina_numbers.Global_slot.Stable.V1.t
        ; coinbase_receiver : Public_key.Compressed.Stable.V1.t
        ; current_state : Mina_state.Protocol_state.Value.Stable.V2.t
        ; state_hashes : Fp.Stable.V1.t * Fp.Stable.V1.t
        }

      let to_latest = Fn.id
    end
  end]
end

let apply_diff args_serialized =
  try
    let ApplyDiffArgs.
          { diff; global_slot; coinbase_receiver; current_state; state_hashes }
        =
      Bin_prot.Reader.of_bytes [%bin_reader: ApplyDiffArgs.Stable.Latest.t]
        args_serialized
    in
    let logger = Logger.null () in
    let current_state_view =
      Mina_state.Protocol_state.Body.view current_state.body
    in
    let verifier =
      match !verifier with
      | Some verifier ->
          verifier
      | None ->
          failwith "Verifier not set"
    in
    let _staged_ledger =
      match !staged_ledger with
      | Some ledger ->
          ledger
      | None ->
          failwith "staged ledger not initialized"
    in
    let open Deferred.Let_syntax in
    let result =
      Real_staged_ledger.apply ?skip_verification:None
        ~constraint_constants:!constraint_constants ~global_slot _staged_ledger
        diff ~logger ~verifier ~current_state_view
        ~state_and_body_hash:state_hashes ~coinbase_receiver
        ~supercharge_coinbase:false
    in
    let res =
      Thread_safe.block_on_async_exn (fun () ->
          let%map unwrapped_result = result in
          match unwrapped_result with
          | Ok value ->
              let ( `Hash_after_applying staged_ledger_hash
                  , `Ledger_proof _
                  , `Staged_ledger ledger
                  , `Pending_coinbase_update _ ) =
                value
              in
              staged_ledger := Some ledger;
              Some staged_ledger_hash
          | Error _ ->
              None )
    in
    Bin_prot.Writer.to_bytes
      [%bin_writer: Staged_ledger_hash.Stable.Latest.t option] res
  with e ->
    let bt = Printexc.get_backtrace () in
    let msg = Exn.to_string e in
    Core_kernel.printf !"except: %s\n%s\n%!" msg bt ;
    raise e

let get_coverage _ =
  List.map (Bisect.Runtime.get_coverage_flattened ())
    ~f:(fun { filename; points; counts } ->
      ( filename
      , Array.fold_right points ~f:(fun x acc -> Int64.of_int x :: acc) ~init:[]
      , Array.fold_right counts ~f:(fun x acc -> Int64.of_int x :: acc) ~init:[]
      ) )

module Transaction_pool = struct
  module Transaction_pool =
    Network_pool.Transaction_pool.Make
      (Staged_ledger)
      (Mock_transition_frontier)

  let setup with_logging =
    Parallel.init_master () ;
    let logger = if with_logging then Logger.create () else Logger.null () in
    let precomputed_values = Lazy.force Precomputed_values.for_unit_tests in
    let constraint_constants = !constraint_constants in
    (* TODO: are these constants ok? *)
    let consensus_constants = precomputed_values.consensus_constants in
    let time_controller = Block_time.Controller.basic ~logger in
    (* TODO: make proof level configurable *)
    let proof_level = Genesis_constants.Proof_level.None in
    let frontier, _best_tip_diff_w = Mock_transition_frontier.create () in
    let _, _best_tip_ref = frontier in
    let frontier_pipe_r, _frontier_pipe_w =
      Pipe_lib.Broadcast_pipe.create @@ Some frontier
    in
    let trust_system = Trust_system.null () in
    [%log info] "Starting verifier..." ;
    let verifier =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Verifier.create ~logger ~proof_level ~constraint_constants
            ~conf_dir:(Some "/tmp")
            ~pids:(Child_processes.Termination.create_pid_table ()) )
    in
    let config =
      Transaction_pool.Resource_pool.make_config ~trust_system
        ~pool_max_size:3000 ~verifier
        ~genesis_constants:Genesis_constants.compiled
    in
    [%log info] "Creating transaction pool..." ;
    let pool, _rsink, _lsink =
      Transaction_pool.create ~config ~logger ~constraint_constants
        ~consensus_constants ~time_controller
        ~frontier_broadcast_pipe:frontier_pipe_r ~log_gossip_heard:false
        ~on_remote_push:(Fn.const Async.Deferred.unit)
    in
    transaction_pool := Some pool

  let verify_and_apply_impl cs =
    let pool =
      match !transaction_pool with
      | None ->
          failwith "transaction pool not setup"
      | Some pool ->
          pool
    in
    let peer =
      Network_peer.Peer.create
        (Unix.Inet_addr.of_string "1.2.3.4")
        ~peer_id:(Network_peer.Peer.Id.unsafe_of_string "not used")
        ~libp2p_port:8302
    in
    (* For logal use Network_peer.Envelope.Sender.Local *)
    let sender = Network_peer.Envelope.Sender.Remote peer in
    let diff = Network_peer.Envelope.Incoming.wrap ~data:cs ~sender in
    let result =
      Async.Thread_safe.block_on_async_exn
      @@ fun () ->
      Transaction_pool.Resource_pool.Diff.verify
        (Transaction_pool.resource_pool pool)
        diff
    in
    match result with
    | Ok diff -> (
        let result =
          Async.Thread_safe.block_on_async_exn
          @@ fun () ->
          Transaction_pool.Resource_pool.Diff.unsafe_apply
            (Transaction_pool.resource_pool pool)
            diff
        in
        match result with
        | Ok (`Accept, _, _) ->
            true
        | Ok (`Reject, _, _) | Error _ ->
            false )
    | Error _ ->
        false

  let verify_and_apply encoded =
    try
      let cmd =
        Bin_prot.Reader.of_string User_command.Stable.V2.bin_reader_t encoded
      in
      (*Core_kernel.printf !"%{sexp:User_command.t}\n%!" cmd;*)
      verify_and_apply_impl [ cmd ]
    with e ->
      let bt = Printexc.get_backtrace () in
      let msg = Exn.to_string e in
      Core_kernel.printf !"except: %s\n%s\n%!" msg bt ;
      raise e
end

let run_command =
  Command.basic ~summary:"Run the fuzzer"
    Command.Let_syntax.(
      let%map_open seed = anon ("seed" %: int)
      and break_on_invariant =
        flag "invariant-break" (optional bool)
          ~doc:
            "Save fuzzcase and terminate fuzzer on invariant violations \
             (default=false)"
      in
      fun () ->
        let break_on_invariant =
          match break_on_invariant with Some b -> b | None -> false
        in
        let minimum_fee = Mina_compile_config.minimum_user_command_fee in
        Rust.staged_ledger_fuzzer break_on_invariant (Int64.of_int seed)
          (Int64.of_int (Currency.Fee.to_nanomina_int minimum_fee))
          set_constraint_constants set_initial_accounts create_diff apply_diff
          get_coverage Transaction_pool.setup Transaction_pool.verify_and_apply)

let () =
  Command.run
    (Command.group ~summary:"staged_ledger_fuzzer"
       [ ("run", run_command)
       ; (Parallel.worker_command_name, Parallel.worker_command)
       ] )
