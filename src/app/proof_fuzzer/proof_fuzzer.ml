open Core
open Async
open Mina_base
open Mina_transaction
module Rust = Mina_tree.Rust
module Fp = Kimchi_pasta.Basic.Fp
module Ledger = Mina_ledger.Ledger
module Length = Mina_numbers.Length
module Global_slot = Mina_numbers.Global_slot

let ledger = ref (Mina_ledger.Ledger.create_ephemeral ~depth:10 ())

let constraint_constants : Genesis_constants.Constraint_constants.t ref =
  ref Genesis_constants.Constraint_constants.compiled

let genesis_proof : Genesis_proof.t option ref = ref None

let verifier : Verifier.Prod.Worker_state.t option ref = ref None

let deserialize_constants constants_bytes =
  Bin_prot.Reader.of_bytes
    [%bin_reader: Genesis_constants.Constraint_constants.t] constants_bytes

let set_constraint_constants constants_bytes =
  constraint_constants := deserialize_constants constants_bytes

let create_initial_accounts accounts =
  let constraint_constants = !constraint_constants in
  Genesis_ledger_helper.Ledger.packed_genesis_ledger_of_accounts
    ~depth:constraint_constants.ledger_depth
    (lazy (List.map ~f:(fun a -> (None, a)) accounts))

let deserialize_accounts accounts_bytes =
  Bin_prot.Reader.of_bytes [%bin_reader: Account.Stable.Latest.t list]
    accounts_bytes

let set_initial_accounts accounts_bytes =
  try
    let logger = Logger.null () in
    let accounts = deserialize_accounts accounts_bytes in
    let ledger_packed = create_initial_accounts accounts in
    ledger := Lazy.force (Genesis_ledger.Packed.t ledger_packed) ;
    let ledger_hash = Ledger.merkle_root !ledger in

    let constraint_constants = !constraint_constants in

    let inputs = Genesis_ledger_helper.Genesis_proof.generate_inputs
      ~runtime_config:Runtime_config.default
      ~proof_level:Genesis_constants.Proof_level.Full
      ~ledger:ledger_packed
      ~constraint_constants
      ~genesis_constants:Genesis_constants.compiled
      ~blockchain_proof_system_id:None
      ~genesis_epoch_data:Consensus.Genesis_epoch_data.compiled in
      genesis_proof :=
        (match Async.Thread_safe.block_on_async_exn (fun () ->
          Genesis_ledger_helper.init_from_inputs ~logger inputs)
        with
        | Ok genesis_proof -> Some genesis_proof
        | Error _ ->
            failwith "Error computing genesis proof") ;

    Bin_prot.Writer.to_bytes [%bin_writer: Fp.t] ledger_hash
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

let get_genesis_protocol_state _ =
  try
    let genesis_proof =
      match !genesis_proof with
      | Some genesis_proof ->
          genesis_proof
      | None ->
          failwith "genesis_proof not initialized"
    in

  let protocol_state = genesis_proof.protocol_state_with_hashes.data.body in
  Bin_prot.Writer.to_bytes [%bin_writer: Mina_state.Protocol_state.Body.Value.Stable.Latest.t] protocol_state
with e ->
  let bt = Printexc.get_backtrace () in
  let msg = Exn.to_string e in
  Core_kernel.printf !"except: %s\n%s\n%!" msg bt ;
  raise e

(* let to_hex_string bytes = "0x" ^ (String.uppercase (Hex.encode ~reverse:false (Bytes.to_string bytes))) *)

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
          setup false ;
          Option.value_exn !transaction_pool
          (* failwith "transaction pool not setup" *)
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
      verify_and_apply_impl [ cmd ]
    with e ->
      let bt = Printexc.get_backtrace () in
      let msg = Exn.to_string e in
      Core_kernel.printf !"except: %s\n%s\n%!" msg bt ;
      raise e
end

let parse_create_tx_witness_inputs input_bytes =
  let message, input, w = Base.(Bin_prot.Reader.of_bytes [%bin_reader: (Mina_base.Sok_message.Stable.Latest.t * Mina_state.Snarked_ledger_state.Stable.Latest.t * Transaction_witness.Stable.Latest.t)]
    input_bytes) in
  let sok_digest = Mina_base.Sok_message.digest message in
  input, w, sok_digest

let create_tx_witness input_bytes =
  try
    let constraint_constants = !constraint_constants in
    let input, w, _sok_digest = parse_create_tx_witness_inputs input_bytes in
    match w.transaction with
    | Command (Zkapp_command zkapp_command) -> (
      let witnesses_specs_stmts =
        Or_error.try_with (fun () ->
            Transaction_snark.zkapp_command_witnesses_exn
              ~constraint_constants
              ~global_slot:w.block_global_slot
              ~state_body:w.protocol_state_body
              ~fee_excess:Currency.Amount.Signed.zero
              [ ( `Pending_coinbase_init_stack w.init_stack
                , `Pending_coinbase_of_statement
                    { Transaction_snark
                      .Pending_coinbase_stack_state
                      .source =
                        input.source.pending_coinbase_stack
                    ; target =
                        input.target.pending_coinbase_stack
                    }
                , `Sparse_ledger w.first_pass_ledger
                , `Sparse_ledger w.second_pass_ledger
                , `Connecting_ledger_hash
                    input.connecting_ledger_left
                , zkapp_command )
              ]
            |> List.rev )
        |> Result.map_error ~f:(fun e ->
                Error.createf
                  !"Failed to generate inputs for \
                    zkapp_command : %s: %s"
                  ( Zkapp_command.to_yojson zkapp_command
                  |> Yojson.Safe.to_string )
                  (Error.to_string_hum e) ) in
        match Or_error.ok_exn witnesses_specs_stmts with
          | [] ->
              failwith "no witnesses generated"
          | list ->
            Bin_prot.Writer.to_bytes [%bin_writer: (
              Transaction_snark.Zkapp_command_segment.Witness.Stable.Latest.t
              * Transaction_snark.Zkapp_command_segment.Basic.Stable.Latest.t
              * Transaction_snark.Statement.With_sok.Stable.Latest.t ) list] list
    )
    | _ -> failwith "witness generation for nonzkapp tx not implemented"
  with e ->
    let bt = Printexc.get_backtrace () in
    let msg = Exn.to_string e in
    Core_kernel.printf !"except: %s\n%s\n%!" msg bt ;
    raise e

let snark_module =
  lazy
    ( module Transaction_snark.Make (struct
      let constraint_constants = !constraint_constants

      let proof_level = Genesis_constants.Proof_level.Full
    end) : Transaction_snark.S )

let create_tx_proof input_bytes =
  try
    let module M = (val Lazy.force snark_module) in
    let input, w, sok_digest = parse_create_tx_witness_inputs input_bytes in

    match w.transaction with
    | Command (Zkapp_command zkapp_command) -> (
      let witnesses_specs_stmts =
        Or_error.try_with (fun () ->
            Transaction_snark.zkapp_command_witnesses_exn
              ~constraint_constants:M.constraint_constants
              ~global_slot:w.block_global_slot
              ~state_body:w.protocol_state_body
              ~fee_excess:Currency.Amount.Signed.zero
              [ ( `Pending_coinbase_init_stack w.init_stack
                , `Pending_coinbase_of_statement
                    { Transaction_snark
                      .Pending_coinbase_stack_state
                      .source =
                        input.source.pending_coinbase_stack
                    ; target =
                        input.target.pending_coinbase_stack
                    }
                , `Sparse_ledger w.first_pass_ledger
                , `Sparse_ledger w.second_pass_ledger
                , `Connecting_ledger_hash
                    input.connecting_ledger_left
                , zkapp_command )
              ]
            |> List.rev )
        |> Result.map_error ~f:(fun e ->
                Error.createf
                  !"Failed to generate inputs for \
                    zkapp_command : %s: %s"
                  ( Zkapp_command.to_yojson zkapp_command
                  |> Yojson.Safe.to_string )
                  (Error.to_string_hum e) ) in
        match Or_error.ok_exn witnesses_specs_stmts with
          | [] ->
              failwith "no witnesses generated"
          | (witness, spec, stmt) :: rest -> (
              (* let t = Stdlib.Sys.time () in *)
              let p1 =
                (M.of_zkapp_command_segment_exn ~witness ~statement:{ stmt with sok_digest } ~spec)
              in
              let (p1 : Ledger_proof.t) = Async.Thread_safe.block_on_async_exn (fun () -> p1) in
              let p =
                Deferred.List.fold ~init:p1 rest
                  ~f:(fun _acc (witness, spec, stmt) ->
                    (* let%bind (prev : Ledger_proof.t) =
                      Deferred.return acc
                    in
                    let%bind (curr : Ledger_proof.t) = *)
                    (M.of_zkapp_command_segment_exn ~witness ~statement:{ stmt with sok_digest } ~spec)
                    (* in
                    Deferred.Or_error.ok_exn (M.merge ~sok_digest prev curr) *)
                  )
              in
              let p = Async.Thread_safe.block_on_async_exn (fun () -> p) in
              (* (if
                not (Transaction_snark.Statement.equal
                  (Ledger_proof.statement p) input)
              then
                failwith "transaction snark statement mismatch") ; *)
              (* Core_kernel.printf "Time for producing proof: %f\n" ((Stdlib.Sys.time ()) -. t) ; *)
              Bin_prot.Writer.to_bytes [%bin_writer: Ledger_proof.Stable.Latest.t] p))
    | _ -> (
      let (`If_this_is_used_it_should_have_a_comment_justifying_it tx) = Transaction.to_valid_unsafe w.transaction in
      let res = M.of_non_zkapp_command_transaction
          ~statement:{ input with sok_digest }
          { Transaction_protocol_state.Poly.transaction =
              tx
          ; block_data = w.protocol_state_body
          ; global_slot = w.block_global_slot
          }
          ~init_stack:w.init_stack
          (unstage
              (Mina_ledger.Sparse_ledger.handler
                w.first_pass_ledger ) ) in
      let res = Async.Thread_safe.block_on_async_exn (fun () -> res) in
      Bin_prot.Writer.to_bytes [%bin_writer: Ledger_proof.Stable.Latest.t] res
    )
  with e ->
    let bt = Printexc.get_backtrace () in
    let msg = Exn.to_string e in
    failwith (Core_kernel.sprintf !"except: %s\n%s\n%!" msg bt)

let check_proof proof_bytes =
  try
    let logger = Logger.null () in
    let proof =
      Base.(Bin_prot.Reader.of_bytes [%bin_reader: (Ledger_proof.Stable.Latest.t * Mina_base.Sok_message.Stable.Latest.t)]
        proof_bytes) in
    let constraint_constants = !constraint_constants in
    let verifier =
      match !verifier with
      | Some verifier ->
          verifier
      | None ->
          let v = Async.Thread_safe.block_on_async_exn (fun () ->
            Verifier.Prod.Worker_state.create { logger; constraint_constants;
            proof_level = Genesis_constants.Proof_level.Full; conf_dir = None;})
          in
          verifier := Some v ;
          v
    in
    let verify_transaction_snarks (w : Verifier.Prod.Worker_state.t) ts =
      let (module M) = Verifier.Prod.Worker_state.get w in
      M.verify_transaction_snarks ts in
    let res = verify_transaction_snarks verifier [proof] in
    let res = Async.Thread_safe.block_on_async_exn (fun () -> res) in
    match res with
    | Ok () -> 0
    | Error _err -> 1
      (* failwith (Error.to_string_hum err) *)
  with e ->
    let bt = Printexc.get_backtrace () in
    let msg = Exn.to_string e in
    Core_kernel.printf !"except: %s\n%s\n%!" msg bt ;
    raise e

let apply_tx user_command_bytes =
  try
    let command =
      Bin_prot.Reader.of_bytes [%bin_reader: User_command.Stable.Latest.t]
        user_command_bytes
    in
    let tx = Transaction.Command command in
    let constraint_constants = !constraint_constants in
    let ledger = !ledger in
    let genesis_proof = Option.value_exn !genesis_proof in
    let protocol_state = genesis_proof.protocol_state_with_hashes.data.body in
    let txn_state_view = Mina_state.Protocol_state.Body.view protocol_state in
    let _applied =
      Ledger.apply_transactions ~constraint_constants
        ~global_slot:txn_state_view.global_slot_since_genesis ~txn_state_view
        ledger [ tx ]
    in
    let ledger_hash = Ledger.merkle_root ledger in
    Bin_prot.Writer.to_bytes [%bin_writer: Fp.t] ledger_hash
  with e ->
    let bt = Printexc.get_backtrace () in
    let msg = Exn.to_string e in
    Core_kernel.printf !"except: %s\n%s\n%!" msg bt ;
    raise e

let run_command =
  Command.basic ~summary:"Run the fuzzer"
    Command.Param.(
      map
        (anon ("seed" %: int))
        ~f:(fun seed () ->
          let _ = create_tx_witness in
          Rust.proof_fuzzer
            (Int64.of_int seed)
            set_constraint_constants
            set_initial_accounts
            get_genesis_protocol_state
            create_tx_proof
            check_proof
            get_coverage
          ))

let run_tx_witness_generation_fuzzer_command =
  Command.basic ~summary:"Run the transaction witness generation fuzzer"
    Command.Param.(
      map
        (anon ("seed" %: int))
        ~f:(fun seed () ->
          Rust.tx_witness_generation_fuzzer
            (Int64.of_int seed)
            set_constraint_constants
            set_initial_accounts
            get_genesis_protocol_state
            Transaction_pool.verify_and_apply
            apply_tx
            create_tx_proof
            get_coverage
          ))

let () =
  Command.run
    (Command.group ~summary:"proof_fuzzer"
      [
        ("run", run_command)
        ; ("run-tx-witness-generation-fuzzer", run_tx_witness_generation_fuzzer_command)
        ; (Parallel.worker_command_name, Parallel.worker_command)
      ] )
