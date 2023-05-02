open Core
open Mina_base
open Mina_transaction
module Rust = Mina_tree.Rust
module Fp = Kimchi_pasta.Basic.Fp
module Ledger = Mina_ledger.Ledger
module Length = Mina_numbers.Length
module Global_slot = Mina_numbers.Global_slot

let txn_state_view : Zkapp_precondition.Protocol_state.View.t =
  { snarked_ledger_hash =
      Fp.of_string
        "19095410909873291354237217869735884756874834695933531743203428046904386166496"
      (*; timestamp = Block_time.of_int64(1600251300000L)*)
  ; blockchain_length = Length.of_int 1
  ; last_vrf_output = ()
  ; min_window_density = Length.of_int 77
  ; total_currency =
      Currency.Amount.of_string "10016100000000000"
      (*; global_slot_since_hard_fork = Global_slot.of_int(0)*)
  ; global_slot_since_genesis = Global_slot.of_int 0
  ; staking_epoch_data =
      { ledger =
          { hash =
              Fp.of_string
                "19095410909873291354237217869735884756874834695933531743203428046904386166496"
          ; total_currency = Currency.Amount.of_string "10016100000000000"
          }
      ; seed = Mina_base.Epoch_seed.of_decimal_string "0"
      ; start_checkpoint = State_hash.zero
      ; lock_checkpoint = State_hash.zero
      ; epoch_length = Length.of_int 1
      }
  ; next_epoch_data =
      { ledger =
          { hash =
              Fp.of_string
                "19095410909873291354237217869735884756874834695933531743203428046904386166496"
          ; total_currency = Currency.Amount.of_string "10016100000000000"
          }
      ; seed =
          Fp.of_string
            "18512313064034685696641580142878809378857342939026666126913761777372978255172"
      ; start_checkpoint = State_hash.zero
      ; lock_checkpoint =
          Fp.of_string
            "9196091926153144288494889289330016873963015481670968646275122329689722912273"
      ; epoch_length = Length.of_int 2
      }
  }

let ledger = ref (Mina_ledger.Ledger.create_ephemeral ~depth:10 ())

let constraint_constants : Genesis_constants.Constraint_constants.t ref =
  ref Genesis_constants.Constraint_constants.compiled

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

let set_constraint_constants constants_bytes =
  constraint_constants := deserialize_constants constants_bytes

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
  let ledger_hash = Ledger.merkle_root ledger_ in
  Bin_prot.Writer.to_bytes [%bin_writer: Fp.t] ledger_hash

let apply_tx user_command_bytes =
  try
    let command =
      Bin_prot.Reader.of_bytes [%bin_reader: User_command.Stable.Latest.t]
        user_command_bytes
    in
    (*Core_kernel.printf !"%{sexp:User_command.t}\n%!" command;*)
    let tx = Transaction.Command command in
    let constraint_constants = !constraint_constants in
    let ledger = !ledger in
    (*
     let apply_transactions ~constraint_constants ~global_slot ~txn_state_view
      ledger txns

    *)
    let _applied =
      Ledger.apply_transactions ~constraint_constants
        ~global_slot:txn_state_view.global_slot_since_genesis ~txn_state_view
        ledger [ tx ]
    in
    (*Core_kernel.printf !"%{sexp:Ledger.Transaction_applied.t Or_error.t}\n%!" applied;*)
    let ledger_hash = Ledger.merkle_root ledger in
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
    let cmd =
      Bin_prot.Reader.of_string User_command.Stable.V2.bin_reader_t encoded
    in
    verify_and_apply_impl [ cmd ]
end

let run_command =
  Command.basic ~summary:"Run the fuzzer"
    Command.Let_syntax.(
      let%map_open
        seed = anon ("seed" %: int)
      and
        break_on_invariant = flag "invariant-break" (optional bool) ~doc:"Save fuzzcase and terminate fuzzer on invariant violations (default=false)"
      in
      fun () ->
        let break_on_invariant = match break_on_invariant with
          | Some b -> b
          | None -> false
        in
        let minimum_fee = Mina_compile_config.minimum_user_command_fee in
          Rust.transaction_fuzzer break_on_invariant (Int64.of_int seed) (Int64.of_int (Currency.Fee.to_mina_int minimum_fee)) set_constraint_constants
          set_initial_accounts apply_tx get_coverage Transaction_pool.setup
          Transaction_pool.verify_and_apply
        )

let reproduce_command =
  Command.basic ~summary:"Reproduce fuzzcase"
    Command.Param.(
      map
        (anon ("fuzzcase" %: string))
        ~f:(fun fuzzcase () ->
          Rust.transaction_fuzzer_reproduce set_constraint_constants
            set_initial_accounts apply_tx Transaction_pool.setup
            Transaction_pool.verify_and_apply (Bytes.of_string fuzzcase) ))

let () =
  Command.run
    (Command.group ~summary:"transaction_fuzzer"
       [ ("run", run_command)
       ; ("reproduce", reproduce_command)
       ; (Parallel.worker_command_name, Parallel.worker_command)
       ] )
