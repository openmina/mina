open Core
open Mina_base
open Mina_transaction
module Rust = Mina_tree.Rust
module Fp = Kimchi_pasta.Basic.Fp
module Ledger = Mina_ledger.Ledger
module Length = Mina_numbers.Length
module Global_slot = Mina_numbers.Global_slot





let txn_state_view: Zkapp_precondition.Protocol_state.View.t = {
  snarked_ledger_hash = Fp.of_string("19095410909873291354237217869735884756874834695933531743203428046904386166496")
  ; timestamp = Block_time.of_int64(1600251300000L)
  ; blockchain_length = Length.of_int(1)
  ; last_vrf_output = ()
  ; min_window_density = Length.of_int(77)
  ; total_currency = Currency.Amount.of_int(10016100000000000)
  ; global_slot_since_hard_fork = Global_slot.of_int(0)
  ; global_slot_since_genesis = Global_slot.of_int(0)
  ; staking_epoch_data = {
    ledger = {
        hash = Fp.of_string("19095410909873291354237217869735884756874834695933531743203428046904386166496")
        ; total_currency = Currency.Amount.of_int(10016100000000000)
    }
    ; seed = Mina_base.Epoch_seed.of_decimal_string("0")
    ; start_checkpoint = State_hash.zero
    ; lock_checkpoint = State_hash.zero
    ; epoch_length = Length.of_int(1)
    }
  ; next_epoch_data = {
    ledger = {
        hash = Fp.of_string("19095410909873291354237217869735884756874834695933531743203428046904386166496")
        ; total_currency = Currency.Amount.of_int(10016100000000000)
    }
    ; seed = Fp.of_string("18512313064034685696641580142878809378857342939026666126913761777372978255172")
    ; start_checkpoint = State_hash.zero
    ; lock_checkpoint = Fp.of_string("9196091926153144288494889289330016873963015481670968646275122329689722912273")
    ; epoch_length = Length.of_int(2)
    }
  }


let constraint_constants: (Genesis_constants.Constraint_constants.t option) ref = ref None

let deserialize_constants constants_bytes =
  Bin_prot.Reader.of_bytes [%bin_reader: Genesis_constants.Constraint_constants.t] constants_bytes


let set_constraint_constants constants_bytes = constraint_constants := Some (deserialize_constants constants_bytes)


let create_initial_accounts accounts =
  let constraint_constants = match !constraint_constants with
  | Some(constraint_constants) -> constraint_constants
  | None -> failwith "constraint_constants not initialized"
  in
  let packed =
    Genesis_ledger_helper.Ledger.packed_genesis_ledger_of_accounts
      ~depth:constraint_constants.ledger_depth (lazy (List.map ~f:(fun a -> (None, a)) accounts))
  in
  Lazy.force (Genesis_ledger.Packed.t packed)

let deserialize_accounts accounts_bytes =
  Bin_prot.Reader.of_bytes [%bin_reader: Account.Stable.Latest.t list] accounts_bytes

let ledger: (Ledger.t option) ref = ref None

let set_initial_accounts accounts_bytes =
  let ledger_ = create_initial_accounts (deserialize_accounts accounts_bytes) in
    ledger := Some ledger_;
    let ledger_hash = Ledger.merkle_root ledger_ in
    Bin_prot.Writer.to_bytes [%bin_writer: Fp.t] ledger_hash

let apply_tx user_command_bytes =
  try
    let command = Bin_prot.Reader.of_bytes [%bin_reader: User_command.Stable.Latest.t] user_command_bytes in
    (*Core_kernel.printf !"%{sexp:User_command.t}\n%!" command;*)
    let tx = Transaction.Command command in
    let constraint_constants = match !constraint_constants with
      | Some(constraint_constants) -> constraint_constants
      | None -> failwith "constraint_constants not initialized"
    in
    let ledger = match !ledger with
      | Some(ledger) -> ledger
      | None -> failwith "ledger not initialized"
    in
    let _applied = Ledger.apply_transaction ~constraint_constants ~txn_state_view ledger tx in
    (*Core_kernel.printf !"%{sexp:Ledger.Transaction_applied.t Or_error.t}\n%!" applied;*)
    let ledger_hash = Ledger.merkle_root ledger in
    Bin_prot.Writer.to_bytes [%bin_writer: Fp.t] ledger_hash
  with
    e -> let msg = Exn.to_string e in
    let bt = Printexc.get_backtrace () in
    Core_kernel.printf !"except: %s\n%s\n%!" msg bt;
    raise e

let get_coverage _ =
  List.map (Bisect.Runtime.get_coverage_flattened ()) ~f:(
    fun {filename; points; counts} -> (
      filename,
      Array.fold_right points ~f:(fun x acc -> (Int64.of_int x) :: acc) ~init:[],
      Array.fold_right counts ~f:(fun x acc -> (Int64.of_int x) :: acc) ~init:[]
    )
  )

let run_command =
  Command.basic
    ~summary:"Run the fuzzer"
    Command.Param.(map
      (anon ("seed" %: int))
      ~f:(fun seed ->
        fun () -> Rust.transaction_fuzzer (Int64.of_int seed) set_constraint_constants set_initial_accounts apply_tx get_coverage
        ))

let reproduce_command =
  Command.basic
    ~summary:"Reproduce fuzzcase"
    Command.Param.(map
      (anon ("fuzzcase" %: string))
      ~f:(fun fuzzcase ->
        fun () -> Rust.transaction_fuzzer_reproduce set_constraint_constants set_initial_accounts apply_tx (Bytes.of_string fuzzcase)
        ))

let () =
  Command.run
    (Command.group ~summary:"transaction_fuzzer"
      [ "run", run_command;
        "reproduce", reproduce_command ])