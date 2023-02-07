open Core
(*open Async*)
open Mina_base
open Mina_transaction
module Rust = Mina_tree.Rust
module Fp = Kimchi_pasta.Basic.Fp
module Ledger = Mina_ledger.Ledger
module Length = Mina_numbers.Length
module Global_slot = Mina_numbers.Global_slot

(* let constraint_constants = Genesis_constants.Constraint_constants.compiled *)

let proof_level = Genesis_constants.Proof_level.Full

let constraint_constants: Genesis_constants.Constraint_constants.t = {
  sub_windows_per_window = 11
  ; ledger_depth = 35
  ; work_delay = 2
  ; block_window_duration_ms = 180000
  ; transaction_capacity_log_2 = 7
  ; pending_coinbase_depth = 5
  ; coinbase_amount =
      Currency.Amount.of_int 720000000000
  ; supercharged_coinbase_factor = 2
  ; account_creation_fee =
      Currency.Fee.of_int 1000000000
  ; fork = None
}

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

let create_initial_accounts accounts =
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
    let tx = Transaction.Command command in
    let ledger = match !ledger with
      | Some(ledger) -> ledger
      | None -> failwith "ledger not initialized"
    in
    let applied = Ledger.apply_transaction ~constraint_constants ~txn_state_view ledger tx in
      Core_kernel.printf !"%{sexp:Ledger.Transaction_applied.t Or_error.t}\n%!" applied;
    let ledger_hash = Ledger.merkle_root ledger in
    Bin_prot.Writer.to_bytes [%bin_writer: Fp.t] ledger_hash
  with
    e -> let msg = Exn.to_string e in
    let bt = Printexc.get_backtrace () in
    Core_kernel.printf !"except: %s\n%s\n%!" msg bt;
    raise e

let () =
Printexc.record_backtrace true;
Core_kernel.printf !"starting...\n%!";
Rust.transaction_fuzzer set_initial_accounts apply_tx


