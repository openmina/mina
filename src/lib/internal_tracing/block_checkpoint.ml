(* TODO: define order *)
(* TODO: define structure *)
(* TODO: define wait/computation *)
(* TODO: descriptions *)
type block_production_checkpoint =
  [ `Begin_block_production
  | `Find_best_tip
  | `Get_transactions_from_pool
  | `Generate_next_state
  | `Create_staged_ledger_diff
  | `Get_snark_work_for_pending_transactions
  | `Validate_and_apply_transactions
  | `Generate_staged_ledger_diff
  | `Generate_staged_ledger_diff_done
  | `Create_staged_ledger_diff_done
  | `Generate_next_state_done
  | `Apply_staged_ledger_diff
  | `Apply_staged_ledger_diff_done
  | `Generate_transition
  | `Consensus_state_update
  | `Consensus_state_update_done
  | `Generate_transition_done
  | `Produce_state_transition_proof
  | `Produce_state_transition_proof_step
  | `Produce_state_transition_proof_step_compute_prev_proof_parts
  | `Produce_state_transition_proof_step_compute_prev_proof_parts_done
  | `Produce_state_transition_proof_step_generate_witness_conv
  | `Produce_state_transition_proof_step_backend_tick_proof_create_async
  | `Produce_state_transition_proof_step_backend_tick_proof_create_async_done
  | `Produce_state_transition_proof_wrap_proof
  | `Produce_state_transition_proof_wrap_proof_done
  | `Produce_state_transition_proof_1
  | `Produce_state_transition_proof_2
  | `Produce_state_transition_proof_3
  | `Produce_state_transition_proof_4
  | `Produce_state_transition_proof_5
  | `Produce_state_transition_proof_6
  | `Produce_state_transition_proof_7
  | `Produce_state_transition_proof_8
  | `Produce_state_transition_proof_9
  | `Produce_state_transition_proof_10
  | `Produce_state_transition_proof_11
  | `Produce_state_transition_proof_12
  | `Produce_state_transition_proof_13
  | `Produce_state_transition_proof_14
  | `Produce_state_transition_proof_15
  | `Produce_state_transition_proof_16
  | `Produce_state_transition_proof_17
  | `Produce_state_transition_proof_18
  | `Produce_state_transition_proof_19
  | `Produce_state_transition_proof_20
  | `Produce_state_transition_proof_done
  | `Produce_chain_transition_proof
  | `Produce_validated_transition
  | `Send_breadcrumb_to_transition_frontier
  | `Wait_for_confirmation
  | `Transition_accepted
  | `Transition_accept_timeout
  | `Failure ]
[@@deriving bin_io, yojson, enumerate, equal, hash, sexp_of, compare]

let block_production_checkpoint_to_yojson =
  Util.flatten_yojson_variant block_production_checkpoint_to_yojson

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
[@@deriving bin_io, yojson, enumerate, equal, hash, sexp_of, compare]

let external_block_validation_checkpoint_to_yojson =
  Util.flatten_yojson_variant external_block_validation_checkpoint_to_yojson

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
  | `Update_ledger_and_get_statements
  | `Update_ledger_and_get_statements_done
  | `Update_coinbase_stack_done
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
  | `Move_frontier_root
  | `Move_frontier_root_done
  | `Full_frontier_diffs_applied
  | `Synchronize_persistent_frontier
  | `Persistent_frontier_synchronized
  | `Notify_frontier_extensions
  | `Notify_SPRC_handle_diffs
  | `SPRC_add_scan_state_to_ref_table
  | `SPRC_add_to_work_table
  | `SPRC_add_to_work_table_done
  | `SPRC_remove_from_work_table
  | `SPRC_remove_from_work_table_done
  | `SPRC_update_best_tip_table
  | `SPRC_update_best_tip_table_done
  | `SPRC_add_scan_state_to_ref_table_done
  | `Notify_SPRC_write_view
  | `Notify_SPRC_write_view_done
  | `Notify_frontier_extensions_done
  | `Add_breadcrumb_to_frontier_done
  | `Parent_breadcrumb_not_found
  | `Schedule_catchup
  | `Download_ancestry_state_hashes
  | `Failure ]
[@@deriving bin_io, yojson, enumerate, equal, hash, sexp_of, compare]

let block_processing_checkpoint_to_yojson =
  Util.flatten_yojson_variant block_processing_checkpoint_to_yojson

type catchup_checkpoint =
  [ `To_download
  | `To_initial_validate
  | `To_verify
  | `Wait_for_parent
  | `To_build_breadcrumb
  | `Catchup_job_finished
  | `Failure ]
[@@deriving bin_io, yojson, enumerate, equal, hash, sexp_of, compare]

let catchup_checkpoint_to_yojson =
  Util.flatten_yojson_variant catchup_checkpoint_to_yojson

type t =
  [ block_production_checkpoint
  | external_block_validation_checkpoint
  | catchup_checkpoint
  | block_processing_checkpoint ]
[@@deriving bin_io, yojson, enumerate, equal, hash, sexp_of, compare]

let to_string (c : t) =
  match to_yojson c with `String name -> name | _ -> assert false
