open Core
module Checkpoint = Block_checkpoint
module Trace = Block_trace

module Entry = struct
  type t =
    { checkpoint : Checkpoint.t
    ; started_at : float
    ; duration : float
    ; metadata : Yojson.Safe.t
    ; checkpoints : t list
    }
  [@@deriving to_yojson]

  let of_flat_entry entry =
    let { Trace.Entry.checkpoint; started_at; duration; metadata } = entry in
    { checkpoint; started_at; duration; metadata; checkpoints = [] }
end

type section = { title : string; checkpoints : Entry.t list }
[@@deriving to_yojson]

type t =
  { source : Trace.block_source
  ; blockchain_length : int
  ; sections : section list
  ; status : Trace.status
  ; total_time : float
  ; metadata : Yojson.Safe.t
  }
[@@deriving to_yojson]

let to_yojson t =
  let blockchain_length_int = t.blockchain_length in
  match to_yojson t with
  | `Assoc fields ->
      `Assoc (("blockchain_length_int", `Int blockchain_length_int) :: fields)
  | other ->
      other

let checkpoint_children (c : Checkpoint.t) : Checkpoint.t list =
  match c with
  | `Initial_validation ->
      [ `Validate_proofs; `Done_validating_proofs ]
  | `Validate_transition ->
      [ `Check_transition_not_in_frontier
      ; `Check_transition_not_in_process
      ; `Check_transition_can_be_connected
      ; `Register_transition_for_processing
      ]
  | `Build_breadcrumb ->
      [ `Validate_staged_ledger_diff; `Create_breadcrumb ]
  | `Validate_staged_ledger_diff ->
      [ `Check_completed_works; `Prediff; `Apply_diff; `Diff_applied ]
  | `Check_completed_works ->
      []
  | `Apply_diff ->
      [ `Update_coinbase_stack
      ; `Update_coinbase_stack_done
      ; `Check_for_sufficient_snark_work
      ; `Check_zero_fee_excess
      ; `Fill_work_and_enqueue_transactions
      ; `Update_pending_coinbase_collection
      ; `Verify_scan_state_after_apply
      ; `Hash_new_staged_ledger
      ; `Make_staged_ledger_hash
      ]
  | `Update_coinbase_stack ->
      [ `Update_ledger_and_get_statements
      ; `Update_ledger_and_get_statements_done
      ]
  | `Hash_new_staged_ledger ->
      [ `Hash_scan_state; `Get_merkle_root ]
  | `Add_and_finalize ->
      [ `Add_breadcrumb_to_frontier; `Add_breadcrumb_to_frontier_done ]
  | `Add_breadcrumb_to_frontier ->
      [ `Calculate_diffs
      ; `Apply_catchup_tree_diffs
      ; `Apply_full_frontier_diffs
      ; `Full_frontier_diffs_applied
      ; `Synchronize_persistent_frontier
      ; `Persistent_frontier_synchronized
      ; `Notify_frontier_extensions
      ; `Notify_frontier_extensions_done
      ]
  | `Apply_full_frontier_diffs ->
      [ `Move_frontier_root; `Move_frontier_root_done ]
  | `Notify_frontier_extensions ->
      [ `Notify_SPRC_handle_diffs
      ; `Notify_SPRC_write_view
      ; `Notify_SPRC_write_view_done
      ]
  | `Notify_SPRC_handle_diffs ->
      [ `SPRC_add_scan_state_to_ref_table
      ; `SPRC_add_scan_state_to_ref_table_done
      ; `SPRC_add_to_work_table
      ; `SPRC_add_to_work_table_done
      ; `SPRC_remove_from_work_table
      ; `SPRC_remove_from_work_table_done
      ; `SPRC_update_best_tip_table
      ; `SPRC_update_best_tip_table_done
      ]
  | `Generate_next_state ->
      [ `Create_staged_ledger_diff
      ; `Create_staged_ledger_diff_done
      ; `Apply_staged_ledger_diff
      ; `Apply_staged_ledger_diff_done
      ; `Generate_transition
      ; `Generate_transition_done
      ]
  | `Generate_transition ->
      [ `Consensus_state_update; `Consensus_state_update_done ]
  | `Produce_state_transition_proof ->
      [ `Produce_state_transition_proof_step
      ; `Produce_state_transition_proof_wrap_proof
      ; `Produce_state_transition_proof_wrap_proof_done
      ; `Produce_state_transition_proof_1
      ; `Produce_state_transition_proof_2
      ; `Produce_state_transition_proof_3
      ; `Produce_state_transition_proof_4
      ; `Produce_state_transition_proof_5
      ; `Produce_state_transition_proof_6
      ; `Produce_state_transition_proof_7
      ; `Produce_state_transition_proof_8
      ; `Produce_state_transition_proof_9
      ; `Produce_state_transition_proof_10
      ; `Produce_state_transition_proof_11
      ; `Produce_state_transition_proof_12
      ; `Produce_state_transition_proof_13
      ; `Produce_state_transition_proof_14
      ; `Produce_state_transition_proof_15
      ; `Produce_state_transition_proof_16
      ; `Produce_state_transition_proof_17
      ; `Produce_state_transition_proof_18
      ; `Produce_state_transition_proof_19
      ; `Produce_state_transition_proof_20
      ]
  | `Apply_staged_ledger_diff ->
      [ `Update_coinbase_stack
      ; `Update_coinbase_stack_done
      ; `Check_for_sufficient_snark_work
      ; `Check_zero_fee_excess
      ; `Fill_work_and_enqueue_transactions
      ; `Update_pending_coinbase_collection
      ; `Verify_scan_state_after_apply
      ; `Hash_new_staged_ledger
      ; `Make_staged_ledger_hash
      ]
  | `Create_staged_ledger_diff ->
      [ `Get_snark_work_for_pending_transactions
      ; `Validate_and_apply_transactions
      ; `Generate_staged_ledger_diff
      ; `Generate_staged_ledger_diff_done
      ]
  | `Produce_validated_transition ->
      [ `Build_breadcrumb ]
  | _ ->
      []

let is_parent_of parent entry =
  let children = checkpoint_children parent.Entry.checkpoint in
  List.mem children entry.Entry.checkpoint ~equal:Checkpoint.equal

let has_children entry =
  not (List.is_empty (checkpoint_children entry.Entry.checkpoint))

(* TODOX: instead of processing checkpoints at the same level here
   process the flat trace using information about parent/children relationship
   to skip children when finding the end *)
let postprocess_checkpoints checkpoints =
  match checkpoints with
  | [] ->
      []
  | first :: _ as checkpoints ->
      let next_timestamp = ref first.Entry.started_at in
      List.rev_map checkpoints ~f:(fun entry ->
          let ended_at = !next_timestamp in
          next_timestamp := entry.started_at ;
          { entry with duration = ended_at -. entry.started_at } )

let postprocess_entry_checkpoints entry =
  let checkpoints = postprocess_checkpoints entry.Entry.checkpoints in
  { entry with checkpoints }

let merge_into_parent parent entry =
  let entry' = postprocess_entry_checkpoints entry in
  let checkpoints = entry' :: parent.Entry.checkpoints in
  { parent with checkpoints }

let rec collapse_pending_stack_with_children entry acc stack =
  match stack with
  | parent :: _ as stack when is_parent_of parent entry ->
      (acc, entry :: stack)
  | child :: parent :: rest ->
      let parent' = merge_into_parent parent child in
      collapse_pending_stack_with_children entry acc (parent' :: rest)
  | [ sibling ] ->
      let sibling' = postprocess_entry_checkpoints sibling in
      (sibling' :: acc, [ entry ])
  | [] ->
      (acc, [ entry ])

let rec collapse_pending_stack_simple entry acc stack =
  match stack with
  | parent :: rest when is_parent_of parent entry ->
      let parent' = merge_into_parent parent entry in
      (acc, parent' :: rest)
  | child :: parent :: rest ->
      let parent' = merge_into_parent parent child in
      collapse_pending_stack_simple entry acc (parent' :: rest)
  | [ sibling ] ->
      let sibling' = postprocess_entry_checkpoints sibling in
      (entry :: sibling' :: acc, [])
  | [] ->
      (entry :: acc, [])

let structure_checkpoints checkpoints =
  let checkpoints = List.rev_map ~f:Entry.of_flat_entry checkpoints in
  postprocess_checkpoints @@ fst
  @@ List.fold checkpoints ~init:([], []) ~f:(fun (accum, stack) entry ->
         if has_children entry then
           collapse_pending_stack_with_children entry accum stack
         else collapse_pending_stack_simple entry accum stack )

let of_flat_trace trace =
  let { Trace.source
      ; blockchain_length
      ; status
      ; checkpoints
      ; other_checkpoints = _
      ; total_time
      ; metadata
      } =
    trace
  in
  let checkpoints = structure_checkpoints checkpoints in
  (* TODOX: complete sections *)
  let section = { title = "All"; checkpoints } in
  let sections = [ section ] in
  { source; blockchain_length; sections; status; total_time; metadata }
