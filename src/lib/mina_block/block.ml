[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-21"]

[@@@ocaml.warning "-27"]

[@@@ocaml.warning "-26"]

open Core_kernel
open Mina_base
open Mina_state
open Blockchain_snark

[%%versioned
module Stable = struct
  module V2 = struct
    type t =
      { header : Header.Stable.V2.t
      ; body : Staged_ledger_diff.Body.Stable.V1.t
      }
    [@@deriving fields, sexp]

    let to_yojson t =
      `Assoc
        [ ( "protocol_state"
          , Protocol_state.value_to_yojson (Header.protocol_state t.header) )
        ; ("protocol_state_proof", `String "<opaque>")
        ; ("staged_ledger_diff", `String "<opaque>")
        ; ("delta_transition_chain_proof", `String "<opaque>")
        ; ( "current_protocol_version"
          , `String
              (Protocol_version.to_string
                 (Header.current_protocol_version t.header) ) )
        ; ( "proposed_protocol_version"
          , `String
              (Option.value_map
                 (Header.proposed_protocol_version_opt t.header)
                 ~default:"<None>" ~f:Protocol_version.to_string ) )
        ]

    let to_latest = Fn.id

    module Creatable = struct
      let id = "block"

      type nonrec t = t

      let sexp_of_t = sexp_of_t

      let t_of_sexp = t_of_sexp

      type 'a creator = header:Header.t -> body:Staged_ledger_diff.Body.t -> 'a

      let map_creator c ~f ~header ~body = f (c ~header ~body)

      let create ~header ~body = { header; body }
    end

    let equal =
      Comparable.lift Consensus.Data.Consensus_state.Value.equal
        ~f:
          (Fn.compose Mina_state.Protocol_state.consensus_state
             (Fn.compose Header.protocol_state header) )

    include (
      Allocation_functor.Make.Basic
        (Creatable) :
          Allocation_functor.Intf.Output.Basic_intf
            with type t := t
             and type 'a creator := 'a Creatable.creator )

    include (
      Allocation_functor.Make.Sexp
        (Creatable) :
          Allocation_functor.Intf.Output.Sexp_intf
            with type t := t
             and type 'a creator := 'a Creatable.creator )
  end
end]

type with_hash = t State_hash.With_state_hashes.t [@@deriving sexp]

[%%define_locally
Stable.Latest.(create, header, body, t_of_sexp, sexp_of_t, to_yojson, equal)]

let wrap_with_hash block =
  With_hash.of_data block
    ~hash_data:
      ( Fn.compose Protocol_state.hashes
      @@ Fn.compose Header.protocol_state header )

let timestamp block =
  block |> header |> Header.protocol_state |> Protocol_state.blockchain_state
  |> Blockchain_state.timestamp

let transactions ~constraint_constants block =
  let consensus_state =
    block |> header |> Header.protocol_state |> Protocol_state.consensus_state
  in
  let staged_ledger_diff =
    block |> body |> Staged_ledger_diff.Body.staged_ledger_diff
  in
  let coinbase_receiver =
    Consensus.Data.Consensus_state.coinbase_receiver consensus_state
  in
  let supercharge_coinbase =
    Consensus.Data.Consensus_state.supercharge_coinbase consensus_state
  in
  Staged_ledger.Pre_diff_info.get_transactions ~constraint_constants
    ~coinbase_receiver ~supercharge_coinbase staged_ledger_diff
  |> Result.map_error ~f:Staged_ledger.Pre_diff_info.Error.to_error
  |> Or_error.ok_exn

let payments block =
  block |> body |> Staged_ledger_diff.Body.staged_ledger_diff
  |> Staged_ledger_diff.commands
  |> List.filter_map ~f:(function
       | { data = Signed_command ({ payload = { body = Payment _; _ }; _ } as c)
         ; status
         } ->
           Some { With_status.data = c; status }
       | _ ->
           None )

let account_ids_accessed t =
  let transactions =
    transactions
      ~constraint_constants:Genesis_constants.Constraint_constants.compiled t
  in
  List.map transactions ~f:(fun { data = txn; status } ->
      Mina_transaction.Transaction.account_access_statuses txn status )
  |> List.concat
  |> List.dedup_and_sort
       ~compare:[%compare: Account_id.t * [ `Accessed | `Not_accessed ]]


let%test_module "blocks tests" =
  ( module struct

    type ledger_with_blocks =
      (
        Mina_base.Account.Stable.V2.t list
        * Staged_ledger.Scan_state.Stable.V2.t
        * Pending_coinbase.Stable.V2.t
        * Stable.Latest.t
        * Stable.Latest.t list
      )
        [@@deriving bin_io]

    let%test_unit "Apply berkeleynet blocks" =
        let proof_level = Genesis_constants.Proof_level.compiled in
        let constraint_constants =
          Genesis_constants.Constraint_constants.compiled
        in
        Printf.eprintf "OK\n%!";

        let read_file_into_string filename = Stdio.In_channel.read_all filename in

        let bin_file =
          read_file_into_string "/home/sebastien/travaux/ledger/blocks.bin"
        in

        Core.Printf.eprintf "START READING FILE\n%!" ;

        let file: ledger_with_blocks =
          bin_read_ledger_with_blocks
            (Bigstring.of_string bin_file)
            ~pos_ref:(ref 0)
        in
        Printf.eprintf "OK\n%!";

        let accounts, scan_state, pending_coinbase_collection, prev_block, blocks = file in
        let ledger = Mina_ledger.Ledger.create ~depth:35 () in

        Printf.eprintf "COMPILING SNARK\n%!";

        let module T = Transaction_snark.Make (struct
                           let constraint_constants = constraint_constants

                           let proof_level = proof_level
                         end) in
        let module B = Blockchain_snark_state.Make (struct
                           let tag = T.tag

                           let constraint_constants = constraint_constants

                           let proof_level = proof_level
                         end) in

        Printf.eprintf "COMPILING SNARK DONE\n%!";

        let header = header prev_block in
        let proof = Header.protocol_state_proof header in
        let state = Header.protocol_state header in

        Printf.eprintf "################################## VERIFYING PROOF\n%!";
        let result = Async.Thread_safe.block_on_async_exn (fun () ->
                         B.Proof.verify [ (state, proof) ]
          ) in

        Printf.eprintf "################################## VERIFYING PROOF DONE\n%!";
        Printf.eprintf "OK\n%!";

        exit 0;

        List.iter accounts
          ~f:(fun account ->
            let account_id = Account.identifier account in
            match
              Mina_ledger.Ledger.get_or_create_account ledger account_id account
            with
            | Ok _ ->
                ()
            | _ ->
                failwith "Error message"
          );

        let sl = Staged_ledger.my_new ~constraint_constants ~ledger ~scan_state ~pending_coinbase_collection in
        let logger = Logger.null () in
        let verifier =
          Async.Thread_safe.block_on_async_exn (fun () ->
              Verifier.create ~logger ~proof_level ~constraint_constants
                ~conf_dir:None
                ~pids:(Child_processes.Termination.create_pid_table ())
                () ) in

        let hash = Staged_ledger.hash sl in
        Printf.eprintf !"STAGED_LEDGER_HASH=%{sexp: Staged_ledger_hash.t}\n%!" hash;

        let prev_block = ref prev_block in

            (* let block_height = block
             *     .header
             *     .protocol_state
             *     .body
             *     .consensus_state
             *     .blockchain_length
             *     .0
             *     .as_u32(); *)

        let sl = ref sl in

        List.iter blocks
          ~f:(fun block ->

            let protocol_state = Header.protocol_state block.header in
            let cs = Protocol_state.consensus_state protocol_state in
            let global_slot = Consensus.Data.Consensus_state.global_slot_since_genesis cs in

            let block_length = Consensus.Data.Consensus_state.blockchain_length cs in

            Printf.eprintf "block %d\n%!" (Unsigned.UInt32.to_int block_length);

            let staged_ledger_diff =
              block |> body |> Staged_ledger_diff.Body.staged_ledger_diff
            in

            let parent_protocol_state = Header.protocol_state !prev_block.header in
            let current_state_view = Protocol_state.(Body.view @@ body parent_protocol_state) in

            let res =
              Async.Thread_safe.block_on_async_exn (fun () ->
                  Staged_ledger.apply ~constraint_constants ~global_slot !sl staged_ledger_diff
                    ~logger
                    ~verifier
                    ~current_state_view
                    ~state_and_body_hash:(let body_hash =
                                             Protocol_state.(Body.hash @@ body parent_protocol_state)
                                           in
                                           ( (Protocol_state.hashes_with_body parent_protocol_state ~body_hash)
                                               .state_hash
                                           , body_hash ) )
                    ~coinbase_receiver:(Consensus.Data.Consensus_state.coinbase_receiver cs)
                    ~supercharge_coinbase:false
                )
            in
            prev_block := block;

            (match res with
            | Error err ->
               failwith "error"
            | Ok
( `Hash_after_applying _hash
, `Ledger_proof _ledger_proof
, `Staged_ledger sl'
, `Pending_coinbase_update _ ) ->
               sl := sl' );

            if (Unsigned.UInt32.to_int block_length) = 1047 then
              (
                exit 0
              );


            (* let ( `Hash_after_applying staged_ledger_hash
             *     , `Ledger_proof _
             *     , `Staged_ledger ledger
             *     , `Pending_coinbase_update _ ) =
             *   (res |> Async_kernel.Deferred.Result.ok_exn)
             * in *)


            ()
          );

        ()
  end )
