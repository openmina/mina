open Core_kernel
open Async_kernel
open Mina_base

(* open Mina_base *)
(* open Mina_state
 * open Blockchain_snark *)

type t =
  { proof_level : Genesis_constants.Proof_level.t
  ; constraint_constants : Genesis_constants.Constraint_constants.t
  }

type invalid = Common.invalid [@@deriving bin_io_unversioned, to_yojson]

let invalid_to_error = Common.invalid_to_error

type ledger_proof = Ledger_proof.t

let create ~logger:_ ?enable_internal_tracing:_ ?internal_trace_filename:_
    ~proof_level ~constraint_constants ~pids:_ ~conf_dir:_ () =
  (* match proof_level with *)
  (* | Genesis_constants.Proof_level.Full ->
   *     failwith "Unable to handle proof-level=Full"
   * | Check | None -> *)
  Pickles.Side_loaded.srs_precomputation () ;
    Deferred.return { proof_level; constraint_constants }

let verify_blockchain_snarks _ _ = Deferred.Or_error.return (Ok ())

(* N.B.: Valid_assuming is never returned, in fact; we assert a return type
   containing Valid_assuming to match the expected type
*)

(* let verify_commands
 *       _
 *       (cs : User_command.Verifiable.t With_status.t list) :
 *       _ list Deferred.t =
 *   let cs = List.map cs ~f:Common.check in
 *   let to_verify =
 *     List.concat_map cs ~f:(function
 *         | `Valid _ ->
 *            []
 *         | `Valid_assuming (_, xs) ->
 *            xs
 *         | `Invalid_keys _
 *           | `Invalid_signature _
 *           | `Invalid_proof _
 *           | `Missing_verification_key _
 *           | `Unexpected_verification_key _
 *           | `Mismatched_authorization_kind _ ->
 *            [] )
 *   in
 *   let%map all_verified =
 *     Pickles.Side_loaded.verify ~typ:Zkapp_statement.typ to_verify
 *   in
 *   List.map cs ~f:(function
 *       | `Valid c ->
 *          `Valid c
 *       | `Valid_assuming (c, xs) ->
 *          if Or_error.is_ok all_verified then `Valid c
 *          else `Valid_assuming xs
 *       | `Invalid_keys keys ->
 *          `Invalid_keys keys
 *       | `Invalid_signature keys ->
 *          `Invalid_signature keys
 *       | `Invalid_proof err ->
 *          `Invalid_proof err
 *       | `Missing_verification_key keys ->
 *          `Missing_verification_key keys
 *       | `Unexpected_verification_key keys ->
 *          `Unexpected_verification_key keys
 *       | `Mismatched_authorization_kind keys ->
 *          `Mismatched_authorization_kind keys ) *)

let verify_commands _t (cs : User_command.Verifiable.t With_status.t list) :
    _ list Deferred.Or_error.t =
  let cs = List.map cs ~f:Common.check in
  let to_verify =
    List.concat_map cs ~f:(function
      | `Valid _ ->
          []
      | `Valid_assuming (_, xs) ->
          xs
      | `Invalid_keys _
      | `Invalid_signature _
      | `Invalid_proof _
      | `Missing_verification_key _
      | `Unexpected_verification_key _
      | `Mismatched_authorization_kind _ ->
          [] )
  in
  let%map all_verified =
    Pickles.Side_loaded.verify ~typ:Zkapp_statement.typ to_verify
  in
  Or_error.return
  @@ List.map cs ~f:(function
       | `Valid c ->
           `Valid c
       | `Valid_assuming (c, xs) ->
           if Or_error.is_ok all_verified then `Valid c else `Valid_assuming xs
       | `Invalid_keys keys ->
           `Invalid_keys keys
       | `Invalid_signature keys ->
           `Invalid_signature keys
       | `Invalid_proof err ->
           `Invalid_proof err
       | `Missing_verification_key keys ->
           `Missing_verification_key keys
       | `Unexpected_verification_key keys ->
           `Unexpected_verification_key keys
       | `Mismatched_authorization_kind keys ->
           `Mismatched_authorization_kind keys )

let verify_transaction_snarks _ ts =
  (* Don't check if the proof has default sok, becasue they were probably not
     intended to be checked. If it has some value then check that against the
     message passed.
     This is particularly used to test that invalid proofs are not added to the
     snark pool
  *)
  if
    List.for_all ts ~f:(fun (proof, message) ->
        let msg_digest = Sok_message.digest message in
        let sok_digest = Transaction_snark.sok_digest proof in
        Sok_message.Digest.(equal sok_digest default)
        || Mina_base.Sok_message.Digest.equal sok_digest msg_digest )
  then Deferred.Or_error.return (Ok ())
  else
    Deferred.Or_error.return
      (Or_error.error_string "Transaction_snark.verify: Mismatched sok_message")

let get_blockchain_verification_key { proof_level; constraint_constants } =
  Deferred.Or_error.try_with (fun () ->
      let module T = Transaction_snark.Make (struct
        let constraint_constants = constraint_constants

        let proof_level = proof_level
      end) in
      let module B = Blockchain_snark.Blockchain_snark_state.Make (struct
        let tag = T.tag

        let constraint_constants = constraint_constants

        let proof_level = proof_level
      end) in
      Deferred.return @@ Lazy.force B.Proof.verification_key )

let toggle_internal_tracing _ _ = Deferred.Or_error.ok_unit

let set_itn_logger_data _ ~daemon_port:_ = Deferred.Or_error.ok_unit
