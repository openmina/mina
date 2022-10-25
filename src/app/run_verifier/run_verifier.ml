[%%import "/src/config.mlh"]

open Core
open Async
open Blockchain_snark
open Mina_base
open Mina_state
(*open Mina_base
  open Cli_lib
  open Signature_lib
  open Init*)
(*module YJ = Yojson.Safe*)

let run () =
  let _logger = Logger.create () in
  let conf_dir = "/tmp/test" in
  (*Mina_lib.Conf_dir.compute_conf_dir None in*)
  let%bind () = File_system.create_dir conf_dir in
  let open Deferred.Let_syntax in
  let proof_level = Genesis_constants.Proof_level.compiled in
  let constraint_constants = Genesis_constants.Constraint_constants.compiled in
  printf "Compiling transaction snark module...\n%!" ;
  let before_time = Unix.gettimeofday () in
  let module T = Transaction_snark.Make (struct
    let constraint_constants = constraint_constants

    let proof_level = proof_level
  end) in
  let after_time = Unix.gettimeofday () in
  printf "Transaction snark module creation time: %fs\n%!"
    (after_time -. before_time) ;
  printf "Compiling blockchain snark module...\n%!" ;
  let before_time = Unix.gettimeofday () in
  let module B = Blockchain_snark_state.Make (struct
    let tag = T.tag

    let constraint_constants = constraint_constants

    let proof_level = proof_level
  end) in
  let after_time = Unix.gettimeofday () in
  printf "Blockchain snark module creation time: %fs\n%!"
    (after_time -. before_time) ;
  printf "Input JSON:\n%!" ;
  let%map input_line =
    match%map Reader.read_line (Lazy.force Reader.stdin) with
    | `Ok input_line ->
        input_line
    | `Eof ->
        failwith "early EOF while reading json"
  in
  printf "Parsing.\n%!" ;
  let input =
    match
      [%derive.of_yojson: Blockchain_snark.Blockchain.t list]
        (Yojson.Safe.from_string input_line)
    with
    | Ok input ->
        input
    | Error err ->
        failwithf "Could not parse JSON: %s" err ()
  in
  let x =
    List.map input ~f:(fun snark ->
        ( Blockchain_snark.Blockchain.state snark
        , Blockchain_snark.Blockchain.proof snark ) )
  in
  let verify : (Protocol_state.Value.t * Proof.t) list -> bool Deferred.t =
    B.Proof.verify
  in
  printf "Calling verifier.\n%!" ;
  let before_time = Unix.gettimeofday () in
  let%bind result = verify x in
  let after_time = Unix.gettimeofday () in
  printf "Verification time: %fs\n%!" (after_time -. before_time) ;
  match result with
  | true ->
      printf "Proofs verified successfully" ;
      exit 0
  | false ->
      printf "Proofs failed to verify" ;
      exit 1

let () =
  Random.self_init () ;
  ignore @@ run () ;
  never_returns (Scheduler.go ())
