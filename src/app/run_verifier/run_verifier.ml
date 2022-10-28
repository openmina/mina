[%%import "/src/config.mlh"]

open Core
open Async
open Blockchain_snark
open Mina_base
open Mina_state

let run () =
  let open Deferred.Let_syntax in
  let proof_level = Genesis_constants.Proof_level.compiled in
  let constraint_constants = Genesis_constants.Constraint_constants.compiled in
  Stdlib.Printf.printf "Compiling transaction snark module...\n%!" ;
  let before_time = Unix.gettimeofday () in
  let module T = Transaction_snark.Make (struct
    let constraint_constants = constraint_constants

    let proof_level = proof_level
  end) in
  let after_time = Unix.gettimeofday () in
  Stdlib.Printf.printf "Transaction snark module creation time: %fs\n%!"
    (after_time -. before_time) ;
  Stdlib.Printf.printf "Compiling blockchain snark module...\n%!" ;
  let before_time = Unix.gettimeofday () in
  let module B = Blockchain_snark_state.Make (struct
    let tag = T.tag

    let constraint_constants = constraint_constants

    let proof_level = proof_level
  end) in
  let after_time = Unix.gettimeofday () in
  Stdlib.Printf.printf "Blockchain snark module creation time: %fs\n%!"
    (after_time -. before_time) ;
  Stdlib.Printf.printf "Input JSON:\n%!" ;
  let input_line = Stdlib.input_line Stdlib.stdin in
  Stdlib.Printf.printf "Parsing.\n%!" ;
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
  Stdlib.Printf.printf "Calling verifier.\n%!" ;
  let before_time = Unix.gettimeofday () in
  Stdlib.Printf.printf "Breakpoint before calling verify: break @ %s %d\n%!"
    __MODULE__ __LINE__ ;
  let%bind result = verify x in
  let after_time = Unix.gettimeofday () in
  Stdlib.Printf.printf "Verification time: %fs\n%!" (after_time -. before_time) ;
  match result with
  | true ->
      Stdlib.Printf.printf "Proofs verified successfully" ;
      exit 0
  | false ->
      Stdlib.Printf.printf "Proofs failed to verify" ;
      exit 1

let () =
  Random.self_init () ;
  Stdlib.Printf.printf "Starting verifier\n%!" ;
  let _deferred = run () in
  never_returns (Scheduler.go ())
