open Core
open Async
open Blockchain_snark
open Mina_base
open Mina_state

let parse_json_line_or_file file_or_json =
  let json_string =
    if String.is_prefix file_or_json ~prefix:"@" then
      let filename = String.drop_prefix file_or_json 1 in
      In_channel.read_all filename
    else file_or_json
  in
  match
    [%derive.of_yojson: Blockchain_snark.Blockchain.t list]
      (Yojson.Safe.from_string json_string)
  with
  | Ok input ->
      input
  | Error err ->
      Stdlib.Printf.printf "Could not parse JSON: %s" err ;
      Stdlib.exit 1

let get_input () =
  let input =
    match Sys.get_argv () with
    | [| _; filename |] ->
        parse_json_line_or_file ("@" ^ filename)
    | _ ->
        Stdlib.Printf.printf
          "No block json file provided, please input JSON (single line) or \
           @path/to/file.json:\n\
           %!" ;
        let input_line = Stdlib.input_line Stdlib.stdin in
        Stdlib.Printf.printf "Parsing.\n%!" ;
        parse_json_line_or_file input_line
  in
  List.map input ~f:(fun snark ->
      ( Blockchain_snark.Blockchain.state snark
      , Blockchain_snark.Blockchain.proof snark ) )

let run () =
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
  let input = get_input () in
  let verify : (Protocol_state.Value.t * Proof.t) list -> bool =
    B.Proof.verify
  in
  Stdlib.Printf.printf "Calling verifier.\n%!" ;
  let before_time = Unix.gettimeofday () in
  Stdlib.Printf.printf "Breakpoint before calling verify: break @ %s %d\n%!"
    __MODULE__ __LINE__ ;
  let result = verify input in
  let after_time = Unix.gettimeofday () in
  Stdlib.Printf.printf "Verification time: %fs\n%!" (after_time -. before_time) ;
  match result with
  | true ->
      Stdlib.Printf.printf "Proofs verified successfully" ;
      ()
  | false ->
      Stdlib.Printf.printf "Proofs failed to verify" ;
      Stdlib.exit 1

let () =
  Random.self_init () ;
  Stdlib.Printf.printf "Starting verifier\n%!" ;
  run ()
