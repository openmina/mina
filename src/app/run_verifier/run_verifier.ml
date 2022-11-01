open Core
open Async
open Blockchain_snark
open Mina_base
open Mina_state

let parse_json_file path =
  let json_string = In_channel.read_all path in
  if String.is_prefix json_string ~prefix:"[" then
    [%derive.of_yojson: Blockchain_snark.Blockchain.t list]
      (Yojson.Safe.from_string json_string)
  else
    Result.map
      ~f:(fun bc -> [ bc ])
      ([%derive.of_yojson: Blockchain_snark.Blockchain.t]
         (Yojson.Safe.from_string json_string) )

let get_input () =
  Stdlib.Printf.printf "Input path/to/file.json:\n%!" ;
  let input =
    let input_line = Stdlib.input_line Stdlib.stdin in
    Stdlib.Printf.printf "Parsing.\n%!" ;
    parse_json_file input_line
  in
  Result.map input ~f:(fun input ->
      List.map input ~f:(fun snark ->
          ( Blockchain_snark.Blockchain.state snark
          , Blockchain_snark.Blockchain.proof snark ) ) )

let run () =
  let files = List.tl_exn @@ Array.to_list @@ Sys.get_argv () in
  Stdlib.Printf.printf "Module name: %s\n%!" __MODULE__ ;
  Stdlib.Printf.printf "To set a breakpoint in this file: break @ %s LINE\n%!"
    __MODULE__ ;
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
  let verify : (Protocol_state.Value.t * Proof.t) list -> bool =
    B.Proof.verify
  in
  let rec repl () =
    match get_input () with
    | Error err ->
        Stdlib.Printf.printf "Error processing input: %s\n%!" err ;
        repl ()
    | Ok input -> (
        Stdlib.Printf.printf "Calling verifier.\n%!" ;
        let before_time = Unix.gettimeofday () in
        let result = verify input in
        let after_time = Unix.gettimeofday () in
        Stdlib.Printf.printf "Verification time: %fs\n%!"
          (after_time -. before_time) ;
        match result with
        | true ->
            Stdlib.Printf.printf "Proofs verified successfully.\n%!" ;
            repl ()
        | false ->
            Stdlib.Printf.printf "Proofs failed to verify.\n%!" ;
            repl () )
  in
  let process_files files =
    match files with
    | [] ->
        Stdlib.Printf.printf "No more files to process.\n%!"
    | _file :: _rest ->
        Stdlib.exit 0
  in
  match files with [] -> repl () | files -> process_files files

let () =
  Random.self_init () ;
  Stdlib.Printf.printf "Starting verifier\n%!" ;
  run ()
