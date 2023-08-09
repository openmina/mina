open Core
open Async

module Interrupt_handler = struct
  type t = Pending_handler_setup | Idle | Child_running of Pid.t

  let state = ref Pending_handler_setup

  let interrupt state =
    match state with
    | Pending_handler_setup ->
        (* NOTE: this cannot happen, it is the handler itself that calls this function *)
        print_endline "[WARN] calling interrupt before setting up the handler" ;
        state
    | Idle ->
        print_endline "[WARN] calling interrupt but there is no child running" ;
        state
    | Child_running pid ->
        ( match Signal.send Signal.kill (`Pid pid) with
        | `Ok ->
            print_endline "Kill signal sent to child"
        | `No_such_process ->
            print_endline "Could not send kill signal, pid doesn't exist" ) ;
        Idle

  let maybe_setup_handler = function
    | Pending_handler_setup ->
        Signal.handle [ Signal.int ] ~f:(fun _ -> state := interrupt !state)
    | Idle ->
        ()
    | Child_running _ ->
        print_endline
          "[WARN] setting up a new pid with an older process still running"

  let setup pid =
    maybe_setup_handler !state ;
    state := Child_running pid ;
    Unix.waitpid pid
end

module Time_span_with_json = struct
  type t = Time.Span.t

  let to_yojson total = `String (Time.Span.to_string_hum total)

  let of_yojson = function
    | `String time ->
        Ok (Time.Span.of_string time)
    | _ ->
        Error "Snark_worker.Functor: Could not parse timespan"
end

(*FIX: register_event fails when adding base types to the constructors*)
module String_with_json = struct
  type t = string

  let to_yojson s = `String s

  let of_yojson = function
    | `String s ->
        Ok s
    | _ ->
        Error "Snark_worker.Functor: Could not parse string"
end

module Int_with_json = struct
  type t = int

  let to_yojson s = `Int s

  let of_yojson = function
    | `Int s ->
        Ok s
    | _ ->
        Error "Snark_worker.Functor: Could not parse int"
end

type Structured_log_events.t +=
  | Merge_snark_generated of { time : Time_span_with_json.t }
  [@@deriving register_event { msg = "Merge SNARK generated in $time" }]

type Structured_log_events.t +=
  | Base_snark_generated of
      { time : Time_span_with_json.t
      ; transaction_type : String_with_json.t
      ; zkapp_command_count : Int_with_json.t
      ; proof_zkapp_command_count : Int_with_json.t
      }
  [@@deriving
    register_event
      { msg =
          "Base SNARK generated in $time for $transaction_type transaction \
           with $zkapp_command_count zkapp_command and \
           $proof_zkapp_command_count proof zkapp_command"
      }]

module Make (Inputs : Intf.Inputs_intf) :
  Intf.S0 with type ledger_proof := Inputs.Ledger_proof.t = struct
  open Inputs
  module Rpcs = Rpcs.Make (Inputs)

  module Work = struct
    open Snark_work_lib

    module Single = struct
      module Spec = struct
        type t = (Transaction_witness.t, Ledger_proof.t) Work.Single.Spec.t
        [@@deriving sexp, yojson]

        let transaction t =
          Option.map (Work.Single.Spec.witness t) ~f:(fun w ->
              w.Transaction_witness.transaction )

        let statement = Work.Single.Spec.statement
      end
    end

    module Spec = struct
      type t = Single.Spec.t Work.Spec.t [@@deriving sexp, yojson]

      let instances = Work.Spec.instances
    end

    module Result = struct
      type t = (Spec.t, Ledger_proof.t) Work.Result.t

      let transactions (t : t) =
        One_or_two.map t.spec.instances ~f:(fun i -> Single.Spec.transaction i)
    end
  end

  let perform (s : Worker_state.t) public_key
      ({ instances; fee } as spec : Work.Spec.t) =
    One_or_two.Deferred_result.map instances ~f:(fun w ->
        let open Deferred.Or_error.Let_syntax in
        let%map proof, time =
          perform_single s
            ~message:(Mina_base.Sok_message.create ~fee ~prover:public_key)
            w
        in
        ( proof
        , (time, match w with Transition _ -> `Transition | Merge _ -> `Merge)
        ) )
    |> Deferred.Or_error.map ~f:(function
         | `One (proof1, metrics1) ->
             { Snark_work_lib.Work.Result.proofs = `One proof1
             ; metrics = `One metrics1
             ; spec
             ; prover = public_key
             }
         | `Two ((proof1, metrics1), (proof2, metrics2)) ->
             { Snark_work_lib.Work.Result.proofs = `Two (proof1, proof2)
             ; metrics = `Two (metrics1, metrics2)
             ; spec
             ; prover = public_key
             } )

  let dispatch rpc shutdown_on_disconnect query address =
    let%map res =
      Rpc.Connection.with_client
        ~handshake_timeout:
          (Time.Span.of_sec Mina_compile_config.rpc_handshake_timeout_sec)
        ~heartbeat_config:
          (Rpc.Connection.Heartbeat_config.create
             ~timeout:
               (Time_ns.Span.of_sec
                  Mina_compile_config.rpc_heartbeat_timeout_sec )
             ~send_every:
               (Time_ns.Span.of_sec
                  Mina_compile_config.rpc_heartbeat_send_every_sec )
             () )
        (Tcp.Where_to_connect.of_host_and_port address)
        (fun conn -> Rpc.Rpc.dispatch rpc conn query)
    in
    match res with
    | Error exn ->
        if shutdown_on_disconnect then
          failwithf
            !"Shutting down. Error using the RPC call, %s,: %s"
            (Rpc.Rpc.name rpc) (Exn.to_string_mach exn) ()
        else
          Error
            ( Error.createf
                !"Error using the RPC call, %s: %s"
                (Rpc.Rpc.name rpc)
            @@ Exn.to_string_mach exn )
    | Ok res ->
        res

  let emit_proof_metrics metrics instances logger =
    One_or_two.iter (One_or_two.zip_exn metrics instances)
      ~f:(fun ((time, tag), single) ->
        match tag with
        | `Merge ->
            Mina_metrics.(
              Cryptography.Snark_work_histogram.observe
                Cryptography.snark_work_merge_time_sec (Time.Span.to_sec time)) ;
            [%str_log info] (Merge_snark_generated { time })
        | `Transition ->
            let transaction_type, zkapp_command_count, proof_zkapp_command_count
                =
              (*should be Some in the case of `Transition*)
              match Option.value_exn single with
              | Mina_transaction.Transaction.Command
                  (Mina_base.User_command.Zkapp_command zkapp_command) ->
                  let init =
                    match
                      (Mina_base.Account_update.of_fee_payer
                         zkapp_command.Mina_base.Zkapp_command.fee_payer )
                        .authorization
                    with
                    | Proof _ ->
                        (1, 1)
                    | _ ->
                        (1, 0)
                  in
                  let c, p =
                    Mina_base.Zkapp_command.Call_forest.fold
                      zkapp_command.account_updates ~init
                      ~f:(fun (count, proof_updates_count) account_update ->
                        ( count + 1
                        , if
                            Mina_base.Control.(
                              Tag.equal Proof
                                (tag
                                   (Mina_base.Account_update.authorization
                                      account_update ) ))
                          then proof_updates_count + 1
                          else proof_updates_count ) )
                  in
                  Mina_metrics.(
                    Cryptography.(
                      Counter.inc snark_work_zkapp_base_time_sec
                        (Time.Span.to_sec time) ;
                      Counter.inc_one snark_work_zkapp_base_submissions ;
                      Counter.inc zkapp_transaction_length (Float.of_int c) ;
                      Counter.inc zkapp_proof_updates (Float.of_int p))) ;
                  ("zkapp_command", c, p)
              | Command (Signed_command _) ->
                  Mina_metrics.(
                    Counter.inc Cryptography.snark_work_base_time_sec
                      (Time.Span.to_sec time)) ;
                  ("signed command", 1, 0)
              | Coinbase _ ->
                  Mina_metrics.(
                    Counter.inc Cryptography.snark_work_base_time_sec
                      (Time.Span.to_sec time)) ;
                  ("coinbase", 1, 0)
              | Fee_transfer _ ->
                  Mina_metrics.(
                    Counter.inc Cryptography.snark_work_base_time_sec
                      (Time.Span.to_sec time)) ;
                  ("fee_transfer", 1, 0)
            in
            [%str_log info]
              (Base_snark_generated
                 { time
                 ; transaction_type
                 ; zkapp_command_count
                 ; proof_zkapp_command_count
                 } ) )

  let main
      (module Rpcs_versioned : Intf.Rpcs_versioned_S
        with type Work.ledger_proof = Inputs.Ledger_proof.t ) ~logger
      ~proof_level daemon_address shutdown_on_disconnect =
    let constraint_constants =
      (* TODO: Make this configurable. *)
      Genesis_constants.Constraint_constants.compiled
    in
    let%bind state =
      Worker_state.create ~constraint_constants ~proof_level ()
    in
    let wait ?(sec = 0.5) () = after (Time.Span.of_sec sec) in
    (* retry interval with jitter *)
    let retry_pause sec = Random.float_range (sec -. 2.0) (sec +. 2.0) in
    let log_and_retry label error sec k =
      let error_str = Error.to_string_hum error in
      (* HACK: the bind before the call to go () produces an evergrowing
           backtrace history which takes forever to print and fills our disks.
           If the string becomes too long, chop off the first 10 lines and include
           only that *)
      ( if String.length error_str < 4096 then
        [%log error] !"Error %s: %{sexp:Error.t}" label error
      else
        let lines = String.split ~on:'\n' error_str in
        [%log error] !"Error %s: %s" label
          (String.concat ~sep:"\\n" (List.take lines 10)) ) ;
      let%bind () = wait ~sec () in
      (* FIXME: Use a backoff algo here *)
      k ()
    in
    let rec go () =
      let%bind daemon_address =
        let%bind cwd = Sys.getcwd () in
        [%log debug]
          !"Snark worker working directory $dir"
          ~metadata:[ ("dir", `String cwd) ] ;
        let path = "snark_coordinator" in
        match%bind Sys.file_exists path with
        | `Yes -> (
            let%map s = Reader.file_contents path in
            try Host_and_port.of_string (String.strip s)
            with _ -> daemon_address )
        | `No | `Unknown ->
            return daemon_address
      in
      [%log debug]
        !"Snark worker using daemon $addr"
        ~metadata:[ ("addr", `String (Host_and_port.to_string daemon_address)) ] ;
      match%bind
        dispatch Rpcs_versioned.Get_work.Latest.rpc shutdown_on_disconnect ()
          daemon_address
      with
      | Error e ->
          log_and_retry "getting work" e (retry_pause 10.) go
      | Ok None ->
          let random_delay =
            Worker_state.worker_wait_time
            +. (0.5 *. Random.float Worker_state.worker_wait_time)
          in
          (* No work to be done -- quietly take a brief nap *)
          [%log info] "No jobs available. Napping for $time seconds"
            ~metadata:[ ("time", `Float random_delay) ] ;
          let%bind () = wait ~sec:random_delay () in
          go ()
      | Ok (Some (work, public_key)) -> (
          [%log info]
            "SNARK work $work_ids received from $address. Starting proof \
             generation"
            ~metadata:
              [ ("address", `String (Host_and_port.to_string daemon_address))
              ; ( "work_ids"
                , Transaction_snark_work.Statement.compact_json
                    (One_or_two.map (Work.Spec.instances work)
                       ~f:Work.Single.Spec.statement ) )
              ] ;
          let%bind () = wait () in
          (* Pause to wait for stdout to flush *)
          match%bind perform state public_key work with
          | Error e ->
              let%bind () =
                match%map
                  dispatch Rpcs_versioned.Failed_to_generate_snark.Latest.rpc
                    shutdown_on_disconnect (e, work, public_key) daemon_address
                with
                | Error e ->
                    [%log error]
                      "Couldn't inform the daemon about the snark work failure"
                      ~metadata:[ ("error", Error_json.error_to_yojson e) ]
                | Ok () ->
                    ()
              in
              log_and_retry "performing work" e (retry_pause 10.) go
          | Ok result ->
              emit_proof_metrics result.metrics
                (Work.Result.transactions result)
                logger ;
              [%log info] "Submitted completed SNARK work $work_ids to $address"
                ~metadata:
                  [ ("address", `String (Host_and_port.to_string daemon_address))
                  ; ( "work_ids"
                    , Transaction_snark_work.Statement.compact_json
                        (One_or_two.map (Work.Spec.instances work)
                           ~f:Work.Single.Spec.statement ) )
                  ] ;
              let rec submit_work () =
                match%bind
                  dispatch Rpcs_versioned.Submit_work.Latest.rpc
                    shutdown_on_disconnect result daemon_address
                with
                | Error e ->
                    log_and_retry "submitting work" e (retry_pause 10.)
                      submit_work
                | Ok () ->
                    go ()
              in
              submit_work () )
    in
    go ()

  let command_from_rpcs
      (module Rpcs_versioned : Intf.Rpcs_versioned_S
        with type Work.ledger_proof = Inputs.Ledger_proof.t ) =
    Command.async ~summary:"Snark worker"
      (let open Command.Let_syntax in
      let%map_open daemon_port =
        flag "--daemon-address" ~aliases:[ "daemon-address" ]
          (required (Arg_type.create Host_and_port.of_string))
          ~doc:"HOST-AND-PORT address daemon is listening on"
      and proof_level =
        flag "--proof-level" ~aliases:[ "proof-level" ]
          (optional (Arg_type.create Genesis_constants.Proof_level.of_string))
          ~doc:"full|check|none"
      and shutdown_on_disconnect =
        flag "--shutdown-on-disconnect"
          ~aliases:[ "shutdown-on-disconnect" ]
          (optional bool)
          ~doc:
            "true|false Shutdown when disconnected from daemon (default:true)"
      and conf_dir = Cli_lib.Flag.conf_dir in
      fun () ->
        let logger =
          Logger.create () ~metadata:[ ("process", `String "Snark Worker") ]
        in
        Option.value_map ~default:() conf_dir ~f:(fun conf_dir ->
            let logrotate_max_size = 1024 * 10 in
            let logrotate_num_rotate = 1 in
            Logger.Consumer_registry.register ~id:Logger.Logger_id.snark_worker
              ~processor:(Logger.Processor.raw ())
              ~transport:
                (Logger_file_system.dumb_logrotate ~directory:conf_dir
                   ~log_filename:"mina-snark-worker.log"
                   ~max_size:logrotate_max_size ~num_rotate:logrotate_num_rotate ) ) ;
        Signal.handle [ Signal.term ] ~f:(fun _signal ->
            [%log info]
              !"Received signal to terminate. Aborting snark worker process" ;
            Core.exit 0 ) ;
        let proof_level =
          Option.value ~default:Genesis_constants.Proof_level.compiled
            proof_level
        in
        main
          (module Rpcs_versioned)
          ~logger ~proof_level daemon_port
          (Option.value ~default:true shutdown_on_disconnect))

  let rec go_stdio
      (module Rpcs_versioned : Intf.Rpcs_versioned_S
        with type Work.ledger_proof = Inputs.Ledger_proof.t ) ~logger ~output_fd
      ~input_fd worker_state =
    let read buf ~pos ~len =
      Core_unix_bigstring_unix.really_read input_fd ~pos ~len buf
    in
    let input =
      try
        Bin_prot.Utils.bin_read_stream ~read
          Rpcs_versioned.Get_work.Latest.bin_reader_response
      with exn ->
        [%log error] "Error when reading from input fd"
          ~metadata:[ ("error", `String (Exn.to_string exn)) ] ;
        None
    in
    match input with
    | None ->
        [%log info] "EOF reached in worker, exiting" ;
        exit 0
    | Some (work, public_key) ->
        let work_ids =
          Transaction_snark_work.Statement.compact_json
            (One_or_two.map (Work.Spec.instances work)
               ~f:Work.Single.Spec.statement )
        in
        [%log info]
          "SNARK work $work_ids received from stdin. Starting proof generation"
          ~metadata:[ ("work_ids", work_ids) ] ;
        let%bind result = perform worker_state public_key work in
        ( match result with
        | Error error ->
            [%log error] "Error generating proof"
              ~metadata:
                [ ("work_ids", work_ids)
                ; ("error", `String (Error.to_string_hum error))
                ]
        | Ok result -> (
            [%log info] "Proof generated successfully"
              ~metadata:[ ("work_ids", work_ids) ] ;
            let dump =
              Bin_prot.Utils.bin_dump ~header:true
                Rpcs_versioned.Submit_work.Latest.bin_writer_query result
            in
            try Core_unix_bigstring_unix.really_write output_fd dump
            with _ -> printf "closed file descriptor\n" ) ) ;
        go_stdio
          (module Rpcs_versioned)
          ~logger ~output_fd ~input_fd worker_state

  let command_from_stdio
      (module Rpcs_versioned : Intf.Rpcs_versioned_S
        with type Work.ledger_proof = Inputs.Ledger_proof.t ) =
    let open Command.Let_syntax in
    Command.async ~summary:"Run snark worker directly"
      (let%map_open proof_level =
         flag "--proof-level" ~doc:""
           (optional_with_default Genesis_constants.Proof_level.Full
              (Command.Arg_type.of_alist_exn
                 [ ("Full", Genesis_constants.Proof_level.Full)
                 ; ("Check", Check)
                 ; ("None", None)
                 ] ) )
       in
       fun () ->
         let logger =
           Logger.create () ~metadata:[ ("process", `String "Snark Worker") ]
         in
         [%log info] "Starting standalone snark worker..." ;
         let open Async in
         (* We toggle this flag so that rayon doesn't use the global thread pool. In doing
            so we ensure that the forked child process is in charge of creating the
            global pool so that no state is lost during the fork. *)
         Kimchi_bindings.Rayon.toggle_thread_pool true ;
         let%bind worker_state =
           Worker_state.create
             ~constraint_constants:
               Genesis_constants.Constraint_constants.compiled ~proof_level ()
         in
         Kimchi_bindings.Rayon.toggle_thread_pool false ;
         [%log info] "Snark worker state is initialized" ;
         (* Lets run a full GC cycle before forking *)
         Gc.compact () ;
         let rec fork_and_work_loop () =
           (* Cannot create with Async because that will create new threads to handle the pipes.
              So we first create raw file descriptor pipes and then wrap them in Reader/Writer
              when necessary, and not before. *)
           let m2w_r, m2w_w = Core.Unix.pipe ~close_on_exec:false () in
           let w2m_r, w2m_w = Core.Unix.pipe ~close_on_exec:false () in
           match Core.Unix.fork () with
           | `In_the_child ->
               Core.Unix.close m2w_w ;
               Core.Unix.close w2m_r ;
               [%log info] "calling go_stdio" ;
               go_stdio
                 (module Rpcs_versioned)
                 ~logger ~output_fd:w2m_w ~input_fd:m2w_r worker_state
           | `In_the_parent child_pid ->
               [%log info] "Forked, child PID: $pid"
                 ~metadata:[ ("pid", `Int (Pid.to_int child_pid)) ] ;
               Core.Unix.close m2w_r ;
               Core.Unix.close w2m_w ;
               let w2m_r =
                 Reader.create
                   (Fd.create Fd.Kind.Fifo w2m_r (Info.of_string "w2m_r"))
               in
               let m2w_w =
                 Writer.create
                   (Fd.create Fd.Kind.Fifo m2w_w (Info.of_string "m2w_w"))
               in
               let wait_for_child = Interrupt_handler.setup child_pid in
               let rec handle_jobs_loop () =
                 let read buf ~pos ~len =
                   Core_unix_bigstring_unix.really_read Core_unix.stdin ~pos
                     ~len buf
                 in
                 let input =
                   try
                     Bin_prot.Utils.bin_read_stream ~read
                       Rpcs_versioned.Get_work.Latest.bin_reader_response
                   with exn ->
                     [%log info]
                       "EOF or error when reading from stdin, quitting"
                       ~metadata:[ ("error", `String (Exn.to_string exn)) ] ;
                     Core.exit 0
                 in
                 Writer.write_bin_prot m2w_w
                   Rpcs_versioned.Get_work.V2.bin_writer_response input ;
                 let%bind result =
                   choose
                     [ choice wait_for_child (fun s -> `Child_exit s)
                     ; choice
                         (Reader.read_bin_prot w2m_r
                            Rpcs_versioned.Submit_work.Latest.bin_reader_query )
                         (fun data -> `Response data)
                     ]
                 in
                 match result with
                 | `Child_exit exit_status ->
                     [%log info] "Child exited (choose), status: %s"
                       (Core_unix.Exit_or_signal.to_string_hum exit_status) ;
                     Deferred.unit
                 | `Response rr -> (
                     match rr with
                     | `Eof ->
                         [%log info] "Child reached EOF" ;
                         Deferred.unit
                     | `Ok result ->
                         let dump =
                           Bin_prot.Utils.bin_dump ~header:true
                             Rpcs_versioned.Submit_work.Latest.bin_writer_query
                             result
                         in
                         Core_unix_bigstring_unix.really_write Core.Unix.stderr
                           dump ;
                         handle_jobs_loop () )
               in
               let%bind () = handle_jobs_loop () in
               fork_and_work_loop ()
         in
         fork_and_work_loop () )

  let arguments ~proof_level ~daemon_address ~shutdown_on_disconnect =
    [ "-daemon-address"
    ; Host_and_port.to_string daemon_address
    ; "-proof-level"
    ; Genesis_constants.Proof_level.to_string proof_level
    ; "-shutdown-on-disconnect"
    ; Bool.to_string shutdown_on_disconnect
    ]
end
