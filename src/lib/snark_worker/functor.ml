open Core
open Async

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

  let coordinator_url =
    Core_kernel.Sys.getenv_opt "MINA_SNARK_COORDINATOR_URL"
    |> Option.value ~default:"http://localhost:8080"

  let hostname =
    Core_kernel.Sys.getenv_opt "HOSTNAME"
    |> Option.map ~f:(fun v -> List.nth (String.split_on_chars v ~on:[ '-' ]) 0)
    |> Option.join

  let snarker_name =
    Core_kernel.Sys.getenv_opt "MINA_NODE_NAME"
    |> fun opt -> match opt with Some v -> Some v | None -> hostname

  let coordinator_stats_put id content =
    let url = String.concat ~sep:"/" [ coordinator_url; "worker-stats"; id ] in
    match Ezcurl.put ~url ~content:(`String content) () with
    | Ok response ->
        Some response.Ezcurl.body
    | Error (_curl_code, msg) ->
        Stdlib.Printf.printf
          "+++ coordinator stats put: %s %s -> failed: %s \n%!" url content msg ;
        None

  let int_of_time t = t *. 1000. |> round |> int_of_float

  let main
      (module Rpcs_versioned : Intf.Rpcs_versioned_S
        with type Work.ledger_proof = Inputs.Ledger_proof.t ) ~logger
      ~proof_level daemon_address shutdown_on_disconnect =
    let constraint_constants =
      (* TODO: Make this configurable. *)
      Genesis_constants.Constraint_constants.compiled
    in
    let now () = Unix.gettimeofday () |> int_of_time in
    let register_t = now () in
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
    let id_opt : string option ref = ref None in
    let notify_job_get_init t =
      Option.map !id_opt ~f:(fun id ->
          coordinator_stats_put id
            (Yojson.Safe.to_string
               (`Assoc [ ("kind", `String "JobGetInit"); ("time", `Int t) ]) ) )
    in
    let notify_job_get_error t error
        ( received_time
        , request_work_start_time
        , request_work_end_time
        , respond_time ) =
      Option.map !id_opt ~f:(fun id ->
          let request_work_end_time =
            if Float.(request_work_end_time < 0.0) then respond_time
            else request_work_end_time
          in
          coordinator_stats_put id
            (Yojson.Safe.to_string
               (`Assoc
                 [ ("kind", `String "JobGetError")
                 ; ("time", `Int t)
                 ; ("error", error)
                 ; ("job_get_node_received_t", `Int (int_of_time received_time))
                 ; ( "job_get_node_request_work_init_t"
                   , `Int (int_of_time request_work_start_time) )
                 ; ( "job_get_node_request_work_success_t"
                   , `Int (int_of_time request_work_end_time) )
                 ] ) ) )
    in
    let notify_job_get_success t job_ids
        ( received_time
        , request_work_start_time
        , request_work_end_time
        , respond_time ) =
      Option.map !id_opt ~f:(fun id ->
          let request_work_end_time =
            if Float.(request_work_end_time < 0.0) then respond_time
            else request_work_end_time
          in
          coordinator_stats_put id
            (Yojson.Safe.to_string
               (`Assoc
                 [ ("kind", `String "JobGetSuccess")
                 ; ("time", `Int t)
                 ; ("ids", `String job_ids)
                 ; ("job_get_node_received_t", `Int (int_of_time received_time))
                 ; ( "job_get_node_request_work_init_t"
                   , `Int (int_of_time request_work_start_time) )
                 ; ( "job_get_node_request_work_success_t"
                   , `Int (int_of_time request_work_end_time) )
                 ] ) ) )
    in
    let notify_work_create_error t job_ids error =
      Option.map !id_opt ~f:(fun id ->
          coordinator_stats_put id
            (Yojson.Safe.to_string
               (`Assoc
                 [ ("kind", `String "WorkCreateError")
                 ; ("time", `Int t)
                 ; ("ids", `String job_ids)
                 ; ("error", `String error)
                 ] ) ) )
    in
    let notify_work_create_success t job_ids =
      Option.map !id_opt ~f:(fun id ->
          coordinator_stats_put id
            (Yojson.Safe.to_string
               (`Assoc
                 [ ("kind", `String "WorkCreateSuccess")
                 ; ("time", `Int t)
                 ; ("ids", `String job_ids)
                 ] ) ) )
    in
    (* let notify_work_submit_error t job_ids error =
         Option.map !id_opt ~f:(fun id ->
             coordinator_stats_put id
               (Yojson.Safe.to_string
                  (`Assoc
                    [ ("kind", `String "WorkSubmitError")
                    ; ("time", `Int t)
                    ; ("ids", `String job_ids)
                    ; ("error", `String error)
                    ] ) ) )
       in *)
    let notify_work_submit_success t job_ids
        (received_time, add_work_start_time, add_work_end_time) =
      Option.map !id_opt ~f:(fun id ->
          coordinator_stats_put id
            (Yojson.Safe.to_string
               (`Assoc
                 [ ("kind", `String "WorkSubmitSuccess")
                 ; ("time", `Int t)
                 ; ("ids", `String job_ids)
                 ; ( "work_submit_node_received_t"
                   , `Int (int_of_time received_time) )
                 ; ( "work_submit_node_add_work_init_t"
                   , `Int (int_of_time add_work_start_time) )
                 ; ( "work_submit_node_add_work_success_t"
                   , `Int (int_of_time add_work_end_time) )
                 ] ) ) )
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
      let job_get_init_t = now () in
      let _ = notify_job_get_init job_get_init_t in
      match%bind
        dispatch Rpcs_versioned.Get_work.Latest.rpc shutdown_on_disconnect ()
          daemon_address
      with
      | Error e ->
          let err =
            `Assoc
              [ ("kind", `String "Other")
              ; ("error", `String (Error.to_string_hum e))
              ]
          in
          let _ = notify_job_get_error (now ()) err (-1.0, -1.0, -1.0, -1.0) in
          log_and_retry "getting work" e (retry_pause 10.) go
      | Ok (None, times) ->
          let err = `Assoc [ ("kind", `String "NoAvailableJob") ] in
          let _ = notify_job_get_error (now ()) err times in
          let random_delay =
            Worker_state.worker_wait_time
            +. (0.5 *. Random.float Worker_state.worker_wait_time)
          in
          (* No work to be done -- quietly take a brief nap *)
          [%log info] "No jobs available. Napping for $time seconds"
            ~metadata:[ ("time", `Float random_delay) ] ;
          let%bind () = wait ~sec:random_delay () in
          go ()
      | Ok (Some (work, public_key), times) -> (
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

          ( match !id_opt with
          | Some _ ->
              ()
          | None ->
              let key =
                Option.value snarker_name
                  ~default:
                    (Signature_lib.Public_key.Compressed.to_base58_check
                       public_key )
              in
              let content =
                Yojson.Safe.to_string
                  (`Assoc
                    [ ("kind", `String "Register"); ("time", `Int register_t) ]
                    )
              in
              id_opt := coordinator_stats_put key content ;
              let _ = notify_job_get_init job_get_init_t in
              () ) ;
          let hash_single_work_spec s =
            Work.Single.Spec.statement s
            |> Transaction_snark.Statement.hash |> string_of_int
          in
          let job_ids =
            match One_or_two.to_list (Work.Spec.instances work) with
            | [ single ] ->
                hash_single_work_spec single
            | [ first; second ] ->
                hash_single_work_spec first ^ "," ^ hash_single_work_spec second
            | _ ->
                assert false
          in
          let _ = notify_job_get_success (now ()) job_ids times in

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
              let _ =
                notify_work_create_error (now ()) job_ids
                  (Error.to_string_hum e)
              in
              log_and_retry "performing work" e (retry_pause 10.) go
          | Ok result ->
              emit_proof_metrics result.metrics
                (Work.Result.transactions result)
                logger ;
              let _ = notify_work_create_success (now ()) job_ids in
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
                | Ok times ->
                    let _ = notify_work_submit_success (now ()) job_ids times in
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

  let arguments ~proof_level ~daemon_address ~shutdown_on_disconnect =
    [ "-daemon-address"
    ; Host_and_port.to_string daemon_address
    ; "-proof-level"
    ; Genesis_constants.Proof_level.to_string proof_level
    ; "-shutdown-on-disconnect"
    ; Bool.to_string shutdown_on_disconnect
    ]
end
