open Async_kernel
open Core_kernel
open Pipe_lib.Strict_pipe
open Mina_base
open Mina_state
open Cache_lib
open Mina_block
open Network_peer
open Internal_tracing

module type CONTEXT = sig
  val logger : Logger.t

  val precomputed_values : Precomputed_values.t

  val constraint_constants : Genesis_constants.Constraint_constants.t

  val consensus_constants : Consensus.Constants.t
end

let validate_transition ~context:(module Context : CONTEXT) ~frontier
    ~unprocessed_transition_cache enveloped_transition =
  let module Context = struct
    include Context

    let logger =
      Logger.extend logger
        [ ("selection_context", `String "Transition_handler.Validator") ]
  end in
  let open Result.Let_syntax in
  let transition =
    Envelope.Incoming.data enveloped_transition
    |> Mina_block.Validation.block_with_hash
  in
  let transition_hash = State_hash.With_state_hashes.state_hash transition in
  Block_tracing.External.checkpoint_current `Validate_transition ;
  let root_breadcrumb = Transition_frontier.root frontier in
  let blockchain_length =
    Envelope.Incoming.data enveloped_transition
    |> Mina_block.Validation.block |> Mina_block.blockchain_length
    |> Mina_numbers.Length.to_int
  in
  Block_tracing.External.checkpoint_current ~blockchain_length
    `Check_transition_not_in_frontier ;
  let%bind () =
    Option.fold
      (Transition_frontier.find frontier transition_hash)
      ~init:Result.(Ok ())
      ~f:(fun _ _ -> Result.Error (`In_frontier transition_hash))
  in
  Block_tracing.External.checkpoint_current `Check_transition_not_in_process ;
  let%bind () =
    Option.fold
      (Unprocessed_transition_cache.final_state unprocessed_transition_cache
         enveloped_transition )
      ~init:Result.(Ok ())
      ~f:(fun _ final_state -> Result.Error (`In_process final_state))
  in
  Block_tracing.External.checkpoint_current `Check_transition_can_be_connected ;
  let%map () =
    Result.ok_if_true
      (Consensus.Hooks.equal_select_status `Take
         (Consensus.Hooks.select
            ~context:(module Context)
            ~existing:
              (Transition_frontier.Breadcrumb.consensus_state_with_hashes
                 root_breadcrumb )
            ~candidate:(With_hash.map ~f:Mina_block.consensus_state transition) ) )
      ~error:`Disconnected
  in
  Block_tracing.External.checkpoint_current `Register_transition_for_processing ;
  (* we expect this to be Ok since we just checked the cache *)
  Unprocessed_transition_cache.register_exn unprocessed_transition_cache
    enveloped_transition

let run ~context:(module Context : CONTEXT) ~trust_system ~time_controller
    ~frontier ~transition_reader
    ~(valid_transition_writer :
       ( [ `Block of
           ( Mina_block.initial_valid_block Envelope.Incoming.t
           , State_hash.t )
           Cached.t ]
         * [ `Valid_cb of Mina_net2.Validation_callback.t option ]
       , drop_head buffered
       , unit )
       Writer.t ) ~unprocessed_transition_cache =
  let open Context in
  let module Lru = Core_extended_cache.Lru in
  O1trace.background_thread "validate_blocks_against_frontier" (fun () ->
      Reader.iter transition_reader
        ~f:(fun (`Block transition_env, `Valid_cb vc) ->
          let transition_with_hash, _ = Envelope.Incoming.data transition_env in
          let transition_hash =
            State_hash.With_state_hashes.state_hash transition_with_hash
          in
          let state_hash_b58 = State_hash.to_base58_check transition_hash in
          Block_tracing.External.with_state_hash (Some state_hash_b58)
          @@ fun () ->
          let transition = With_hash.data transition_with_hash in
          let sender = Envelope.Incoming.sender transition_env in
          match
            validate_transition
              ~context:(module Context)
              ~frontier ~unprocessed_transition_cache transition_env
          with
          | Ok cached_transition ->
              let%map () =
                Trust_system.record_envelope_sender trust_system logger sender
                  ( Trust_system.Actions.Sent_useful_gossip
                  , Some
                      ( "external transition $state_hash"
                      , [ ("state_hash", State_hash.to_yojson transition_hash)
                        ; ("transition", Mina_block.to_yojson transition)
                        ] ) )
              in
              let transition_time =
                Mina_block.header transition
                |> Header.protocol_state |> Protocol_state.blockchain_state
                |> Blockchain_state.timestamp |> Block_time.to_time_exn
              in
              Perf_histograms.add_span
                ~name:"accepted_transition_remote_latency"
                (Core_kernel.Time.diff
                   Block_time.(now time_controller |> to_time_exn)
                   transition_time ) ;
              Block_tracing.External.complete state_hash_b58 ;
              Writer.write valid_transition_writer
                (`Block cached_transition, `Valid_cb vc)
          | Error (`In_frontier _) | Error (`In_process _) ->
              Block_tracing.External.failure ~reason:"In_frontier or In_process"
                state_hash_b58 ;
              Trust_system.record_envelope_sender trust_system logger sender
                ( Trust_system.Actions.Sent_old_gossip
                , Some
                    ( "external transition with state hash $state_hash"
                    , [ ("state_hash", State_hash.to_yojson transition_hash)
                      ; ("transition", Mina_block.to_yojson transition)
                      ] ) )
          | Error `Disconnected ->
              Block_tracing.External.failure ~reason:"Disconnected"
                state_hash_b58 ;
              Mina_metrics.(Counter.inc_one Rejected_blocks.worse_than_root) ;
              [%log error]
                ~metadata:
                  [ ("state_hash", State_hash.to_yojson transition_hash)
                  ; ("reason", `String "not selected over current root")
                  ; ( "protocol_state"
                    , Header.protocol_state (Mina_block.header transition)
                      |> Protocol_state.value_to_yojson )
                  ]
                "Validation error: external transition with state hash \
                 $state_hash was rejected for reason $reason" ;
              Trust_system.record_envelope_sender trust_system logger sender
                ( Trust_system.Actions.Disconnected_chain
                , Some
                    ( "received transition that was not connected to our chain \
                       from $sender"
                    , [ ( "sender"
                        , Envelope.Sender.to_yojson
                            (Envelope.Incoming.sender transition_env) )
                      ; ("transition", Mina_block.to_yojson transition)
                      ] ) ) ) )
