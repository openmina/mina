open Core
open Async
open Signature_lib
open Mina_base

(* open Mina_state *)
open Mina_block

module Client = Graphql_lib.Client.Make (struct
  let preprocess_variables_string = Fn.id

  let headers = String.Map.empty
end)

module Args = struct
  open Command.Param

  let zip2 = map2 ~f:(fun arg1 arg2 -> (arg1, arg2))

  let zip3 = map3 ~f:(fun arg1 arg2 arg3 -> (arg1, arg2, arg3))

  let zip4 arg1 arg2 arg3 arg4 =
    return (fun a b c d -> (a, b, c, d)) <*> arg1 <*> arg2 <*> arg3 <*> arg4

  let zip5 arg1 arg2 arg3 arg4 arg5 =
    return (fun a b c d e -> (a, b, c, d, e))
    <*> arg1 <*> arg2 <*> arg3 <*> arg4 <*> arg5

  let zip6 arg1 arg2 arg3 arg4 arg5 arg6 =
    return (fun a b c d e f -> (a, b, c, d, e, f))
    <*> arg1 <*> arg2 <*> arg3 <*> arg4 <*> arg5 <*> arg6

  let zip7 arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
    return (fun a b c d e f g -> (a, b, c, d, e, f, g))
    <*> arg1 <*> arg2 <*> arg3 <*> arg4 <*> arg5 <*> arg6 <*> arg7
end

let or_error_str ~f_ok ~error = function
  | Ok x ->
      f_ok x
  | Error e ->
      sprintf "%s\n%s\n" error (Error.to_string_hum e)

let stop_daemon =
  let open Deferred.Let_syntax in
  let open Daemon_rpcs in
  let open Command.Param in
  Command.async ~summary:"Stop the daemon"
    (Cli_lib.Background_daemon.rpc_init (return ()) ~f:(fun port () ->
         let%map res = Daemon_rpcs.Client.dispatch Stop_daemon.rpc () port in
         printf "%s"
           (or_error_str res
              ~f_ok:(fun _ -> "Daemon stopping\n")
              ~error:"Daemon likely stopped" ) ) )

let get_balance_graphql =
  let open Command.Param in
  let pk_flag =
    flag "--public-key" ~aliases:[ "public-key" ]
      ~doc:"PUBLICKEY Public key for which you want to check the balance"
      (required Cli_lib.Arg_type.public_key_compressed)
  in
  let token_flag =
    flag "--token" ~aliases:[ "token" ]
      ~doc:"TOKEN_ID The token ID for the account"
      (optional_with_default Token_id.default Cli_lib.Arg_type.token_id)
  in
  Command.async ~summary:"Get balance associated with a public key"
    (Cli_lib.Background_daemon.graphql_init (Args.zip2 pk_flag token_flag)
       ~f:(fun graphql_endpoint (public_key, token) ->
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Get_tracked_account.(
               make @@ makeVariables ~public_key ~token ())
             graphql_endpoint
         in
         match response.account with
         | Some account ->
             if Token_id.(equal default) token then
               printf "Balance: %s mina\n"
                 (Currency.Balance.to_formatted_string account.balance.total)
             else
               printf "Balance: %s tokens\n"
                 (Currency.Balance.to_formatted_string account.balance.total)
         | None ->
             printf "There are no funds in this account\n" ) )

let get_tokens_graphql =
  let open Command.Param in
  let pk_flag =
    flag "--public-key" ~aliases:[ "public-key" ]
      ~doc:"PUBLICKEY Public key for which you want to find accounts"
      (required Cli_lib.Arg_type.public_key_compressed)
  in
  Command.async ~summary:"Get all token IDs that a public key has accounts for"
    (Cli_lib.Background_daemon.graphql_init pk_flag
       ~f:(fun graphql_endpoint public_key ->
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Get_all_accounts.(
               make @@ makeVariables ~public_key ())
             graphql_endpoint
         in
         printf "Accounts are held for token IDs:\n" ;
         Array.iter response.accounts ~f:(fun account ->
             printf "%s " (Token_id.to_string account.token) ) ) )

let get_time_offset_graphql =
  Command.async
    ~summary:
      "Get the time offset in seconds used by the daemon to convert real time \
       into blockchain time"
    (Cli_lib.Background_daemon.graphql_init (Command.Param.return ())
       ~f:(fun graphql_endpoint () ->
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Time_offset.(make @@ makeVariables ())
             graphql_endpoint
         in
         let time_offset = response.timeOffset in
         printf
           "Current time offset:\n\
            %i\n\n\
            Start other daemons with this offset by setting the \
            MINA_TIME_OFFSET environment variable in the shell before \
            executing them:\n\
            export MINA_TIME_OFFSET=%i\n"
           time_offset time_offset ) )

let print_trust_statuses statuses json =
  if json then
    printf "%s\n"
      (Yojson.Safe.to_string
         (`List
           (List.map
              ~f:(fun (peer, status) ->
                `List
                  [ Network_peer.Peer.to_yojson peer
                  ; Trust_system.Peer_status.to_yojson status
                  ] )
              statuses ) ) )
  else
    let ban_status status =
      match status.Trust_system.Peer_status.banned with
      | Unbanned ->
          "Unbanned"
      | Banned_until tm ->
          sprintf "Banned_until %s" (Time.to_string_abs tm ~zone:Time.Zone.utc)
    in
    List.fold ~init:()
      ~f:(fun () (peer, status) ->
        printf "%s, %0.04f, %s\n"
          (Network_peer.Peer.to_multiaddr_string peer)
          status.trust (ban_status status) )
      statuses

let round_trust_score trust_status =
  let open Trust_system.Peer_status in
  let trust = Float.round_decimal trust_status.trust ~decimal_digits:4 in
  { trust_status with trust }

let get_trust_status =
  let open Command.Param in
  let open Deferred.Let_syntax in
  let address_flag =
    flag "--ip-address" ~aliases:[ "ip-address" ]
      ~doc:
        "IP An IPv4 or IPv6 address for which you want to query the trust \
         status"
      (required Cli_lib.Arg_type.ip_address)
  in
  let json_flag = Cli_lib.Flag.json in
  let flags = Args.zip2 address_flag json_flag in
  Command.async ~summary:"Get the trust status associated with an IP address"
    (Cli_lib.Background_daemon.rpc_init flags ~f:(fun port (ip_address, json) ->
         match%map
           Daemon_rpcs.Client.dispatch Daemon_rpcs.Get_trust_status.rpc
             ip_address port
         with
         | Ok statuses ->
             print_trust_statuses
               (List.map
                  ~f:(fun (peer, status) -> (peer, round_trust_score status))
                  statuses )
               json
         | Error e ->
             printf "Failed to get trust status %s\n" (Error.to_string_hum e) )
    )

let ip_trust_statuses_to_yojson ip_trust_statuses =
  let items =
    List.map ip_trust_statuses ~f:(fun (ip_addr, status) ->
        `Assoc
          [ ("ip", `String (Unix.Inet_addr.to_string ip_addr))
          ; ("status", Trust_system.Peer_status.to_yojson status)
          ] )
  in
  `List items

let get_trust_status_all =
  let open Command.Param in
  let open Deferred.Let_syntax in
  let nonzero_flag =
    flag "--nonzero-only" ~aliases:[ "nonzero-only" ] no_arg
      ~doc:"Only show trust statuses whose trust score is nonzero"
  in
  let json_flag = Cli_lib.Flag.json in
  let flags = Args.zip2 nonzero_flag json_flag in
  Command.async
    ~summary:"Get trust statuses for all peers known to the trust system"
    (Cli_lib.Background_daemon.rpc_init flags ~f:(fun port (nonzero, json) ->
         match%map
           Daemon_rpcs.Client.dispatch Daemon_rpcs.Get_trust_status_all.rpc ()
             port
         with
         | Ok ip_trust_statuses ->
             (* always round the trust scores for display *)
             let ip_rounded_trust_statuses =
               List.map ip_trust_statuses ~f:(fun (ip_addr, status) ->
                   (ip_addr, round_trust_score status) )
             in
             let filtered_ip_trust_statuses =
               if nonzero then
                 List.filter ip_rounded_trust_statuses
                   ~f:(fun (_ip_addr, status) ->
                     not Float.(equal status.trust zero) )
               else ip_rounded_trust_statuses
             in
             print_trust_statuses filtered_ip_trust_statuses json
         | Error e ->
             printf "Failed to get trust statuses %s\n" (Error.to_string_hum e) )
    )

let reset_trust_status =
  let open Command.Param in
  let open Deferred.Let_syntax in
  let address_flag =
    flag "--ip-address" ~aliases:[ "ip-address" ]
      ~doc:
        "IP An IPv4 or IPv6 address for which you want to reset the trust \
         status"
      (required Cli_lib.Arg_type.ip_address)
  in
  let json_flag = Cli_lib.Flag.json in
  let flags = Args.zip2 address_flag json_flag in
  Command.async ~summary:"Reset the trust status associated with an IP address"
    (Cli_lib.Background_daemon.rpc_init flags ~f:(fun port (ip_address, json) ->
         match%map
           Daemon_rpcs.Client.dispatch Daemon_rpcs.Reset_trust_status.rpc
             ip_address port
         with
         | Ok status ->
             print_trust_statuses status json
         | Error e ->
             printf "Failed to reset trust status %s\n" (Error.to_string_hum e) )
    )

let get_public_keys =
  let open Daemon_rpcs in
  let open Command.Param in
  let with_details_flag =
    flag "--with-details" ~aliases:[ "with-details" ] no_arg
      ~doc:"Show extra details (eg. balance, nonce) in addition to public keys"
  in
  let error_ctx = "Failed to get public-keys" in
  Command.async ~summary:"Get public keys"
    (Cli_lib.Background_daemon.rpc_init
       (Args.zip2 with_details_flag Cli_lib.Flag.json)
       ~f:(fun port (is_balance_included, json) ->
         if is_balance_included then
           Daemon_rpcs.Client.dispatch_pretty_message ~json
             ~join_error:Or_error.join ~error_ctx
             (module Cli_lib.Render.Public_key_with_details)
             Get_public_keys_with_details.rpc () port
         else
           Daemon_rpcs.Client.dispatch_pretty_message ~json
             ~join_error:Or_error.join ~error_ctx
             (module Cli_lib.Render.String_list_formatter)
             Get_public_keys.rpc () port ) )

let read_json filepath ~flag =
  let%map res =
    Deferred.Or_error.try_with ~here:[%here] (fun () ->
        let%map json_contents = Reader.file_contents filepath in
        Ok (Yojson.Safe.from_string json_contents) )
  in
  match res with
  | Ok c ->
      c
  | Error e ->
      Or_error.errorf "Could not read %s at %s\n%s" flag filepath
        (Error.to_string_hum e)

let verify_receipt =
  let open Deferred.Let_syntax in
  let open Daemon_rpcs in
  let open Command.Param in
  let open Cli_lib.Arg_type in
  let proof_path_flag =
    flag "--proof-path" ~aliases:[ "proof-path" ]
      ~doc:"PROOFFILE File to read json version of payment receipt"
      (required string)
  in
  let payment_path_flag =
    flag "--payment-path" ~aliases:[ "payment-path" ]
      ~doc:"PAYMENTPATH File to read json version of verifying payment"
      (required string)
  in
  let address_flag =
    flag "--address" ~aliases:[ "address" ]
      ~doc:"PUBLICKEY Public-key address of sender"
      (required public_key_compressed)
  in
  let token_flag =
    flag "--token" ~aliases:[ "token" ]
      ~doc:"TOKEN_ID The token ID for the account"
      (optional_with_default Token_id.default Cli_lib.Arg_type.token_id)
  in
  Command.async ~summary:"Verify a receipt of a sent payment"
    (Cli_lib.Background_daemon.rpc_init
       (Args.zip4 payment_path_flag proof_path_flag address_flag token_flag)
       ~f:(fun port (payment_path, proof_path, pk, token_id) ->
         let account_id = Account_id.create pk token_id in
         let dispatch_result =
           let open Deferred.Or_error.Let_syntax in
           let%bind payment_json =
             read_json payment_path ~flag:"payment-path"
           in
           let%bind proof_json = read_json proof_path ~flag:"proof-path" in
           let to_deferred_or_error result ~error =
             Result.map_error result ~f:(fun s ->
                 Error.of_string (sprintf "%s: %s" error s) )
             |> Deferred.return
           in
           let%bind payment =
             User_command.of_yojson payment_json
             |> to_deferred_or_error
                  ~error:
                    (sprintf "Payment file %s has invalid json format"
                       payment_path )
           and proof =
             [%of_yojson: Receipt.Chain_hash.t * User_command.t list] proof_json
             |> to_deferred_or_error
                  ~error:
                    (sprintf "Proof file %s has invalid json format" proof_path)
           in
           Daemon_rpcs.Client.dispatch Verify_proof.rpc
             (account_id, payment, proof)
             port
         in
         match%map dispatch_result with
         | Ok (Ok ()) ->
             printf "Payment is valid on the existing blockchain!\n"
         | Error e | Ok (Error e) ->
             eprintf "Error verifying the receipt: %s\n" (Error.to_string_hum e) )
    )

let get_nonce :
       rpc:(Account_id.t, Account.Nonce.t option Or_error.t) Rpc.Rpc.t
    -> Account_id.t
    -> Host_and_port.t
    -> (Account.Nonce.t, string) Deferred.Result.t =
 fun ~rpc account_id port ->
  let open Deferred.Let_syntax in
  let%map res = Daemon_rpcs.Client.dispatch rpc account_id port in
  match Or_error.join res with
  | Ok (Some n) ->
      Ok n
  | Ok None ->
      Error "No account found at that public_key"
  | Error e ->
      Error (Error.to_string_hum e)

let get_nonce_cmd =
  let open Command.Param in
  (* Ignores deprecation of public_key type for backwards compatibility *)
  let[@warning "-3"] address_flag =
    flag "--address" ~aliases:[ "address" ]
      ~doc:"PUBLICKEY Public-key address you want the nonce for"
      (required Cli_lib.Arg_type.public_key_compressed)
  in
  let token_flag =
    flag "--token" ~aliases:[ "token" ]
      ~doc:"TOKEN_ID The token ID for the account"
      (optional_with_default Token_id.default Cli_lib.Arg_type.token_id)
  in
  let flags = Args.zip2 address_flag token_flag in
  Command.async ~summary:"Get the current nonce for an account"
    (Cli_lib.Background_daemon.rpc_init flags ~f:(fun port (pk, token_flag) ->
         let account_id = Account_id.create pk token_flag in
         match%bind
           get_nonce ~rpc:Daemon_rpcs.Get_nonce.rpc account_id port
         with
         | Error e ->
             eprintf "Failed to get nonce\n%s\n" e ;
             exit 2
         | Ok nonce ->
             printf "%s\n" (Account.Nonce.to_string nonce) ;
             exit 0 ) )

let status =
  let open Daemon_rpcs in
  let flag = Args.zip2 Cli_lib.Flag.json Cli_lib.Flag.performance in
  Command.async ~summary:"Get running daemon status"
    (Cli_lib.Background_daemon.rpc_init flag ~f:(fun port (json, performance) ->
         Daemon_rpcs.Client.dispatch_pretty_message ~json ~join_error:Fn.id
           ~error_ctx:"Failed to get status"
           (module Daemon_rpcs.Types.Status)
           Get_status.rpc
           (if performance then `Performance else `None)
           port ) )

let status_clear_hist =
  let open Daemon_rpcs in
  let flag = Args.zip2 Cli_lib.Flag.json Cli_lib.Flag.performance in
  Command.async ~summary:"Clear histograms reported in status"
    (Cli_lib.Background_daemon.rpc_init flag ~f:(fun port (json, performance) ->
         Daemon_rpcs.Client.dispatch_pretty_message ~json ~join_error:Fn.id
           ~error_ctx:"Failed to clear histograms reported in status"
           (module Daemon_rpcs.Types.Status)
           Clear_hist_status.rpc
           (if performance then `Performance else `None)
           port ) )

let get_nonce_exn ~rpc public_key port =
  match%bind get_nonce ~rpc public_key port with
  | Error e ->
      eprintf "Failed to get nonce\n%s\n" e ;
      exit 3
  | Ok nonce ->
      return nonce

let unwrap_user_command (`UserCommand x) = x

let batch_send_payments =
  let module Payment_info = struct
    type t =
      { receiver : string
      ; amount : Currency.Amount.t
      ; fee : Currency.Fee.t
      ; valid_until : Mina_numbers.Global_slot.t option [@sexp.option]
      }
    [@@deriving sexp]
  end in
  let payment_path_flag = Command.Param.(anon @@ ("payments-file" %: string)) in
  let get_infos payments_path =
    match%bind
      Reader.load_sexp payments_path [%of_sexp: Payment_info.t list]
    with
    | Ok x ->
        return x
    | Error _ ->
        let sample_info () : Payment_info.t =
          let keypair = Keypair.create () in
          { Payment_info.receiver =
              Public_key.(
                Compressed.to_base58_check (compress keypair.public_key))
          ; valid_until = Some (Mina_numbers.Global_slot.random ())
          ; amount = Currency.Amount.of_int (Random.int 100)
          ; fee = Currency.Fee.of_int (Random.int 100)
          }
        in
        eprintf "Could not read payments from %s.\n" payments_path ;
        eprintf
          "The file should be a sexp list of payments with optional expiry \
           slot number \"valid_until\". Here is an example file:\n\
           %s\n"
          (Sexp.to_string_hum
             ([%sexp_of: Payment_info.t list]
                (List.init 3 ~f:(fun _ -> sample_info ())) ) ) ;
        exit 5
  in
  let main port (privkey_path, payments_path) =
    let open Deferred.Let_syntax in
    let%bind keypair =
      Secrets.Keypair.Terminal_stdin.read_exn ~which:"Mina keypair" privkey_path
    and infos = get_infos payments_path in
    let ts : User_command_input.t list =
      List.map infos ~f:(fun { receiver; valid_until; amount; fee } ->
          let signer_pk = Public_key.compress keypair.public_key in
          let receiver_pk =
            Public_key.of_base58_check_decompress_exn receiver
          in
          User_command_input.create ~signer:signer_pk ~fee
            ~fee_token:Token_id.default (* TODO: Multiple tokens. *)
            ~fee_payer_pk:signer_pk ~memo:Signed_command_memo.empty ~valid_until
            ~body:
              (Payment
                 { source_pk = signer_pk
                 ; receiver_pk
                 ; token_id = Token_id.default
                 ; amount
                 } )
            ~sign_choice:(User_command_input.Sign_choice.Keypair keypair) () )
    in
    Daemon_rpcs.Client.dispatch_with_message Daemon_rpcs.Send_user_commands.rpc
      ts port
      ~success:(fun _ -> "Successfully enqueued payments in pool")
      ~error:(fun e ->
        sprintf "Failed to send payments %s" (Error.to_string_hum e) )
      ~join_error:Or_error.join
  in
  Command.async ~summary:"Send multiple payments from a file"
    (Cli_lib.Background_daemon.rpc_init
       (Args.zip2 Cli_lib.Flag.privkey_read_path payment_path_flag)
       ~f:main )

let send_payment_graphql =
  let open Command.Param in
  let open Cli_lib.Arg_type in
  let receiver_flag =
    flag "--receiver" ~aliases:[ "receiver" ]
      ~doc:"PUBLICKEY Public key to which you want to send money"
      (required public_key_compressed)
  in
  let amount_flag =
    flag "--amount" ~aliases:[ "amount" ]
      ~doc:"VALUE Payment amount you want to send" (required txn_amount)
  in
  let args =
    Args.zip3 Cli_lib.Flag.signed_command_common receiver_flag amount_flag
  in
  Command.async ~summary:"Send payment to an address"
    (Cli_lib.Background_daemon.graphql_init args
       ~f:(fun
            graphql_endpoint
            ({ Cli_lib.Flag.sender; fee; nonce; memo }, receiver, amount)
          ->
         let%map response =
           let input =
             Mina_graphql.Types.Input.SendPaymentInput.make_input ~to_:receiver
               ~from:sender ~amount ~fee ?memo ?nonce ()
           in
           Graphql_client.query_exn
             Graphql_queries.Send_payment.(make @@ makeVariables ~input ())
             graphql_endpoint
         in
         printf "Dispatched payment with ID %s\n"
           response.sendPayment.payment.id ) )

let delegate_stake_graphql =
  let open Command.Param in
  let open Cli_lib.Arg_type in
  let receiver_flag =
    flag "--receiver" ~aliases:[ "receiver" ]
      ~doc:"PUBLICKEY Public key to which you want to delegate your stake"
      (required public_key_compressed)
  in
  let args = Args.zip2 Cli_lib.Flag.signed_command_common receiver_flag in
  Command.async ~summary:"Delegate your stake to another public key"
    (Cli_lib.Background_daemon.graphql_init args
       ~f:(fun
            graphql_endpoint
            ({ Cli_lib.Flag.sender; fee; nonce; memo }, receiver)
          ->
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Send_delegation.(
               make
               @@ makeVariables ~receiver ~sender
                    ~fee:(Currency.Fee.to_uint64 fee)
                    ?nonce ?memo ())
             graphql_endpoint
         in
         printf "Dispatched stake delegation with ID %s\n"
           response.sendDelegation.delegation.id ) )

let create_new_token_graphql =
  let open Command.Param in
  let open Cli_lib.Arg_type in
  let receiver_flag =
    flag "--receiver" ~aliases:[ "receiver" ]
      ~doc:"PUBLICKEY Public key to create the new token for"
      (optional public_key_compressed)
  in
  let args = Args.zip2 Cli_lib.Flag.signed_command_common receiver_flag in
  Command.async ~summary:"Create a new token"
    (Cli_lib.Background_daemon.graphql_init args
       ~f:(fun
            graphql_endpoint
            ({ Cli_lib.Flag.sender; fee; nonce; memo }, receiver)
          ->
         let receiver = Option.value ~default:sender receiver in
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Send_create_token.(
               make
               @@ makeVariables ~sender ~receiver
                    ~fee:(Currency.Fee.to_uint64 fee)
                    ?nonce ?memo ())
             graphql_endpoint
         in
         printf "Dispatched create new token command with TRANSACTION_ID %s\n"
           response.createToken.createNewToken.id ) )

let create_new_account_graphql =
  let open Command.Param in
  let open Cli_lib.Arg_type in
  let receiver_flag =
    flag "--receiver" ~aliases:[ "receiver" ]
      ~doc:"PUBLICKEY Public key to create the new account for"
      (required public_key_compressed)
  in
  let token_owner_flag =
    flag "--token-owner" ~aliases:[ "token-owner" ]
      ~doc:"PUBLICKEY Public key for the owner of the token"
      (optional public_key_compressed)
  in
  let token_flag =
    flag "--token" ~aliases:[ "token" ]
      ~doc:"TOKEN_ID The ID of the token to create the account for"
      (required token_id)
  in
  let args =
    Args.zip4 Cli_lib.Flag.signed_command_common receiver_flag token_owner_flag
      token_flag
  in
  Command.async ~summary:"Create a new account for a token"
    (Cli_lib.Background_daemon.graphql_init args
       ~f:(fun
            graphql_endpoint
            ( { Cli_lib.Flag.sender; fee; nonce; memo }
            , receiver
            , token_owner
            , token )
          ->
         let%bind token_owner =
           match token_owner with
           | Some token_owner ->
               Deferred.return token_owner
           | None when Token_id.(equal default) token ->
               (* NOTE: Doesn't matter who we say the owner is for the default
                        token, arbitrarily choose the receiver.
               *)
               Deferred.return receiver
           | None -> (
               let%map token_owner =
                 Graphql_client.(
                   query_exn
                     Graphql_queries.Get_token_owner.(
                       make @@ makeVariables ~token ()))
                   graphql_endpoint
               in
               match token_owner.tokenOwner with
               | Some token_owner ->
                   Graphql_lib.Decoders.public_key token_owner
               | None ->
                   failwith
                     "Unknown token: Cannot find the owner for the given token"
               )
         in
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Send_create_token_account.(
               make
               @@ makeVariables ~sender ~receiver ~tokenOwner:token_owner ~token
                    ~fee:(Currency.Fee.to_uint64 fee)
                    ?nonce ?memo ())
             graphql_endpoint
         in
         printf
           "Dispatched create new token account command with TRANSACTION_ID %s\n"
           response.createTokenAccount.createNewTokenAccount.id ) )

let mint_tokens_graphql =
  let open Command.Param in
  let open Cli_lib.Arg_type in
  let receiver_flag =
    flag "--receiver" ~aliases:[ "receiver" ]
      ~doc:
        "PUBLICKEY Public key of the account to create new tokens in (defaults \
         to the sender)"
      (optional public_key_compressed)
  in
  let token_flag =
    flag "--token" ~aliases:[ "token" ]
      ~doc:"TOKEN_ID The ID of the token to mint" (required token_id)
  in
  let amount_flag =
    flag "--amount" ~aliases:[ "amount" ]
      ~doc:"VALUE Number of new tokens to create" (required txn_amount)
  in
  let args =
    Args.zip4 Cli_lib.Flag.signed_command_common receiver_flag token_flag
      amount_flag
  in
  Command.async ~summary:"Mint more of a token owned by the command's sender"
    (Cli_lib.Background_daemon.graphql_init args
       ~f:(fun
            graphql_endpoint
            ({ Cli_lib.Flag.sender; fee; nonce; memo }, receiver, token, amount)
          ->
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Send_mint_tokens.(
               make
               @@ makeVariables ~sender ?receiver ~token
                    ~amount:(Currency.Amount.to_uint64 amount)
                    ~fee:(Currency.Fee.to_uint64 fee)
                    ?nonce ?memo ())
             graphql_endpoint
         in
         printf "Dispatched mint token command with TRANSACTION_ID %s\n"
           response.mintTokens.mintTokens.id ) )

let cancel_transaction_graphql =
  let txn_id_flag =
    Command.Param.(
      flag "--id" ~aliases:[ "id" ] ~doc:"ID Transaction ID to be cancelled"
        (required Cli_lib.Arg_type.user_command))
  in
  Command.async
    ~summary:
      "Cancel a transaction -- this submits a replacement transaction with a \
       fee larger than the cancelled transaction."
    (Cli_lib.Background_daemon.graphql_init txn_id_flag
       ~f:(fun graphql_endpoint user_command ->
         let receiver_pk = Signed_command.receiver_pk user_command in
         let cancel_sender_pk = Signed_command.fee_payer_pk user_command in
         let open Deferred.Let_syntax in
         let cancel_fee =
           let fee = Currency.Fee.to_uint64 (Signed_command.fee user_command) in
           let replace_fee =
             Currency.Fee.to_uint64 Network_pool.Indexed_pool.replace_fee
           in
           let open Unsigned.UInt64.Infix in
           (* fee amount "inspired by" network_pool/indexed_pool.ml *)
           Currency.Fee.of_uint64 (fee + replace_fee)
         in
         printf "Fee to cancel transaction is %s coda.\n"
           (Currency.Fee.to_formatted_string cancel_fee) ;
         let cancel_query =
           let input =
             Mina_graphql.Types.Input.SendPaymentInput.make_input
               ~to_:receiver_pk ~from:cancel_sender_pk
               ~amount:Currency.Amount.zero ~fee:cancel_fee
               ~nonce:(Signed_command.nonce user_command)
               ()
           in
           Graphql_queries.Send_payment.(make @@ makeVariables ~input ())
         in
         let%map cancel_response =
           Graphql_client.query_exn cancel_query graphql_endpoint
         in
         printf "🛑 Cancelled transaction! Cancel ID: %s\n"
           cancel_response.sendPayment.payment.id ) )

let send_rosetta_transactions_graphql =
  Command.async
    ~summary:
      "Dispatch one or more transactions, provided to stdin in rosetta format"
    (Cli_lib.Background_daemon.graphql_init (Command.Param.return ())
       ~f:(fun graphql_endpoint () ->
         let lexbuf = Lexing.from_channel In_channel.stdin in
         let lexer = Yojson.init_lexer () in
         match%bind
           Deferred.Or_error.try_with ~here:[%here] (fun () ->
               Deferred.repeat_until_finished () (fun () ->
                   try
                     let transaction_json =
                       Yojson.Basic.from_lexbuf ~stream:true lexer lexbuf
                     in
                     let%map response =
                       Graphql_client.query_exn
                         Graphql_queries.Send_rosetta_transaction.(
                           make
                           @@ makeVariables ~transaction:transaction_json ())
                         graphql_endpoint
                     in
                     printf "Dispatched command with TRANSACTION_ID %s\n"
                       response.sendRosettaTransaction.userCommand.id ;
                     `Repeat ()
                   with Yojson.End_of_input -> return (`Finished ()) ) )
         with
         | Ok () ->
             Deferred.return ()
         | Error err ->
             Format.eprintf "Error:@.%s@.@."
               (Yojson.Safe.pretty_to_string (Error_json.error_to_yojson err)) ;
             Core_kernel.exit 1 ) )

module Export_logs = struct
  let pp_export_result tarfile = printf "Exported logs to %s\n%!" tarfile

  let tarfile_flag =
    let open Command.Param in
    flag "--tarfile" ~aliases:[ "tarfile" ]
      ~doc:"STRING Basename of the tar archive (default: date_time)"
      (optional string)

  let export_via_graphql =
    Command.async ~summary:"Export daemon logs to tar archive"
      (Cli_lib.Background_daemon.graphql_init tarfile_flag
         ~f:(fun graphql_endpoint basename ->
           let%map response =
             Graphql_client.query_exn
               Graphql_queries.Export_logs.(make @@ makeVariables ?basename ())
               graphql_endpoint
           in
           pp_export_result response.exportLogs.exportLogs.tarfile ) )

  let export_locally =
    let run ~tarfile ~conf_dir =
      let open Mina_lib in
      let conf_dir = Conf_dir.compute_conf_dir conf_dir in
      fun () ->
        match%map Conf_dir.export_logs_to_tar ?basename:tarfile ~conf_dir with
        | Ok result ->
            pp_export_result result
        | Error err ->
            failwithf "Error when exporting logs: %s"
              (Error_json.error_to_yojson err |> Yojson.Safe.to_string)
              ()
    in
    let open Command.Let_syntax in
    Command.async ~summary:"Export local logs (no daemon) to tar archive"
      (let%map tarfile = tarfile_flag and conf_dir = Cli_lib.Flag.conf_dir in
       run ~tarfile ~conf_dir )
end

let get_transaction_status =
  Command.async ~summary:"Get the status of a transaction"
    (Cli_lib.Background_daemon.rpc_init
       Command.Param.(anon @@ ("txn-id" %: string))
       ~f:(fun port serialized_transaction ->
         match Signed_command.of_base58_check serialized_transaction with
         | Ok user_command ->
             Daemon_rpcs.Client.dispatch_with_message
               Daemon_rpcs.Get_transaction_status.rpc user_command port
               ~success:(fun status ->
                 sprintf !"Transaction status : %s\n"
                 @@ Transaction_inclusion_status.State.to_string status )
               ~error:(fun e ->
                 sprintf "Failed to get transaction status : %s"
                   (Error.to_string_hum e) )
               ~join_error:Or_error.join
         | Error _e ->
             eprintf "Could not deserialize user command" ;
             exit 16 ) )

let wrap_key =
  Command.async ~summary:"Wrap a private key into a private key file"
    (let open Command.Let_syntax in
    let%map_open privkey_path = Cli_lib.Flag.privkey_write_path in
    Cli_lib.Exceptions.handle_nicely
    @@ fun () ->
    let open Deferred.Let_syntax in
    let%bind privkey =
      Secrets.Password.hidden_line_or_env "Private key: " ~env:"CODA_PRIVKEY"
    in
    let pk = Private_key.of_base58_check_exn (Bytes.to_string privkey) in
    let kp = Keypair.of_private_key_exn pk in
    Secrets.Keypair.Terminal_stdin.write_exn kp ~privkey_path)

let dump_keypair =
  Command.async ~summary:"Print out a keypair from a private key file"
    (let open Command.Let_syntax in
    let%map_open privkey_path = Cli_lib.Flag.privkey_read_path in
    Cli_lib.Exceptions.handle_nicely
    @@ fun () ->
    let open Deferred.Let_syntax in
    let%map kp =
      Secrets.Keypair.Terminal_stdin.read_exn ~which:"Mina keypair" privkey_path
    in
    printf "Public key: %s\nPrivate key: %s\n"
      ( kp.public_key |> Public_key.compress
      |> Public_key.Compressed.to_base58_check )
      (kp.private_key |> Private_key.to_base58_check))

let handle_export_ledger_response ~json = function
  | Error e ->
      Daemon_rpcs.Client.print_rpc_error e ;
      exit 1
  | Ok (Error e) ->
      printf !"Ledger not found: %s\n" (Error.to_string_hum e) ;
      exit 1
  | Ok (Ok accounts) ->
      if json then (
        Yojson.Safe.pretty_print Format.std_formatter
          (Runtime_config.Accounts.to_yojson
             (List.map accounts ~f:(fun a ->
                  Genesis_ledger_helper.Accounts.Single.of_account a None ) ) ) ;
        printf "\n" )
      else printf !"%{sexp:Account.t list}\n" accounts ;
      return ()

let export_ledger =
  let state_hash_flag =
    Command.Param.(
      flag "--state-hash" ~aliases:[ "state-hash" ]
        ~doc:
          "STATE-HASH State hash, if printing a staged ledger (default: state \
           hash for the best tip)"
        (optional string))
  in
  let ledger_kind =
    let available_ledgers =
      [ "staged-ledger"
      ; "snarked-ledger"
      ; "staking-epoch-ledger"
      ; "next-epoch-ledger"
      ]
    in
    let t =
      Command.Param.Arg_type.of_alist_exn
        (List.map available_ledgers ~f:(fun s -> (s, s)))
    in
    let ledger_args = String.concat ~sep:"|" available_ledgers in
    Command.Param.(anon (ledger_args %: t))
  in
  let plaintext_flag = Cli_lib.Flag.plaintext in
  let flags = Args.zip3 state_hash_flag plaintext_flag ledger_kind in
  Command.async
    ~summary:
      "Print the specified ledger (default: staged ledger at the best tip). \
       Note: Exporting snarked ledger is an expensive operation and can take a \
       few seconds"
    (Cli_lib.Background_daemon.rpc_init flags
       ~f:(fun port (state_hash, plaintext, ledger_kind) ->
         let check_for_state_hash () =
           if Option.is_some state_hash then (
             Format.eprintf "A state hash should not be given for %s@."
               ledger_kind ;
             Core_kernel.exit 1 )
         in
         let response =
           match ledger_kind with
           | "staged-ledger" ->
               let state_hash =
                 Option.map ~f:State_hash.of_base58_check_exn state_hash
               in
               Daemon_rpcs.Client.dispatch Daemon_rpcs.Get_ledger.rpc state_hash
                 port
           | "snarked-ledger" ->
               let state_hash =
                 Option.map ~f:State_hash.of_base58_check_exn state_hash
               in
               printf
                 "Generating snarked ledger(this may take a few seconds)...\n" ;
               Daemon_rpcs.Client.dispatch Daemon_rpcs.Get_snarked_ledger.rpc
                 state_hash port
           | "staking-epoch-ledger" ->
               check_for_state_hash () ;
               Daemon_rpcs.Client.dispatch Daemon_rpcs.Get_staking_ledger.rpc
                 Daemon_rpcs.Get_staking_ledger.Current port
           | "next-epoch-ledger" ->
               check_for_state_hash () ;
               Daemon_rpcs.Client.dispatch Daemon_rpcs.Get_staking_ledger.rpc
                 Daemon_rpcs.Get_staking_ledger.Next port
           | _ ->
               (* unreachable *)
               failwithf "Unknown ledger kind: %s" ledger_kind ()
         in
         response >>= handle_export_ledger_response ~json:(not plaintext) ) )

let hash_ledger =
  let open Command.Let_syntax in
  Command.async
    ~summary:
      "Print the Merkle root of the ledger contained in the specified file"
    (let%map ledger_file =
       Command.Param.(
         flag "--ledger-file"
           ~doc:"LEDGER-FILE File containing an exported ledger"
           (required string))
     and plaintext = Cli_lib.Flag.plaintext in
     fun () ->
       let process_accounts accounts =
         let constraint_constants =
           Genesis_constants.Constraint_constants.compiled
         in
         let packed_ledger =
           Genesis_ledger_helper.Ledger.packed_genesis_ledger_of_accounts
             ~depth:constraint_constants.ledger_depth accounts
         in
         let ledger = Lazy.force @@ Genesis_ledger.Packed.t packed_ledger in
         Format.printf "%s@."
           (Ledger.merkle_root ledger |> Ledger_hash.to_base58_check)
       in
       Deferred.return
       @@
       if plaintext then
         In_channel.with_file ledger_file ~f:(fun in_channel ->
             let sexp = In_channel.input_all in_channel |> Sexp.of_string in
             let accounts =
               lazy
                 (List.map
                    ([%of_sexp: Account.t list] sexp)
                    ~f:(fun acct -> (None, acct)) )
             in
             process_accounts accounts )
       else
         let json = Yojson.Safe.from_file ledger_file in
         match Runtime_config.Accounts.of_yojson json with
         | Ok runtime_accounts ->
             let accounts =
               lazy (Genesis_ledger_helper.Accounts.to_full runtime_accounts)
             in
             process_accounts accounts
         | Error err ->
             Format.eprintf "Could not parse JSON in file %s: %s@" ledger_file
               err ;
             ignore (exit 1 : 'a Deferred.t) )

let currency_in_ledger =
  let open Command.Let_syntax in
  Command.async
    ~summary:
      "Print the total currency for each token present in the ledger contained \
       in the specified file"
    (let%map ledger_file =
       Command.Param.(
         flag "--ledger-file"
           ~doc:"LEDGER-FILE File containing an exported ledger"
           (required string))
     and plaintext = Cli_lib.Flag.plaintext in
     fun () ->
       let process_accounts accounts =
         (* track currency total for each token
            use uint64 to make arithmetic simple
         *)
         let currency_tbl : Unsigned.UInt64.t Token_id.Table.t =
           Token_id.Table.create ()
         in
         List.iter accounts ~f:(fun (acct : Account.t) ->
             let token_id = Account.token acct in
             let balance = acct.balance |> Currency.Balance.to_uint64 in
             match Token_id.Table.find currency_tbl token_id with
             | None ->
                 Token_id.Table.add_exn currency_tbl ~key:token_id ~data:balance
             | Some total ->
                 let new_total = Unsigned.UInt64.add total balance in
                 Token_id.Table.set currency_tbl ~key:token_id ~data:new_total ) ;
         let tokens =
           Token_id.Table.keys currency_tbl
           |> List.dedup_and_sort ~compare:Token_id.compare
         in
         List.iter tokens ~f:(fun token ->
             let total =
               Token_id.Table.find_exn currency_tbl token
               |> Currency.Balance.of_uint64
               |> Currency.Balance.to_formatted_string
             in
             if Token_id.equal token Token_id.default then
               Format.printf "MINA: %s@." total
             else
               Format.printf "TOKEN %s: %s@." (Token_id.to_string token) total )
       in
       Deferred.return
       @@
       if plaintext then
         In_channel.with_file ledger_file ~f:(fun in_channel ->
             let sexp = In_channel.input_all in_channel |> Sexp.of_string in
             let accounts = [%of_sexp: Account.t list] sexp in
             process_accounts accounts )
       else
         let json = Yojson.Safe.from_file ledger_file in
         match Runtime_config.Accounts.of_yojson json with
         | Ok runtime_accounts ->
             let accounts =
               Genesis_ledger_helper.Accounts.to_full runtime_accounts
               |> List.map ~f:(fun (_sk_opt, acct) -> acct)
             in
             process_accounts accounts
         | Error err ->
             Format.eprintf "Could not parse JSON in file %s: %s@" ledger_file
               err ;
             ignore (exit 1 : 'a Deferred.t) )

let constraint_system_digests =
  Command.async ~summary:"Print MD5 digest of each SNARK constraint"
    (Command.Param.return (fun () ->
         (* TODO: Allow these to be configurable. *)
         let proof_level = Genesis_constants.Proof_level.compiled in
         let constraint_constants =
           Genesis_constants.Constraint_constants.compiled
         in
         let all =
           Transaction_snark.constraint_system_digests ~constraint_constants ()
           @ Blockchain_snark.Blockchain_snark_state.constraint_system_digests
               ~proof_level ~constraint_constants ()
         in
         let all =
           List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2) all
         in
         List.iter all ~f:(fun (k, v) -> printf "%s\t%s\n" k (Md5.to_hex v)) ;
         Deferred.unit ) )

let snark_job_list =
  let open Deferred.Let_syntax in
  let open Command.Param in
  Command.async
    ~summary:
      "List of snark jobs in JSON format that are yet to be included in the \
       blocks"
    (Cli_lib.Background_daemon.rpc_init (return ()) ~f:(fun port () ->
         match%map
           Daemon_rpcs.Client.dispatch_join_errors
             Daemon_rpcs.Snark_job_list.rpc () port
         with
         | Ok str ->
             printf "%s" str
         | Error e ->
             Daemon_rpcs.Client.print_rpc_error e ) )

let snark_pool_list =
  let open Command.Param in
  Command.async ~summary:"List of snark works in the snark pool in JSON format"
    (Cli_lib.Background_daemon.graphql_init (return ())
       ~f:(fun graphql_endpoint () ->
         Deferred.map
           (Graphql_client.query_exn
              Graphql_queries.Snark_pool.(make @@ makeVariables ())
              graphql_endpoint )
           ~f:(fun response ->
             let lst =
               [%to_yojson: Cli_lib.Graphql_types.Completed_works.t]
                 (Array.to_list
                    (Array.map
                       ~f:(fun w ->
                         { Cli_lib.Graphql_types.Completed_works.Work.work_ids =
                             Array.to_list w.work_ids
                         ; fee = Currency.Fee.of_uint64 w.fee
                         ; prover = w.prover
                         } )
                       response.snarkPool ) )
             in
             print_string (Yojson.Safe.to_string lst) ) ) )

let pooled_user_commands =
  let public_key_flag =
    Command.Param.(
      anon @@ maybe @@ ("public-key" %: Cli_lib.Arg_type.public_key_compressed))
  in
  Command.async
    ~summary:"Retrieve all the user commands that are pending inclusion"
    (Cli_lib.Background_daemon.graphql_init public_key_flag
       ~f:(fun graphql_endpoint public_key ->
         let module Q = Graphql_queries.Pooled_user_commands in
         let graphql = Q.(make @@ makeVariables ?public_key ()) in
         let%map response = Graphql_client.query_exn graphql graphql_endpoint in
         let json_response = Q.serialize response |> Q.toJson in
         print_string (Yojson.Basic.to_string json_response) ) )

let to_signed_fee_exn sign magnitude =
  let sgn = match sign with `PLUS -> Sgn.Pos | `MINUS -> Neg in
  let magnitude = Currency.Fee.of_uint64 magnitude in
  Currency.Fee.Signed.create ~sgn ~magnitude

let pending_snark_work =
  let open Command.Param in
  Command.async
    ~summary:
      "List of snark works in JSON format that are not available in the pool \
       yet"
    (Cli_lib.Background_daemon.graphql_init (return ())
       ~f:(fun graphql_endpoint () ->
         Deferred.map
           (Graphql_client.query_exn
              Graphql_queries.Pending_snark_work.(make @@ makeVariables ())
              graphql_endpoint )
           ~f:(fun response ->
             let lst =
               [%to_yojson: Cli_lib.Graphql_types.Pending_snark_work.t]
                 (Array.map
                    ~f:(fun bundle ->
                      Array.map bundle.workBundle ~f:(fun w ->
                          let f = w.fee_excess in
                          let hash_of_string =
                            Mina_base.Frozen_ledger_hash.of_base58_check_exn
                          in
                          { Cli_lib.Graphql_types.Pending_snark_work.Work
                            .work_id = w.work_id
                          ; fee_excess =
                              to_signed_fee_exn f.sign f.fee_magnitude
                          ; supply_increase =
                              Currency.Amount.of_uint64 w.supply_increase
                          ; source_ledger_hash =
                              hash_of_string w.source_ledger_hash
                          ; target_ledger_hash =
                              hash_of_string w.target_ledger_hash
                          } ) )
                    response.pendingSnarkWork )
             in
             print_string (Yojson.Safe.to_string lst) ) ) )

let start_tracing =
  let open Deferred.Let_syntax in
  let open Command.Param in
  Command.async
    ~summary:"Start async tracing to $config-directory/trace/$pid.trace"
    (Cli_lib.Background_daemon.rpc_init (return ()) ~f:(fun port () ->
         match%map
           Daemon_rpcs.Client.dispatch Daemon_rpcs.Start_tracing.rpc () port
         with
         | Ok () ->
             printf "Daemon started tracing!"
         | Error e ->
             Daemon_rpcs.Client.print_rpc_error e ) )

let stop_tracing =
  let open Deferred.Let_syntax in
  let open Command.Param in
  Command.async ~summary:"Stop async tracing"
    (Cli_lib.Background_daemon.rpc_init (return ()) ~f:(fun port () ->
         match%map
           Daemon_rpcs.Client.dispatch Daemon_rpcs.Stop_tracing.rpc () port
         with
         | Ok () ->
             printf "Daemon stopped printing!"
         | Error e ->
             Daemon_rpcs.Client.print_rpc_error e ) )

let set_coinbase_receiver_graphql =
  let open Command.Param in
  let open Cli_lib.Arg_type in
  let pk_flag =
    choose_one ~if_nothing_chosen:Raise
      [ flag "--public-key" ~aliases:[ "public-key" ]
          ~doc:"PUBLICKEY Public key of account to send coinbase rewards to"
          (optional public_key_compressed)
        |> map ~f:(Option.map ~f:Option.some)
      ; flag "--block-producer" ~aliases:[ "block-producer" ]
          ~doc:"Send coinbase rewards to the block producer's public key" no_arg
        |> map ~f:(function true -> Some None | false -> None)
      ]
  in
  Command.async ~summary:"Set the coinbase receiver"
    (Cli_lib.Background_daemon.graphql_init pk_flag
       ~f:(fun graphql_endpoint public_key ->
         let print_pk_opt () = function
           | None ->
               "block producer"
           | Some pk ->
               "public key " ^ Public_key.Compressed.to_base58_check pk
         in
         let%map result =
           Graphql_client.query_exn
             Graphql_queries.Set_coinbase_receiver.(
               make @@ makeVariables ?public_key ())
             graphql_endpoint
         in
         printf
           "Was sending coinbases to the %a\nNow sending coinbases to the %a\n"
           print_pk_opt result.setCoinbaseReceiver.lastCoinbaseReceiver
           print_pk_opt result.setCoinbaseReceiver.currentCoinbaseReceiver ) )

let set_snark_worker =
  let open Command.Param in
  let public_key_flag =
    flag "--address" ~aliases:[ "address" ]
      ~doc:
        "PUBLICKEY Public-key address you wish to start snark-working on; null \
         to stop doing any snark work"
      (optional Cli_lib.Arg_type.public_key_compressed)
  in
  Command.async
    ~summary:"Set key you wish to snark work with or disable snark working"
    (Cli_lib.Background_daemon.graphql_init public_key_flag
       ~f:(fun graphql_endpoint optional_public_key ->
         let graphql =
           Graphql_queries.Set_snark_worker.(
             make @@ makeVariables ?public_key:optional_public_key ())
         in
         Deferred.map (Graphql_client.query_exn graphql graphql_endpoint)
           ~f:(fun response ->
             ( match optional_public_key with
             | Some public_key ->
                 printf
                   !"New snark worker public key : %s\n"
                   (Public_key.Compressed.to_base58_check public_key)
             | None ->
                 printf "Will stop doing snark work\n" ) ;
             printf "Previous snark worker public key : %s\n"
               (Option.value_map response.setSnarkWorker.lastSnarkWorker
                  ~default:"None" ~f:Public_key.Compressed.to_base58_check ) ) )
    )

let set_snark_work_fee =
  Command.async ~summary:"Set fee reward for doing transaction snark work"
  @@ Cli_lib.Background_daemon.graphql_init
       Command.Param.(anon @@ ("fee" %: Cli_lib.Arg_type.txn_fee))
       ~f:(fun graphql_endpoint fee ->
         let graphql =
           Graphql_queries.Set_snark_work_fee.(
             make @@ makeVariables ~fee:(Currency.Fee.to_uint64 fee) ())
         in
         Deferred.map (Graphql_client.query_exn graphql graphql_endpoint)
           ~f:(fun response ->
             printf
               !"Updated snark work fee: %i\nOld snark work fee: %i\n"
               (Currency.Fee.to_int fee)
               (Unsigned.UInt64.to_int response.setSnarkWorkFee.lastFee) ) )

let import_key =
  Command.async
    ~summary:
      "Import a password protected private key to be tracked by the daemon.\n\
       Set MINA_PRIVKEY_PASS environment variable to use non-interactively \
       (key will be imported using the same password)."
    (let%map_open.Command access_method =
       choose_one
         ~if_nothing_chosen:(Default_to `None)
         [ Cli_lib.Flag.Uri.Client.rest_graphql_opt
           |> map ~f:(Option.map ~f:(fun port -> `GraphQL port))
         ; Cli_lib.Flag.conf_dir
           |> map ~f:(Option.map ~f:(fun conf_dir -> `Conf_dir conf_dir))
         ]
     and privkey_path = Cli_lib.Flag.privkey_read_path in
     fun () ->
       let open Deferred.Let_syntax in
       let initial_password = ref None in
       let do_graphql graphql_endpoint =
         let%bind password =
           match Sys.getenv Secrets.Keypair.env with
           | Some password ->
               Deferred.return (Bytes.of_string password)
           | None ->
               let password =
                 Secrets.Password.read_hidden_line ~error_help_message:""
                   "Secret key password: "
               in
               initial_password := Some password ;
               password
         in
         let graphql =
           Graphql_queries.Import_account.(
             make
             @@ makeVariables ~path:privkey_path
                  ~password:(Bytes.to_string password) ())
         in
         match%map Graphql_client.query graphql graphql_endpoint with
         | Ok res ->
             let res = res.importAccount in
             if res.already_imported then Ok (`Already_imported res.public_key)
             else Ok (`Imported res.public_key)
         | Error (`Failed_request _ as err) ->
             Error err
         | Error (`Graphql_error _ as err) ->
             Ok err
       in
       let do_local conf_dir =
         let wallets_disk_location = conf_dir ^/ "wallets" in
         let%bind ({ Keypair.public_key; _ } as keypair) =
           let rec go () =
             match !initial_password with
             | None ->
                 Secrets.Keypair.Terminal_stdin.read_exn ~which:"mina keypair"
                   privkey_path
             | Some password -> (
                 (* We've already asked for the password once for a failed
                    GraphQL query, try that one instead of asking again.
                 *)
                 match%bind
                   Secrets.Keypair.read ~privkey_path
                     ~password:(Lazy.return password)
                 with
                 | Ok res ->
                     return res
                 | Error `Incorrect_password_or_corrupted_privkey ->
                     printf "Wrong password! Please try again\n" ;
                     initial_password := None ;
                     go ()
                 | Error err ->
                     Secrets.Privkey_error.raise ~which:"mina keypair" err )
           in
           go ()
         in
         let pk = Public_key.compress public_key in
         let%bind wallets =
           Secrets.Wallets.load ~logger:(Logger.create ())
             ~disk_location:wallets_disk_location
         in
         (* Either we already are tracking it *)
         match Secrets.Wallets.check_locked wallets ~needle:pk with
         | Some _ ->
             Deferred.return (`Already_imported pk)
         | None ->
             (* Or we import it *)
             let%map pk =
               Secrets.Wallets.import_keypair_terminal_stdin wallets keypair
             in
             `Imported pk
       in
       let print_result = function
         | `Already_imported public_key ->
             printf
               !"Key already present, no need to import : %s\n"
               (Public_key.Compressed.to_base58_check public_key)
         | `Imported public_key ->
             printf
               !"\n😄 Imported account!\nPublic key: %s\n"
               (Public_key.Compressed.to_base58_check public_key)
         | `Graphql_error _ as e ->
             don't_wait_for (Graphql_lib.Client.Connection_error.ok_exn e)
       in
       match access_method with
       | `GraphQL graphql_endpoint -> (
           match%map do_graphql graphql_endpoint with
           | Ok res ->
               print_result res
           | Error err ->
               don't_wait_for (Graphql_lib.Client.Connection_error.ok_exn err) )
       | `Conf_dir conf_dir ->
           let%map res = do_local conf_dir in
           print_result res
       | `None -> (
           let default_graphql_endpoint =
             Cli_lib.Flag.(Uri.Client.{ Types.name; value = default })
           in
           match%bind do_graphql default_graphql_endpoint with
           | Ok res ->
               Deferred.return (print_result res)
           | Error _res ->
               let conf_dir = Mina_lib.Conf_dir.compute_conf_dir None in
               eprintf
                 "%sWarning: Could not connect to a running daemon.\n\
                  Importing to local directory %s%s\n"
                 Bash_colors.orange conf_dir Bash_colors.none ;
               let%map res = do_local conf_dir in
               print_result res ) )

let export_key =
  let privkey_path = Cli_lib.Flag.privkey_write_path in
  let pk_flag =
    let open Command.Param in
    flag "--public-key" ~aliases:[ "public-key" ]
      ~doc:"PUBLICKEY Public key of account to be exported"
      (required Cli_lib.Arg_type.public_key_compressed)
  in
  let conf_dir = Cli_lib.Flag.conf_dir in
  let flags = Args.zip3 privkey_path pk_flag conf_dir in
  Command.async
    ~summary:
      "Export a tracked account so that it can be saved or transferred between \
       machines.\n\
      \ Set MINA_PRIVKEY_PASS environment variable to use non-interactively \
       (key will be exported using the same password)."
    (Cli_lib.Background_daemon.graphql_init flags
       ~f:(fun _ (export_path, pk, conf_dir) ->
         let open Deferred.Let_syntax in
         let%bind home = Sys.home_directory () in
         let conf_dir =
           Option.value
             ~default:(home ^/ Cli_lib.Default.conf_dir_name)
             conf_dir
         in
         let wallets_disk_location = conf_dir ^/ "wallets" in
         let%bind wallets =
           Secrets.Wallets.load ~logger:(Logger.create ())
             ~disk_location:wallets_disk_location
         in
         let password =
           lazy
             (Secrets.Password.hidden_line_or_env
                "Password for exported account: " ~env:Secrets.Keypair.env )
         in
         let%bind account =
           let open Deferred.Result.Let_syntax in
           let%bind _ = Secrets.Wallets.unlock wallets ~needle:pk ~password in
           Secrets.Wallets.find_identity wallets ~needle:pk
           |> Result.of_option ~error:`Not_found
           |> Deferred.return
         in
         let kp =
           match account with
           | Ok (`Keypair kp) ->
               Ok kp
           | Ok (`Hd_index i) ->
               Error
                 (sprintf
                    !"account is an HD account (hardware wallet), the \
                      associated index is %{Unsigned.UInt32}"
                    i )
           | Error `Bad_password ->
               Error
                 (sprintf
                    !"wrong password provided for account \
                      %{Public_key.Compressed.to_base58_check}"
                    pk )
           | Error (`Key_read_error e) ->
               Error
                 (sprintf
                    !"Error reading the secret key file for account \
                      %{Public_key.Compressed.to_base58_check}: %s"
                    pk
                    (Secrets.Privkey_error.to_string e) )
           | Error `Not_found ->
               Error
                 (sprintf
                    !"account not found corresponding to account \
                      %{Public_key.Compressed.to_base58_check}"
                    pk )
         in
         match kp with
         | Ok kp ->
             let%bind () =
               Secrets.Keypair.Terminal_stdin.write_exn kp
                 ~privkey_path:export_path
             in
             printf
               !"😄 Account exported to %s: %s\n"
               export_path
               (Public_key.Compressed.to_base58_check pk) ;
             Deferred.unit
         | Error e ->
             printf "❌ Export failed -- %s\n" e ;
             Deferred.unit ) )

let list_accounts =
  Command.async ~summary:"List all owned accounts"
    (let%map_open.Command access_method =
       choose_one
         ~if_nothing_chosen:(Default_to `None)
         [ Cli_lib.Flag.Uri.Client.rest_graphql_opt
           |> map ~f:(Option.map ~f:(fun port -> `GraphQL port))
         ; Cli_lib.Flag.conf_dir
           |> map ~f:(Option.map ~f:(fun conf_dir -> `Conf_dir conf_dir))
         ]
     in
     fun () ->
       let do_graphql graphql_endpoint =
         match%map
           Graphql_client.query
             Graphql_queries.Get_tracked_accounts.(make @@ makeVariables ())
             graphql_endpoint
         with
         | Ok response -> (
             match response.trackedAccounts with
             | [||] ->
                 printf
                   "😢 You have no tracked accounts!\n\
                    You can make a new one using `mina accounts create`\n" ;
                 Ok ()
             | accounts ->
                 Array.iteri accounts ~f:(fun i w ->
                     printf
                       "Account .%d:\n\
                       \  Public key: %s\n\
                       \  Balance: %s\n\
                       \  Locked: %b\n"
                       (i + 1)
                       (Public_key.Compressed.to_base58_check w.public_key)
                       (Currency.Balance.to_formatted_string w.balance.total)
                       (Option.value ~default:true w.locked) ) ;
                 Ok () )
         | Error (`Failed_request _ as err) ->
             Error err
         | Error (`Graphql_error _ as err) ->
             don't_wait_for (Graphql_lib.Client.Connection_error.ok_exn err) ;
             Ok ()
       in
       let do_local conf_dir =
         let wallets_disk_location = conf_dir ^/ "wallets" in
         let%map wallets =
           Secrets.Wallets.load ~logger:(Logger.create ())
             ~disk_location:wallets_disk_location
         in
         match wallets |> Secrets.Wallets.pks with
         | [] ->
             printf
               "😢 You have no tracked accounts!\n\
                You can make a new one using `mina accounts create`\n"
         | accounts ->
             List.iteri accounts ~f:(fun i public_key ->
                 printf "Account .%d:\n  Public key: %s\n" (i + 1)
                   (Public_key.Compressed.to_base58_check public_key) )
       in
       match access_method with
       | `GraphQL graphql_endpoint -> (
           match%map do_graphql graphql_endpoint with
           | Ok () ->
               ()
           | Error err ->
               don't_wait_for (Graphql_lib.Client.Connection_error.ok_exn err) )
       | `Conf_dir conf_dir ->
           do_local conf_dir
       | `None -> (
           let default_graphql_endpoint =
             Cli_lib.Flag.(Uri.Client.{ Types.name; value = default })
           in
           match%bind do_graphql default_graphql_endpoint with
           | Ok () ->
               Deferred.unit
           | Error _res ->
               let conf_dir = Mina_lib.Conf_dir.compute_conf_dir None in
               eprintf
                 "%sWarning: Could not connect to a running daemon.\n\
                  Listing from local directory %s%s\n"
                 Bash_colors.orange conf_dir Bash_colors.none ;
               do_local conf_dir ) )

let create_account =
  let open Command.Param in
  Command.async ~summary:"Create new account"
    (Cli_lib.Background_daemon.graphql_init (return ())
       ~f:(fun graphql_endpoint () ->
         let%bind password =
           Secrets.Keypair.Terminal_stdin.prompt_password
             "Password for new account: "
         in
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Create_account.(
               make @@ makeVariables ~password:(Bytes.to_string password) ())
             graphql_endpoint
         in
         let pk_string =
           Public_key.Compressed.to_base58_check
             response.createAccount.public_key
         in
         printf "\n😄 Added new account!\nPublic key: %s\n" pk_string ) )

let create_hd_account =
  Command.async ~summary:Secrets.Hardware_wallets.create_hd_account_summary
    (Cli_lib.Background_daemon.graphql_init Cli_lib.Flag.Signed_command.hd_index
       ~f:(fun graphql_endpoint hd_index ->
         let%map response =
           Graphql_client.(
             query_exn
               Graphql_queries.Create_hd_account.(
                 make @@ makeVariables ~hd_index ()))
             graphql_endpoint
         in
         let pk_string =
           Public_key.Compressed.to_base58_check
             response.createHDAccount.public_key
         in
         printf "\n😄 created HD account with HD-index %s!\nPublic key: %s\n"
           (Mina_numbers.Hd_index.to_string hd_index)
           pk_string ) )

let unlock_account =
  let open Command.Param in
  let pk_flag =
    flag "--public-key" ~aliases:[ "public-key" ]
      ~doc:"PUBLICKEY Public key to be unlocked"
      (required Cli_lib.Arg_type.public_key_compressed)
  in
  Command.async ~summary:"Unlock a tracked account"
    (Cli_lib.Background_daemon.graphql_init pk_flag
       ~f:(fun graphql_endpoint pk_str ->
         let password =
           Deferred.map ~f:Or_error.return
             (Secrets.Password.hidden_line_or_env "Password to unlock account: "
                ~env:Secrets.Keypair.env )
         in
         match%bind password with
         | Ok password_bytes ->
             let%map response =
               Graphql_client.query_exn
                 Graphql_queries.Unlock_account.(
                   make
                   @@ makeVariables ~public_key:pk_str
                        ~password:(Bytes.to_string password_bytes)
                        ())
                 graphql_endpoint
             in
             let pk_string =
               Public_key.Compressed.to_base58_check
                 response.unlockAccount.public_key
             in
             printf "\n🔓 Unlocked account!\nPublic key: %s\n" pk_string
         | Error e ->
             Deferred.return
               (printf "❌ Error unlocking account: %s\n" (Error.to_string_hum e)) )
    )

let lock_account =
  let open Command.Param in
  let pk_flag =
    flag "--public-key" ~aliases:[ "public-key" ]
      ~doc:"PUBLICKEY Public key of account to be locked"
      (required Cli_lib.Arg_type.public_key_compressed)
  in
  Command.async ~summary:"Lock a tracked account"
    (Cli_lib.Background_daemon.graphql_init pk_flag
       ~f:(fun graphql_endpoint pk ->
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Lock_account.(
               make @@ makeVariables ~public_key:pk ())
             graphql_endpoint
         in
         let pk_string =
           Public_key.Compressed.to_base58_check response.lockAccount.public_key
         in
         printf "🔒 Locked account!\nPublic key: %s\n" pk_string ) )

let generate_libp2p_keypair =
  Command.async
    ~summary:"Generate a new libp2p keypair and print out the peer ID"
    (let open Command.Let_syntax in
    let%map_open _privkey_path = Cli_lib.Flag.privkey_write_path in
    Cli_lib.Exceptions.handle_nicely
    @@ fun () ->
    Deferred.ignore_m
      (let _logger = Logger.null () in
       let bytes =
         match
           (*  *)
           Hex.Safe.of_hex
             "0101010159992fc38fea3a3a8129615183fc54049476354311c61cf2d171195c571c9a300101015087da8690c01514f2f2d85993b8bd3790d75eb8ef49e851490e53ec866eda1501010101010161548f6e8e12e9c8c1699d6dd6590a26ffce9962baf75e06e7d4f410daf1a5060120aeac68524e6138bbec3030401ac89cd2707e0c396138243171128321db61a75c01208acb2b2033fd3d1f7e3e9499713ceaad1db52188e47658bdb7d105202a0ea8390101e55509659101d75f88ac72f31a7fdddee4a5e2d25e899aea5cb2907045b04b2101c04af305f73e9eb19af302f7cfc3c579f2a5e340fed20b04d6c71e97a4bd78260185f6ae102a303d1352eabeb8461f62a47a687b6dcde76233b9207168a0acf72c010101020101fca065735d8201000001010101fd6c80020001012101010e0b0101040101040101040101040101050101050101050101040101040101040101040120f01b19955c45b860c75fddac330dc90a933b9578044619de261c3982674800000101fc41f3b2a1d64a0f0d01010101fdb5af03000101fee41b0101fdb5af0300010101010106b4f567acda61a61642aa5c1d728d3b829ce1a263c2c5a519b9cf6381ddf9200101fc41938499bab1f10c01985cdeffe02f26d0c4661fa5bff66bdfd5bece77413ecc3c32d5869173ee302a01c588b7bd5e57cf47e806a4cd6678551d628d74253108552ead9ef41cf400ee3a0196fd1ddd9e748da63eab7d58caa73144fd2d1661fbb87e3c6567b230eaeed5340101fe111301010101019d1e3c6f49af98599fa6c6c9f933266edfcf0d82eae0ebbf79e7825be3797a2c0101fc41f3bcca7666020d01ed7c570176dc0ad7a4ad2f9cc1a36d90a881aaacc7ec221e18e1dde8902ced3b012e332afbc05ea5020548f0d1bcc0222647160d6305fa402ab50be935e1fe542501d817780b754d6bc669a72230eede191e3bbb98f2854baabb39dbef8a374a581c0101fe0b10010101a72af01c0df4110349de662f17198cb90c87c28ba3f88ab085cb468ab2a32011000101f2763f3ae2cc8d117b10b2c0da93bdeb8e16be8d6ebf6ca4ea0ef62e11730e080001013cf8a0610472875c6f7fa93f78ca5e3d389302cd969ba2688866c8d85093b505010001010101fe22010101fee41b0101070101000101fc00647b3d7801000001010101010101010101000101fc71aa67ae977d4f4901fcf17650d49f6a8210000101fc425e7a33a117b43501fca2900163cb9376ab000101fc40e4ebff9dee74dc01fc15a29c3e1867098f0001000101fc307bff78bf53078901fc7bab8050ad3a12c60001003c37758e80a4ec5736e8261c7a6afddd2298a710c4e1121965f2373dd8b414190100ecd3f102856dcd418cc90d490124cc4872660b87a9a241773aa07dbeaa34aa0601000101fc131bc222f902effd01fc427c31dafc55f61b0001010101000101fcbc5d57b401224fa401fc19dccb7aee8f072f000101000101fc9d60e26e5c05cb8d01fc1dfa66fcb597e5c4000101000101fcd2bfa991fef5ae3d01fc49e56ee16a5144db000101000101fc0317203c0e16f5c401fccbdd11528f99e791000101000101fcfafe0592925c854301fcb432f79fcee3918c000101000101fcf40e48173fc48a3501fcc31d37814f56c809000101000101fc0ca4460d24808c8c01fc9d6bf6e50da50349000101000101fc96b11f683d06459b01fce6c7b3dd545a42d0000101000101fc14981c116811132401fce76cc7a79ca3a4f1000101000101fc2c18a6160f15475201fcd67a7dcdca724dd6000101000101fc9cb417842fc3059001fc5406999b84d0b1f2000101000101fc567459f7db3beb7e01fc0a17ee8a6a86cdcf000101000101fc5acf4e4cfab8b13101fcacce28e8d9101874000101000101fcc6b39f34b3afee6301fc3818c57075359031000101000101fc9459c0e6f91a5bac01fcc4eb39105e234e56000101000101fc4f739a038452c2c101fc0360b7ff256ea210000101000101fc5c3a13a02c675ebb01fc2cde56a18726a626000101000101fcd04cff627868762c01fca05a502803a0525d00000100010101fc825245c209400d6301fc88804a818825cd9e01fc14de87a127f8ec4c01fcb2aa3303d244872e00019f08fa6a81ff37327988be047e83c85eab4c37019055b96ce155a405686c1a07745d61be9e5f07faefad579b5e1aebb1e7b89c23978551667aa8bbf4fbac0121010101010101000101fcba6a3bf01bfc7f8401fc11ab65654a13bebe000101000101fc5a204e591b9beb4501fc5edf99341b18270c000101000101fc3c33ddc0d541be4701fcfa0774d894b79756000101000101fc8187483f8eb19ae901fc30f89dd380ee4757000101000101fccf904646ee97be4a01fcc08eee26aa8454a1000101000101fcc0e62a8349ac76a901fcd1481aecbfa32011000101000101fcfcb1dff3367ed4d701fc358317e4ef9006ba000101000101fc99749673a510776a01fc466f141a8e40fdae000101000101fcc47a0decf6a5fbf001fc6bf7d1ac2a976ad9000101000101fc2cbb138f9c86d6e101fccb460514fc521550000101000101fc9d5d83617913128901fc1da855383a54a8e5000101000101fcadc24112868b53ab01fc4cee6b734f980163000101000101fc0cf7208aa6d7642d01fcfbad2cdd5fc9e4d7000101000101fc8bb5dde953fb41d001fcc22d24b7a98d1f57000101000101fc6cf0bbdb14f6d03f01fc090739ac700668f6000101000101fc3382b35a55db0a5d01fcfaf91fd6d13d76e4000101000101fcfe13a06260ebf6f401fcd39f2a63410bea8600000101010101000101fcc3865529bdbd94dc01fcee981f81529635eb000101000101fcb024832c38ac4d4401fc4e1c316e0314721f000101000101fc03e22105dd623ac001fcf5b8c1a2437d0bb3000101000101fcf731d9515277a3f201fc37977e7c40271620000101000101fcb0eb934edfec437c01fc0a1a02a08f6208d6000101000101fcdc4bcac6177e3a3601fc76cee0eccfa91bf4000101000101fc6a44e37c31710bde01fc14edbd41c5a49edc000101000101fc703056eac3b50bbd01fcf5ea51944064ca22000101000101fca52933d1386c7de501fccae26d68e08a9c68000101000101fc97b38e9f0359970001fca3d3609743027925000101000101fcae58fa5afc8752c801fc2b0c9e6dbeceed92000101000101fc8f9e13929383a9bb01fcebfda3be8631b415000101000101fcf12bfe82858ea05b01fcf9c3114c5e467f33000101000101fcb82fb91ad355478501fcd60cad7864786379000101000101fce3c6bfd81280816a01fc1097e0e73988ea0f000101000101fcd19c4a3210bcbd7c01fce94eeff814a046ab000101000101fcdc7ff360dd4fc74901fc332c5dfe582deba60000000100010296169967240a77d3f34b54bfedb7f087a45703e6c07fb1ea7cc8cc651f75572d0838104217d481591463ce2a45c5a2c64a2f5f5fb6ae81c94035d2a6ab7cf430980ad35290f798aefbfd6d15219fd45b0176da475dadb8a39201805ce90c6526404a3daeed22d59710ccc3044d3cd9a470417faf24757c8f1aa1b75693eeb22e010201010101000101fcb9b75415f429681d01fc21a9c6e5e722c6ed000101000101fcdada77ebeb2a22fc01fce5bac758aeacd6b1000101000101fcf1f6d0ce3f70d1ed01fc77fb91374d168f3e000101000101fc6899f1981eb6c12401fc02869eea526b595b000101000101fc112c6692d2623fda01fc3653404a7d4e48e9000101000101fc7ec5323fe9f01ae201fc774372a695d9a92c000101000101fc6130c41ef51b6fb501fc682fbe870ecab173000101000101fc9d43275f015cd0ec01fc4eb8a5afd44fc189000101000101fc30db790202cf81e101fcac89a78d1f0ed10e000101000101fcb2817ca1bc9d4bba01fcb8355c489ac76108000101000101fc775c3165a9faf35e01fcc94d23f293987bb3000101000101fcb68a26d1b7f0091d01fcbd455cef791ecd17000101000101fce7516d61308858a701fc6771e4338b55e94a000101000101fc2f690ca86a438a9401fcdfbd42e36fbaeb7f000101000101fc3fba63138d557c6401fca7a4ddea303356b5000101000101fc1feabb8065cf4da501fc139134424a6bb366000101000101fced31b55d16cee47e01fc5dd097755b58c5b0000101000101fca9ad95d6e2d9d43301fc57e328a41ff600bf000001010101000101fc0cc569ca6d33a10801fc8b86127dfa0ff0c3000101000101fc8900b3fe9ad68b7b01fc1128b96a8dd2f3b3000101000101fcc107358670b8cf7301fc9ce7ebc325ec9b72000101000101fc254fab5728b65b0a01fcf280eb3a8398afb1000101000101fcb92aa756b47073ad01fcfc9ef1d523f94f36000101000101fc007c2f8e621c875901fc87cc26a273f6c79c000101000101fcaff2b69e1d82542301fc7ef57df6d16bb9d5000101000101fcf68690a0d0f77b1401fc682a2440ce625e62000101000101fc0ead5632631bc6af01fcfbe11fa110728009000101000101fc9141accabe1e58f901fca6c9b73e99d51e57000101000101fcbdc364cda3504eaa01fc68c5fec10ae714d8000101000101fc6dde5e82dfac1db201fca507dbc517a208fe000101000101fc7faae6e9d60f4e8201fc2c7fb94e983af6fc000101000101fcca041b712b8831c301fc2926aeb6c7ac3991000101000101fcef5181fd478f0fdc01fc87953bc4437aa900000101000101fcbf3286d11ed8386001fcab05aaa270bcb3f9000101000101fc5a47afe5e0d2759401fcf6d2c3ae02a7cbd1000101000101fc047c2df9f60f78be01fc54e2fb3291583de10000010101019bf81148fffa4c9f4b0b5344b3f56930bbec26d682b3425f297220e512ce1a0e010109df544f5273d3b73cbc8ae50c2ecef40ed91fb64024ae3b32add904c6c212180101f410bf4fca3dc11eb7c538d9d36d42ea2e26039b8a5f17604f489cddfd448b11010127b95bb114eadb61ed884923b17a29537775dcefb00655152cc142bac0c8c9380105bcac4d513ce7714647a72822af2cd9b5d82fe20630233e8a838a66556bd11013cd92918c5569bda3f879410513310277fe22e609dd288b725d7bc25f017f6117bd7b17c1bd1eaa46de509022c659941e4d63c8e9f5619bd113bbc68f39f16202a40bafc6a7a135214c37b2ba8b371105e81d82a56b3cd913b07d226ea1e539294ea75b10058ea44c5519625179580c5a2dfdb60a488e1547098bf55ee1b437010101fb92f10914802d2de3597b9fdde58da8ac38c4bf3b65bc00a75d87183001813901014990ec57adb0dfce72b13155b81afb2622a1f0836e6a0c9957dd8ef79632c021010127e0dd4c4fecbd0e43c01476e2e999cb48b790c93f3c764195f9a5ce70f22d2f0101014244d8a8fa4836c030ce51c045de200e703caa55e056598620664613399343380101d820849427e02bf138a781cea7858dfefe5cda6917de414c3a782c12a69a3c130101a1ffeab3002f867427974113060bda8915012afc8d0c92f61b32cc513575663001015b005b9c405c20537931d66d8823fc959c1333aaebace9a75aaf8038329c602b010515a5bbe05f9dc4502c0060c5553e21ae4a2fd8522257f77b3992633acce8b12681daa6d725d3107dab7fac53b768db28af39312d066a22c4c85ad111cbc9312e65537fe2dde7e34587187e817635a68142a27fa69dd5c82f9ce3f4d80cb02122acbd1655c50a623a907461eeba33b4e73aa1bcaa595ac651c1e442fe8f7e5c348bcc2fee50f395f30b09c16f8de06f729bcc46cc5860c3667f508418243651320101239d4c46e01e0feecfc405e4e17e026b4638424053798893c9b0cb8d1432943d0101172f93b7093397960408b6ef055b109b1f416652bdabfad25369bdc74ea33635010136cf42b12ae0adbc33043ba76023dd159646339d560e89b25835cf7fc390c80901a995c3d36436cdf3ea3d15789a72250a4e8a13427897df05b83f8b4d470aa03dcdf7c442e8c9d36feea866aa468c75d9c0af3a94f670947453594ebda58e89390101010101011a4a37174c6903cf892b76868128d60340d98b78ec3bc49e3c18b7ae4629d334a4b0ed11c7e95d18aef4a5f141cddf44fca5e9360fda0178b02f66ad887a8c29010101fb9ddc3ac2326c3aef22f307a041579ae6b26af3aeee691bc79df869f53b85027323b2f4d4230e42591958101c041f282308e78ddf29804322729f324b4e7c1401010101763f1431f409104060c696a239f75529bdf1cc75411d39fae3ef2e74a19f36fc148047a7665de2b52af33de7ecff3b41dce765671acfb579eb52aee16bc33901010151697da3a1f05c0189b1c297d430cf66a7f20147860de82cae6c948a11e0612f0a7331e83101a355a0fc27f286303702c9a2208c2b538e448b6c66ab3f311f0a01010501012b8742a9d03b3efa0a313ececb96eb08217efe8abd5224862816fdde5700b002311bf3142fd064d1b7b2bfc0ac82fdd3f28d0478dc6bb4ebfd2fa65d6037db1c0101cfe8744e5c0909e53edabcc406f712b1aa6830bf7b56fb9ba02c2c3ba04c151ae18c114274615eda0391b5d857efb42eb627ff6657f30ce651a23f6a40938e1b0101a81d1740eac6797c2188636d3ab9ccf54d5952a7961051d1d07ed6d8467f212996161875248228acc8f2d7ba1b326b965565b8e18237471c57507c9129829e3b010155fbeabf20cce0b0ad12dce688bc4fe52c2ff86f38fc95f606663ebd258dbf04ed8809e0b4112b48ee5facd457969a7f117387d5b8b2277cf2eb247af0bb533f0101febb25d82fc989c1e2def4075e48e024e9e5e9eb97a7e26cfe949c2f5787ca1890353024139086159911433c090ce9ec86f78f348138dd3c15ed58d5223cdc30010129a1a349521e371840f9d1b67a5d888b0e5ce76ab168d553a2d3313d3a25e53132fb6c029a46c72cf15acc082aa2cbcbc00673f3391e09c185aad9a57fbbfb0f01010111192a91530a88bc8ab8a2b988105658e749afc015bff2cb6fe2bd646d1f695a2622753b53139049d36c1b9ff87b5eebffddbe01fca1123ab07c38541e8b84cc278aee987d5f4648e6893d80436a6bd00e31ca7ebe6941574904187c6e0d31a62c9cd4b6337639d94306e09514ccf55fbd52ddafaea4b7ba6ad79dd38622f8ba03efb282c53357004926c0df092fe2c11e652bb8d0e927bc7cd9ade8efa44fb43b69450a0c5381650686fe58731eb4f177de8733a411667fe4030c915fb8d631035b08b42e0983a0bc4ad037c5f6a04ba58af97184259175b91aed31c25e63202c3c4bc80f9dd51e5d2f12b088628038cef7a03a4aa8d0c784abe5db1e8fffc22895c927c93dedb15756946ed0ce978ef071f682483989c2d7ef70d9182ceb522202ac866d6fc2d2cda4fed36ce4b25670c962b7c3d70f0af0d387237b3d570102682972e3b5323d204009b783235573a7c1e9223ba4eb9c2357d19a808c9fb92d13bb39c5d8f3fb0375ac19efbbcf6fb1d560fac0692d47e3990c2a031817ac064e6dcc40469ed9ebb7cfc808850c9ddfffa3951b53b431a31d720f90de504406c71b03110a6621bf2f75c6114e71bf8ea073dd2a562d4ab579956b1426ee171309bb66622f0363a83d7e4f9b84e0448548f1a8d43bde23e3af092f4c413533284eb7f2e80a9a2cb45cc829c6239d3965d502ddde87a6a678296c7e213c879d100a5a5b816f3037ebccfa1b81beabcb21d4941916176136d92222713549337934ea8d120cc47437f10ce20f8811311e5f4eecbd4de5258e843a21bd74108bd929648a79b0ec6f640278f78ef6053b72b5f3f991044de523f9eb1845f5dec22b2a290927c0d28a53a563b7d4def1dba7eba0aa1518b899975f25de7d303971220f0955c09d1c8f7e9bfb077ce4ef614cf281c4a848dd1bdbe4529ac8e078f7ca3e6d614a1e8e3e10d7ce2d02fe7d82e4899d0fcf9423f0cc7a8c2939fa1e616d19ba1d1b24b447ad3341b0f4c44d105ddedb2140d42f43305d2aac6b5de664c711cf7322918b7ac758cc52ea6c507fcda1197d109322232e8610de8e1dd70fe619a2f43cc084c5bcd7b342d38e2024ab842fff898b9fe7871dd53d7f1bc70c022ce3397b07fc252651d643919c0de29ea5a988a88e4683746a1925f986273afb286f0ed3e576d52b61d249cbb48583ffd339394b57e9450e9058ec274f4be4413febf2ece48096c9cba1ce075b14664766c57417a0d02115c2b1fc6f5f35c51b2e85670f8870deb486c35e92543888f42542c43e1d29a259c103753053531958370f7fb984451316fe800a445eb0eff56cf24912958451da957652ae0b9bc8e31512fa8d1f5961976eb39fb2dd877857a6854fa91f8d33a6bc2fd2957561511b2c56d11e186ea130cfa8114a86620b7c18ba5a95f47a3f4b1ecfbe6ec1c8e8da012f47033f012c24a022c3c461b4a17ce99f9808976980c69b0a1532f6a167991432b8891e2fafb97b0d8fef149352c27ddc0e89c12252612b0068cac2b3104f00d46a722b1ae257ea0512ece0c5093d245456a236de0cb73496c67fa3e393d51958e8d00f036509b1dbfca5cfd3c956aa4fbf341761bfe6b125c0a2042904bb18e36153303e8af3c1246295211902cae14bbb58571e2258e207aaa4c1d5a2ba17e8720a5833229973f0bf8fb4bcb97ecddf23f0b38e9c02539f05ab77720a3305a72ba7d068e570c4c78ec0cde6694ed2900e4d8a0c7bba2f11d5c638072b74066a312b8e3cd42a5831b1432138c310de55024b7e7315f31630fccd7faf2d57010ca26e8f5a6eb60076fb0d1f893b82e4e65354dcbbf43db6653616c90c7cd929ff2d39bcb10d3857bfa35f86ae67a9a2787225421d1f6b0c003e4e80806c150f8142b61becc14310d356f6b2b993a6016f978bf2b6b11e00776971e51204f233734fbcb82f89be8a5f2041121dac7643966c60500f040e7ed7b625859620bc003dbde4b84827034eac4b15a2386b95be6192880a544a71afc4a141de4edd893fb492d11a57e1c1e626726e0e1a63e94b71cc137c100b3300a9ba6b571ba85828e8fdac11bca3d132a0c50813793bc6451ce4724aa81213f7dac04e4007ea262e26a456d629123b07ea783ae5d8fed4cdaabc2c54e58bbed44ae0f8714355c82f662d286ad1b0d6c0a1a499867fd2ebdeddaa35847382dfceae696fe467b7031a0330f36d670f19c57aea05415c0e2a9a84c028299d32c2068aac564d5e25301d660ae08c0477e688d3e28244cbbcc844463d930ec62c9487188b56aed196701f3dda3a4c5ef0f380208f1639937582c572561c1c62666e6781edf8cd2b757f03a671969b9a155265d554f559c9d83f65b2e044cf90832214d5da13ec79b51c02b931698a79bb0539aa6852c30338fdf759010599cb0447d814c9a36bf2388e096c8686795883776b9d3bd60c5c6c0392070938d25e70084c9e665880a4f2b62e1da5afa9010f00f413c4364287cedb2cb9e3a61d31494494997600812f95d8080eeaffb45bd48e4194aef9ad5a87be9303f1ed6f4453e9ef53bb8ef2ad86820d5d674abadb646f52b812eac6752ebe6887011522304a214e366cd3cf073b7a1fef220c171577c509c2153a7eccc0aff98df49cbbc9793b19ea70516c28e9230aca40204b76bcba0d2a2806a51ff52af733d16be3cd704e0070214adc8d90072c87c05ac165c02603987573fb8af4621484448bdfe67bda438b9b765ffcb5222a61dbea39b1408902dd5543ee0155793d955a33d0e2843b146e0cad4625f7732a4bb8b27ae4305cd2e8be075b73b6de92db36e1c3418c871385bc82075080f51c0ed8b2daae0f099b91a6ef50a1d73d82d2199190c086b841a86c33bcd260e22c1fe4d95aec5c4e56e5676eff166a3b92a5229014374483b10e167a97abf180188a02385d5094d2b5fbc549309651d9a171d620c9b3d1efee489d27087faf043434acef83441f4183777613417603a18ed8af5ad28e06af905042d37d3fe2d925a95a722bb6f4fd91628e8435c520e211043af84f1ca4302b8f2860188fabed0246e7b27e22a562ae0a57b1e77b0007a3ad1b3a548077b0364516c8871a244e060d8a0d1d922474b618ea8b02dc9758fe3077b1777c3b477c1e717423852cb33a358bd98df14b5937ba75e2769dbf23deadf1aa1c0930a7063795a681f7a6d1052e1bbd5bc7e8db3c02fdab617ea0d3807217a8814d2006f4702e27bcc8fc29008e27c01cc11fabc2eba775260ba94442d5e8c52bd4edbb530f52f19aaa42a71fba3d037b49dc6cf82b3d3f9a44fb928b96b3d9c930f204193dd62977970b68080101014ce66d306e6557e4b4f7d882e39012f8c7e5184f47bad42c16bb2ee74cfda61501017b2c61438af7a35a1cabf8b1a1e4d67500912148c93e9bcd8caa2816343eaa250101642d1a6a52ecd044244a2910173e62a77f7e23c748a931270803b06d3918471201016feb0ab9c4aca731dc2e4e1052445aded2dc19a8228b08cee700fbe23ef9df330105fbd54b38f00e74d0ef79d432608c7a025461d60fc7764b71aa33ddd3c657f13cf2b8e98811d5d0bbe1ed69d3c5a9c5447d72232910ff9f203e8af52798aff2038e0570ed59badabcb2b4894a6cbce257d7e83641ef85d466ff530ff4de2ca80aacbf4a9313904d500807cd374a1179b208495684380b4870e3841bba2bed172bc830e2188139fb2aacc20c0d8e3f8e25761dc1fa64e2a77b44d7c5ed6fc9e33901013f13def8a8a224b04c4035509736659036c9978073b01042cafe45d6f3b7a82301011257b587f312eacad9f03d79ae9f6fc58a3e52aa20dda367f0a421cc6966f6300101e42491a0a1dca4f1a39b800fefc2c980f24a4d979b2182065d08976690e7aa3e01010164e1e2d4f89544478f02849179fbf25235ef72eeb532d030f89aef946b0e9c260101dc4857c5600ebc405cae261a0f6a4dafca7a790f2403b880c4a9d1863d82232101018503175e9fa57b40e4b6358ed75c81e0383e8ccb8978a23796dc97bf27133b380101d7f9d586e7361bc736c522ee8757cbc7c2016c7424d8fcb8fe7b3880a001c52b01054e6cc43913116fe46cc6b9efe28421eec5d80f93c7511958dd02a9ccb29db8261f0df9b286fc95f3220207097366178b0641b909f6954a8a7bf5b4b055ebf1299ee6d52d1c0bf2e71a71e9c6a93af99f7c7c88a200374dc535304f9c7055c4026b354ba00426896e6f24072cc127904bc3cfbc9ead31ae292108fea0f5a55b177fa5e16a9b7eebb7fbabb33c9a2bdf9d8130b4fdf2b189015cd194624a3d5f2b01018c681b699e8a629ffe3a272ee21fecdca1f9df8926c053047d66d566c4c5db370101da501bda1112016ba13856edbc6fbdfaeed17a30f971d7fa0556efcdcb46962f0101fe503efe26a94754dfe83ed163581e7a1a2f86a9fc0c5f0762f9ddd5c700572501010101000101010100010101010101010101fd40420f00010101010101f436f2bb64609000330c381cec3663c4d921ad4d7e1fbd17feaa55d77956183f010101fdd08503000101ffff01220100000000000000000000000000000000000000000000000000000000000000000001010001010101f436f2bb64609000330c381cec3663c4d921ad4d7e1fbd17feaa55d77956183f010101208adb24c1f05947842a4523a9ef9d278a8a5114c8c3751a0b7efade5101ad3300010101010101fee803010101f436f2bb64609000330c381cec3663c4d921ad4d7e1fbd17feaa55d77956183f01010110ca24cfef82e28b9a1d6f2b03c1a6aaadab6c4072e4a9c9f66aa5b12d7a1a3731b59f30bbfdeb237727dfcb6e9887f5e9e5bdfcb95456482dcfda3d897f9d2e0100010000000101010101fce9eb7cfec800000001010101fce9eb7cfec800000001010101fd02f84d3801010002010001010101fc5b8a08f85e10000000010101010101fc9bcc17f85e10000000000159992fc38fea3a3a8129615183fc54049476354311c61cf2d171195c571c9a3000010200000000"
         with
         | Some v ->
             v
         | None ->
             raise (Invalid_argument "")
       in
       (* let tag = "B \0\0\0\0\0\0\0" in
          let bytes = String.concat [ tag; bytes ] in *)
       let bytes_str = Bigstring.of_string bytes in
       let pos = 0 in
       let transition_result =
         (* Protocol_state.Poly.Stable.V1.of_yojson *)
         External_transition.Raw.Stable.bin_read_to_latest_opt bytes_str
           ~pos_ref:(ref pos)
       in
       printf "pos=%d" pos ;

       let _transition =
         match transition_result with
         | Some v ->
             v
         | None ->
             raise (Invalid_argument "123")
       in

       (* let proto_state = Result.ok_or_failwith result in *)
       (* let transition_v2 =
            External_transition.Validated.Stable.V1.to_latest transition
          in *)
       (* let transition = External_transition.Validated.lower transition in
          let proto_state =
            transition |> Mina_block.Validated.header |> Header.protocol_state
          in
          let state_hash = (Protocol_state.hashes proto_state).state_hash in
          printf "a: %s" (State_hash.to_base58_check state_hash) ; *)
       Deferred.return () ))

let trustlist_ip_flag =
  Command.Param.(
    flag "--ip-address" ~aliases:[ "ip-address" ]
      ~doc:"CIDR An IPv4 CIDR mask for the client trustlist (eg, 10.0.0.0/8)"
      (required Cli_lib.Arg_type.cidr_mask))

let trustlist_add =
  let open Deferred.Let_syntax in
  let open Daemon_rpcs in
  Command.async ~summary:"Add an IP to the trustlist"
    (Cli_lib.Background_daemon.rpc_init trustlist_ip_flag
       ~f:(fun port trustlist_ip ->
         let trustlist_ip_string = Unix.Cidr.to_string trustlist_ip in
         match%map Client.dispatch Add_trustlist.rpc trustlist_ip port with
         | Ok (Ok ()) ->
             printf "Added %s to client trustlist" trustlist_ip_string
         | Ok (Error e) ->
             eprintf "Error adding %s to client trustlist: %s"
               trustlist_ip_string (Error.to_string_hum e)
         | Error e ->
             eprintf "Unknown error doing daemon RPC: %s"
               (Error.to_string_hum e) ) )

let trustlist_remove =
  let open Deferred.Let_syntax in
  let open Daemon_rpcs in
  Command.async ~summary:"Remove a CIDR mask from the trustlist"
    (Cli_lib.Background_daemon.rpc_init trustlist_ip_flag
       ~f:(fun port trustlist_ip ->
         let trustlist_ip_string = Unix.Cidr.to_string trustlist_ip in
         match%map Client.dispatch Remove_trustlist.rpc trustlist_ip port with
         | Ok (Ok ()) ->
             printf "Removed %s to client trustlist" trustlist_ip_string
         | Ok (Error e) ->
             eprintf "Error removing %s from client trustlist: %s"
               trustlist_ip_string (Error.to_string_hum e)
         | Error e ->
             eprintf "Unknown error doing daemon RPC: %s"
               (Error.to_string_hum e) ) )

let trustlist_list =
  let open Deferred.Let_syntax in
  let open Daemon_rpcs in
  let open Command.Param in
  Command.async ~summary:"List the CIDR masks in the trustlist"
    (Cli_lib.Background_daemon.rpc_init (return ()) ~f:(fun port () ->
         match%map Client.dispatch Get_trustlist.rpc () port with
         | Ok ips ->
             printf
               "The following IPs are permitted to connect to the daemon \
                control port:\n" ;
             List.iter ips ~f:(fun ip -> printf "%s\n" (Unix.Cidr.to_string ip))
         | Error e ->
             eprintf "Unknown error doing daemon RPC: %s"
               (Error.to_string_hum e) ) )

let get_peers_graphql =
  Command.async ~summary:"List the peers currently connected to the daemon"
    (Cli_lib.Background_daemon.graphql_init
       Command.Param.(return ())
       ~f:(fun graphql_endpoint () ->
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Get_peers.(make @@ makeVariables ())
             graphql_endpoint
         in
         Array.iter response.getPeers ~f:(fun peer ->
             printf "%s\n"
               (Network_peer.Peer.to_multiaddr_string
                  { host = Unix.Inet_addr.of_string peer.host
                  ; libp2p_port = peer.libp2pPort
                  ; peer_id = peer.peerId
                  } ) ) ) )

let add_peers_graphql =
  let open Command in
  let seed =
    Param.(
      flag "--seed" ~aliases:[ "-seed" ]
        ~doc:
          "true/false Whether to add these peers as 'seed' peers, which may \
           perform peer exchange. Default: true"
        (optional bool))
  in
  let peers =
    Param.(anon Anons.(non_empty_sequence_as_list ("peer" %: string)))
  in
  Command.async
    ~summary:
      "Add peers to the daemon\n\n\
       Addresses take the format /ip4/IPADDR/tcp/PORT/p2p/PEERID"
    (Cli_lib.Background_daemon.graphql_init (Param.both peers seed)
       ~f:(fun graphql_endpoint (input_peers, seed) ->
         let open Deferred.Let_syntax in
         let peers =
           List.map input_peers ~f:(fun peer ->
               match
                 Mina_net2.Multiaddr.of_string peer
                 |> Mina_net2.Multiaddr.to_peer
               with
               | Some peer ->
                   peer
               | None ->
                   eprintf
                     "Could not parse %s as a peer address. It should use the \
                      format /ip4/IPADDR/tcp/PORT/p2p/PEERID"
                     peer ;
                   Core.exit 1 )
         in
         let seed = Option.value ~default:true seed in
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Add_peers.(make @@ makeVariables ~peers ~seed ())
             graphql_endpoint
         in
         printf "Requested to add peers:\n" ;
         Array.iter response.addPeers ~f:(fun peer ->
             printf "%s\n"
               (Network_peer.Peer.to_multiaddr_string
                  { host = Unix.Inet_addr.of_string peer.host
                  ; libp2p_port = peer.libp2pPort
                  ; peer_id = peer.peerId
                  } ) ) ) )

let compile_time_constants =
  Command.async
    ~summary:"Print a JSON map of the compile-time consensus parameters"
    (Command.Param.return (fun () ->
         let home = Core.Sys.home_directory () in
         let conf_dir = home ^/ Cli_lib.Default.conf_dir_name in
         let genesis_dir =
           let home = Core.Sys.home_directory () in
           home ^/ Cli_lib.Default.conf_dir_name
         in
         let config_file =
           (* TODO: eventually, remove CODA_ variable *)
           let mina_config_file = "MINA_CONFIG_FILE" in
           let coda_config_file = "CODA_CONFIG_FILE" in
           match (Sys.getenv mina_config_file, Sys.getenv coda_config_file) with
           | Some config_file, _ ->
               config_file
           | None, Some config_file ->
               (* we print a deprecation warning on daemon startup, don't print here *)
               config_file
           | None, None ->
               conf_dir ^/ "daemon.json"
         in
         let open Async in
         let%map ({ consensus_constants; _ } as precomputed_values), _ =
           config_file |> Genesis_ledger_helper.load_config_json >>| Or_error.ok
           >>| Option.value ~default:(`Assoc [])
           >>| Runtime_config.of_yojson >>| Result.ok
           >>| Option.value ~default:Runtime_config.default
           >>= Genesis_ledger_helper.init_from_config_file ~genesis_dir
                 ~logger:(Logger.null ()) ~proof_level:None
           >>| Or_error.ok_exn
         in
         let all_constants =
           `Assoc
             [ ( "genesis_state_timestamp"
               , `String
                   ( Block_time.to_time
                       consensus_constants.genesis_state_timestamp
                   |> Core.Time.to_string_iso8601_basic ~zone:Core.Time.Zone.utc
                   ) )
             ; ("k", `Int (Unsigned.UInt32.to_int consensus_constants.k))
             ; ( "coinbase"
               , `String
                   (Currency.Amount.to_formatted_string
                      precomputed_values.constraint_constants.coinbase_amount )
               )
             ; ( "block_window_duration_ms"
               , `Int
                   precomputed_values.constraint_constants
                     .block_window_duration_ms )
             ; ("delta", `Int (Unsigned.UInt32.to_int consensus_constants.delta))
             ; ( "sub_windows_per_window"
               , `Int
                   (Unsigned.UInt32.to_int
                      consensus_constants.sub_windows_per_window ) )
             ; ( "slots_per_sub_window"
               , `Int
                   (Unsigned.UInt32.to_int
                      consensus_constants.slots_per_sub_window ) )
             ; ( "slots_per_window"
               , `Int
                   (Unsigned.UInt32.to_int consensus_constants.slots_per_window)
               )
             ; ( "slots_per_epoch"
               , `Int
                   (Unsigned.UInt32.to_int consensus_constants.slots_per_epoch)
               )
             ]
         in
         Core_kernel.printf "%s\n%!" (Yojson.Safe.to_string all_constants) ) )

let node_status =
  let open Command.Param in
  let open Deferred.Let_syntax in
  let daemon_peers_flag =
    flag "--daemon-peers" ~aliases:[ "daemon-peers" ] no_arg
      ~doc:"Get node statuses for peers known to the daemon"
  in
  let peers_flag =
    flag "--peers" ~aliases:[ "peers" ]
      (optional (Arg_type.comma_separated string))
      ~doc:"CSV-LIST Peer multiaddrs for obtaining node status"
  in
  let show_errors_flag =
    flag "--show-errors" ~aliases:[ "show-errors" ] no_arg
      ~doc:"Include error responses in output"
  in
  let flags = Args.zip3 daemon_peers_flag peers_flag show_errors_flag in
  Command.async ~summary:"Get node statuses for a set of peers"
    (Cli_lib.Background_daemon.rpc_init flags
       ~f:(fun port (daemon_peers, peers, show_errors) ->
         if
           (Option.is_none peers && not daemon_peers)
           || (Option.is_some peers && daemon_peers)
         then (
           eprintf
             "Must provide exactly one of daemon-peers or peer-ids flags\n%!" ;
           don't_wait_for (exit 33) ) ;
         let peer_ids_opt =
           Option.map peers ~f:(fun peers ->
               List.map peers ~f:Mina_net2.Multiaddr.of_string )
         in
         match%map
           Daemon_rpcs.Client.dispatch Daemon_rpcs.Get_node_status.rpc
             peer_ids_opt port
         with
         | Ok all_status_data ->
             let all_status_data =
               if show_errors then all_status_data
               else
                 List.filter all_status_data ~f:(fun td ->
                     match td with Ok _ -> true | Error _ -> false )
             in
             List.iter all_status_data ~f:(fun peer_status_data ->
                 printf "%s\n%!"
                   ( Yojson.Safe.to_string
                   @@ Mina_networking.Rpcs.Get_node_status.response_to_yojson
                        peer_status_data ) )
         | Error err ->
             printf "Failed to get node status: %s\n%!"
               (Error.to_string_hum err) ) )

let next_available_token_cmd =
  Command.async
    ~summary:
      "The next token ID that has not been allocated. Token IDs are allocated \
       sequentially, so all lower token IDs have been allocated"
    (Cli_lib.Background_daemon.graphql_init (Command.Param.return ())
       ~f:(fun graphql_endpoint () ->
         let%map response =
           Graphql_client.query_exn
             Graphql_queries.Next_available_token.(make @@ makeVariables ())
             graphql_endpoint
         in
         printf "Next available token ID: %s\n"
           (Token_id.to_string response.nextAvailableToken) ) )

let object_lifetime_statistics =
  let open Daemon_rpcs in
  let open Command.Param in
  Command.async ~summary:"Dump internal object lifetime statistics to JSON"
    (Cli_lib.Background_daemon.rpc_init (return ()) ~f:(fun port () ->
         match%map
           Client.dispatch Get_object_lifetime_statistics.rpc () port
         with
         | Ok stats ->
             print_endline stats
         | Error err ->
             printf "Failed to get object lifetime statistics: %s\n%!"
               (Error.to_string_hum err) ) )

let archive_blocks =
  let params =
    let open Command.Let_syntax in
    let%map_open files =
      Command.Param.anon
        Command.Anons.(sequence ("FILES" %: Command.Param.string))
    and success_file =
      Command.Param.flag "--successful-files" ~aliases:[ "successful-files" ]
        ~doc:"PATH Appends the list of files that were processed successfully"
        (Command.Flag.optional Command.Param.string)
    and failure_file =
      Command.Param.flag "--failed-files" ~aliases:[ "failed-files" ]
        ~doc:"PATH Appends the list of files that failed to be processed"
        (Command.Flag.optional Command.Param.string)
    and log_successes =
      Command.Param.flag "--log-successful" ~aliases:[ "log-successful" ]
        ~doc:
          "true/false Whether to log messages for files that were processed \
           successfully"
        (Command.Flag.optional_with_default true Command.Param.bool)
    and archive_process_location = Cli_lib.Flag.Host_and_port.Daemon.archive
    and precomputed_flag =
      Command.Param.flag "--precomputed" ~aliases:[ "precomputed" ] no_arg
        ~doc:"Blocks are in precomputed JSON format"
    and extensional_flag =
      Command.Param.flag "--extensional" ~aliases:[ "extensional" ] no_arg
        ~doc:"Blocks are in extensional JSON format"
    in
    ( files
    , success_file
    , failure_file
    , log_successes
    , archive_process_location
    , precomputed_flag
    , extensional_flag )
  in
  Command.async
    ~summary:
      "Archive a block from a file.\n\n\
       If an archive address is given, this process will communicate with the \
       archive node directly; otherwise it will communicate through the daemon \
       over the rest-server"
    (Cli_lib.Background_daemon.graphql_init params
       ~f:(fun
            graphql_endpoint
            ( files
            , success_file
            , failure_file
            , log_successes
            , archive_process_location
            , precomputed_flag
            , extensional_flag )
          ->
         if Bool.equal precomputed_flag extensional_flag then
           failwith
             "Must provide exactly one of -precomputed and -extensional flags" ;
         let make_send_block ~graphql_make ~archive_dispatch block =
           match archive_process_location with
           | Some archive_process_location ->
               (* Connect directly to the archive node. *)
               archive_dispatch archive_process_location block
           | None ->
               (* Send the requests over GraphQL. *)
               (* let block = block_to_yojson block |> Yojson.Safe.to_basic in *)
               let%map.Deferred.Or_error _res =
                 (* Don't catch this error: [query_exn] already handles
                    printing etc.
                 *)
                 Graphql_client.query (graphql_make block) graphql_endpoint
                 |> Deferred.Result.map_error ~f:(function
                      | `Failed_request e ->
                          Error.create "Unable to connect to Mina daemon" ()
                            (fun () ->
                              Sexp.List
                                [ List
                                    [ Atom "uri"
                                    ; Atom
                                        (Uri.to_string graphql_endpoint.value)
                                    ]
                                ; List
                                    [ Atom "uri_flag"
                                    ; Atom graphql_endpoint.name
                                    ]
                                ; List [ Atom "error_message"; Atom e ]
                                ] )
                      | `Graphql_error e ->
                          Error.createf "GraphQL error: %s" e )
               in
               ()
         in
         let output_file_line path =
           match path with
           | Some path ->
               let file = Out_channel.create ~append:true path in
               fun line -> Out_channel.output_lines file [ line ]
           | None ->
               fun _line -> ()
         in
         let add_to_success_file = output_file_line success_file in
         let add_to_failure_file = output_file_line failure_file in
         let send_precomputed_block =
           make_send_block
             ~graphql_make:(fun block ->
               Graphql_queries.Archive_precomputed_block.(
                 make @@ makeVariables ~block ()) )
             ~archive_dispatch:
               Mina_lib.Archive_client.dispatch_precomputed_block
         in
         let send_extensional_block =
           make_send_block
             ~graphql_make:(fun block ->
               Graphql_queries.Archive_extensional_block.(
                 make @@ makeVariables ~block ()) )
             ~archive_dispatch:
               Mina_lib.Archive_client.dispatch_extensional_block
         in
         Deferred.List.iter files ~f:(fun path ->
             match%map
               let%bind.Deferred.Or_error block_json =
                 Or_error.try_with (fun () ->
                     In_channel.with_file path ~f:(fun in_channel ->
                         Yojson.Safe.from_channel in_channel ) )
                 |> Result.map_error ~f:(fun err ->
                        Error.tag_arg err "Could not parse JSON from file" path
                          String.sexp_of_t )
                 |> Deferred.return
               in
               let open Deferred.Or_error.Let_syntax in
               if precomputed_flag then
                 let%bind precomputed_block =
                   Mina_block.Precomputed.of_yojson block_json
                   |> Result.map_error ~f:(fun err ->
                          Error.tag_arg (Error.of_string err)
                            "Could not parse JSON as a precomputed block from \
                             file"
                            path String.sexp_of_t )
                   |> Deferred.return
                 in
                 send_precomputed_block precomputed_block
               else if extensional_flag then
                 let%bind extensional_block =
                   Archive_lib.Extensional.Block.of_yojson block_json
                   |> Result.map_error ~f:(fun err ->
                          Error.tag_arg (Error.of_string err)
                            "Could not parse JSON as an extensional block from \
                             file"
                            path String.sexp_of_t )
                   |> Deferred.return
                 in
                 send_extensional_block extensional_block
               else
                 (* should be unreachable *)
                 failwith
                   "Expected exactly one of precomputed, extensional flags"
             with
             | Ok () ->
                 if log_successes then
                   Format.printf "Sent block to archive node from %s@." path ;
                 add_to_success_file path
             | Error err ->
                 Format.eprintf
                   "Failed to send block to archive node from %s. Error:@.%s@."
                   path (Error.to_string_hum err) ;
                 add_to_failure_file path ) ) )

let receipt_chain_hash =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Compute the next receipt chain hash from the previous hash and \
       transaction ID"
    (let%map_open previous_hash =
       flag "--previous-hash"
         ~doc:"Previous receipt chain hash, base58check encoded"
         (required string)
     and transaction_id =
       flag "--transaction-id" ~doc:"Transaction ID, base58check encoded"
         (required string)
     in
     fun () ->
       let previous_hash =
         Receipt.Chain_hash.of_base58_check_exn previous_hash
       in
       (* What we call transaction IDs in GraphQL are just base58_check-encoded
          transactions. It's easy to handle, and we return it from the
          transaction commands above, so lets use this format.
       *)
       let transaction = Signed_command.of_base58_check_exn transaction_id in
       let hash =
         Receipt.Chain_hash.cons (Signed_command transaction.payload)
           previous_hash
       in
       printf "%s\n" (Receipt.Chain_hash.to_base58_check hash) )

let chain_id_inputs =
  let open Deferred.Let_syntax in
  Command.async ~summary:"Print the inputs that yield the current chain id"
    (Cli_lib.Background_daemon.rpc_init (Command.Param.all_unit [])
       ~f:(fun port () ->
         let open Daemon_rpcs in
         match%map Client.dispatch Chain_id_inputs.rpc () port with
         | Ok (genesis_state_hash, genesis_constants, snark_keys) ->
             let open Format in
             printf "Genesis state hash: %s@."
               (State_hash.to_base58_check genesis_state_hash) ;
             printf "Genesis_constants:@." ;
             printf "  Protocol:          %s@."
               ( Genesis_constants.Protocol.to_yojson genesis_constants.protocol
               |> Yojson.Safe.to_string ) ;
             printf "  Txn pool max size: %d@."
               genesis_constants.txpool_max_size ;
             printf "  Num accounts:      %s@."
               ( match genesis_constants.num_accounts with
               | Some n ->
                   Int.to_string n
               | None ->
                   "None" ) ;
             printf "Snark keys:@." ;
             List.iter snark_keys ~f:(printf "  %s@.")
         | Error err ->
             Format.eprintf "Could not get chain id inputs: %s@."
               (Error.to_string_hum err) ) )

let hash_transaction =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Compute the hash of a transaction from its transaction ID"
    (let%map_open transaction =
       flag "--transaction-id" ~doc:"ID ID of the transaction to hash"
         (required string)
     in
     fun () ->
       let signed_command =
         Signed_command.of_base58_check transaction |> Or_error.ok_exn
       in
       let hash =
         Transaction_hash.hash_command (Signed_command signed_command)
       in
       printf "%s\n" (Transaction_hash.to_base58_check hash) )

let humanize_graphql_error
    ~(graphql_endpoint : Uri.t Cli_lib.Flag.Types.with_name) = function
  | `Failed_request e ->
      Error.create "Unable to connect to Mina daemon" () (fun () ->
          Sexp.List
            [ List [ Atom "uri"; Atom (Uri.to_string graphql_endpoint.value) ]
            ; List [ Atom "uri_flag"; Atom graphql_endpoint.name ]
            ; List [ Atom "error_message"; Atom e ]
            ] )
  | `Graphql_error e ->
      Error.createf "GraphQL error: %s" e

let runtime_config =
  Command.async
    ~summary:"Compute the runtime configuration used by a running daemon"
    (Cli_lib.Background_daemon.graphql_init (Command.Param.return ())
       ~f:(fun graphql_endpoint () ->
         match%bind
           Graphql_client.query
             Graphql_queries.Runtime_config.(make @@ makeVariables ())
             graphql_endpoint
         with
         | Ok runtime_config ->
             Format.printf "%s@."
               (Yojson.Basic.pretty_to_string runtime_config.runtimeConfig) ;
             return ()
         | Error err ->
             Format.eprintf
               "Failed to retrieve runtime configuration. Error:@.%s@."
               (Error.to_string_hum
                  (humanize_graphql_error ~graphql_endpoint err) ) ;
             exit 1 ) )

let thread_graph =
  Command.async
    ~summary:
      "Return a Graphviz Dot graph representation of the internal thread \
       hierarchy"
    (Cli_lib.Background_daemon.graphql_init (Command.Param.return ())
       ~f:(fun graphql_endpoint () ->
         match%bind
           Graphql_client.query
             Graphql_queries.Thread_graph.(make @@ makeVariables ())
             graphql_endpoint
         with
         | Ok graph ->
             print_endline graph.threadGraph ;
             return ()
         | Error e ->
             Format.eprintf
               "Failed to retrieve runtime configuration. Error:@.%s@."
               (Error.to_string_hum
                  (humanize_graphql_error ~graphql_endpoint e) ) ;
             exit 1 ) )

module Visualization = struct
  let create_command (type rpc_response) ~name ~f
      (rpc : (string, rpc_response) Rpc.Rpc.t) =
    let open Deferred.Let_syntax in
    Command.async
      ~summary:(sprintf !"Produce a visualization of the %s" name)
      (Cli_lib.Background_daemon.rpc_init
         Command.Param.(anon @@ ("output-filepath" %: string))
         ~f:(fun port filename ->
           let%map message =
             match%map Daemon_rpcs.Client.dispatch rpc filename port with
             | Ok response ->
                 f filename response
             | Error e ->
                 sprintf "Could not save file: %s\n" (Error.to_string_hum e)
           in
           print_string message ) )

  module Frontier = struct
    let name = "transition-frontier"

    let command =
      create_command ~name Daemon_rpcs.Visualization.Frontier.rpc
        ~f:(fun filename -> function
        | `Active () ->
            Visualization_message.success name filename
        | `Bootstrapping ->
            Visualization_message.bootstrap name )
  end

  module Registered_masks = struct
    let name = "registered-masks"

    let command =
      create_command ~name Daemon_rpcs.Visualization.Registered_masks.rpc
        ~f:(fun filename () -> Visualization_message.success name filename)
  end

  let command_group =
    Command.group ~summary:"Visualize data structures special to Mina"
      [ (Frontier.name, Frontier.command)
      ; (Registered_masks.name, Registered_masks.command)
      ]
end

let accounts =
  Command.group ~summary:"Client commands concerning account management"
    ~preserve_subcommand_order:()
    [ ("list", list_accounts)
    ; ("create", create_account)
    ; ("import", import_key)
    ; ("export", export_key)
    ; ("unlock", unlock_account)
    ; ("lock", lock_account)
    ]

let client =
  Command.group ~summary:"Lightweight client commands"
    ~preserve_subcommand_order:()
    [ ("get-balance", get_balance_graphql)
    ; ("get-tokens", get_tokens_graphql)
    ; ("send-payment", send_payment_graphql)
    ; ("delegate-stake", delegate_stake_graphql)
    ; ("create-token", create_new_token_graphql)
    ; ("create-token-account", create_new_account_graphql)
    ; ("mint-tokens", mint_tokens_graphql)
    ; ("cancel-transaction", cancel_transaction_graphql)
    ; ("set-snark-worker", set_snark_worker)
    ; ("set-snark-work-fee", set_snark_work_fee)
    ; ("export-logs", Export_logs.export_via_graphql)
    ; ("export-local-logs", Export_logs.export_locally)
    ; ("stop-daemon", stop_daemon)
    ; ("status", status)
    ]

let client_trustlist_group =
  Command.group ~summary:"Client trustlist management"
    ~preserve_subcommand_order:()
    [ ("add", trustlist_add)
    ; ("list", trustlist_list)
    ; ("remove", trustlist_remove)
    ]

let advanced =
  Command.group ~summary:"Advanced client commands"
    [ ("get-nonce", get_nonce_cmd)
    ; ("client-trustlist", client_trustlist_group)
    ; ("get-trust-status", get_trust_status)
    ; ("get-trust-status-all", get_trust_status_all)
    ; ("get-public-keys", get_public_keys)
    ; ("reset-trust-status", reset_trust_status)
    ; ("batch-send-payments", batch_send_payments)
    ; ("status-clear-hist", status_clear_hist)
    ; ("wrap-key", wrap_key)
    ; ("dump-keypair", dump_keypair)
    ; ("constraint-system-digests", constraint_system_digests)
    ; ("start-tracing", start_tracing)
    ; ("stop-tracing", stop_tracing)
    ; ("snark-job-list", snark_job_list)
    ; ("pooled-user-commands", pooled_user_commands)
    ; ("snark-pool-list", snark_pool_list)
    ; ("pending-snark-work", pending_snark_work)
    ; ("generate-libp2p-keypair", generate_libp2p_keypair)
    ; ("compile-time-constants", compile_time_constants)
    ; ("node-status", node_status)
    ; ("visualization", Visualization.command_group)
    ; ("verify-receipt", verify_receipt)
    ; ("generate-keypair", Cli_lib.Commands.generate_keypair)
    ; ("validate-keypair", Cli_lib.Commands.validate_keypair)
    ; ("validate-transaction", Cli_lib.Commands.validate_transaction)
    ; ("send-rosetta-transactions", send_rosetta_transactions_graphql)
    ; ("next-available-token", next_available_token_cmd)
    ; ("time-offset", get_time_offset_graphql)
    ; ("get-peers", get_peers_graphql)
    ; ("add-peers", add_peers_graphql)
    ; ("object-lifetime-statistics", object_lifetime_statistics)
    ; ("archive-blocks", archive_blocks)
    ; ("compute-receipt-chain-hash", receipt_chain_hash)
    ; ("hash-transaction", hash_transaction)
    ; ("set-coinbase-receiver", set_coinbase_receiver_graphql)
    ; ("chain-id-inputs", chain_id_inputs)
    ; ("runtime-config", runtime_config)
    ; ("vrf", Cli_lib.Commands.Vrf.command_group)
    ; ("thread-graph", thread_graph)
    ]

let ledger =
  Command.group ~summary:"Ledger commands"
    [ ("export", export_ledger)
    ; ("hash", hash_ledger)
    ; ("currency", currency_in_ledger)
    ]
