open Core
open Async
open Integration_test_lib
open Mina_base

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs
  open Engine
  open Dsl

  (* TODO: find a way to avoid this type alias (first class module signatures restrictions make this tricky) *)
  type network = Network.t

  type node = Network.Node.t

  type dsl = Dsl.t

  (* TODO: refactor all currency values to decimal represenation *)
  (* TODO: test account creation fee *)
  (* TODO: test snark work *)
  let config =
    let open Test_config in
    let open Test_config.Wallet in
    let make_timing ~min_balance ~cliff_time ~cliff_amount ~vesting_period
        ~vesting_increment : Mina_base.Account_timing.t =
      let open Currency in
      Timed
        { initial_minimum_balance = Balance.of_int min_balance
        ; cliff_time = Mina_numbers.Global_slot.of_int cliff_time
        ; cliff_amount = Amount.of_int cliff_amount
        ; vesting_period = Mina_numbers.Global_slot.of_int vesting_period
        ; vesting_increment = Amount.of_int vesting_increment
        }
    in
    { default with
      requires_graphql = true
    ; block_producers =
        [ { balance = "400000"; timing = Untimed } (* 400_000_000_000_000 *)
        ; { balance = "300000"; timing = Untimed } (* 300_000_000_000_000 *)
        ; { balance = "30000"
          ; timing =
              make_timing ~min_balance:10_000_000_000_000 ~cliff_time:8
                ~cliff_amount:0 ~vesting_period:4
                ~vesting_increment:5_000_000_000_000
          }
          (* 30_000_000_000_000 mina is the total. initially, the balance will be 10k mina. after 8 global slots, the cliff is hit, although the cliff amount is 0. 4 slots after that, 5_000_000_000_000 mina will vest, and 4 slots after that another 5_000_000_000_000 will vest, and then twice again, for a total of 30k mina all fully liquid and unlocked at the end of the schedule*)
        ]
    ; extra_genesis_accounts =
        [ { balance = "1000"; timing = Untimed }
        ; { balance = "1000"; timing = Untimed }
        ; { balance = "1000"
          ; timing =
              make_timing ~min_balance:10_000_000_000_000 ~cliff_time:8
                ~cliff_amount:0 ~vesting_period:4
                ~vesting_increment:5_000_000_000_000
          }
        ]
    ; num_snark_workers =
        3
        (* this test doesn't need snark workers, however turning it on in this test just to make sure the snark workers function within integration tests *)
    }

  let run network t =
    let open Network in
    let open Malleable_error.Let_syntax in
    let logger = Logger.create () in
    let all_nodes = Network.all_nodes network in
    let%bind () = wait_for t (Wait_condition.nodes_to_initialize all_nodes) in
    let[@warning "-8"] [ untimed_node_a; untimed_node_b; timed_node_c ] =
      Network.block_producers network
    in
    [%log info] "extra genesis keypairs: %s"
      (List.to_string (Network.extra_genesis_keypairs network)
         ~f:(fun { Signature_lib.Keypair.public_key; _ } ->
           public_key |> Signature_lib.Public_key.to_bigstring
           |> Bigstring.to_string)) ;
    let[@warning "-8"] [ fish1; fish2; fish3_timed ] =
      Network.extra_genesis_keypairs network
    in
    (* create a signed txn which we'll use to make a successfull txn, and then a replay attack *)
    let amount = Currency.Amount.of_int 100_000_000_000 in
    let fee = Currency.Fee.of_int 1_000_000_000 in
    let test_constants = Engine.Network.constraint_constants network in
    let receiver_pub_key =
      fish1.public_key |> Signature_lib.Public_key.compress
    in
    let sender_kp = fish2 in
    let sender_pub_key =
      sender_kp.public_key |> Signature_lib.Public_key.compress
    in
    (* hardcoded copy of extra_genesis_accounts[0] and extra_genesis_accounts[1], update here if they change *)
    let receiver_original_balance = Currency.Amount.of_string "1000" in
    let sender_original_balance = Currency.Amount.of_string "1000" in
    let txn_body =
      Signed_command_payload.Body.Payment
        { source_pk = sender_pub_key
        ; receiver_pk = receiver_pub_key
        ; token_id = Token_id.default
        ; amount
        }
    in
    let%bind { nonce = sender_current_nonce; _ } =
      Network.Node.must_get_account_data ~logger untimed_node_b
        ~public_key:sender_pub_key
    in
    let user_command_input =
      User_command_input.create ~fee ~nonce:sender_current_nonce
        ~fee_token:(Signed_command_payload.Body.token txn_body)
        ~fee_payer_pk:sender_pub_key ~valid_until:None
        ~memo:(Signed_command_memo.create_from_string_exn "")
        ~body:txn_body ~signer:sender_pub_key
        ~sign_choice:(User_command_input.Sign_choice.Keypair sender_kp) ()
    in
    [%log info] "user_command_input: $user_command"
      ~metadata:
        [ ( "user_command"
          , User_command_input.Stable.Latest.to_yojson user_command_input )
        ] ;
    let%bind txn_signed =
      User_command_input.to_user_command
        ~get_current_nonce:(fun _ ->
          failwith "get_current_nonce, don't call me")
        ~nonce_map:
          (Account_id.Map.of_alist_exn
             [ ( Account_id.create sender_pub_key
                   (Signed_command_payload.Body.token txn_body)
               , (sender_current_nonce, sender_current_nonce) )
             ])
        ~get_account:(fun _ -> `Bootstrapping)
        ~constraint_constants:test_constants ~logger user_command_input
      |> Deferred.bind ~f:Malleable_error.or_hard_error
    in
    let (signed_cmmd, _)
          : Signed_command.t
            * (Unsigned.uint32 * Unsigned.uint32) Account_id.Map.t =
      txn_signed
    in
    (* setup complete *)
    let%bind () =
      section "send a single payment between 2 untimed accounts"
        (let%bind { hash; _ } =
           Network.Node.must_send_payment_with_raw_sig untimed_node_b ~logger
             ~sender_pub_key:
               (Signed_command_payload.Body.source_pk signed_cmmd.payload.body)
             ~receiver_pub_key:
               (Signed_command_payload.Body.receiver_pk
                  signed_cmmd.payload.body)
             ~amount:
               ( Signed_command_payload.amount signed_cmmd.payload
               |> Option.value_exn )
             ~fee:(Signed_command_payload.fee signed_cmmd.payload)
             ~nonce:signed_cmmd.payload.common.nonce
             ~memo:
               (Signed_command_memo.to_raw_bytes_exn
                  signed_cmmd.payload.common.memo)
             ~token:(Signed_command_payload.token signed_cmmd.payload)
             ~valid_until:signed_cmmd.payload.common.valid_until
             ~raw_signature:
               (Mina_base.Signature.Raw.encode signed_cmmd.signature)
         in
         wait_for t
           (Wait_condition.signed_command_to_be_included_in_frontier
              ~txn_hash:hash ~node_included_in:(`Node untimed_node_b)))
    in
    let%bind () =
      section
        "check that the account balances are what we expect after the previous \
         txn"
        (let%bind { total_balance = receiver_balance; _ } =
           Network.Node.must_get_account_data ~logger untimed_node_b
             ~public_key:receiver_pub_key
         in
         let%bind { total_balance = sender_balance; _ } =
           Network.Node.must_get_account_data ~logger untimed_node_b
             ~public_key:sender_pub_key
         in
         (* let node_a_num_produced_blocks =
              Map.find (network_state t).blocks_produced_by_node
                (Network.Node.id untimed_node_a)
              |> Option.value ~default:[] |> List.length
            in
            let node_b_num_produced_blocks =
              Map.find (network_state t).blocks_produced_by_node
                (Network.Node.id untimed_node_b)
              |> Option.value ~default:[] |> List.length
            in
            let coinbase_reward = Currency.Amount.of_int 720_000_000_000 in *)
         (* TODO, the intg test framework is ignoring test_constants.coinbase_amount for whatever reason, so hardcoding this until that is fixed *)
         let receiver_expected =
           Currency.Amount.add receiver_original_balance amount
           |> Option.value_exn
         in
         let sender_expected =
           Currency.Amount.sub sender_original_balance amount
           (* ( Currency.Amount.add amount (Currency.Amount.of_fee fee)
              |> Option.value_exn ) *)
           (* TODO: put the fee back in *)
           |> Option.value_exn
         in
         (* [%log info] "coinbase_amount: %s"
            (Currency.Amount.to_formatted_string coinbase_reward) ; *)
         [%log info] "txn_amount: %s"
           (Currency.Amount.to_formatted_string amount) ;
         [%log info] "receiver_expected: %s"
           (Currency.Amount.to_formatted_string receiver_expected) ;
         [%log info] "receiver_balance: %s"
           (Currency.Balance.to_formatted_string receiver_balance) ;
         [%log info] "sender_expected: %s"
           (Currency.Amount.to_formatted_string sender_expected) ;
         [%log info] "sender_balance: %s"
           (Currency.Balance.to_formatted_string sender_balance) ;
         if
           (* node_a is the receiver *)
           (* node_a_balance >= 400_000_000_000_000 + txn_amount *)
           (* coinbase_amount is much less than txn_amount, so that even if node_a receives a coinbase, the balance (before receiving currency from a txn) should be less than original_amount + txn_amount *)
           Currency.Amount.( >= )
             (Currency.Balance.to_amount receiver_balance)
             receiver_expected
           (* node_b is the sender *)
           (* node_b_balance <= (300_000_000_000_000 + node_b_num_produced_blocks*possible_coinbase_reward*2) - (txn_amount + txn_fee) *)
           (* if one is unlucky, node_b could theoretically win a bunch of blocks in a row, which is why we have the `node_b_num_produced_blocks*possible_coinbase_reward*2` bit.  the *2 is because untimed accounts get supercharged rewards *)
           (* TODO, the fee is not calculated in at the moment *)
           && Currency.Amount.( <= )
                (Currency.Balance.to_amount sender_balance)
                sender_expected
         then Malleable_error.return ()
         else
           Malleable_error.soft_error_format ~value:()
             "Error with account balances.  receiver balance is %d and should \
              be %d, sender balance is %d and should be %d.  and txn_amount is \
              %d"
             (Currency.Balance.to_int receiver_balance)
             (Currency.Amount.to_int receiver_expected)
             (Currency.Balance.to_int sender_balance)
             (Currency.Amount.to_int sender_expected)
             (Currency.Amount.to_int amount))
    in
    let%bind () =
      section
        "attempt to send again the same signed transaction command as before \
         to conduct a replay attack. expecting a bad nonce"
        (let open Deferred.Let_syntax in
        match%bind
          Network.Node.send_payment_with_raw_sig untimed_node_b ~logger
            ~sender_pub_key:
              (Signed_command_payload.Body.source_pk signed_cmmd.payload.body)
            ~receiver_pub_key:
              (Signed_command_payload.Body.receiver_pk signed_cmmd.payload.body)
            ~amount:
              ( Signed_command_payload.amount signed_cmmd.payload
              |> Option.value_exn )
            ~fee:(Signed_command_payload.fee signed_cmmd.payload)
            ~nonce:signed_cmmd.payload.common.nonce
            ~memo:
              (Signed_command_memo.to_raw_bytes_exn
                 signed_cmmd.payload.common.memo)
            ~token:(Signed_command_payload.token signed_cmmd.payload)
            ~valid_until:signed_cmmd.payload.common.valid_until
            ~raw_signature:
              (Mina_base.Signature.Raw.encode signed_cmmd.signature)
        with
        | Ok { nonce; _ } ->
            Malleable_error.soft_error_format ~value:()
              "Replay attack succeeded, but it should fail because the nonce \
               is old.  attempted nonce: %d"
              (Unsigned.UInt32.to_int nonce)
        | Error error ->
            (* expect GraphQL error due to bad nonce *)
            let err_str = Error.to_string_mach error in
            let err_str_lowercase = String.lowercase err_str in
            if
              String.is_substring
                ~substring:"either different from inferred nonce"
                err_str_lowercase
            then (
              [%log info] "Got expected bad nonce error from GraphQL" ;
              Malleable_error.return () )
            else (
              [%log error]
                "Payment failed in GraphQL, but for unexpected reason: %s"
                err_str ;
              Malleable_error.soft_error_format ~value:()
                "Payment failed for unexpected reason: %s" err_str ))
    in
    let%bind () =
      section
        "attempt to send again the same signed transaction command as before, \
         but changing the nonce, to conduct a replay attack.  expecting an \
         Invalid_signature"
        (let open Deferred.Let_syntax in
        match%bind
          Network.Node.send_payment_with_raw_sig untimed_node_a ~logger
            ~sender_pub_key:
              (Signed_command_payload.Body.source_pk signed_cmmd.payload.body)
            ~receiver_pub_key:
              (Signed_command_payload.Body.receiver_pk signed_cmmd.payload.body)
            ~amount:
              ( Signed_command_payload.amount signed_cmmd.payload
              |> Option.value_exn )
            ~fee:(Signed_command_payload.fee signed_cmmd.payload)
            ~nonce:
              (Mina_numbers.Account_nonce.succ signed_cmmd.payload.common.nonce)
            ~memo:
              (Signed_command_memo.to_raw_bytes_exn
                 signed_cmmd.payload.common.memo)
            ~token:(Signed_command_payload.token signed_cmmd.payload)
            ~valid_until:signed_cmmd.payload.common.valid_until
            ~raw_signature:
              (Mina_base.Signature.Raw.encode signed_cmmd.signature)
        with
        | Ok { nonce; _ } ->
            Malleable_error.soft_error_format ~value:()
              "Replay attack succeeded, but it should fail because the \
               signature is wrong.  attempted nonce: %d"
              (Unsigned.UInt32.to_int nonce)
        | Error error ->
            (* expect GraphQL error due to invalid signature *)
            let err_str = Error.to_string_mach error in
            let err_str_lowercase = String.lowercase err_str in
            if
              String.is_substring ~substring:"invalid_signature"
                err_str_lowercase
            then (
              [%log info] "Got expected invalid signature error from GraphQL" ;
              Malleable_error.return () )
            else (
              [%log error]
                "Payment failed in GraphQL, but for unexpected reason: %s"
                err_str ;
              Malleable_error.soft_error_format ~value:()
                "Payment failed for unexpected reason: %s" err_str ))
    in
    let%bind () =
      section "send a single payment from timed account using available liquid"
        (let amount = Currency.Amount.of_int 1_000_000_000_000 in
         let receiver = fish1 in
         let receiver_pub_key =
           receiver.public_key |> Signature_lib.Public_key.compress
         in
         let sender = fish3_timed in
         let sender_pub_key =
           sender.public_key |> Signature_lib.Public_key.compress
         in
         let%bind { hash; _ } =
           Network.Node.must_send_payment ~logger timed_node_c ~sender_pub_key
             ~receiver_pub_key ~amount ~fee
         in
         wait_for t
           (Wait_condition.signed_command_to_be_included_in_frontier
              ~txn_hash:hash ~node_included_in:(`Node timed_node_c)))
    in
    section "unable to send payment from timed account using illiquid tokens"
      (let amount = Currency.Amount.of_int 25_000_000_000_000 in
       let receiver = untimed_node_b in
       let%bind receiver_pub_key = Util.pub_key_of_node receiver in
       let sender = timed_node_c in
       let%bind sender_pub_key = Util.pub_key_of_node sender in
       let%bind { total_balance = timed_node_c_total; _ } =
         Network.Node.must_get_account_data ~logger timed_node_c
           ~public_key:sender_pub_key
       in
       [%log info] "timed_node_c total balance: %s"
         (Currency.Balance.to_formatted_string timed_node_c_total) ;
       [%log info]
         "Attempting to send txn from timed_node_c to untimed_node_a for \
          amount of %s"
         (Currency.Amount.to_formatted_string amount) ;
       (* TODO: refactor this using new [expect] dsl when it's available *)
       let open Deferred.Let_syntax in
       match%bind
         Node.send_payment ~logger sender ~sender_pub_key ~receiver_pub_key
           ~amount ~fee
       with
       | Ok _ ->
           Malleable_error.soft_error_string ~value:()
             "Payment succeeded, but expected it to fail because of a minimum \
              balance violation"
       | Error error ->
           (* expect GraphQL error due to insufficient funds *)
           let err_str = Error.to_string_mach error in
           let err_str_lowercase = String.lowercase err_str in
           if
             String.is_substring ~substring:"insufficient_funds"
               err_str_lowercase
           then (
             [%log info] "Got expected insufficient funds error from GraphQL" ;
             Malleable_error.return () )
           else (
             [%log error]
               "Payment failed in GraphQL, but for unexpected reason: %s"
               err_str ;
             Malleable_error.soft_error_format ~value:()
               "Payment failed for unexpected reason: %s" err_str ))
end
