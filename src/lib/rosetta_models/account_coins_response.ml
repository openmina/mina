(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 * Schema Account_coins_response.t : AccountCoinsResponse is returned on the /account/coins endpoint and includes all unspent Coins owned by an AccountIdentifier.
 *)

type t =
  { block_identifier: Block_identifier.t
  ; (* If a blockchain is UTXO-based, all unspent Coins owned by an account_identifier should be returned alongside the balance. It is highly recommended to populate this field so that users of the Rosetta API implementation don't need to maintain their own indexer to track their UTXOs. *)
    coins: Coin.t list
  ; (* Account-based blockchains that utilize a nonce or sequence number should include that number in the metadata. This number could be unique to the identifier or global across the account address. *)
    metadata: Yojson.Safe.t option [@default None] }
[@@deriving yojson {strict= false}, show]

(** AccountCoinsResponse is returned on the /account/coins endpoint and includes all unspent Coins owned by an AccountIdentifier. *)
let create (block_identifier : Block_identifier.t) (coins : Coin.t list) : t =
  {block_identifier; coins; metadata= None}
