open Core
open Mina_block
open Network_pool

let decode msg ~bin_prot =
  let b = Bigstring.of_string msg in
  Bigstring.read_bin_prot b bin_prot.Bin_prot.Type_class.reader
  |> is_ok

(* let decode_gossip_message msg = decode msg ~bin_prot:Message.Latest.T.bin_msg *)

let decode_external_transition msg =
  decode msg ~bin_prot:External_transition.Raw.Stable.V1.bin_t

let decode_snark_pool_diff msg =
  decode msg ~bin_prot:Snark_pool.Diff_versioned.Stable.V1.bin_t

let decode_tx_pool_diff msg =
  decode msg ~bin_prot:Transaction_pool.Diff_versioned.Stable.V1.bin_t

let foo msg =
  Stdlib.Printf.printf "=== importand message from OCaml %s !!!" msg

let () =
  (* Callback.register "decode_gossip_message" decode_gossip_message; *)
  Callback.register "foo" foo;
  Callback.register "decode_external_transition" decode_external_transition;
  Callback.register "decode_snark_pool_diff" decode_snark_pool_diff;
  Callback.register "decode_tx_pool_diff" decode_tx_pool_diff
