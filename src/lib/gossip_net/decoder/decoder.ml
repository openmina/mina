(* open Core *)
(* open Mina_block *)
(* open Network_pool *)
(* open Gossip_net *)

(* let decode msg ~bin_prot = *)
(*   let b = Bigstring.of_string msg in *)
(*   Bigstring.read_bin_prot b bin_prot.Bin_prot.Type_class.reader *)
(*   |> is_ok *)

(* let decode_external_transition msg = *)
(*   decode msg ~bin_prot:External_transition.Raw.Stable.V1.bin_t *)

(* let decode_snark_pool_diff msg = *)
(*   decode msg ~bin_prot:Snark_pool.Diff_versioned.Stable.V1.bin_t *)

(* let decode_tx_pool_diff msg = *)
(*   decode msg ~bin_prot:Transaction_pool.Diff_versioned.Stable.V1.bin_t *)

exception Underflow of int

let main () =
  let bytes_len = 64 * 1024 in
  let bytes = Bytes.make bytes_len (char_of_int 0) in
  let nl = Str.regexp "\n" in
  let rec read buf ~pos ~len =
    Out_channel.flush stderr ;
    let read_len = min len bytes_len in
    if read_len == 0 then ()
    else if Option.is_none (In_channel.really_input stdin bytes 0 read_len) then
      raise (Underflow pos)
    else
      let _foo =
        Memcpy.(
          memcpy_from_bytes
            (bigarray Ctypes.array1
               (Bigarray.Array1.size_in_bytes buf)
               Bigarray.Char )
            ~src:(Bytes.sub bytes 0 read_len)
            ~dst:buf ~dst_off:pos)
      in
      read buf ~pos:(pos + read_len) ~len:(len - read_len)
  in
  while true do
    try
      let _tx_pool_diff =
        Bin_prot.Utils.bin_read_stream ~read
          Mina_networking.Gossip_net.Message.Latest.T.bin_reader_msg
      in
      Printf.printf "ok\n" ; Out_channel.flush stdout
    with
    | Underflow 0 ->
        exit 0
    | Underflow p ->
        Printf.printf "error: underflow at pos %d\n" p ;
        exit 1
    | Invalid_argument f ->
        Printf.printf "error: invalid argument %s\n" f ;
        exit 1
    | e ->
        let e = Str.global_replace nl "\\n" (Printexc.to_string e) in
        Printf.printf "fail: %s\n" e ;
        Out_channel.flush stdout
  done

let () = main ()
