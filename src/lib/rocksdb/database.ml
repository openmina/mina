(* rocksdb.ml -- expose RocksDB operations for Coda *)

open Core

type t = { uuid : Uuid.Stable.V1.t; db : (unit[@sexp.opaque]) }
[@@deriving sexp]

let create _directory = failwith "unimplemented"

let create_checkpoint _t _dir = failwith "unimplemented"

let make_checkpoint _t _dir = failwith "unimplemented"

let get_uuid t = t.uuid

let close _t = failwith "unimplemented"

let get _t ~(key : Bigstring.t) : Bigstring.t option =
  ignore key ; failwith "unimplemented"

let get_batch _t ~(keys : Bigstring.t list) : Bigstring.t option list =
  ignore keys ; failwith "unimplemented"

let set _t ~(key : Bigstring.t) ~(data : Bigstring.t) : unit =
  ignore key ; ignore data ; failwith "unimplemented"

let set_batch _t ?(remove_keys = [])
    ~(key_data_pairs : (Bigstring.t * Bigstring.t) list) : unit =
  ignore remove_keys ; ignore key_data_pairs ; failwith "unimplemented"

module Batch = struct
  type t = unit

  let remove _t ~key = ignore key ; failwith "unimplemented"

  let set _t ~key ~data = ignore key ; ignore data ; failwith "unimplemented"

  let with_batch _t ~f = ignore f ; failwith "unimplemented"
end

let copy _t = failwith "copy: not implemented"

let remove _t ~(key : Bigstring.t) : unit =
  ignore key ; failwith "unimplemented"

let to_alist _t : (Bigstring.t * Bigstring.t) list = failwith "unimplemented"

let to_bigstring = Bigstring.of_string
