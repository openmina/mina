(* rocksdb.ml -- expose RocksDB operations for Coda *)

open Core

module Bigstring_frozen = struct
  module T = struct
    include Bigstring.Stable.V1

    (* we're not mutating Bigstrings, which would invalidate hashes
       OK to use these hash functions
    *)
    let hash = hash_t_frozen

    let hash_fold_t = hash_fold_t_frozen
  end

  include T
  include Hashable.Make_binable (T)
end

type t =
  { uuid : Uuid.Stable.V1.t
  ; table : Bigstring_frozen.t Bigstring_frozen.Table.t
  }
[@@deriving sexp]

let to_alist t =
  let unsorted = Bigstring_frozen.Table.to_alist t.table in
  (* sort by key *)
  List.sort
    ~compare:(fun (k1, _) (k2, _) -> Bigstring_frozen.compare k1 k2)
    unsorted

let get_uuid t = t.uuid

let create _ =
  { uuid = Uuid_unix.create (); table = Bigstring_frozen.Table.create () }

let create_checkpoint t _ =
  { uuid = Uuid_unix.create ()
  ; table =
      Bigstring_frozen.Table.of_alist_exn
      @@ Bigstring_frozen.Table.to_alist t.table
  }

let close _ = ()

let get t ~key = Bigstring_frozen.Table.find t.table key

let get_batch t ~keys = List.map keys ~f:(Bigstring_frozen.Table.find t.table)

let set t ~key ~data = Bigstring_frozen.Table.set t.table ~key ~data

let set_batch t ?(remove_keys = []) ~key_data_pairs =
  List.iter key_data_pairs ~f:(fun (key, data) -> set t ~key ~data) ;
  List.iter remove_keys ~f:(fun key ->
      Bigstring_frozen.Table.remove t.table key )

let remove t ~key = Bigstring_frozen.Table.remove t.table key

let make_checkpoint _ _ = ()

module Batch = struct
  type t = unit

  let remove t ~key = ignore t ; ignore key ; failwith "unimplemented"

  let set t ~key ~data =
    ignore t ; ignore key ; ignore data ; failwith "unimplemented"

  let with_batch t ~f = ignore t ; f ; failwith "unimplemented"
end
