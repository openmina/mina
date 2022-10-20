open Core

let flatten_yojson_variant f v =
  match f v with `List [ tag ] -> tag | _ -> assert false

module Operation = struct
  type persistent =
    [ `Crawl_successors_in_db
    | `Get_transition_in_db
    | `Get_best_tip_in_db
    | `Get_protocol_states_for_root_scan_state_in_db
    | `Get_root_hash_in_db
    | `Get_root_in_db
    | `Get_arcs_in_db
    | `Add_transition_in_db
    | `Move_root_in_db
    | `Set_best_tip_in_db ]
  [@@deriving to_yojson, enumerate, equal, hash, sexp_of, compare]

  let persistent_to_yojson = flatten_yojson_variant persistent_to_yojson

  (* TODO: trace get_or_create_account case where a new account needs to
     be created using `Create_new_account *)
  type ledger =
    [ `Create_new_account
    | `Remove_accounts
    | `Set_account
    | `Get_account
    | `Set_accounts_batch
    | `Get_accounts_batch
    | `Merkle_path
    | `Commit_mask
    | `Copy_mask
    | `Hash_account
    | `Hash_merge
    | `Merkle_root
    | `Merkle_path_at_addr
    | `Merkle_path_at_index
    | `Get_or_create_account ]
  [@@deriving to_yojson, enumerate, equal, hash, sexp_of, compare]

  let ledger_to_yojson = flatten_yojson_variant ledger_to_yojson

  type t = [ persistent | ledger ]
  [@@deriving to_yojson, enumerate, equal, hash, sexp_of, compare]

  let to_string (c : t) =
    match to_yojson c with `String name -> name | _ -> assert false
end

module Distributions = struct
  type range_info =
    { mutable count : int
    ; mutable mean_time : float [@key "meanTime"]
    ; mutable max_time : float [@key "maxTime"]
    ; mutable total_time : float [@key "totalTime"]
    }
  [@@deriving to_yojson]

  type t =
    { operation : Operation.t
    ; mutable count : int
    ; mutable total_time : float [@key "totalTime"]
    ; one_to_ten_us : range_info [@key "oneToTenUs"]
    ; ten_to_one_hundred_us : range_info [@key "tenToOneHundredUs"]
    ; one_hundred_us_to_one_ms : range_info [@key "oneHundredUsToOneMs"]
    ; one_to_ten_ms : range_info [@key "oneToTenMs"]
    ; ten_to_one_hundred_ms : range_info [@key "tenToOneHundredMs"]
    ; one_hundred_ms_to_one_s : range_info [@key "oneHundredMsToOneS"]
    ; one_to_ten_s : range_info [@key "oneToTenS"]
    ; ten_to_one_hundred_s : range_info [@key "tenToOneHundredS"]
    ; one_hundred_s : range_info [@key "oneHundredS"]
    }
  [@@deriving to_yojson]

  type listing = t list [@@deriving to_yojson]

  type store = (Operation.t, t) Hashtbl.t

  let bootstrap_store : store = Hashtbl.create (module Operation)

  let frontier_store : store = Hashtbl.create (module Operation)

  let store : store ref = ref bootstrap_store

  let bootstrap_complete () = store := frontier_store

  let empty_range_info () =
    { count = 0; mean_time = 0.0; max_time = 0.0; total_time = 0.0 }

  let empty_operation_entry operation =
    { operation
    ; count = 0
    ; total_time = 0.0
    ; one_to_ten_us = empty_range_info ()
    ; ten_to_one_hundred_us = empty_range_info ()
    ; one_hundred_us_to_one_ms = empty_range_info ()
    ; one_to_ten_ms = empty_range_info ()
    ; ten_to_one_hundred_ms = empty_range_info ()
    ; one_hundred_ms_to_one_s = empty_range_info ()
    ; one_to_ten_s = empty_range_info ()
    ; ten_to_one_hundred_s = empty_range_info ()
    ; one_hundred_s = empty_range_info ()
    }

  let ten_us = 0.00001

  let one_hundred_us = 0.0001

  let one_ms = 0.001

  let ten_ms = 0.01

  let one_hundred_ms = 0.1

  let one_s = 1.0

  let ten_s = 10.0

  let one_hundred_s = 100.0

  let range_for_duration record duration =
    let open Float in
    if duration < ten_us then record.one_to_ten_us
    else if duration < one_hundred_us then record.ten_to_one_hundred_us
    else if duration < one_ms then record.one_hundred_us_to_one_ms
    else if duration < ten_ms then record.one_to_ten_ms
    else if duration < one_hundred_ms then record.ten_to_one_hundred_ms
    else if duration < one_s then record.one_hundred_ms_to_one_s
    else if duration < ten_s then record.one_to_ten_s
    else if duration < one_hundred_s then record.ten_to_one_hundred_s
    else record.one_hundred_s

  let record operation duration =
    let record =
      Hashtbl.find_or_add !store operation ~default:(fun () ->
          empty_operation_entry operation )
    in
    let range = range_for_duration record duration in
    record.count <- record.count + 1 ;
    record.total_time <- record.total_time +. duration ;
    range.count <- range.count + 1 ;
    range.total_time <- range.total_time +. duration ;
    range.max_time <- Float.max range.max_time duration ;
    let f_count = Float.of_int range.count in
    range.mean_time <-
      range.mean_time +. ((duration -. range.mean_time) /. f_count) ;
    ()

  let frontier_all () = Hashtbl.data frontier_store

  let bootstrap_all () = Hashtbl.data bootstrap_store
end

let record operation duration = Distributions.record operation duration

let now () = Unix.gettimeofday ()

let recursing_flags = Hashtbl.create (module Operation)

let recursing_flag_for_op op =
  Hashtbl.find_or_add recursing_flags op ~default:(fun () -> ref false)

let wrap1 ~op f =
  let recursing = recursing_flag_for_op op in
  fun arg ->
    if !recursing then f arg
    else (
      recursing := true ;
      let start = now () in
      let result = f arg in
      let duration = now () -. start in
      recursing := false ;
      record op duration ;
      result )

let wrap2 ~op f =
  let recursing = recursing_flag_for_op op in
  fun arg1 arg2 ->
    if !recursing then f arg1 arg2
    else (
      recursing := true ;
      let start = now () in
      let result = f arg1 arg2 in
      let duration = now () -. start in
      recursing := false ;
      record op duration ;
      result )

let wrap3 ~op f =
  let recursing = recursing_flag_for_op op in
  fun arg1 arg2 arg3 ->
    if !recursing then f arg1 arg2 arg3
    else (
      recursing := true ;
      let start = now () in
      let result = f arg1 arg2 arg3 in
      let duration = now () -. start in
      recursing := false ;
      record op duration ;
      result )
