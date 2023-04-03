(** Any_ledger lets you use any arbitrary ledger whenever some ledger is
 * required. This uses dynamic dispatch and is equivalent to the notion of
 * consuming a value conforming to an interface in Java.
 *
 * It uses GADTs to type-erase the specific underlying first-class module
 * for some given signature and delegates all function calls.
 *
 * The restriction here is that one cannot conform to some signature that
 * exposes a `create` function because we can't magically pull a conforming
 * module out of thin air. However, one can always just pack any concrete
 * instance with the GADT constructor `witness`.
 *
 * Props to @nholland for showing me this trick.
 * *)

open Core_kernel

module type S = sig
  type key

  type token_id

  type token_id_set

  type account_id

  type account_id_set

  type account

  type hash

  module Location : Location_intf.S

  (** The type of the witness for a base ledger exposed here so that it can
   * be easily accessed from outside this module *)
  type witness
  (* type witness [@@deriving sexp_of] *)

  module type Base_intf =
    Base_ledger_intf.S
      with module Addr = Location.Addr
      with module Location = Location
      with type key := key
       and type token_id := token_id
       and type token_id_set := token_id_set
       and type account_id := account_id
       and type account_id_set := account_id_set
       and type hash := hash
       and type root_hash := hash
       and type account := account

  val cast : (module Base_intf with type t = 'a) -> 'a -> witness

  val cast_database_to_mask :
    (module Base_intf with type t = 'a) -> 'a -> witness

  module M : Base_intf with type t = witness
end

module type Inputs_intf = sig
  include Base_inputs_intf.S

  module Location : Location_intf.S
end

module Make_base (Inputs : Inputs_intf) :
  S
    with module Location = Inputs.Location
    with type key := Inputs.Key.t
     and type token_id := Inputs.Token_id.t
     and type token_id_set := Inputs.Token_id.Set.t
     and type account_id := Inputs.Account_id.t
     and type hash := Inputs.Hash.t
     and type account_id_set := Inputs.Account_id.Set.t
     and type account := Inputs.Account.t = struct
  open Inputs
  module Location = Location

  module type Base_intf =
    Base_ledger_intf.S
      with module Addr = Location.Addr
      with module Location = Location
      with type key := Inputs.Key.t
       and type token_id := Inputs.Token_id.t
       and type token_id_set := Inputs.Token_id.Set.t
       and type account_id := Account_id.t
       and type account_id_set := Account_id.Set.t
       and type hash := Hash.t
       and type root_hash := Hash.t
       and type account := Account.t

  type witness = T : (module Base_intf with type t = 't) * 't -> witness

  (* external mask_create : int -> t = "rust_mask_create" *)
  (* val cast : (module Base_intf with type t = 'a) -> 'a -> witness *)
  (* external rust_cast : (module Base_intf with type t = 'a) -> 'a -> witness *)
  external rust_cast : 'a -> witness = "rust_cast"

  external rust_cast_database_to_mask : 'a -> witness
    = "rust_cast_database_to_mask"

  let cast (_m : (module Base_intf with type t = 'a)) (t : 'a) = rust_cast t

  let cast_database_to_mask (_m : (module Base_intf with type t = 'a)) (t : 'a)
      =
    rust_cast_database_to_mask t

  let _cast2 (m : (module Base_intf with type t = 'a)) (t : 'a) = T (m, t)

  (* let cast (m : (module Base_intf with type t = 'a)) (t : 'a) = T (m, t) *)

  let sexp_of_witness (T ((module B), t)) = B.sexp_of_t t

  (** M can be used wherever a base ledger is demanded, construct instances
   * by using the witness constructor directly
   *
   * We delegate to the underlying functions in the base interface mechanically
   *
   * In the future, this should be a `ppx`.
   *)
  module M : Base_intf with type t = witness = struct
    (* type t *)
    type t = witness [@@deriving sexp_of]

    let t_of_sexp _ = failwith "t_of_sexp unimplemented"

    type index = int

    module Location = Location
    module Path = Merkle_path.Make (Hash)

    type path = Path.t

    module Addr = Location.Addr

    let account_id_to_rust account_id =
      let buf =
        Bigstring.create (Account_id.Stable.Latest.bin_size_t account_id)
      in
      ignore (Account_id.Stable.Latest.bin_write_t buf ~pos:0 account_id : int) ;
      Bigstring.to_bytes buf

    let account_location_from_rust addr = Location.Account (Addr.of_string addr)

    let account_from_rust account =
      Account.bin_read_t (Bigstring.of_bytes account) ~pos_ref:(ref 0)

    let account_to_rust account =
      let buf = Bigstring.create (Account.bin_size_t account) in
      ignore (Account.bin_write_t buf ~pos:0 account : int) ;
      Bigstring.to_bytes buf

    let hash_from_rust hash =
      hash |> Bigstring.of_bytes |> Hash.bin_read_t ~pos_ref:(ref 0)

    let location_to_rust location =
      Location.to_path_exn location |> Addr.to_string

    let path_from_rust path =
      match path with
      | `Left hash ->
          `Left (hash_from_rust hash)
      | `Right hash ->
          `Right (hash_from_rust hash)

    external print_backtrace : int -> unit = "rust_print_backtrace"

    external mask_remove_accounts : 'a -> 'b list -> unit
      = "rust_mask_remove_accounts"

    let remove_accounts_exn m account_ids =
      (*Printf.eprintf "MY_LOG.ANY.REMOVE_ACCOUNTS\n%!" ;*)
      print_backtrace 0 ;
      let account_ids = List.map account_ids ~f:account_id_to_rust in
      mask_remove_accounts m account_ids

    external mask_merkle_path_at_index : 'a -> int -> 'b list
      = "rust_mask_merkle_path_at_index"

    let merkle_path_at_index_exn m index =
      (*Printf.eprintf "MY_LOG.ANY.MERKLE_PATH_AT_INDEX\n%!" ;*)
      mask_merkle_path_at_index m index |> List.map ~f:path_from_rust

    (* let merkle_path_at_index_exn (T ((module Base), t)) =
     *   Printf.eprintf "MY_LOG.ANY.MERKLE_PATH_AT_INDEX\n%!" ;
     *   Base.merkle_path_at_index_exn t *)

    external mask_merkle_path : 'a -> 'b -> 'c list = "rust_mask_merkle_path"

    let merkle_path m location =
      (*Printf.eprintf "MY_LOG.ANY.MERKLE_PATH\n%!" ;*)
      mask_merkle_path m (location_to_rust location)
      |> List.map ~f:path_from_rust

    (* let merkle_path (T ((module Base), t)) =
     *   Printf.eprintf "MY_LOG.ANY.MERKLE_PATH\n%!" ;
     *   Base.merkle_path t *)

    external mask2_merkle_root : 'a -> bytes = "rust_mask_merkle_root"

    let merkle_root a =
      (*Printf.eprintf "MY_LOG.ANY.MERKLE_ROOT\n%!" ;*)
      print_backtrace 0 ;
      let res = mask2_merkle_root a |> hash_from_rust in
      (* let res = Base.merkle_root t in *)
      (*Printf.eprintf "MY_LOG.ANY.MERKLE_ROOT2\n%!" ;*)
      res

    external mask_index_of_account : 'a -> 'b -> int
      = "rust_mask_index_of_account"

    let index_of_account_exn m account_id =
      (*Printf.eprintf "MY_LOG.ANY.INDEX_OF_ACCOUNT\n%!" ;*)
      print_backtrace 0 ;
      mask_index_of_account m (account_id_to_rust account_id)

    external mask_set_at_index : 'a -> int -> 'b -> unit
      = "rust_mask_set_at_index"

    let set_at_index_exn m index account =
      (*Printf.eprintf "MY_LOG.ANY.SET_AT_INDEX\n%!" ;*)
      print_backtrace 0 ;
      mask_set_at_index m index (account_to_rust account)

    external mask_get_at_index : 'a -> int -> 'b = "rust_mask_get_at_index"

    let get_at_index_exn m index =
      (*Printf.eprintf "MY_LOG.ANY.GET_AT_INDEX\n%!" ;*)
      print_backtrace 0 ;
      mask_get_at_index m index |> account_from_rust

    (* let get_at_index_exn (T ((module Base), t)) =
     *   Printf.eprintf "MY_LOG.ANY.GET_AT_INDEX\n%!" ;
     *   Base.get_at_index_exn t *)

    external mask_set_batch_accounts : 'a -> ('b * 'c) list -> unit
      = "rust_mask_set_batch_accounts"

    let set_batch m locations_and_accounts =
      let accounts =
        List.map locations_and_accounts ~f:(fun (location, account) ->
            (location_to_rust location, account_to_rust account) )
      in
      mask_set_batch_accounts m accounts

    external mask_set : 'a -> 'b -> 'c -> unit = "rust_mask_set"

    let set m location account =
      (*Printf.eprintf "MY_LOG.ANY.SET\n%!" ;*)
      print_backtrace 0 ;
      let location = location_to_rust location in
      mask_set m location (account_to_rust account)

    (* let set (T ((module Base), t)) = *)
    (*   Base.set t *)

    external mask_get : 'a -> 'b -> 'c option = "rust_mask_get"

    let get m location =
      (*Printf.eprintf "MY_LOG.ANY.GET\n%!" ;*)
      let addr = location_to_rust location in
      mask_get m addr |> Option.map ~f:account_from_rust

    (* let get (T ((module Base), t)) =
     *   Printf.eprintf "MY_LOG.ANY.GET\n%!" ;
     *   Base.get t *)

    external mask_get_batch : 'a -> 'addr list -> ('addr * 'account option) list
      = "rust_mask_get_batch"

    let get_batch m locations =
      (*Printf.eprintf "MY_LOG.ANY.GET_BATCH\n%!" ;*)
      print_backtrace 0 ;
      let addrs = List.map locations ~f:location_to_rust in
      mask_get_batch m addrs
      |> List.map ~f:(fun (addr, account) ->
             ( account_location_from_rust addr
             , Option.map account ~f:account_from_rust ) )

    (* let get_batch (T ((module Base), t)) = *)
    (*   Base.get_batch t *)

    let get_uuid (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.GET_UUID\n%!" ;*)
      print_backtrace 0 ;
      Base.get_uuid t

    let get_directory (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.GET_DIRECTORY\n%!" ;*)
      print_backtrace 0 ;
      Base.get_directory t

    let last_filled (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.LAST_FILLED\n%!" ;*)
      print_backtrace 0 ;
      Base.last_filled t

    let close (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.CLOSE\n%!" ;*)
      print_backtrace 0 ;
      Base.close t

    let get_or_create_account (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.GET_OR_CREATE_ACCOUNT\n%!" ;*)
      print_backtrace 0 ;
      Base.get_or_create_account t

    (* let location_of_account (T ((module Base), t)) =
     *   Printf.eprintf "MY_LOG.ANY.LOCATION_OF_ACCOUNT\n%!" ;
     *   Base.location_of_account t *)

    external mask_location_of_account : 'a -> 'b -> 'c option
      = "rust_mask_location_of_account"

    let location_of_account m account_id =
      (*Printf.eprintf "MY_LOG.ANY.LOCATION_OF_ACCOUNT\n%!" ;*)
      mask_location_of_account m (account_id_to_rust account_id)
      |> Option.map ~f:(fun addr -> Location.Account (Addr.of_string addr))

    let location_of_account_batch (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.LOCATION_OF_ACCOUNT_BATCH\n%!" ;*)
      print_backtrace 0 ;
      Base.location_of_account_batch t

    let fold_until (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.FOLD_UNTIL\n%!" ;*)
      print_backtrace 0 ;
      Base.fold_until t

    let accounts (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.ACCOUNTS\n%!" ;*)
      Base.accounts t

    let token_owner (T ((module Base), t)) tid =
      (*Printf.eprintf "MY_LOG.ANY.TOKEN_OWNER\n%!" ;*)
      print_backtrace 0 ;
      Base.token_owner t tid

    let tokens (T ((module Base), t)) pk =
      (*Printf.eprintf "MY_LOG.ANY.TOKENS\n%!" ;*)
      print_backtrace 0 ;
      Base.tokens t pk

    let token_owners (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.TOKEN_OWNERS\n%!" ;*)
      print_backtrace 0 ;
      Base.token_owners t

    let iteri (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.ITERI\n%!" ;*)
      print_backtrace 0 ;
      Base.iteri t

    (* ignored_keys must be Base.Keys.Set.t, but that isn't necessarily the same as Keys.Set.t for the
       Keys passed to this functor; as long as we use the same Keys for all ledgers, this should work
    *)
    let foldi_with_ignored_accounts (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.FOLDI_WITH_IGNORED_ACCOUNTS\n%!" ;*)
      print_backtrace 0 ;
      Base.foldi_with_ignored_accounts t

    external mask_foldi : 'a -> ('b -> bytes -> unit) -> unit
      = "rust_mask_foldi"

    let foldi m ~init ~f =
      (*Printf.eprintf "MY_LOG.ANY.FOLDI\n%!" ;*)
      let accum = ref init in
      mask_foldi m (fun addr account ->
          accum := f (Addr.of_string addr) !accum (account_from_rust account) ) ;
      !accum

    (* let foldi (T ((module Base), t)) =
     *   Printf.eprintf "MY_LOG.ANY.FOLDI\n%!" ;
     *   Base.foldi t *)

    external mask_get_list : 'a -> bytes list = "rust_mask_get_list"

    let to_list m =
      (*Printf.eprintf "MY_LOG.ANY.TO_LIST\n%!" ;*)
      print_backtrace 0 ;
      mask_get_list m |> List.map ~f:account_from_rust

    (* let to_list (T ((module Base), t)) =
     *   Printf.eprintf "MY_LOG.ANY.TO_LIST\n%!" ;
     *   Base.to_list t *)

    let make_space_for _m _index =
      (*Printf.eprintf "MY_LOG.ANY.MAKE_SPACE_FOR\n%!" ;*)
      print_backtrace 0
    (* Base.make_space_for t *)

    external mask_get_all_accounts_rooted_at : 'a -> 'b -> ('b * 'c) list
      = "rust_mask_get_all_accounts_rooted_at"

    let get_all_accounts_rooted_at_exn m addr =
      (*Printf.eprintf "MY_LOG.ANY.GET_ALL_ACCOUNTS_ROOTED_AT\n%!" ;*)
      print_backtrace 0 ;
      let accounts = mask_get_all_accounts_rooted_at m (Addr.to_string addr) in
      List.map accounts ~f:(fun (addr, account) ->
          (Addr.of_string addr, account_from_rust account) )

    external mask_set_all_accounts_rooted_at : 'a -> 'b -> bytes list -> unit
      = "rust_mask_set_all_accounts_rooted_at"

    let set_all_accounts_rooted_at_exn m addr accounts =
      (*Printf.eprintf "MY_LOG.ANY.SET_ALL_ACCOUNTS_ROOTED_AT\n%!" ;*)
      print_backtrace 0 ;
      let accounts = List.map accounts ~f:account_to_rust in
      mask_set_all_accounts_rooted_at m (Addr.to_string addr) accounts

    let set_batch_accounts m addresses_and_accounts =
      (*Printf.eprintf "MY_LOG.ANY.SET_BATCH_ACCOUNTS\n%!" ;*)
      print_backtrace 0 ;
      let accounts =
        List.map addresses_and_accounts ~f:(fun (addr, account) ->
            (Addr.to_string addr, account_to_rust account) )
      in
      mask_set_batch_accounts m accounts

    let set_inner_hash_at_addr_exn (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.SET_INNER_HASH_AT_ADDR\n%!" ;*)
      print_backtrace 0 ;
      Base.set_inner_hash_at_addr_exn t

    external mask_get_inner_hash_at_addr : 'a -> 'b -> bytes
      = "rust_mask_get_inner_hash_at_addr"

    let get_inner_hash_at_addr_exn m address =
      let hash =
        mask_get_inner_hash_at_addr m (Addr.to_string address) |> hash_from_rust
      in
      hash

    (* let get_inner_hash_at_addr_exn (T ((module Base), t)) =
     *   Printf.eprintf "MY_LOG.ANY.GET_INNER_HASH_AT_ADDR\n%!" ;
     *   print_backtrace 0 ;
     *   Base.get_inner_hash_at_addr_exn t *)

    external mask_merkle_path_at_addr : 'a -> 'b -> 'c list
      = "rust_mask_merkle_path_at_addr"

    let merkle_path_at_addr_exn m addr =
      (*Printf.eprintf "MY_LOG.ANY.MERKLE_PATH_AT_ADDR\n%!" ;*)
      print_backtrace 0 ;
      mask_merkle_path_at_addr m (Addr.to_string addr)
      |> List.map ~f:path_from_rust

    external mask_num_accounts : 'a -> int = "rust_mask_num_accounts"

    let num_accounts m =
      (*Printf.eprintf "MY_LOG.ANY.NUM_ACCOUNTS\n%!" ;*)
      mask_num_accounts m

    (* let num_accounts (T ((module Base), t)) = *)
    (*   Printf.eprintf "MY_LOG.ANY.NUM_ACCOUNTS\n%!" ; *)
    (*   Base.num_accounts t *)

    external mask_depth : 'a -> int = "rust_mask_depth"

    (* This better be the same depth inside Base or you're going to have a bad
     * time *)
    let depth m =
      (*Printf.eprintf "MY_LOG.ANY.DEPTH\n%!" ;*)
      mask_depth m

    (* let depth (T ((module Base), t)) =
     *   Printf.eprintf "MY_LOG.ANY.DEPTH\n%!" ;
     *   Base.depth t *)

    let detached_signal (T ((module Base), t)) =
      (*Printf.eprintf "MY_LOG.ANY.DETACHED\n%!" ;*)
      print_backtrace 0 ;
      Base.detached_signal t
  end
end
