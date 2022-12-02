open Core_kernel
open Currency
open Signature_lib

module Statement = struct
  module Arg = struct
    [%%versioned
    module Stable = struct
      module V2 = struct
        type t = Transaction_snark.Statement.Stable.V2.t One_or_two.Stable.V1.t
        [@@deriving hash, sexp, compare]

        let to_latest = Fn.id
      end
    end]
  end

  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V2 = struct
      type t = Transaction_snark.Statement.Stable.V2.t One_or_two.Stable.V1.t
      [@@deriving equal, compare, hash, sexp, yojson]

      let to_latest = Fn.id

      type _unused = unit constraint t = Arg.Stable.V2.t

      include Hashable.Make_binable (Arg.Stable.V2)

      module Partial_hash = struct
        let hash_fold_t =
          One_or_two.hash_fold_t
            Transaction_snark.Statement.Stable.Latest.Partial_view.hash_fold_t

        let hash t = Hash.get_hash_value (hash_fold_t (Hash.create ()) t)

        let compare =
          One_or_two.compare
            Transaction_snark.Statement.Stable.Latest.Partial_view.compare

        include Hashable.Make_binable (struct
          (* Alias because deriving doesn't support `norec` *)
          type outer_t = t [@@deriving sexp, bin_io_unversioned]

          type t = outer_t [@@deriving sexp, bin_io_unversioned]

          let hash_fold_t = hash_fold_t

          let hash = hash

          let compare = compare
        end)

        module Table = struct
          let __versioned__ = () (* Workaround for ppx_versioned *)

          include Table
        end
      end
    end
  end]

  type t = Stable.Latest.t [@@deriving sexp, hash, compare, yojson, equal]

  module Partial_hash = Stable.Latest.Partial_hash
  include Hashable.Make (Stable.Latest)

  let gen = One_or_two.gen Transaction_snark.Statement.gen

  let compact_json t =
    `List
      ( One_or_two.map ~f:(fun s -> `Int (Transaction_snark.Statement.hash s)) t
      |> One_or_two.to_list )

  let work_ids t : int One_or_two.t =
    One_or_two.map t ~f:Transaction_snark.Statement.hash
end

module Info = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V2 = struct
      type t =
        { statements : Statement.Stable.V2.t
        ; work_ids : int One_or_two.Stable.V1.t
        ; fee : Fee.Stable.V1.t
        ; prover : Public_key.Compressed.Stable.V1.t
        }
      [@@deriving compare, sexp, to_yojson]

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t =
    { statements : Statement.t
    ; work_ids : int One_or_two.t
    ; fee : Fee.t
    ; prover : Public_key.Compressed.t
    }
  [@@deriving to_yojson, sexp, compare]
end

module T = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V2 = struct
      type t = Mina_wire_types.Transaction_snark_work.V2.t =
        { fee : Fee.Stable.V1.t
        ; proofs : Ledger_proof.Stable.V2.t One_or_two.Stable.V1.t
        ; prover : Public_key.Compressed.Stable.V1.t
        }
      [@@deriving equal, compare, sexp, yojson]

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t =
    { fee : Fee.t
    ; proofs : Ledger_proof.t One_or_two.t
    ; prover : Public_key.Compressed.t
    }
  [@@deriving compare, yojson, sexp]

  let statement t = One_or_two.map t.proofs ~f:Ledger_proof.statement

  let info t =
    let statements = One_or_two.map t.proofs ~f:Ledger_proof.statement in
    { Info.statements
    ; work_ids = One_or_two.map statements ~f:Transaction_snark.Statement.hash
    ; fee = t.fee
    ; prover = t.prover
    }
end

include T

type unchecked = t

module Checked = struct
  include T

  let create_unsafe = Fn.id
end

let forget = Fn.id

let fee { fee; _ } = fee
