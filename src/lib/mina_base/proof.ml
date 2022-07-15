[%%import "/src/config.mlh"]

open Core_kernel

let blockchain_dummy = Dummy_values.blockchain_proof

let transaction_dummy = Dummy_values.transaction_proof

[%%versioned
module Stable = struct
  module V1 = struct
    type t = Pickles.Proof.Branching_2.Stable.V1.t
    [@@deriving sexp, yojson, compare]

    let to_latest = Fn.id
  end
end]

[%%define_locally Stable.Latest.(to_yojson, of_yojson)]
