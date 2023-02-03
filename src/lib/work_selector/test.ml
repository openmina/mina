open Core_kernel
open Async
open Currency
open Pipe_lib

module Make_test (Make_selection_method : Intf.Make_selection_method_intf) =
struct
  module T = Inputs.Test_inputs

  let reassignment_wait = 2000

  module Lib = Work_lib.Make (T)
  module Selection_method = Make_selection_method (T) (Lib)

  let gen_staged_ledger =
    (*Staged_ledger for tests is a list of work specs*)
    Quickcheck.Generator.list
    @@ Snark_work_lib.Work.Single.Spec.gen Int.quickcheck_generator Fee.gen

  let precomputed_values = Precomputed_values.for_unit_tests

  let init_state sl reassignment_wait logger =
    let tf_reader, tf_writer = Broadcast_pipe.create None in
    let work_state =
      Lib.State.init ~reassignment_wait ~frontier_broadcast_pipe:tf_reader
        ~logger
    in
    let%map () = Broadcast_pipe.Writer.write tf_writer (Some sl) in
    work_state
end
