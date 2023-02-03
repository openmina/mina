open Core_kernel

module Make
    (Inputs : Intf.Inputs_intf)
    (Lib : Intf.Lib_intf with module Inputs := Inputs) =
struct
  let work ~snark_pool ~fee ~logger (state : Lib.State.t) =
    Lib.State.remove_old_assignments state ~logger ;
    let unseen_jobs = Lib.State.all_unseen_works state in
    match Lib.get_expensive_work ~snark_pool ~fee unseen_jobs with
    | [] ->
        None
    | expensive_work ->
        let rand = Random.float 1. in
        let max_i = (List.length expensive_work) - 1 in
        let i = rand *. rand *. (float max_i) |> round |> int_of_float in
        let x = List.nth_exn expensive_work i in
        Lib.State.set state x ; Some x

  let remove = Lib.State.remove

  let pending_work_statements = Lib.pending_work_statements
end

let%test_module "test" =
  ( module struct
    module Test = Test.Make_test (Make)
  end )
