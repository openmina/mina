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
        let random_window =
          Option.value_map ~default:Int.max_value ~f:Int.of_string
            (Sys.getenv_opt "MINA_RANDOM_WORK_SELECTION_WINDOW")
        in
        let i =
          Random.int (Int.min random_window (List.length expensive_work))
        in
        let x = List.nth_exn expensive_work i in
        Lib.State.set state x ; Some x

  let remove = Lib.State.remove

  let pending_work_statements = Lib.pending_work_statements
end

let%test_module "test" =
  ( module struct
    module Test = Test.Make_test (Make)
  end )
