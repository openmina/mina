include Impl

module Logger_id = struct
  let mina : Consumer_registry.id = "default"

  let best_tip_diff = "best_tip_diff"

  let rejected_blocks = "rejected_blocks"

  let snark_worker = "snark_worker"

  let oversized_logs = "oversized_logs"
end

open Core

let logger_key : Impl.t Type_equal.Id.t =
  Univ_map.Key.create ~name:"logger" sexp_of_opaque

let with_logger logger f =
  Async_kernel.Async_kernel_scheduler.with_local logger_key logger ~f

let get_logger_opt () =
  Async_kernel.Async_kernel_scheduler.find_local logger_key

let get_logger () = Option.value (get_logger_opt ()) ~default:(Impl.null ())
