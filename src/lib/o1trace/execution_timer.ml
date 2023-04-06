open Core_kernel

module T =
  Plugins.Register_plugin
    (struct
      type state = Time_ns.Span.t ref [@@deriving sexp_of]

      let name = "Execution_timer"

      let init_state _thread_name = ref Time_ns.Span.zero
    end)
    ()

include T

let rec record_elapsed_time (fiber : Thread.Fiber.t) elapsed_time =
  let state = Plugins.plugin_state (module T) fiber.thread in
  (state := Time_ns.Span.(!state + elapsed_time)) ;
  match fiber.parent with
  | None ->
      ()
  | Some parent ->
      record_elapsed_time parent elapsed_time

let on_job_enter _fiber = ()

let long_runtime_hook =
  ref (fun (_kind : string) (_name : string) (_time : Time_ns.Span.t) -> ())

let collect_names fiber =
  let rec loop acc (fiber_opt : Thread.Fiber.t option) =
    match fiber_opt with
    | None ->
        String.concat ~sep:"/" acc
    | Some fiber ->
        loop (fiber.thread.name :: acc) fiber.parent
  in
  loop [] (Some fiber)

let on_job_exit ~thread_kind fiber elapsed_time =
  record_elapsed_time fiber elapsed_time ;
  if Float.(Time_ns.Span.to_ms elapsed_time >= 50.) then
    !long_runtime_hook thread_kind (collect_names fiber) elapsed_time

let elapsed_time_of_thread thread = !(Plugins.plugin_state (module T) thread)

let set_long_runtime_hook f = long_runtime_hook := f
