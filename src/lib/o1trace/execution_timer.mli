open Core_kernel

include Plugins.Plugin_intf

val elapsed_time_of_thread : Thread.t -> Time_ns.Span.t

val set_long_runtime_hook : (string -> Time_ns.Span.t -> unit) -> unit
