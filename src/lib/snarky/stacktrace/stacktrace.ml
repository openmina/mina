
module Rust = struct
  external rust_maybe_save_stacktrace : int -> string option = "rust_maybe_save_stacktrace"
end
