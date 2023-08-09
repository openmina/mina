let run_in_thread f = Async_kernel.Deferred.return (f ())

let block_on_async_exn f = Async.Thread_safe.block_on_async_exn f
