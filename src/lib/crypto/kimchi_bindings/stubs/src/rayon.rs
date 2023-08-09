use std::sync::Mutex;

pub static LOCAL_THREAD_POOL_ENABLED: Mutex<bool> = Mutex::new(false);

#[ocaml_gen::func]
#[ocaml::func]
pub fn caml_toggle_rayon_thread_pool(enabled: bool) {
    let mut pool = LOCAL_THREAD_POOL_ENABLED.lock().unwrap();
    *pool = enabled;
}

pub fn maybe_run_in_local_pool<OP, R>(op: OP) -> R
where
    OP: FnOnce() -> R + Send,
    R: Send,
{
    if *LOCAL_THREAD_POOL_ENABLED.lock().unwrap() {
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(0)
            .build()
            .unwrap();
        pool.install(op)
    } else {
        op()
    }
}
