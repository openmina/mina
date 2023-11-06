module type S = Intf.S

include S

val prove_from_input_sexp : t -> Base.Sexp.t -> bool Async.Deferred.t

val prove_from_input_binprot : t -> bool Async.Deferred.t
