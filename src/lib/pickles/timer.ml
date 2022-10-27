open Core_kernel

let l = ref ""

let r = ref (Time.now ())

let start =
  Common.when_profiling
    (fun loc ->
      r := Time.now () ;
      l := loc )
    ignore

let clock =
  Common.when_profiling
    (fun loc ->
      let t = Time.now () in
      let diff = Time.diff t !r in
      if Time.Span.(diff > sec 0.2) then
        printf "%s -> %s: %s\n%!" !l loc (Time.Span.to_string_hum diff) ;
      r := t ;
      l := loc )
    ignore
