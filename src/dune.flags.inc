(include dune.linker.inc)
(include dune.alloc.inc)

(env
  (_
    (flags (:standard -short-paths
            -ccopt=%{read:dune-linker}
            -cclib=-L/usr/lib/gcc/x86_64-linux-gnu/9/
            -cclib=-L/usr/lib/x86_64-linux-gnu/
            -cclib=-l%{read:dune-alloc}
            -w @a-4-16-29-40-41-42-44-45-48-58-59-60-66-67-69-70))))
