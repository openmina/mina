# Verifier compatible with `ocamldebug`

This is a minimal script for verifying a block proof. The code in this branch is slightly modified to avoid the use of threads (which `ocamldebug` cannot handle), and to produce a workable bytecode version (`ocamldebug` needs a bytecode program, it cannot handle native code).

The instructions here assume that we are in the `run_verifier_app` branch.

## Instructions

This assumes that the regular setup described [in the wiki](https://github.com/name-placeholder/mina-wiki/tree/master/build_and_run_mina_cli) has been completed.

Also that the data from [this repository](https://github.com/name-placeholder/mina-block-verifier-poc) is available (we use the block json data as input)

```
# Setup opam env
eval $(opam env)
# Build the bytecode program
env DUNE_PROFILE=devnet dune build src/app/run_verifier/run_verifier.bc
# Build the native program
env DUNE_PROFILE=devnet dune build src/app/run_verifier/run_verifier.exe
```

This should result in these two files:

```
_build/default/src/app/run_verifier/run_verifier.exe # native
_build/default/src/app/run_verifier/run_verifier.bc  # bytecode
```

Next, we need to copy the dynamically linked library containing the kimchi code and bindings so that the bytecode program can pick it up (native is linked statically):

```
cp ./_build/cargo_kimchi_bindgen/debug/libwires_15_stubs.so src/lib/crypto/kimchi_bindings/stubs/dllwires_15_stubs.so
```

We are going to first run the native program to produce the cached files so that when we run the bytecode version inside `ocamldebug` it runs faster.

```
> _build/default/src/app/run_verifier/run_verifier.exe < ../mina-block-verifier-poc/src/data/6324_state_with_proof_for_mina_node_minified.json
Starting verifier
Compiling transaction snark module...
### VF NOT Found in cache
### PF NOT Found in cache
### VF NOT Found in cache
### PF NOT Found in cache
### VF NOT Found in cache
### PF NOT Found in cache
### VF NOT Found in cache
### PF NOT Found in cache
### VF NOT Found in cache
### PF NOT Found in cache
Transaction_snark.system: 34.399s
Transaction snark module creation time: 34.399031s
Compiling blockchain snark module...
### VF NOT Found in cache
### PF NOT Found in cache
Blockchain snark module creation time: 13.786125s
Input JSON:
Parsing.
Calling verifier.
Verification time: 2.901198s
Proofs verified successfully
```

This execution will output some files into `/tmp/coda_cache_dir/`. These files will be loaded in future runs to avoid some expensive computations.

Finally, we will run the verifier program inside `ocamldebug` (manual [here](https://v2.ocaml.org/manual/debugger.html)):

```
> ocamldebug ./_build/default/src/app/run_verifier/run_verifier.bc
        OCaml Debugger version 4.14.0

(ocd) set bigstep 1318010000
(ocd) set arguments < ../mina-block-verifier-poc/src/data/6324_state_with_proof_for_mina_node_minified.json
(ocd) run
Loading program... done.
Starting verifier
Compiling transaction snark module...
Transaction_snark.system: 1.484m
Transaction snark module creation time: 89.049605s
Compiling blockchain snark module...
Blockchain snark module creation time: 135.773009s
Input JSON:
Parsing.
Calling verifier.
Verification time: 29.296549s
Proofs verified successfullyTime: 2126817752
Program exit.
(ocd)
```

The `set bigstep 1318010000` command tells `ocamldebug` to not take snapshots too often (which are used for backwards debugging), this speeds up the execution considerably.
The `set arguments < ...` command sets the stdin input.
Finally, `run` executes the program.