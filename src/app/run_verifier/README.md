# Verifier compatible with `ocamldebug`

This is a minimal script for verifying a block proof. The code in this branch is modified to avoid the use of threads (which `ocamldebug` cannot handle), async code (which makes code-stepping more complicated), and to produce a workable bytecode version (`ocamldebug` needs a bytecode program, it cannot handle native code).

The instructions here assume that we are in the `run_verifier_app` branch.

## Instructions

This assumes that the regular setup described [in the wiki](https://github.com/name-placeholder/mina-wiki/tree/master/build_and_run_mina_cli) has been completed.

Also that the data from [this repository](https://github.com/name-placeholder/mina-block-verifier-poc) is available (we use the block json data as input)

First, we need to copy the dynamically linked library containing the kimchi code and bindings so that the bytecode program can pick it up (native is linked statically):

```
cp ./_build/cargo_kimchi_bindgen/debug/libwires_15_stubs.so src/lib/crypto/kimchi_bindings/stubs/dllwires_15_stubs.so
```

```
# Setup opam env
eval $(opam env)
# Optional step to add support for debugging OCaml to VScode
opam pin add earlybird git@github.com:tizoc/ocamlearlybird.git\#414-breakpoint-fixes
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

`run_verifier` accepts as arguments a sequence of paths to files containing JSON-encoded state + proof values (either stand-alone, or an array containing a sequence). The files will be parsed and the results verified one by one. Once there are no more files available (or if none was provided) a prompt will ask for another path. If "quit" is provided (either in the prompt or as an argument) the program exits.

We are going to first run the native program to produce the cached files so that when we run the bytecode version inside `ocamldebug` it runs faster.

```
> _build/default/src/app/run_verifier/run_verifier.exe ../mina-block-verifier-poc/src/data/6324_state_with_proof_for_mina_node_minified.json quit
Starting verifier
Module name: Dune__exe__Run_verifier
To set a breakpoint in this file: break @ Dune__exe__Run_verifier LINE
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

Finally, we will run the verifier program inside `ocamldebug` (manual [here](https://v2.ocaml.org/manual/debugger.html)) or the VSCode extension (see bellow):

```
> ocamldebug ./_build/default/src/app/run_verifier/run_verifier.bc
        OCaml Debugger version 4.14.0

(ocd) set checkpoints off
(ocd) break @ Dune__exe__Run_verifier 84
Loading program... done.
Breakpoint 1 at 0:19755948: file src/app/run_verifier/run_verifier.ml, line 83, characters 27-47
(ocd) run
Starting verifier
Module name: Dune__exe__Run_verifier
To set a breakpoint in this file: break @ Dune__exe__Run_verifier LINE
Compiling transaction snark module...
Transaction_snark.system: 2.164m
Transaction snark module creation time: 129.844209s
Compiling blockchain snark module...
Blockchain snark module creation time: 167.108895s
Input path/to/file.json:
../mina-block-verifier-poc/src/data/6324_state_with_proof_for_mina_node_minified.json
Parsing.
Calling verifier.
Time: 1890739160 - pc: 0:19755952 - module Dune__exe__Run_verifier
Breakpoint: 1
84         <|b|>let result = verify input in
(ocd) step
Time: 1890739161 - pc: 0:12214456 - module Pickles
1113       let verify ts = <|b|>verify_promise ts
(ocd) step
Time: 1890739162 - pc: 0:12214472 - module Pickles
1105         <|b|>verify_promise
....<SNIP>....
```

The `set checkpoints off` command tells `ocamldebug` to not take snapshots (which are used for backwards debugging), this speeds up the execution considerably.
`break @ Dune__exe__Run_verifier 84` sets a breakpoint before the call to `verify`.
`run` executes the program.

After we reach the breakpoint, checkpoints can be enabled again with `set checkpoints on`. This will slow the execution, but allow the debugger to step backwards too.

## VScode ocamldebug integration

First, install a fork of the extension backend by pinning it:

```
opam pin add earlybird git@github.com:tizoc/ocamlearlybird.git\#414-breakpoint-fixes
```

Then install the forked extension:

```
code --install-extension src/app/run_verifier/ocamlearlybird-1.2.0.vsix
```

And finally, add this VSCode [debugger configuration](https://code.visualstudio.com/docs/editor/debugging#_launch-configurations):

```
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "run_verifier",
            "type": "ocamlearlybird",
            "request": "launch",
            "stopOnEntry": false,
            "console": "integratedTerminal",
            "program": "${workspaceFolder}/_build/default/src/app/run_verifier/run_verifier.bc",
            "onlyDebugGlob": "<${workspaceFolder}/src/**/*>",
            "yieldSteps": 102400,
            "cwd": "${workspaceFolder}",
            "env": {
                "OPAM_SWITCH_PREFIX": "${workspaceFolder}/_opam",
                "CAML_LD_LIBRARY_PATH": "${workspaceFolder}/_opam/lib/stublibs:${workspaceFolder}/_opam/lib/ocaml/stublibs:${workspaceFolder}/_opam/lib/ocaml"
            }
        }
    ]
}
```

This assumes that the opam switch is local to the mina project directory, if that is not the case (this can be checked with `opam switch`), replace `"${workspaceFolder}/_opam"` inside the paths for the `env` values with the path to the opam switch (usually `~/.opam/SWITCH_NAME`).

Then in the VSCode settings for the "ocamlearlybird" extension enable the "Connect to a running ocamlearlybird server" option (may require a VSCode restart to take effect).

Finally, in a terminal with the opam environment already initialized (`eval $(opam env)`), run the `ocamlearlybird serve` command, and execute the `run_verifier` launcher in VSCode's debug panel. This will open a new terminal tab where `run_verifier` will execute.