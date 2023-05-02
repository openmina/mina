
## Requirements

- A working `opam` install
- Rust `nightly-2023-05-02`
- `libjemalloc-dev` package

## Fuzzer setup

```bash
## Clone the fuzzing repository
git clone -b transaction_fuzzer_rampup https://github.com/openmina/mina.git mina_transaction_fuzzer
cd mina_transaction_fuzzer
## Set rustup override
rustup override set nightly-2023-05-02
## Initialize the git submodules
git submodule update --init --recursive
git config --local --add submodule.recurse true
## Initialize opam with sandboxing disabled
## (required to be able to build Rust crates with external dependencies)
opam init --disable-sandboxing
## Create a local opam switch for this directory
opam switch create . 4.14.0
## Load the opam switch environment
eval $(opam config env)
## Install OCaml dependencies
opam switch import -y opam.export
## Pin external package repos+versions
./scripts/pin-external-packages.sh
## (Optional) raise stack size with ulimit if the build the build step fails
ulimit -s 81920
## Build the transaction fuzzer app
make build-transaction-fuzzer DUNE_PROFILE=devnet
```

## Running the fuzzer

```bash
## Run the fuzzer (and build with the specified profile if necessary)
make run-transaction-fuzzer DUNE_PROFILE=devnet
```

By default the fuzzer will keep running if an invariant violation is found, to change this behavior use `INVARIANT_BREAK=true`:

```bash
make run-transaction-fuzzer INVARIANT_BREAK=true
```
