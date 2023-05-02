
## Setup steps

```bash
## Clone the fuzzing repository
git clone -b transaction_fuzzer_rampup https://github.com/openmina/mina.git mina_transaction_fuzzer
cd mina_transaction_fuzzer
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
## Build the transaction fuzzer app
make build-transaction-fuzzer DUNE_PROFILE=devnet
```

## Running the fuzzer

```bash
## Run the fuzzer (and build if necessary)
make run-transaction-fuzzer DUNE_PROFILE=devnet
```