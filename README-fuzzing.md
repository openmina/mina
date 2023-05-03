# The Mina Fuzzer

We want to maximize the security and stability of the Mina network, and for such a complex and dynamic system, we need to use approaches that cover as many lines of code as possible. Additionally, testing must be scalable to keep pace with the growth of the network.

One of the methods that are suitable for testing in such complex and challenging environments is fuzzing. This method of testing involves generating random inputs and feeding them into the system to trigger unexpected behaviors. This can cover a wide range of inputs, including edge cases that may not be considered during regular testing or through manual analysis. 

The Mina Fuzzer specifically targets the transaction application logic which is the code that defines the rules and processes that govern how transactions are created, validated, and recorded on the blockchain. There are two kinds of transactions that are tested:
 
* legacy transactions, which are signed MINA transfers.
* zkApp transactions, which are more complex and can do multiple account updates (changes to the account’s properties, including changes to its balance) in a single transaction, also they support operations to work with custom tokens.


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
