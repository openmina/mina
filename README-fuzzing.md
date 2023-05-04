# The Mina Transaction Fuzzer

We want to maximize the security and stability of the Mina network, and for such a complex and dynamic system, we need to use approaches that cover as many lines of code as possible. Additionally, testing must be scalable to keep pace with the growth of the network.

One of the methods that are suitable for testing in such complex and challenging environments is *fuzzing*. This method of testing involves generating random inputs and feeding them into the system to trigger unexpected behaviors. This can cover a wide range of inputs, including edge cases that may not be considered during regular testing or through manual analysis.

We have developed the Mina Transaction Fuzzer to specifically target the transaction application logic, which is the code that defines the rules and processes that govern how transactions are created, validated, and recorded on the blockchain.

In Mina there are two kinds of transactions:

* legacy transactions, which are signed MINA transfers.
* zkApp transactions, which are more complex and can do multiple account updates (changes to the account’s properties, including changes to its balance) in a single transaction, also they support operations to work with custom tokens.

This fuzzer focuses in testing the *zkApp* transaction kind.


## Requirements

Assuming an environment on which the mina daemon can be built, the only extra requirement is the Rust nightly toolchain.

Otherwise the full list is:

- In apt based Linux distributions: `apt install curl rsync git make unzip build-essential libbz2-dev zlib1g-dev libjemalloc-dev capnproto`
- A working [Go language](https://go.dev/) install (version 1.18.5).
- A working OCaml and `opam` install. See [here](https://opam.ocaml.org/doc/Install.html) for install instructions.
- Rustup and Rust `nightly-2023-05-02`. See [here](https://rustup.rs/) for rustup install instructions.

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

### Other options

- `SEED` controls the seed value used by the fuzzer (default: `0`)
- `REPORTS_PATH` defines the output directory where the report files will be saved (default: `./fuzzing/reports/`)

### Reproducing fuzzcases

If a bug condition (for example, an invariant violation) is found the fuzzer will stop and save a fuzzcase file containing the actual ledger state and the transaction that triggered the bug condition.

For example:

```bash
$ make run-transaction-fuzzer INVARIANT_BREAK=true
export LD_LIBRARY_PATH=`pwd`/_build/default/src/lib/mina_tree && \
	export FUZZCASES_PATH=`pwd`/fuzzing/fuzzcases/ && \
	export REPORTS_PATH=`pwd`/fuzzing/reports/ && \
	export RUST_BUILD_PATH=`pwd`/src/lib/mina_tree/ && \
	export OCAML_BUILD_PATH=`pwd`/_build/default/ && \
	export LLVM_PROFILE_FILE=/dev/null && \
	export RUST_BACKTRACE=1 && \
	mkdir -p $FUZZCASES_PATH $REPORTS_PATH && \
	./_build/default/src/app/transaction_fuzzer/transaction_fuzzer.exe run -invariant-break true 0 || exit 0
Saving coverage report (OCaml)
Saving coverage report (Rust)
=== COV Rust ===
  1%   45/2437: src/scan_state/transaction_logic.rs
  4%   11/ 230: src/scan_state/zkapp_logic.rs
  2%   56/2667: Total

=== COV OCaml ===
  1%    9/ 451: src/lib/transaction_logic/zkapp_command_logic.ml
  2%   30/1466: src/lib/network_pool/transaction_pool.ml
  3%   40/1039: src/lib/transaction_logic/mina_transaction_logic.ml
  6%   62/ 892: src/lib/mina_base/zkapp_command.ml
 14%  134/ 898: src/lib/mina_base/account_update.ml
 18%   46/ 247: src/lib/verifier/prod.ml
 18%  144/ 771: src/lib/mina_base/zkapp_precondition.ml
  8%  465/5764: Total

Environment variable MINA_TIME_OFFSET not found, using default of 0
 => Invariant violation: increment_nonce permission
Saving fuzzcase: /home/user/mina/fuzzing/fuzzcases/15380143365595797233957438716143531525663009213364747952469309100623056851816.fuzzcase
```

We can use fuzzcase files to reproduce bug conditions **deterministically**.

To reproduce a fuzzcase we use `make reproduce-transaction-fuzzer` and pass the path to the fuzzcase file in the `FUZZCASE` variable:

```bash
$ make reproduce-transaction-fuzzer FUZZCASE=./fuzzing/fuzzcases/15380143365595797233957438716143531525663009213364747952469309100623056851816.fuzzcase
export LD_LIBRARY_PATH=`pwd`/_build/default/src/lib/mina_tree && \
	export RUST_BACKTRACE=1 && \
	./_build/default/src/app/transaction_fuzzer/transaction_fuzzer.exe reproduce ./fuzzing/fuzzcases/15380143365595797233957438716143531525663009213364747952469309100623056851816.fuzzcase || exit 0
Loading fuzzcase: ./fuzzing/fuzzcases/15380143365595797233957438716143531525663009213364747952469309100623056851816.fuzzcase
{"timestamp":"2023-05-03 12:41:34.108186Z","level":"Info","source":{"module":"Transaction_fuzzer","location":"File \"src/app/transaction_fuzzer/transaction_fuzzer.ml\", line 180, characters 4-15"},"message":"Starting verifier...","metadata":{}}
{"timestamp":"2023-05-03 12:41:34.109521Z","level":"Info","source":{"module":"Verifier__Prod","location":"File \"src/lib/verifier/prod.ml\", line 315, characters 4-15"},"message":"Starting a new verifier process","metadata":{}}
{"timestamp":"2023-05-03 12:41:34.489910Z","level":"Info","source":{"module":"Verifier__Prod","location":"File \"src/lib/verifier/prod.ml\", line 349, characters 4-15"},"message":"Daemon started process of kind $process_kind with pid $verifier_pid","metadata":{"process_kind":"Verifier","verifier_pid":726394}}
{"timestamp":"2023-05-03 12:41:34.489997Z","level":"Info","source":{"module":"Transaction_fuzzer","location":"File \"src/app/transaction_fuzzer/transaction_fuzzer.ml\", line 192, characters 4-15"},"message":"Creating transaction pool...","metadata":{}}
{"timestamp":"2023-05-03 12:41:34.490036Z","level":"Trace","source":{"module":"Network_pool__Network_pool_base","location":"File \"src/lib/network_pool/network_pool_base.ml\", line 233, characters 8-20"},"message":"Nothing to rebroadcast","metadata":{}}
{"timestamp":"2023-05-03 12:41:34.561387Z","level":"Debug","source":{"module":"Network_pool__Transaction_pool","location":"File \"src/lib/network_pool/transaction_pool.ml\", line 820, characters 17-29"},"message":"Got frontier!","metadata":{}}
Environment variable MINA_TIME_OFFSET not found, using default of 0
{"timestamp":"2023-05-03 12:41:34.561437Z","level":"Debug","source":{"module":"Network_pool__Transaction_pool","location":"File \"src/lib/network_pool/transaction_pool.ml\", line 883, characters 17-29"},"message":"Re-validated transaction pool after restart: dropped 0 of 0 previously in pool","metadata":{}}
{"timestamp":"2023-05-03 12:41:34.561454Z","level":"Debug","source":{"module":"Network_pool__Network_pool_base","location":"File \"src/lib/network_pool/network_pool_base.ml\", line 152, characters 8-30"},"message":"transaction_pool $rate_limiter","metadata":{"rate_limiter":{"by_ip":[],"by_peer_id":[]}}}
{"timestamp":"2023-05-03 12:41:34.563000Z","level":"Trace","source":{"module":"Verifier__Prod","location":"File \"src/lib/verifier/prod.ml\", line 469, characters 4-16"},"message":"Verifier trying with $attempts_remaining","metadata":{"attempts_remaining":4}}
{"timestamp":"2023-05-03 12:41:34.563069Z","level":"Trace","source":{"module":"Network_pool__Transaction_pool","location":"File \"src/lib/network_pool/transaction_pool.ml\", line 530, characters 6-28"},"message":"Diff: removed: $removed added: $added from best tip","metadata":{"added":[],"removed":[]}}
{"timestamp":"2023-05-03 12:41:34.564572Z","level":"Debug","source":{"module":"Network_pool__Transaction_pool","location":"File \"src/lib/network_pool/transaction_pool.ml\", line 668, characters 6-28"},"message":"Finished handling diff. Old pool size 0, new pool size 0. Dropped 0 commands during backtracking to maintain max size.","metadata":{}}
{"timestamp":"2023-05-03 12:41:34.574363Z","level":"Debug","source":{"module":"Network_pool__Transaction_pool","location":"File \"src/lib/network_pool/transaction_pool.ml\", line 1324, characters 8-30"},"message":"Dropping $num_for_add commands from pool while adding new commands, and $num_for_size commands due to pool size","metadata":{"num_for_add":0,"num_for_size":0}}
transaction_pool_verify return: true
 => Invariant violation: increment_nonce permission
apply_transaction return: Ok(())
```

## The Front End

To visualize the process of fuzzing a Mina node, we have created [a front end](https://github.com/openmina/openmina-fuzzing-ui) you can view via your internet browser.

### Front end setup

To install and run locally you can follow these steps:

```bash
## Clone the repository
git clone https://github.com/openmina/openmina-fuzzing-ui.git
cd openmina-fuzzing-ui
## Install Angular CLI tools with npm
npm install @angular/cli@15.0.0
## Build and run the frontend
npx ng serve
```

Then visit http://localhost:4200/ in your browser.

### Report files

When running the fuzzer, it is useful to specify a custom `REPORTS_PATH` value pointing to the directory where the front end application will find them:

```bash
## Cleanup old report files
rm $(pwd)/openmina-fuzzing-ui/src/assets/reports/*
## Run fuzzer and output report files where the front end can find them
make run-transaction-fuzzer \
    REPORTS_PATH=$(pwd)/openmina-fuzzing-ui/src/assets/reports/
```

If the frontend is up while the fuzzer is running, it will be automatically updated with the output from the fuzzer.

### Front end guide

![fuzzer1](https://user-images.githubusercontent.com/60480123/235866162-27548c3f-c08b-4488-bfcd-96fa6cdb2799.png)


By default, the website loads up the OCaml tab.

Click on the Rust tab to open up the fuzzing overview for Mina’s Rust code:


![fuzzer2](https://user-images.githubusercontent.com/60480123/235866187-d4a080ff-ee05-4382-9877-f10bc3f31ded.png)


On both the OCaml and Rust tabs, we can see, from top to bottom:

**Overall coverage** - The overall percentage of the OCaml/Rust part of Mina’s codebase that has been fuzzed. It is color coded, with red for under 50% fuzzed, yellow for under 80% fuzzed, and green for over 80% fuzzed.

**Search Files** - type in a file name or file path you want to filter out in the list below in real time.

Below is a list of files of the OCaml/Rust part of the Mina codebase. On the left is their coverage percentage, both visualized as a bar and with a percentage next to it. The column to the right displays the file path for that file. Entries are updated in real time.

Clicking **Coverage** will sort these entries by how much percent they have been fuzzed in a descending order (from most fuzzed to least), clicking on it again will sort them in an ascending order.



Clicking on **Path** will sort entries alphabetically. Click on it again to sort in reverse alphabetical order.

Now click on an entry in the list of files to open up the sidebar.


![fuzzer3](https://user-images.githubusercontent.com/60480123/235866217-526fe5b5-5a12-4866-a098-644a58265c28.png)



The side bar displays the **Source Code** of the selected file along with the number of **total lines** and coverage (which lines have been fuzzed) being represented by **hit lines**.

Coverage highlighting can be toggled on or off by clicking on the highlighter icon in the top right corner of the sidebar.

Each highlight piece of the code has a tooltip that shows the number of times the line was executed during the test run.

The sidebar is color-coded:



* A green highlight means the code was executed at least once on the entire line.
* Yellow means the code was executed at least once on a part of the line and zero times on another part.
* If the highlight is red, then the code was executed zero times.

The user can copy a permalink(similar to GitHub) to a specific line of code by clicking on the line number.


