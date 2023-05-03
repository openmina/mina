# The Mina Transaction Fuzzer

We want to maximize the security and stability of the Mina network, and for such a complex and dynamic system, we need to use approaches that cover as many lines of code as possible. Additionally, testing must be scalable to keep pace with the growth of the network.

One of the methods that are suitable for testing in such complex and challenging environments is *fuzzing*. This method of testing involves generating random inputs and feeding them into the system to trigger unexpected behaviors. This can cover a wide range of inputs, including edge cases that may not be considered during regular testing or through manual analysis.

We have developed the Mina Transaction Fuzzer to specifically target the transaction application logic, which is the code that defines the rules and processes that govern how transactions are created, validated, and recorded on the blockchain.

In Mina there are two kinds of transactions:

* legacy transactions, which are signed MINA transfers.
* zkApp transactions, which are more complex and can do multiple account updates (changes to the account’s properties, including changes to its balance) in a single transaction, also they support operations to work with custom tokens.

This fuzzer focuses in testing the *zkApp* transaction kind.


## Requirements

- A working `opam` install. See [here](https://opam.ocaml.org/doc/Install.html) for install instructions.
- Rustup and Rust `nightly-2023-05-02`. See [here](https://rustup.rs/) for rustup install instructions.
- `libjemalloc-dev` package (`apt-get install libjemalloc-dev` in Ubuntu).

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


## The Front End



To visualize the process of fuzzing a Mina node, we have created a front end you can view via your internet browser.

Click on this link to open up the front end.


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


