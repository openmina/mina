# Stdio-based snark worker

The `snark-worker-stdio` internal subcommand reads snark work input from stdin and writes performed work to stderr.

``` sh
$ make DUNE_PROFILE=devnet
$ _build/default/src/app/cli/src/mina.exe internal snark-worker-stdio < input.bin 2> output.bin
```

The `input.bin` consists of a sequence of 13 snark works.

## Cancelling jobs

To cancel an ongoing job, send a `SIGINT` signal to the main prover process.

## Request values

- A readiness query (will answer once ready), which is the value 0
- A job request, which is the value 1, followed by a job spec.

## Response values

### For readiness queries

- The `true` value.

### For job requests

- `Ok (Some result)` for jobs that were completed successfuly.
- `Ok None` for jobs that were cancelled.
- `Error "error string"` on instances in which an error happened.
