# Stdio-based snark worker

The `snark-worker-stdio` internal subcommand reads snark work input from stdin and writes performed work to stderr.

``` sh
$ make DUNE_PROFILE=devnet
$ _build/default/src/app/cli/src/mina.exe internal snark-worker-stdio < input.bin 2> output.bin
```

The `input.bin` consists of a sequence of 13 snark works.
