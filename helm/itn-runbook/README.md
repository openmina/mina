# Mina ITN Runbook

This Helm chart defines jobs that operate on ITN network according to the playbook below.

TL;DR: The following jobs are executed:
- Optional network deployment (with previous artefacts removed, if any)
- Periodic job for restarting random node (each 6 hours)
- Periodic job for running zkApp transactions generator (each 8 hours, for 4 hours)

## Runbook Configuration

`startNetwork.enabled`: If `true`, network is restarted from scratch

## Mina Network Configuration

When network is restarted, it uses values specified in the [values] directory.

## Mina Helm Charts ConfigMap

Currently Helm charts for Mina nodes are stored as a ConfigMap resource so they
can be accessed by the job that deploys Mina artefacts. Use
[create-charts-configmap.sh](create-charts-configmap.sh) script to create and
deploy/update that ConfigMap in the current/specified namespace using Charts
from the [parent dir](..).

## Runbook

Goals
Ensure the network and testing works on a private network before running the more intensive public version
While going through the following runbook, we constantly check whether the network is in a healthy state or not, as defined above. For each transaction sent statistics are to be collected to determine the success rate and other measures.
This will be run by Viable Systems. Proofs will be on for this network.
There will be a random node restarted via the orchestrator once every 6 hours.
This network should be run with a ledger similar to mainnet (200k accounts)

Runbook:

- Start: Spin up a private cluster network with 204 nodes (30 BPs, 2 seeds, 2 snarkers, 170 standard nodes)
- Hour 4: Check all nodes are connected with the tooling
- Hour 8: Run the transaction generator at goal throughput for 4 hours
- Hour 16: Run the transaction generator at goal throughput for 4 hours
- Hour 24: Shutdown the network

