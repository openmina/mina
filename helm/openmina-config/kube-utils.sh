#!/usr/bin/env sh

set -e

usage() {
    cat <<EOF
Usage:
$0 <command> [<args> ...]
EOF
}

KUBECTL="kubectl"

frontend_port() {
    $KUBECTL get namespace/"$1" --output=jsonpath="{.metadata.annotations['openmina\.com/testnet\.nodePort']}"
}

wait_for_job_status() {
    RESOURCE=$1
    TIMEOUT=$2
    kubectl wait --for=jsonpath='{.status.conditions[*].status}'=True "$RESOURCE" --timeout="$TIMEOUT"
    MSG=$(kubectl get "$RESOURCE" -o jsonpath='{.status.conditions[?(@.status=="True")].message}')
    TYPE=$(kubectl get "$RESOURCE" -o jsonpath='{.status.conditions[?(@.status=="True")].type}')
    if [ "${TYPE}" != "Complete" ]; then echo "Error running job: ${MSG}"; exit 1; fi
}

CMD=$1
shift

if [ -z "$CMD" ]; then
    usage
    exit 1
fi

case "$CMD" in
    "frontend-port")
        frontend_port "$@"
    ;;
    "wait-for-job-status")
        wait_for_job_status "$@"
    ;;
    *)
        echo "No such command $CMD"
        usage
        exit 1
    ;;
esac
