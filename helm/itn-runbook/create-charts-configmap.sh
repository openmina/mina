#!/usr/bin/env sh

set -e

usage() {
    cat <<EOF
Usage:
$0 [NAMESPACE]

Creates ConfigMap with Mina Helm charts in the specified namespace NAMESPACE.
EOF
}

if [ $# -gt 2 ]; then
    usage
    exit 1
fi

case $1 in
    -h|--help)
        usage
        exit
    ;;
    "")
    ;;
    *)
        NAMESPACE="--namespace $1"
    ;;
esac

ROOT=$(dirname $0)
CHARTS=$(mktemp -d)
for chart in seed-node block-producer snark-worker plain-node; do
    helm $NAMESPACE package $ROOT/../$chart --destination $CHARTS
done
kubectl $NAMESPACE create configmap itn-helm-charts --from-file=$CHARTS --output=yaml --dry-run | kubectl $NAMESPACE apply -f -
rm -rf $CHARTS
