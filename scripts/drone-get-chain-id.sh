until mina client status | grep "Chain id"
do
    echo "Waiting for node"
    sleep 5
done
mina client status | grep "Chain id" | tr -s " " | cut -d " " -f3 > $1