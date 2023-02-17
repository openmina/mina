PORT=31319

while curl -sf $(echo "http://1.k8.openmina.com:${PORT}") > /dev/null
do
    echo "Port $PORT in use"
    ((PORT=PORT+1))
    sleep 1
done

echo $PORT > $1