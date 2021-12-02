for i in {4..25}
do
    export DAYNUM="$i"
    envsubst < Day.hs.template > "Day$i.hs"
done
