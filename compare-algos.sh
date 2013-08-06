#! /bin/sh
for i in $(seq 1 $1);
do
    echo -e -n "\033[00;33mRunning for N=$i...\033[0m"
    ./sbt "run misc/test.txt "$i" /tmp/testres1.txt" 2>&1 >/dev/null;
    cmd="cat misc/test.txt | ./src/main/haskell/MapperAdjacencyList | ./src/main/haskell/ReducerAdjacencyList | "
    if [ $i -gt 1 ]
        then
        x=$(expr $i - 1)
        for k in $(seq 1 $x); do
            cmd+="./src/main/haskell/MapperIterate | ./src/main/haskell/ReducerIterate | "
        done;
    fi
    cmd+="./src/main/haskell/ReducerFinal > /tmp/testres2.txt"
    eval $cmd;
    if `comm -3 /tmp/testres1.txt /tmp/testres2.txt > /dev/null` ; then
        echo -e "\033[00;32mOutput is the same\033[0m"
    else
        echo -e "\033[00;31mOutput differs!\033[0m"
        diff /tmp/testres1.txt /tmp/testres2.txt
    fi
done
