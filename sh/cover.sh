#!/bin/sh

S="80 200"
D=rgen/cover/

for i in data/image/cover/*.jpg
do
    for j in $S
    do
        mkdir -p $D/r-$j
        n=$(basename $i .jpg)
        echo "convert: $i"
        if test -f $D/r-$j/$n.jpg
        then echo "$D/r-$j/$n.jpg exists"
        else convert -resize x$j -colorspace rgb $i $D/r-$j/$n.jpg
        fi
    done
done
