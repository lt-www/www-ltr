#!/bin/sh

S=80
D=rgen/cover/r-$S
T="x"$S

mkdir -p $D

for i in data/image/cover/*.jpg
do
    n=$(basename $i .jpg)
    echo "convert: $i"
    if test -f $D/$n.jpg
    then echo "$D/$n.jpg exists"
    else convert -resize $T -colorspace rgb $i $D/$n.jpg
    fi
done
