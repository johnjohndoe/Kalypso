#!/bin/bash
# CVS -> Deegree
for pfad in `find -name CVS`
do
 new=`echo $pfad | sed -e s/CVS/troubadixcvs/g`
 mv $pfad $new
done;

for pfad in `find -name deegreecvs`
do
 new=`echo $pfad | sed -e s/deegreecvs/CVS/g`
 mv $pfad $new
done;
