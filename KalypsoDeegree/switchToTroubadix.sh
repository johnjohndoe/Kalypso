#!/bin/bash
# CVS -> troubadix
for pfad in `find -name CVS`
do
 new=`echo $pfad | sed -e s/CVS/deegreecvs/g`
 mv $pfad $new
done;
for pfad in `find -name troubadixcvs`
do
 new=`echo $pfad | sed -e s/troubadixcvs/CVS/g`
 mv $pfad $new
done;
