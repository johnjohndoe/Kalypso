#!/bin/bash
# CVS -> deegreecvs
for pfad in `find -name CVS`
do
 new=`echo $pfad | sed -e s/CVS/deegreecvs/g`
 mv $pfad $new
done;
