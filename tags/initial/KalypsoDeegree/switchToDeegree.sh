#!/bin/bash
# CVS -> troubadixcvs
for pfad in `find -name CVS`
do
 new=`echo $pfad | sed -e s/CVS/troubadixcvs/g`
 mv $pfad $new
done;
