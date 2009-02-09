#!/bin/sh
#MCR=/progsys/matlab/2007b
#MCR=/tmp/test/gaja3d/linux/MCR/v77
#./MCRInstaller.bin -console < GAJA_install.txt
#MCRROOT=/home/uhgd0008/MCR/v78
MCRROOT=/home/uhgd0034/MCR/v78
echo unzipping...
unzip -o Gaja3dService_linux64.zip
unzip -o triangle.zip -d exec
echo building...
. build.sh
echo running...
./run_Gaja3dService.sh $MCRROOT $*
rm -rf Gaja3dService_* exec
rm -f Gaja3dService Gaja3dService.ctf *.jar *.sh triangle.zip