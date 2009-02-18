#!/bin/sh
#MCR=/progsys/matlab/2007b
#MCR=/tmp/test/gaja3d/linux/MCR/v77
#./MCRInstaller.bin -console < GAJA_install.txt
#MCRROOT=/home/uhgd0008/MCR/v78

MCRROOT=/home/uhgd0034/MCR/v78

echo unzipping...
unzip -o Gaja3dService_linux64.zip

echo setting file permissions...
chmod +x exec/triangle Gaja3dService run_Gaja3dService.sh

echo running...
./run_Gaja3dService.sh $MCRROOT $*
rm -rf Gaja3dService_* exec
rm -f Gaja3dService Gaja3dService.ctf *.jar *.sh