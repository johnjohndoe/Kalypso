#!/bin/sh
#MCRROOT=/progsys/matlab/2008a
#MCRROOT=/home/uhgd0008/MCR/v78
MCRROOT=/home/uhgd0034/MCR/v78

echo unzipping...
unzip -o Gaja3dService_linux64.zip

echo setting file permissions...
chmod +x run_Gaja3dService.sh 

echo running...
./run_Gaja3dService.sh $MCRROOT $*
