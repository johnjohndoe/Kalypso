#!/bin/sh
echo Current directory: `pwd`
export PATH=/usr/local/bin:/usr/bin:/usr/X11R6/bin:/bin
echo ld is at: `which ld`
gcc -c  -ansi -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 -I${MCRROOT}/extern/include -DUNIX -DX11 -pthread  -I./ -O ./Gaja3dService_main.c -o ./Gaja3dService_main.o
gcc -c  -ansi -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 -I${MCRROOT}/extern/include -DUNIX -DX11 -pthread  -I./ -O ./Gaja3dService_mcc_component_data.c -o ./Gaja3dService_mcc_component_data.o
gcc -O -pthread  -o ./Gaja3dService  ./Gaja3dService_main.o ./Gaja3dService_mcc_component_data.o -Wl,-rpath-link,${MCRROOT}/bin/glnxa64 -L${MCRROOT}/sys/os/glnxa64 -L${MCRROOT}/runtime/glnxa64 -L${MCRROOT}/bin/glnxa64 -lmwmclmcrrt -lm
make triangle -C exec
