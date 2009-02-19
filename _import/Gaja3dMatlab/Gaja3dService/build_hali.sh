#!/bin/bash
eval `/usr/local/bin/modulecmd bash add matlab/2008a`
eval `/usr/local/bin/modulecmd bash add intel`

mcc -F ../Gaja3dService.prj

MCRROOT=/progsys/matlab/2008a
icc -c -ansi -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 -I${MCRROOT}/extern/include -DUNIX -DX11 -pthread -I./ -O src/Gaja3dService_main.c -o src/Gaja3dService_main.o
icc -c  -ansi -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 -I${MCRROOT}/extern/include -DUNIX -DX11 -pthread  -I./ -O src/Gaja3dService_mcc_component_data.c -o src/Gaja3dService_mcc_component_data.o
icc -O3 -pthread  -o src/Gaja3dService  src/Gaja3dService_main.o src/Gaja3dService_mcc_component_data.o -L${MCRROOT}/bin/glnxa64 -L${MCRROOT}/sys/os/glnxa64 -L${MCRROOT}/runtime/glnxa64 -L${MCRROOT}/bin/glnxa64 -lmwmclmcrrt -lm -I${MCRROOT}/extern/include
icc -static -O3 -openmp -DLINUX -Isrc_triangle -I/usr/X11R6/include -L/usr/X11R6/lib -lm -o src_triangle/exec/triangle src_triangle/triangle.c

