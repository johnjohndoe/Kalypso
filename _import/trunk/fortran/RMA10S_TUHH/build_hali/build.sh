#!/bin/bash
./mkmf.pl -t ifort_template -p rma-kalypso.exe ../source ../source/modules_*
make
