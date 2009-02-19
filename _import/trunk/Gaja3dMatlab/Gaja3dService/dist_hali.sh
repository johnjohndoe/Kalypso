#!/bin/bash
DIST=hali/dist

rm -rf $DIST
mkdir $DIST
mkdir $DIST/exec

cp -r ../*.jar src/Gaja3dService src/Gaja3dService.ctf src/run_Gaja3dService.sh $DIST 
cp -r src_triangle/triangle $DIST/exec

OLD=$PWD
cd $DIST
rm ../Gaja3dService_linux64.zip
zip -r ../Gaja3dService_linux64.zip *
cd $OLD
