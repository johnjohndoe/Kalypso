#!/bin/bash

INSTALL=/tmp/kalypso_install
SRC=$PWD

echo -e "\n temp_dir: $INSTALL"
echo -e " src: $SRC"
mkdirhier  $INSTALL
rm -r $INSTALL/*

echo -e "\n copy jars"
cp *jar $INSTALL
cd $INSTALL

echo -e "\n unpack existing jars:"
ls *jar
smartrepeat "jar -xf %f " *jar|bash

echo -e "\n clean jars"
  rm $INSTALL/*jar

echo -e "\n copy classes from $SRC to $INSTALL"
cd $SRC
cp -r com datacenter de timeserieSelection  $INSTALL

echo -e "\n generate new jarfile"

cd $INSTALL
jar -cf /tmp/kalypso.jar *

rm -r $INSTALL/*

cp /tmp/kalypso.jar $INSTALL

echo -e "copy additional files from $SRC to $INSTALL"
 cd $SRC
 cp -r symbols xsl tutorial i18n template deegree KalypsoRRM.bat profile.conf kalypsoMain.conf $INSTALL
 cp kalypsoMain_dos.conf $INSTALL/kalypsoMain.conf

 cd $INSTALL
 zip -r /tmp/`date +%Y%b%d_%H`_kalypso.zip .

ls -larth $INSTALL
ls -larth /tmp/*kalypso.zip

