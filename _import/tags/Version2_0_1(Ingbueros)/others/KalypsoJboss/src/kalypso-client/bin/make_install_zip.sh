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
#cp -r symbols com datacenter de timeserieSelection  $INSTALL
cp -r com datacenter de timeserieSelection  $INSTALL

echo -e "\n generate new jarfile"

cd $INSTALL
jar -cf /tmp/kalypso.jar *

rm -r $INSTALL/*

cp /tmp/kalypso.jar $INSTALL

echo -e "copy additional files from $SRC to $INSTALL"
 cd $SRC
 cp -r sce_tool symbols xsl tutorial i18n template deegree KalypsoRRM.bat KalypsoForecast.bat profile.conf kalypsoMain.conf $INSTALL
 cp kalypsoMain_dos_na.conf $INSTALL/kalypsoMain.conf
 cp kalypso_dos.conf $INSTALL/kalypso.conf

 cd $INSTALL
 zip -r /tmp/`date +%Y%b%d_%H`_kalypso.zip .

ls -larth $INSTALL
ls -larth /tmp/*kalypso.zip

