#!/bin/bash
echo "#"> resourceFile.conf
echo "# do not edit - generated file">> resourceFile.conf
echo "#">> resourceFile.conf
cd ../template

ls -1 */* >> ../misc/resourceFile.conf
