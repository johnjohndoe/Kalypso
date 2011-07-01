#!/bin/bash
ZIP_FILE="$GRID_PROCESS.zip"
if [ -e $ZIP_FILE ]; then
	unzip $ZIP_FILE
	rm -f $ZIP_FILE
fi
chmod +x $GRID_PROCESS
(((./$GRID_PROCESS $* | tee $GRID_PROCESS.out) 3>&1 1>&2 2>&3 | tee $GRID_PROCESS.err) 3>&1 1>&2 2>&3)
