#!/bin/bash
CLASSPATH=$CLASSPATH:`./files2Classpath.perl \`find /opt/jboss/jboss-3.0.3/lib/ /opt/jboss/jboss-3.0.3/client/ -name *jar\``
export CLASSPATH
echo $CLASSPATH 
ant -f $1 -Dxdoclet.force=true ejb-jar -v 2>ant.out 
echo -e "fertig\a"
less ant.out
