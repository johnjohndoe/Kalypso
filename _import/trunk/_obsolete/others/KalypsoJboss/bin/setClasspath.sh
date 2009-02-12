#!/bin/bash
CLASSPATH=$CLASSPATH:`./files2Classpath.perl \`find /opt/jboss/jboss-3.0.3/lib/ /opt/jboss/jboss-3.0.3/client/ -name *jar\``
echo CLASSPATH=$CLASSPATH
export CLASSPATH
