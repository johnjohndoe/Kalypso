#/bin/bash
CLASSPATH=.
CLASSPATH=$CLASSPATH:edbc.jar:openroad.jar
CLASSPATH=$CLASSPATH:javaGis1-ejb.jar:javaGis2-ejb.jar:javaGis3-ejb.jar
CLASSPATH=$CLASSPATH:jbossall-client.jar:log4j.jar
echo $CLASSPATH

#java de/tuhh/wb/javagis/Main
echo " usage input.xml asciiExportPrefix rootNode "
echo "xml " $1
echo "export to " $2
echo "root-node " $3
java  de/tuhh/kalypso/data/I_FilterImpl $1 $2 $3 
#test.xml test/test_ 900

