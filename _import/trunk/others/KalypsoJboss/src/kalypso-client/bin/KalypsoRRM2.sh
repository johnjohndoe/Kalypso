#/bin/bash

#CLASSPATH=.:kalypso.jar
#CLASSPATH=$CLASSPATH:edbc.jar:openroad.jar
#CLASSPATH=$CLASSPATH:javaGis1-ejb.jar:javaGis2-ejb.jar:javaGis3-ejb.jar
#CLASSPATH=$CLASSPATH:jbcl.jar
#CLASSPATH=$CLASSPATH:jbossall-client.jar:log4j.jar
#echo $CLASSPATH

java -cp .:kalypso.jar -Xms100m -Xmx200m -Xss100m de/tuhh/wb/javagis/Main
#java de/tuhh/wb/javagis/simpleclient/Main
