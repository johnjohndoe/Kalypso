#/bin/bash
CLASSPATH=.
CLASSPATH=$CLASSPATH:edbc.jar:openroad.jar
CLASSPATH=$CLASSPATH:braju.jar
CLASSPATH=$CLASSPATH:javaGis1-ejb.jar:javaGis2-ejb.jar:javaGis3-ejb.jar
CLASSPATH=$CLASSPATH:hwssystem-ejb.jar
CLASSPATH=$CLASSPATH:hwslogistics-ejb.jar
CLASSPATH=$CLASSPATH:hwsszenario-ejb.jar
CLASSPATH=$CLASSPATH:hwscombination-ejb.jar

CLASSPATH=$CLASSPATH:jbcl.jar
CLASSPATH=$CLASSPATH:jbossall-client.jar
CLASSPATH=$CLASSPATH:log4j.jar
echo $CLASSPATH
export CLASSPATH
java  -Xms100m -Xmx200m -Xss100m de/tuhh/wb/javagis/Main
#java de/tuhh/wb/javagis/simpleclient/Main
