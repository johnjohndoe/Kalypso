#/bin bash
#echo if the application don't start you have to set the path to your JRE
java -Xms100m -Xmx200m -classpath ./classes:./lib/deegree109.jar:./lib/dom.jar:./lib/jaxp-api.jar:./lib/xalan.jar:/xsltc.jar:./lib/xercesImpl.jar:./lib/sax.jar de.latlon.Shape2GML $1 $2
