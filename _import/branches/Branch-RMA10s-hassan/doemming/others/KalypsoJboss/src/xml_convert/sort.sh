#! /bin/bash
java -Dcom.jclark.xsl.sax.parser=org.apache.xerces.parsers.SAXParser \
com.jclark.xsl.sax.Driver $1 relSort.xsl tmp.xml
xmllint --format tmp.xml >$2
rm tmp.xml
