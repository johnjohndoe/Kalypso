#!/bin/bash
CLASSPATH=.:../lib/xt.jar:../lib/xercesImpl.jar
export CLASSPATH
#ant.jar  log4j.jar  xdoclet.jar  xercesImpl.jar  xercesSamples.jar  xmlParserAPIs.jar  xt.jar

java com.jclark.xsl.sax.Driver -Dcom.jclark.xsl.sax.parser=org.apache.xerces.parsers.SAXParser ../src/xml/NA_Control.xml ../src/xsl/2idManager.xsl

