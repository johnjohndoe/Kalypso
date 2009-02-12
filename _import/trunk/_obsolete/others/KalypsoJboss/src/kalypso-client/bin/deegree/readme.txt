*****************************************************************************
*									    
*		deegree command line Shape to GML converter 
*									    
* copyright: lat/lon							    
*            Meckenheimer Alle 176					    
*            53115 Bonn							    
*	     www.lat-lon.de						    
*            								    
*            Date: 25.03.2002						    
*									    
*****************************************************************************

The usage of the application is quite simple. Just call the program with a Shapefile (without extension) as first parameter and the
desired outputfile as second parameter:

java -Xms100m -Xmx500m -classpath .\classes;.\lib\deegree109.jar;.\lib\dom.jar;.\lib\jaxp-api.jar;.\lib\xalan.jar;\xsltc.jar;.\lib\xercesImpl.jar;.\lib\sax.jar de.latlon.Shape2GML ./data/worldcities out.xml