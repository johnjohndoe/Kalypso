type readme.txt
echo if the application don't start you have to set the path to your JRE

echo "usage : runShape2GML <shapefile> <output.gml>"
echo " write shapefile without extension, e.g. hydrotope instead of hydrotope.shp"
java -Xms100m -Xmx500m -classpath .\classes;.\lib\deegree109.jar;.\lib\dom.jar;.\lib\jaxp-api.jar;.\lib\xalan.jar;\xsltc.jar;.\lib\xercesImpl.jar;.\lib\sax.jar de.latlon.Shape2GML %1 %2