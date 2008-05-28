<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd"
     xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml"
     xmlns:swe="http://www.opengis.net/swe" gml:id="resultdefinitions">

     <gml:name>Resultdefinitions für Querprofile</gml:name>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="querprofil_maulprofil">
               <gml:name>Maulprofildefinition</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <list xmlns="http://www.seegrid.csiro.au/xml/st">
                              <simpleType>
                                   <restriction base="double"/>
                              </simpleType>
                         </list>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

</gml:Dictionary>
