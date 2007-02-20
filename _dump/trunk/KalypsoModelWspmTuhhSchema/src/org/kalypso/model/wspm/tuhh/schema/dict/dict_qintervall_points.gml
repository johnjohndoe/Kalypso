<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="
     http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd"
     xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:xlink="http://www.w3.org/1999/xlink"
     xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om"
     xmlns:swe="http://www.opengis.net/swe" gml:id="components">

     <gml:description>Ein Dictionary für die Modellierung von Zeitreihendaten</gml:description>
     <gml:name>Zeitreihenkomponente</gml:name>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Waterlevel">
               <gml:description>WSP [NN+m]</gml:description>
               <gml:name>WSP</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-wsp">
                         <gml:description>WSP [NN+m]</gml:description>
                         <gml:name>WSP</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Depth">
               <gml:description>Tiefe [m]</gml:description>
               <gml:name>Tiefe</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-depth">
                         <gml:description>Tiefe [m]</gml:description>
                         <gml:name>Tiefe</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Area">
               <gml:description>Fläche [m²]</gml:description>
               <gml:name>Fläche</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-area">
                         <gml:description>Fläche [m²]</gml:description>
                         <gml:name>Fläche</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Runoff">
               <gml:description>Q [m³/s]</gml:description>
               <gml:name>Q</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-runoff">
                         <gml:description>Q [m³/s]</gml:description>
                         <gml:name>Q</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m3_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Alpha">
               <gml:description>Alpha [-]</gml:description>
               <gml:name>Alpha</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-alpha">
                         <gml:description>Alpha [-]</gml:description>
                         <gml:name>Alpha</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom=""/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="DeltaArea">
               <gml:description>Delta Fläche [m²]</gml:description>
               <gml:name>Delata Fläche</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-deltaArea">
                         <gml:description>Delta Fläche [m²]</gml:description>
                         <gml:name>Delta Fläche</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="DeltaRunoff">
               <gml:description>Delta Q [m³/s]</gml:description>
               <gml:name>Delta Q</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-deltaRunoff">
                         <gml:description>Delta Q [m³/s]</gml:description>
                         <gml:name>Delta Q</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m3_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="DeltaAlpha">
               <gml:description>Delta Alpha [-]</gml:description>
               <gml:name>Delta Alpha</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-deltaAlpha">
                         <gml:description>Delta Alpha [-]</gml:description>
                         <gml:name>Delta Alpha [-]</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom=""/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

</gml:Dictionary>
