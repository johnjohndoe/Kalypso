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

     <!-- Length Section components -->
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSectionStation">
               <gml:name>Station</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-stat">
                         <gml:description>Gewässerstationierung</gml:description>
                         <gml:name>Stationierung</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#km"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSectionRunOff">
               <gml:name>Abfluss</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen_runoff">
                         <gml:description>Abfluss</gml:description>
                         <gml:name>Abfluss</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m³/s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSectionWaterlevel">
               <gml:name>Abfluss</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen_waterlevel">
                         <gml:description>Wasserstand</gml:description>
                         <gml:name>Wasserstand</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="NN+m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>


     <!--
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="pegelhoehe">
               <gml:name/>
               <swe:property xlink:href="dict_phenomenon.xml#wasserstand"/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="niederschlagsmenge">
               <gml:name/>
               <swe:property xlink:href="dict_phenomenon.xml#niederschlag"/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="0"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mm"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="abflussmenge">
               <gml:name/>
               <swe:property xlink:href="dict_phenomenon.xml#abfluss"/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="#m3s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="datum">
               <gml:name/>
               <swe:property xlink:href="dict_phenomenon.xml#datum"/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="dateTime"> </st:restriction>
                         <swe:frame xlink:href="#zz_europaberlin"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>-->

     <!-- Einfacher skalarer Typ ohne Einheit -->
     <!--
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="skalar">
               <gml:name/>
               <swe:property xlink:href="dict_phenomenon.xml#undefined"/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
-->

     <!-- Einfacher ganzzahliger Typ ohne Einheit -->
     <!--
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="ganzzahl">
               <gml:name/>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0"/>
                              <st:fractionDigits value="0"/>
                         </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
-->

     <!-- Definition der Wechmann Parameter 
     
     TODO: was bedeutet noScale?
     TODO: bessere Spezifikation der Wechmann Parameter
     
     -->

     <!-- 
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="wechmann_w1">
               <gml:name/>
               <swe:property xlink:href="dict_phenomenon.xml#wechmann_w1"/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction  base="decimal"> </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="wechmann_lnk1">
               <gml:name/>
               <swe:property xlink:href="dict_phenomenon.xml#wechmann_lnk1"/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal"> </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="wechmann_k2">
               <gml:name/>
               <swe:property xlink:href="dict_phenomenon.xml#wechmann_k2"/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal"> </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="wechmann_wgr">
               <gml:name/>
               <swe:property xlink:href="dict_phenomenon.xml#wechmann_wgr"/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal"> </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
 -->

</gml:Dictionary>
