<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="
     http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd"
 xmlns:xst="http://www.seegrid.csiro.au/xml/st" xmlns:xlink="http://www.w3.org/1999/xlink"
 xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om"
 xmlns:swe="http://www.opengis.net/swe" gml:id="components">

 <gml:description>Dictionary for profile-observation components. Subtype 'marker'.</gml:description>
 <gml:name>Profile Marker Component Dictionary</gml:name>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="DURCHSTROEMTE">
   <gml:name>Durchströmte Bereiche</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_DurchstroemteBereiche">
     <gml:description>Markierung Durchströmte Bereiche</gml:description>
     <gml:name>Durchströmte Bereiche</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Boolean>
     <swe:restriction>
      <xst:enumeration value="true"/>
      <xst:enumeration value="false"/>
     </swe:restriction>
    </swe:Boolean>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="BORDVOLL">
   <gml:name>Bordvollpunkte</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Bordvoll">
     <gml:description>Markierung Bordvollpunkte</gml:description>
     <gml:name>Bordvollpunkte</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Boolean>
     <swe:restriction>
      <xst:enumeration value="true"/>
      <xst:enumeration value="false"/>
     </swe:restriction>
    </swe:Boolean>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="TRENNFLAECHE">
   <gml:name>Trennflächen</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Trennflaechen">
     <gml:description>Markierung Trennflächen</gml:description>
     <gml:name>Trennflächen</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Word>
     <swe:restriction>
      <xst:enumeration value="none"/>
      <xst:enumeration value="low"/>
      <xst:enumeration value="high"/>
     </swe:restriction>
     <swe:classification/>
    </swe:Word>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="WEHR">
   <gml:name>Trennlinie Wehr</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_TrennlinieWehr">
     <gml:description>Markierung Trennlinie Wehr</gml:description>
     <gml:name>Trennlinie Wehr</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="m"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

</gml:Dictionary>
