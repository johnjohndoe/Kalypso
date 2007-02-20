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

 <gml:description>Dictionary for profile-observation components. Subtype 'building'.</gml:description>
 <gml:name>Profile Building Component Dictionary</gml:name>
 
 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="BREITE">
   <gml:name>größte Breite/Durchmesser</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Breite">
     <gml:description/>
     <gml:name>größte Breite/Durchmesser</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="m"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="HOEHE">
   <gml:name>Gesamthöhe [m]</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Hoehe">
     <gml:description/>
     <gml:name>Gesamthöhe [m]</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="m"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="SOHLGEFAELLE">
   <gml:name>Sohlgefälle [1/1000]</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Sohlgefaelle">
     <gml:description/>
     <gml:name>Sohlgefälle [1/1000]</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="1/1000"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="BEZUGSPUNKT_X">
   <gml:name>Bezugspunkt Breite [m]</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_BEZUGSPUNKT_X">
     <gml:description/>
     <gml:name>Bezugspunkt Breite [m]</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="m"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="BEZUGSPUNKT_Y">
   <gml:name>Bezugspunkt Höhe [NN+m]</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_BEZUGSPUNKT_Y">
     <gml:description/>
     <gml:name>Bezugspunkt Höhe [NN+m]</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="mNN"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="STEIGUNG">
   <gml:name>Verhältnis der Dreieckseiten [1/100]</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_STEIGUNG">
     <gml:description/>
     <gml:name>Verhältnis der Dreieckseiten [1/100]</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="1/100"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="RAUHEIT">
   <gml:name>Rauheit</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_RAUHEIT">
     <gml:description>Rauheitsbeiwert im Durchlass</gml:description>
     <gml:name>Rauheit</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="?"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="FORMBEIWERT">
   <gml:name>Formbeiwert</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_FORMBEIWERT">
     <gml:description/>
     <gml:name>Formbeiwert</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="?"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="UNTERWASSER">
   <gml:name>Unterwasser [NN+m]</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_UNTERWASSER">
     <gml:description>Höhe der Gewässersohle im Unterwasser</gml:description>
     <gml:name>Unterwasser [NN+m]</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="?"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="WEHRART">
   <gml:name>Wehrart</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_WEHRART">
     <gml:description>Form der Wehrkrone</gml:description>
     <gml:name>Wehrart</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Word>
     <swe:restriction>
      <xst:enumeration value="rundkronig"/>
      <xst:enumeration value="breitkronig"/>
      <xst:enumeration value="scharfkantig"/>
      <xst:enumeration value="Beiwert"/>
     </swe:restriction>
     <swe:classification/>
    </swe:Word>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

</gml:Dictionary>
