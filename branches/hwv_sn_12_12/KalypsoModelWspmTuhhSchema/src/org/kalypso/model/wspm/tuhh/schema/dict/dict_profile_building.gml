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
   <gml:name>%breite.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Breite">
     <gml:description>%breite.description</gml:description>
     <gml:name>%breite.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="dict_uom.xml#m"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="HOEHE">
   <gml:name>%hoehe.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Hoehe">
     <gml:description>%hoehe.description</gml:description>
     <gml:name>%hoehe.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="dict_uom.xml#m"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="SOHLGEFAELLE">
   <gml:name>%sohlgefaelle.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Sohlgefaelle">
     <gml:description>%wehrart.description</gml:description>
     <gml:name>%wehrart.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="dict_uom.xml#‰"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="BEZUGSPUNKT_X">
   <gml:name>%bezugspunkt_x.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_BEZUGSPUNKT_X">
     <gml:description>%bezugspunkt_x.description</gml:description>
     <gml:name>%bezugspunkt_x.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="dict_uom.xml#m"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="BEZUGSPUNKT_Y">
   <gml:name>%bezugspunkt_y.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_BEZUGSPUNKT_Y">
        <gml:description>%bezugspunkt_y.description</gml:description>
     <gml:name>%bezugspunkt_y.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="STEIGUNG">
   <gml:name>%steigung.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_STEIGUNG">
     <gml:description>%steigung.description</gml:description>
     <gml:name>%steigung.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="dict_uom.xml#1/m"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="RAUHEIT">
   <gml:name>%rauheit.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_RAUHEIT">
     <gml:description>%rauheit.description</gml:description>
     <gml:name>%rauheit.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom=""/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="FORMBEIWERT">
   <gml:name>%formbeiwert.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_FORMBEIWERT">
     <gml:description/>
     <gml:name>%formbeiwert.name</gml:name>
          <gml:description>%formbeiwert.description</gml:description>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom=""/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="UNTERWASSER">
   <gml:name>%unterwasser.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_UNTERWASSER">
     <gml:description>%unterwasser.description</gml:description>
     <gml:name>%unterwasser.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="WEHRART">
   <gml:name>%wehrart.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_WEHRART">
     <gml:description>%wehrart.description</gml:description>
     <gml:name>%wehrart.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Word>
     <swe:restriction>
      <xst:pattern value=".*"/>
     </swe:restriction>
     <swe:classification/>
    </swe:Word>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>
 
</gml:Dictionary>
