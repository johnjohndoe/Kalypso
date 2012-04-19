<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xmlns:xst="http://www.seegrid.csiro.au/xml/st" xmlns:xlink="http://www.w3.org/1999/xlink"
 xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om"
 xmlns:swe="http://www.opengis.net/swe" gml:id="components">

 <gml:description>Dictionary for sinuositaet components.</gml:description>
 <gml:name>Sinuositaet Component Dictionary</gml:name>
 
 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="KENNUNG">
   <gml:name>%kennung.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Kennung">
     <gml:description>%kennung.description</gml:description>
     <gml:name>%kennung.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
   <swe:Word>
     <swe:restriction>
      <xst:enumeration value="eKeineBeruecksichtigung"/>
      <xst:enumeration value="eNurCmFaktor"/>
      <xst:enumeration value="eNurFliesswegVerlaengerung"/>
      <xst:enumeration value="eBeides"/>
     </swe:restriction>
     <swe:classification/>
    </swe:Word>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="SN">
   <gml:name>%sn.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_SN">
     <gml:description>%sn.description</gml:description>
     <gml:name>%sn.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="dict_uom.xml#"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="LF">
   <gml:name>%lf.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_LF">
     <gml:description>%lf.description</gml:description>
     <gml:name>%lf.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Number>
     <gml:unitOfMeasure uom="dict_uom.xml#"/>
    </swe:Number>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>

 <gml:dictionaryEntry>
  <swe:ItemDefinition gml:id="GERINNE_ART">
   <gml:name>%gerinne_art.name</gml:name>
   <swe:property>
    <swe:Phenomenon gml:id="Phenomenon_Gerinne_Art">
     <gml:description>%gerinne_art.description</gml:description>
     <gml:name>%gerinne_art.name</gml:name>
    </swe:Phenomenon>
   </swe:property>
   <swe:representation>
    <swe:Word>
     <swe:restriction>
      <xst:enumeration value="eKompakt"/>
      <xst:enumeration value="eGegliedert"/>
     </swe:restriction>
     <swe:classification/>
    </swe:Word>
   </swe:representation>
  </swe:ItemDefinition>
 </gml:dictionaryEntry>
 
</gml:Dictionary>
