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

     <gml:description>Dictionary for profile-observation components.</gml:description>
     <gml:name>Profile Component Dictionary</gml:name>

	<!-- Standard Component Types -->

     <gml:dictionaryEntry>
	        <swe:ItemDefinition gml:id="WSPM_BREITE">
		         <swe:property>
		          <swe:Phenomenon gml:id="Phenomenon_Breite">
		           <gml:description></gml:description>
		           <gml:name>Breite</gml:name>
		          </swe:Phenomenon>
		         </swe:property>
		         <swe:representation>
		          <swe:Number>
		           <gml:unitOfMeasure gml:uom="m"/>
		          </swe:Number>
		         </swe:representation>
	        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_HOEHE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Hoehe">
           <gml:description></gml:description>
           <gml:name>Höhe</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="mNN"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>


     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_RAUHEIT">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Rauheit">
           <gml:description></gml:description>
           <gml:name>Rauheit ks</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="ks"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_HOCHWERT">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Hochwert">
           <gml:description></gml:description>
           <gml:name>Hochwert</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="m"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_RECHTSWERT">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Rechtswert">
           <gml:description></gml:description>
           <gml:name>Rechtswert</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="m"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BEWUCHS_AX">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_BewuchsAX">
           <gml:description></gml:description>
           <gml:name>AX</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="m"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BEWUCHS_AY">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_BewuchsAY">
           <gml:description></gml:description>
           <gml:name>AY</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="m"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BEWUCHS_DP">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_BewuchsDP">
           <gml:description></gml:description>
           <gml:name>DP</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="m"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_UNTERKANTEBRUECKE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Brueckenunterkante">
           <gml:description></gml:description>
           <gml:name>Brückenunterkante</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="mNN"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_OBERKANTEBRUECKE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Oberkantebruecke">
           <gml:description></gml:description>
           <gml:name>Brückenoberkante</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="mNN"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_OBERKANTEWEHR">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Wehr">
           <gml:description></gml:description>
           <gml:name>Wehr</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="mNN"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

	<!-- Devider Types -->

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_DEVIDER_DURCHSTROEMTE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_DurchstroemteBereiche">
           <gml:description>Markierung Durchströmte Bereiche</gml:description>
           <gml:name>Durchströmte Bereiche</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Boolean>
          	<swe:restriction>
          		<xst:enumeration>true</xst:enumeration>
          		<xst:enumeration>false</xst:enumeration>
          	</swe:restriction>
          </swe:Boolean>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_DEVIDER_BORDVOLL">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Bordvoll">
           <gml:description>Markierung Bordvollpunkte</gml:description>
           <gml:name>Bordvollpunkte</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Boolean>
          	<swe:restriction>
          		<xst:enumeration>true</xst:enumeration>
          		<xst:enumeration>false</xst:enumeration>
          	</swe:restriction>
          </swe:Boolean>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_DEVIDER_TRENNFLAECHE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Trennflaechen">
           <gml:description>Markierung Trennflächen</gml:description>
           <gml:name>Trennflächen</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Word>
          	<swe:restriction>
          		<xst:enumeration>none</xst:enumeration>
          		<xst:enumeration>low</xst:enumeration>
          		<xst:enumeration>high</xst:enumeration>
          	</swe:restriction>
          </swe:Word>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_DEVIDER_WEHR">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_TrennlinieWehr">
           <gml:description>Markierung Trennlinie Wehr</gml:description>
           <gml:name>Trennlinie Wehr</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="m"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

	<!-- Building Components -->

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_BREITE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Breite">
           <gml:description></gml:description>
           <gml:name>größte Breite/Durchmesser</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="m"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_HOEHE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Hoehe">
           <gml:description></gml:description>
           <gml:name>Gesamthöhe [m]</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="m"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_SOHLGEFAELLE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Sohlgefaelle">
           <gml:description></gml:description>
           <gml:name>Sohlgefälle [1/1000]</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="1/1000"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_BEZUGSPUNKT_X">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_BEZUGSPUNKT_X">
           <gml:description></gml:description>
           <gml:name>Bezugspunkt Breite [m]</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="m"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_BEZUGSPUNKT_Y">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_BEZUGSPUNKT_Y">
           <gml:description></gml:description>
           <gml:name>Bezugspunkt Höhe [NN+m]</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="mNN"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_STEIGUNG">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_STEIGUNG">
           <gml:description></gml:description>
           <gml:name>Verhältnis der Dreieckseiten [1/100]</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="1/100"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_RAUHEIT">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_RAUHEIT">
           <gml:description>Rauheitsbeiwert im Durchlass</gml:description>
           <gml:name>Rauheit</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="?"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_FORMBEIWERT">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_FORMBEIWERT">
           <gml:description></gml:description>
           <gml:name>Formbeiwert</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="?"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_UNTERWASSER">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_UNTERWASSER">
           <gml:description>Höhe der Gewässersohle im Unterwasser</gml:description>
           <gml:name>Unterwasser [NN+m]</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom="?"/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_BUILDING_WEHRART">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_WEHRART">
           <gml:description>Form der Wehrkrone</gml:description>
           <gml:name>Wehrart</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <swe:representation>
          <swe:Number>
           <gml:unitOfMeasure gml:uom=""/>
          </swe:Number>
         </swe:representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

</gml:Dictionary>
