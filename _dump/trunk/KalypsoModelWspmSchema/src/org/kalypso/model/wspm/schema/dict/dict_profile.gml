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

     <gml:description>Dictionary for profile-observation components.</gml:description>
     <gml:name>Profile Component Dictionary</gml:name>

     <gml:dictionaryEntry>
	        <swe:ItemDefinition gml:id="WSPM_BREITE">
		         <swe:property>
		          <swe:Phenomenon gml:id="Phenomenon_Breite">
		           <gml:description></gml:description>
		           <gml:name>Breite</gml:name>
		          </swe:Phenomenon>
		         </swe:property>
		         <representation xmlns="http://www.opengis.net/swe">
		          <Number>
		           <unitOfMeasure gml:uom="m"/>
		          </Number>
		         </representation>
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
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="mNN"/>
          </Number>
         </representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>


     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_RAUHEIT_KS">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_RauheitKs">
           <gml:description></gml:description>
           <gml:name>Rauheit ks</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom=""/>
          </Number>
         </representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WSPM_RAUHEIT_KST">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_RauheitKst">
           <gml:description></gml:description>
           <gml:name>Rauheit kst</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="TODO"/>
          </Number>
         </representation>
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
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="m"/>
          </Number>
         </representation>
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
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="m"/>
          </Number>
         </representation>
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
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="m"/>
          </Number>
         </representation>
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
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="m"/>
          </Number>
         </representation>
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
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="mNN"/>
          </Number>
         </representation>
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
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="mNN"/>
          </Number>
         </representation>
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
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="mNN"/>
          </Number>
         </representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

	<!-- Devider Types -->

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="DURCHSTROEMTE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_DurchstroemteBereiche">
           <gml:description>Markierung Durchströmte Bereiche</gml:description>
           <gml:name>Durchströmte Bereiche</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <representation xmlns="http://www.opengis.net/swe">
          <Boolean>
          	<restriction>
          		<enumeration>true</enumeration>
          		<enumeration>false</enumeration>
          	</restriction>
          </Boolean>
         </representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="BORDVOLL">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Bordvoll">
           <gml:description>Markierung Bordvollpunkte</gml:description>
           <gml:name>Bordvollpunkte</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <representation xmlns="http://www.opengis.net/swe">
          <Boolean>
          	<restriction>
          		<enumeration>true</enumeration>
          		<enumeration>false</enumeration>
          	</restriction>
          </Boolean>
         </representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="TRENNFLAECHE">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_Trennflaechen">
           <gml:description>Markierung Trennflächen</gml:description>
           <gml:name>Trennflächen</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <representation xmlns="http://www.opengis.net/swe">
          <Word>
          	<restriction>
          		<enumeration>none</enumeration>
          		<enumeration>low</enumeration>
          		<enumeration>high</enumeration>
          	</restriction>
          </Word>
         </representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
        <swe:ItemDefinition gml:id="WEHR">
         <swe:property>
          <swe:Phenomenon gml:id="Phenomenon_TrennlinieWehr">
           <gml:description>Markierung Trennlinie Wehr</gml:description>
           <gml:name>Trennlinie Wehr</gml:name>
          </swe:Phenomenon>
         </swe:property>
         <representation xmlns="http://www.opengis.net/swe">
          <Number>
           <unitOfMeasure gml:uom="m"/>
          </Number>
         </representation>
        </swe:ItemDefinition>
     </gml:dictionaryEntry>

</gml:Dictionary>
