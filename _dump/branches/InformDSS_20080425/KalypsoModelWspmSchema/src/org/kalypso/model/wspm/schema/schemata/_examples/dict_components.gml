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

     <gml:description>Ein Dictionary für die Modellierung von
     Profilparameter</gml:description>
     <gml:name>Profilkomponente</gml:name>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="breite">
               <gml:description>Geländebreite</gml:description>
               <gml:name>Breite</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="hoehe">
               <gml:description>Geländehöhe</gml:description>
               <gml:name>Höhe</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="gkk-rechtswert">
               <gml:description>Gausskrügerkoordinaten Rechtswert</gml:description>
               <gml:name>Rechtswert</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="gkk-hochwert">
               <gml:description>Gausskrügerkoordinaten Hochwert</gml:description>
               <gml:name>Hochwert</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
          
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="bewuchs-ax">
               <gml:description>Bewuchsparameter X-Abstand</gml:description>
               <gml:name>Bewuchs AX</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="bewuchs-ay">
               <gml:description>Bewuchsparameter Y-Abstand</gml:description>
               <gml:name>Bewuchs AY</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="bewuchs-dp">
               <gml:description>Bewuchsparameter Durschnittliches Durchmesser</gml:description>
               <gml:name>Durchmesser</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="rauheit-ks">
               <gml:description>KS-Rauheitsparameter</gml:description>
               <gml:name>KS</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="rauheit-kst">
               <gml:description>KST-Rauheitsparameter</gml:description>
               <gml:name>Bewuchs AX</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="markierung-durchstroemtebereiche">
               <gml:description>Markierung Durchströmte Bereiche</gml:description>
               <gml:name>Durchströmte Bereiche</gml:name>
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="markierung-trennflaeche">
               <gml:description>Markierung Trennfläche</gml:description>
               <gml:name>Trennfläche</gml:name>
               <!--swe:property xlink:href="urn:x-ogc:def:phenomenon:OGC:Visibility"/-->
               <swe:property/>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="string">
                              <st:enumeration value="low"/>
                              <st:enumeration value="high"/>
                              <st:enumeration value="none"/>
                         </st:restriction>
                         <swe:classification xlink:href="urn:x-seegrid:definition:vocabulary:visibility"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     
</gml:Dictionary>
