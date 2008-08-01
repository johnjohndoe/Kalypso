<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="
     http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd"
     xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om"
     xmlns:swe="http://www.opengis.net/swe" gml:id="components">
     <gml:description>Ein Dictionary für die Modellierung von Längsschnitten</gml:description>
     <gml:name>LS-komponente</gml:name>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Stat">
               <gml:name>Station</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-stat">
                         <gml:name>Stationierung</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#km"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Kenn">
               <gml:name>Kennung</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-kenn">
                         <gml:name>Kennung</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:Word>
                         <swe:restriction>
                              <st:pattern value="n|b|w|t|k|e|m|i"/>
                         </swe:restriction>
                         <swe:classification>foo</swe:classification>
                    </swe:Word>
                    <!--swe:Word>
                         <swe:classification gml:remoteSchema="dict_kennung.xml"/>
                    </swe:Word-->
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Abfluss">
               <gml:name>Abfluss</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-abfluss">
                         <gml:name>Abfluss</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m3s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Sohle">
               <gml:name>Sohlhöhe</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-sohle">
                         <gml:name>Sohlhöhe</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="h_WSP">
               <gml:name>Höhe WSP</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-h_wsp">
                         <gml:name>Höhe WSP</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="hen">
               <gml:name>Energielinie</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-hen">
                         <gml:name>Energielinie</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="h_BV">
               <gml:name>Bordvoll Höhe</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-h_bv">
                         <gml:name>Bordvoll Höhe</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Boe_li">
               <gml:name>Böschung (links)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-boe_li">
                         <gml:name>Böschung (links)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="Boe_re">
               <gml:name>Böschung (rechts)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-boe_re">
                         <gml:name>Böschung (rechts)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="v_m">
               <gml:name>Mittlere Geschwindigkeit</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-v_m">
                         <gml:name>Mittlere Geschwindigkeit</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="tau_fl">
               <gml:name>Sohlschubspannung (Flussschlauch)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-tau_fl">
                         <gml:name>Sohlschubspannung (Flussschlauch)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#N_m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="lamb_li">
               <gml:name>lambda (links)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-lamb_li">
                         <gml:name>lambda (links)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
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
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="lamb_fl">
               <gml:name>lambda (Flussschlauch)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-lam_fl">
                         <gml:name>lambda (Flussschlauch)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
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
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="lamb_re">
               <gml:name>lambda (rechts)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-lam_re">
                         <gml:name>lambda (rechts)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
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
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="f_li">
               <gml:name>Fläche (links)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-fl_li">
                         <gml:name>Fläche (links)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="f_fl">
               <gml:name>Fläche (Flussschlauch)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-f_fl">
                         <gml:name>Fläche (Flussschlauch)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="f_re">
               <gml:name>Fläche (rechts)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-f_re">
                         <gml:name>Fläche (rechts)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="br_li">
               <gml:name>Breite (links)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-br_li">
                         <gml:name>Breite (links)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="br_fl">
               <gml:name>Breite (Flussschlauch)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-br_fl">
                         <gml:name>Breite (Flussschlauch)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="br_re">
               <gml:name>Breite (rechts)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-br_re">
                         <gml:name>Breite (rechts)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="WehrOK">
               <gml:name>Wehr (Oberkante)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-wehrok">
                         <gml:name>Wehr (Oberkante)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="-1000.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="BrueckOK">
               <gml:name>Brücke (Oberkante)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-brueckok">
                         <gml:name>Brücke (Oberkante)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="-1000.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="BrueckUK">
               <gml:name>Brücke (Unterkante)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-brueckuk">
                         <gml:name>Brücke (Unterkante)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="-1000.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="BrueckB">
               <gml:name>Brücke (Breite in Fließrichtung)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-brueckb">
                         <gml:name>Brücke (Breite in Fließrichtung)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="-1000.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="RohrDN">
               <gml:name>Rohrdurchmesser</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-rohrdn">
                         <gml:name>Rohrdurchmesser</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="-1000.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
</gml:Dictionary>

















