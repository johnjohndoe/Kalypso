<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
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
               <gml:name>Wasserstand</gml:name>
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

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSectionProfileType">
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
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSectionGround">
               <gml:name>Sohlhöhe</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-sohle">
                         <gml:name>Sohlhöhe</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_hen">
               <gml:name>Energielinie</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-hen">
                         <gml:name>Energielinie</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_h_BV">
               <gml:name>Bordvoll Höhe</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-h_bv">
                         <gml:name>Bordvoll Höhe</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Boe_li">
               <gml:name>Böschung (links)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-boe_li">
                         <gml:name>Böschung (links)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Boe_re">
               <gml:name>Böschung (rechts)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-boe_re">
                         <gml:name>Böschung (rechts)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_v_m">
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
          <swe:ItemDefinition gml:id="LengthSection_tau_fl">
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

	<!-- Q -->
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Q_li">
               <gml:name>Abfluss (links)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-Q_li">
                         <gml:name>Abfluss (links)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m3_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Q_fl">
               <gml:name>Abfluss (Flussschlauch)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-Q_fl">
                         <gml:name>Abfluss (Flussschlauch)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m3_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Q_re">
               <gml:name>Abfluss (rechts)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-Q_re">
                         <gml:name>Abfluss (rechts)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m3_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

	<!--  LAMBDA -->
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_lamb_li">
               <gml:name>lambda (links)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-lam_li">
                         <gml:name>lambda (lins)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_lamb_fl">
               <gml:name>lambda (Flussschlauch)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-lam_fl">
                         <gml:name>lambda (Flussschlauch)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_lamb_re">
               <gml:name>lambda (rechts)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-lam_re">
                         <gml:name>lambda (rechts)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_f_li">
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
          <swe:ItemDefinition gml:id="LengthSection_f_fl">
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
          <swe:ItemDefinition gml:id="LengthSection_f_re">
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
          <swe:ItemDefinition gml:id="LengthSection_br_li">
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
          <swe:ItemDefinition gml:id="LengthSection_br_fl">
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
          <swe:ItemDefinition gml:id="LengthSection_br_re">
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
          <swe:ItemDefinition gml:id="LengthSection_WeirOK">
               <gml:name>Wehr (Oberkante)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-wehrok">
                         <gml:name>Wehr (Oberkante)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_BridgeOK">
               <gml:name>Brücke (Oberkante)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-brueckok">
                         <gml:name>Brücke (Oberkante)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_BridgeUK">
               <gml:name>Brücke (Unterkante)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-brueckuk">
                         <gml:name>Brücke (Unterkante)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_BridgeWidth">
               <gml:name>Brücke (Breite in Fließrichtung)</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-brueckb">
                         <gml:name>Brücke (Breite in Fließrichtung)</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_RohrDN">
               <gml:name>Rohrdurchmesser</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-rohrdn">
                         <gml:name>Rohrdurchmesser</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <!-- ALPHA -->
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_AlphaIW">
               <gml:name>Impulsstrombeiwert</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-alphaIW">
                         <gml:name>Impulsstrombeiwert</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0"/>
                              <st:fractionDigits value="5"/>
                         </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_AlphaEW">
               <gml:name>Energiestrombeiwert</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-alphaEW">
                         <gml:name>Energiestrombeiwert</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0"/>
                              <st:fractionDigits value="5"/>
                         </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <!-- I_REIB -->
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_I_Reib">
               <gml:name>Reibungsgefälle</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-I_Reib">
                         <gml:name>Reibungsgefälle</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="5"/>
                         </st:restriction>
                         <swe:noScale>true</swe:noScale>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

</gml:Dictionary>
