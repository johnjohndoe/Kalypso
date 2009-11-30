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
               <gml:name>%LengthSectionStation_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-stat">
                         <gml:description>%LengthSectionStation_description</gml:description>
                         <gml:name>%LengthSectionStation_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="4"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="km"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSectionText">
               <gml:name>%LengthSectionText_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-text">
                         <gml:description>%LengthSectionText_description</gml:description>
                         <gml:name>%LengthSectionText_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:Word>
                         <swe:classification/>
                    </swe:Word>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSectionRunOff">
               <gml:name>%LengthSectionRunOff_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen_runoff">
                         <gml:description>%LengthSectionRunOff_description</gml:description>
                         <gml:name>%LengthSectionRunOff_name</gml:name>
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
               <gml:name>%LengthSectionWaterlevel_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen_waterlevel">
                         <gml:description>%LengthSectionWaterlevel_description</gml:description>
                         <gml:name>%LengthSectionWaterlevel_name</gml:name>
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
               <gml:name>%LengthSectionProfileType_name</gml:name>
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
               <gml:name>%LengthSectionGround_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-sohle">
                         <gml:name>%LengthSectionGround_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_hen">
               <gml:name>%LengthSection_hen_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-hen">
                         <gml:name>%LengthSection_hen_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_h_BV">
               <gml:name>%LengthSection_h_BV_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-h_bv">
                         <gml:name>%LengthSection_h_BV_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Boe_li">
               <gml:name>%LengthSection_Boe_li_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-boe_li">
                         <gml:name>%LengthSection_Boe_li_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Boe_re">
               <gml:name>%LengthSection_Boe_re_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-boe_re">
                         <gml:name>%LengthSection_Boe_re_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_v_m">
               <gml:name>%LengthSection_v_m_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-v_m">
                         <gml:name>%LengthSection_v_m_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_tau_fl">
               <gml:name>%LengthSection_tau_fl_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-tau_fl">
                         <gml:name>%LengthSection_tau_fl_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="2"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="N_m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

	<!-- Q -->
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Q_li">
               <gml:name>%LengthSection_Q_li-name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-Q_li">
                         <gml:name>%LengthSection_Q_li-name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m3_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Q_fl">
               <gml:name>%LengthSection_Q_fl_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-Q_fl">
                         <gml:name>%LengthSection_Q_fl_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m3_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_Q_re">
               <gml:name>%LengthSection_Q_re_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-Q_re">
                         <gml:name>%LengthSection_Q_re_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m3_s"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

	<!--  LAMBDA -->
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_lamb_li">
               <gml:name>%LengthSection_lamb_li_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-lam_li">
                         <gml:name>%LengthSection_lamb_li_name</gml:name>
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
               <gml:name>%LengthSection_lamb_fl_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-lam_fl">
                         <gml:name>%LengthSection_lamb_fl_name</gml:name>
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
               <gml:name>%LengthSection_lamb_re_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-lam_re">
                         <gml:name>%LengthSection_lamb_re_name</gml:name>
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
               <gml:name>%LengthSection_f_li_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-fl_li">
                         <gml:name>%LengthSection_f_li_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_f_fl">
               <gml:name>%LengthSection_f_fl_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-f_fl">
                         <gml:name>%LengthSection_f_fl_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_f_re">
               <gml:name>%LengthSection_f_re_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-f_re">
                         <gml:name>%LengthSection_f_re_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m2"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_br_li">
               <gml:name>%LengthSection_br_li_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-br_li">
                         <gml:name>%LengthSection_br_li_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_br_fl">
               <gml:name>%LengthSection_br_fl_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-br_fl">
                         <gml:name>%LengthSection_br_fl_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_br_re">
               <gml:name>%LengthSection_br_re_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-br_re">
                         <gml:name>%LengthSection_br_re_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0.0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_WeirOK">
               <gml:name>%LengthSection_WeirOK_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-wehrok">
                         <gml:name>%LengthSection_WeirOK_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_BridgeOK">
               <gml:name>%LengthSection_BridgeOK_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-brueckok">
                         <gml:name>%LengthSection_BridgeOK_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_BridgeUK">
               <gml:name>%LengthSection_BridgeUK_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-brueckuk">
                         <gml:name>%LengthSection_BridgeUK_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="mNN"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_BridgeWidth">
               <gml:name>%LengthSection_BridgeWidth_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-brueckb">
                         <gml:name>%LengthSection_BridgeWidth_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_RohrDN">
               <gml:name>%LengthSection_RohrDN_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-rohrdn">
                         <gml:name>%LengthSection_RohrDN_name</gml:name>
                    </swe:Phenomenon>
               </swe:property>
               <swe:representation>
                    <swe:SimpleType>
                         <st:restriction base="decimal">
                              <st:minInclusive value="0"/>
                              <st:fractionDigits value="3"/>
                         </st:restriction>
                         <gml:unitOfMeasure uom="m"/>
                    </swe:SimpleType>
               </swe:representation>
          </swe:ItemDefinition>
     </gml:dictionaryEntry>

     <!-- ALPHA -->
     <gml:dictionaryEntry>
          <swe:ItemDefinition gml:id="LengthSection_AlphaIW">
               <gml:name>%LengthSection_AlphaIW_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-alphaIW">
                         <gml:name>%LengthSection_AlphaIW_name</gml:name>
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
               <gml:name>%LengthSection_AlphaEW_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-alphaEW">
                         <gml:name>%LengthSection_AlphaEW_name</gml:name>
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
               <gml:name>%LengthSection_I_Reib_name</gml:name>
               <swe:property>
                    <swe:Phenomenon gml:id="phen-I_Reib">
                         <gml:name>%LengthSection_I_Reib_name</gml:name>
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
