<?xml version="1.0" encoding="ISO-8859-1"?>
<om:ObservationCollection xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="
     http://www.opengis.net/om ../../../../../../KalypsoContributionsOGC31/etc/schemas/original/om/1.0.30/om.xsd
     http://www.opengis.net/gml ../../../../../../KalypsoContributionsOGC31/etc/schemas/original/gml/3.1.1/base/gml.xsd
     http://www.w3.org/1999/xlink ../../../../../../KalypsoContributionsOGC31/etc/schemas/original/gml/3.1.1/xlink/xlinks.xsd
     http://www.opengis.net/swe ../../../../../../KalypsoContributionsOGC31/etc/schemas/original/sweCommon/1.0.30/swe.xsd
     http://www.ksp.org/om ../../../../../../KalypsoContributionsOGC31/etc/schemas/original/kalypso/omExtensions.xsd
     "
     xmlns:kom="http://www.ksp.org/om" xmlns:xlink="http://www.w3.org/1999/xlink"
     xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om"
     xmlns:swe="http://www.opengis.net/swe">
     
     <gml:name>Wasserstandsmessung mit W/Q-Beziehung und Alarmstufen</gml:name>
     <om:time><!-- für uns bringts nix denke ich, angabe ist aber erforderlich sonst dok nicht gültig --></om:time>
     
     <om:member>
          <om:Observation gml:id="wasserstandsmessung">
               <!-- die reine Zeitreihe wird als CommonObservation
                    modelliert. Als resultDefinition kommt DataDefinition
                    zum Einsatz. Es ermöglicht die inline-Spezifikation
                    des Datenblock im result.
               -->
               <gml:metaDataProperty gml:remoteSchema="schema_om.xsd">
                    <kom:MetaData>
                         <name>Gewässer</name>
                         <value>Schwarze Elster</value>
                    </kom:MetaData>
               </gml:metaDataProperty>
               <gml:metaDataProperty gml:remoteSchema="schema_om.xsd">
                    <kom:MetaData>
                         <name>Kennziffer</name>
                         <value>553050</value>
                    </kom:MetaData>
               </gml:metaDataProperty>
               <gml:metaDataProperty gml:remoteSchema="schema_om.xsd">
                    <kom:MetaData>
                         <name>Szenario</name>
                         <value/>
                    </kom:MetaData>
               </gml:metaDataProperty>
               <gml:metaDataProperty gml:remoteSchema="schema_om.xsd">
                    <kom:MetaData>
                         <name>Vorhersage</name>
                         <value type="DateRangeMarker">
                              <dateFrom>2005-05-18T11:00:00</dateFrom>
                              <dateTo>2005-08-24T11:00:00</dateTo>
                         </value>
                    </kom:MetaData>
               </gml:metaDataProperty>
               <gml:metaDataProperty gml:remoteSchema="schema_om.xsd">
                    <kom:MetaData>
                         <name>OCS-ID</name>
                         <value type="URI"
                              >kalypso-ocs:wiski://HVZ_Modellierung_Saale.Wasserstand.553050</value>
                    </kom:MetaData>
               </gml:metaDataProperty>
               <gml:description>Eine kurze Beschreibung</gml:description>
               <gml:name>Bad Liebenwerda.W.15</gml:name>
               <om:time/>
               <om:procedure>
                    <om:ObservationProcedure gml:id="wiski">
                         <gml:description>Schnittstelle zum Wiski Webdata Provider (WDP) der Firma
                              Kisters AG, Aachen</gml:description>
                         <gml:name>Wiski</gml:name>
                         <om:method/>
                    </om:ObservationProcedure>
               </om:procedure>
               <om:observedProperty>
                    <swe:Phenomenon gml:id="wasserstand">
                         <gml:name>Wasserstand</gml:name>
                    </swe:Phenomenon>
               </om:observedProperty>
               <om:featureOfInterest>
                    <om:Station gml:id="pegelstation">
                         <!-- das Feature (featureOfInterest) -->
                         <gml:description/>
                         <gml:name>Löben</gml:name>
                         <om:position>
                              <!-- TODO: ist die Frage ob man mit dem SRS so als URN kodiert zu Recht kommt -->
                              <gml:Point srsName="urn:kalypso:def:crs:EPSG:5:31468">
                                   <gml:pos>4596940 5710280</gml:pos>
                              </gml:Point>
                         </om:position>
                         <om:procedureHosted xlink:href="#wasserstand"/>
                    </om:Station>
               </om:featureOfInterest>
               <om:resultDefinition>
                    <swe:RecordDefinition recordLength="2" gml:id="rd">
                         <gml:name/>
                         <swe:component>
                              <swe:ItemDefinition gml:id="datum">
                                   <gml:name/>
                                   <swe:property>
                                        <swe:Phenomenon gml:id="pDatum">
                                             <gml:name>Datum</gml:name>
                                        </swe:Phenomenon>
                                   </swe:property>
                                   <swe:representation>
                                        <swe:SimpleType>
                                             <st:restriction base="dateTime"
                                                  xmlns:st="http://www.seegrid.csiro.au/xml/st"/>
                                             <swe:frame xlink:href="#zz_europaberlin"/>
                                        </swe:SimpleType>
                                   </swe:representation>
                              </swe:ItemDefinition>
                         </swe:component>
                         <swe:component>
                              <swe:ItemDefinition gml:id="pegelhoehe">
                                   <gml:name/>
                                   <swe:property xlink:href="#wasserstand"/>
                                   <swe:representation>
                                        <swe:SimpleType>
                                             <st:restriction base="decimal"
                                                  xmlns:st="http://www.seegrid.csiro.au/xml/st">
                                                  <st:minInclusive value="0.0"/>
                                                  <st:fractionDigits value="2"/>
                                             </st:restriction>
                                             <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                                        </swe:SimpleType>
                                   </swe:representation>
                              </swe:ItemDefinition>
                         </swe:component>
                    </swe:RecordDefinition>
               </om:resultDefinition>
               <om:result>
                    <!-- wir benutzen ein CDATA-Block damit der Layout so bleibt, 
                         sonst würde die automatische Formatierung die Sachen durcheinander bringen -->
                    <![CDATA[
2004-04-18T12:03:04Z 170.0
2004-04-19T12:03:04Z 154.8
2004-04-20T12:03:04Z 153.9
]]></om:result>
          </om:Observation>
     </om:member>
     
     <om:member>
          
          <om:Observation gml:id="wq_table1">
               
               <!-- die WQ-Beziehung wird als Observation modelliert
                    weil man hier für die resultDefinition auf eines in einem
                    dictionnary gespeicherte Spezifikation zurückgreift. Das geht nicht
                    für CommonObservation (DataDefinitionType leitet nicht von DefinitionType ab).
                    Schade. 
                    
                    die observedProperty (hier wq-table im phenomenon dictionary) sagt 
                    was das für eine Observation ist: nämlich eine WQ-Table Observation
                    mit der entsprechende resultDefinition usw. 
               -->
               
               <gml:description>Beinhaltet die WQ-Beziehung</gml:description>
               <om:time>
                    <!-- in time könnte der Gültigkeitsbereich der WQ-Beziehung
                         kodiert sein -->
                    <gml:TimeInstant>
                         <gml:timePosition>2005-06-06+02:00</gml:timePosition>
                    </gml:TimeInstant>
               </om:time>
               <om:procedure>
                    <om:ObservationProcedure gml:id="proc-wq-table">
                         <gml:description>KALYPSO WQ-Tabelle</gml:description>
                         <gml:name>WQ-Tabelle</gml:name>
                         <om:method/>
                    </om:ObservationProcedure>
               </om:procedure>
               <om:observedProperty>
                    <swe:Phenomenon gml:id="phen-wq-table">
                         <gml:name>WQ-Tabelle</gml:name>
                    </swe:Phenomenon>
               </om:observedProperty>
               <om:featureOfInterest xlink:href="#pegelstation"/>
               <om:resultDefinition>
                    <swe:RecordDefinition gml:id="wq_rd" recordLength="2">
                         <gml:name/>
                         <swe:component xlink:href="#pegelhoehe"/>
                         <swe:component>
                              <swe:ItemDefinition gml:id="abflussmenge">
                                   <gml:name/>
                                   <swe:property xlink:href="dict_phenomenon.xml#abfluss"/>
                                   <swe:representation>
                                        <swe:SimpleType>
                                             <st:restriction base="decimal"
                                                  xmlns:st="http://www.seegrid.csiro.au/xml/st">
                                                  <st:minInclusive value="0.0"/>
                                                  <st:fractionDigits value="4"/>
                                             </st:restriction>
                                             <gml:unitOfMeasure uom="#m3s"/>
                                        </swe:SimpleType>
                                   </swe:representation>
                              </swe:ItemDefinition>
                         </swe:component>
                    </swe:RecordDefinition>
               </om:resultDefinition>
               <!-- Ich nehme mal an, dass die einheit für die Pegelhöhe in cm angegeben ist - das muss man im Dictionairy anpassen -->
               <om:result><![CDATA[
-69.0 0.0
-68.0 0.0
-67.0 0.0
-66.0 0.0
-65.0 0.0
-64.0 0.0010
-63.0 0.0020
-62.0 0.0030
-61.0 0.0040
-60.0 0.0050 
]]>
               </om:result>
          </om:Observation>
     </om:member>
     
     <!--om:member>
          <om:Measurement>
          
          <gml:name>Alarmstufe 1</gml:name>
          <om:time/>
          <om:procedure>
          <om:ObservationProcedure gml:id="alarmstufeneinteilung">
          <gml:name/>
          <om:method/>
          </om:ObservationProcedure>
          </om:procedure>
          <om:observedProperty>
          <swe:Phenomenon gml:id="alarmstufe">
          <gml:name>Alarmstufe</gml:name>
          </swe:Phenomenon>
          </om:observedProperty>
          <om:featureOfInterest xlink:href="#pegelstation"/>
          <om:result uom="dict_uom.xml#cmaP">20</om:result>
          </om:Measurement>
          </om:member>
          <om:member>
          <om:Measurement>
          <gml:name>Alarmstufe 2</gml:name>
          <om:time/>
          <om:procedure xlink:href="#alarmstufeneinteilung"/>
          <om:observedProperty xlink:href="#alarmstufe"/>
          <om:featureOfInterest xlink:href="#pegelstation"/>
          <om:result uom="dict_uom.xml#cmaP">30</om:result>
          </om:Measurement>
          </om:member>
          <om:member>
          <om:Measurement>
          <gml:name>Alarmstufe 3</gml:name>
          <om:time/>
          <om:procedure xlink:href="#alarmstufeneinteilung"/>
          <om:observedProperty xlink:href="#alarmstufe"/>
          <om:featureOfInterest xlink:href="#pegelstation"/>
          <om:result uom="dict_uom.xml#cmaP">40</om:result>
          </om:Measurement>
          </om:member>
          <om:member>
          <om:Measurement>
          <gml:name>Alarmstufe 4</gml:name>
          <om:time/>
          <om:procedure xlink:href="#alarmstufeneinteilung"/>
          <om:observedProperty xlink:href="#alarmstufe"/>
          <om:featureOfInterest xlink:href="#pegelstation"/>
          <om:result uom="dict_uom.xml#cmaP">50</om:result>
          </om:Measurement>
          </om:member-->
</om:ObservationCollection>
