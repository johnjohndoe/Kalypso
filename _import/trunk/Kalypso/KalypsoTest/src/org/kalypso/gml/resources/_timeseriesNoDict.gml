<?xml version="1.0" encoding="UTF-8"?>
<om:ObservationCollection xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="
     http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     org.kalypso.gml.om ../schema_om.xsd
     "
     xmlns:kom="org.kalypso.gml.om" xmlns:xlink="http://www.w3.org/1999/xlink"
     xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om"
     xmlns:swe="http://www.opengis.net/swe">

     <gml:name>Wasserstandsmessung mit W/Q-Beziehung und Alarmstufen</gml:name>
     <om:time><!-- f�r uns bringts nix denke ich, angabe ist aber erforderlich sonst dok nicht g�ltig --></om:time>
     
     <om:member>
          <om:Observation gml:id="wasserstandsmessung">
               <!-- die reine Zeitreihe wird als CommonObservation
                         modelliert. Als resultDefinition kommt DataDefinition
                         zum Einsatz. Es erm�glicht die inline-Spezifikation
                         des Datenblock im result.
                          -->
               <gml:metaDataProperty gml:remoteSchema="schema_om.xsd">
                    <kom:MetaDataList>
                         <kom:MetaData>
                              <name>Gew�sser</name>
                              <value>Schwarze Elster</value>
                         </kom:MetaData>
                         <kom:MetaData>
                              <name>Kennziffer</name>
                              <value>553050</value>
                         </kom:MetaData>
                         <kom:MetaData>
                              <name>Szenario</name>
                              <value/>
                         </kom:MetaData>
                         <kom:MetaData>
                              <name>Vorhersage</name>
                              <value type="DateRangeMarker">
                                   <dateFrom>2005-05-18T11:00:00</dateFrom>
                                   <dateTo>2005-08-24T11:00:00</dateTo>
                              </value>
                         </kom:MetaData>
                         <kom:MetaData>
                              <name>OCS-ID</name>
                              <value type="URI"
                                   >kalypso-ocs:wiski://HVZ_Modellierung_Saale.Wasserstand.553050</value>
                         </kom:MetaData>
                    </kom:MetaDataList>
               </gml:metaDataProperty>
               <gml:description/>
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
                         <gml:name>L�ben</gml:name>
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
                                   <swe:property/>
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
                              sonst w�rde die automatische Formatierung die Sachen durcheinander bringen -->
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
                         weil man hier f�r die resultDefinition auf eines in einem
                         dictionnary gespeicherte Spezifikation zur�ckgreift. Das geht nicht
                         f�r CommonObservation (DataDefinitionType leitet nicht von DefinitionType ab).
                         Schade. 
                         
                         die observedProperty (hier wq-table im phenomenon dictionary) sagt 
                         was das f�r eine Observation ist: nämlich eine WQ-Table Observation
                         mit der entsprechende resultDefinition usw. 
                          -->

               <gml:description>Beinhaltet die WQ-Beziehung</gml:description>
               <om:time>
                    <!-- in time k�nnte der G�ltigkeitsbereich der WQ-Beziehung
                              kodiert sein -->
                    <gml:TimeInstant>
                         <gml:timePosition>2005-06-06+02:00</gml:timePosition>
                    </gml:TimeInstant>
               </om:time>
               <om:procedure>
                    <om:ObservationProcedure gml:id="wq-table">
                         <gml:description>KALYPSO WQ-Tabelle</gml:description>
                         <gml:name>WQ-Tabelle</gml:name>
                         <om:method/>
                    </om:ObservationProcedure>
               </om:procedure>
               <om:observedProperty>
                    <swe:Phenomenon gml:id="wq-table">
                         <gml:name>WQ-Tabelle</gml:name>
                    </swe:Phenomenon>
               </om:observedProperty>
               <om:featureOfInterest xlink:href="#pegelstation"/>
               <om:resultDefinition>
                    <swe:GridDefinition dimension="2" gml:id="wq-table">
                         <gml:name>WQ-Tabelle Datenblock Spezifikation</gml:name>
                         <swe:map>
                              <!-- map kann eine href  enthalten - der IndexArray sollte irgendwie ausgelager werden-->
                              <swe:IndexArray gml:id="wq_index" arrayLength="unbounded">
                                   <gml:name/>
                              </swe:IndexArray>
                         </swe:map>
                         <swe:tupleMap>
                              <swe:RecordDefinition gml:id="wq_rd" recordLength="2">
                                   <gml:name/>
                                   <swe:component xlink:href="#pegelhoehe"/> 
                                   <swe:component>
                                        <swe:ItemDefinition gml:id="abflussmenge">
                                             <gml:name/>
                                             <swe:property xlink:href="dict_phenomenon.xml#abfluss"/>
                                             <swe:representation>
                                                  <swe:SimpleType>
                                                       <st:restriction base="decimal" xmlns:st="http://www.seegrid.csiro.au/xml/st">
                                                            <st:minInclusive value="0.0"/>
                                                            <st:fractionDigits value="4"/>
                                                       </st:restriction>
                                                       <gml:unitOfMeasure uom="#m3s"/>
                                                  </swe:SimpleType>
                                             </swe:representation>
                                        </swe:ItemDefinition>
                                   </swe:component> 
                              </swe:RecordDefinition>
                         </swe:tupleMap>
                    </swe:GridDefinition>
               </om:resultDefinition>
               <!-- Ich nehme mal an, dass die einheit f�r die Pegelh�he in cm angegeben ist - das muss man im Dictionairy anpassen -->
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

     <!-- jetzt kommen die Alarmstufen, als einzelne Measurement -->
     <om:member>
          <om:Measurement>

               <!-- die observedProperty (hier alarmstufe im phenomenon dictionary) sagt 
                              was das f�r eine Observation ist.
                         -->

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
     </om:member>
</om:ObservationCollection>
