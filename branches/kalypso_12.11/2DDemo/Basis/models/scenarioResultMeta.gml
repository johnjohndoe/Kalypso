<?xml version="1.0" encoding="UTF-8"?><ScenarioResultMeta xmlns:gml="http://www.opengis.net/gml" xmlns:resultMeta="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase/result/meta" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:common="org.kalypso.gml.common" xmlns="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResultMeta" gml:id="root">
 <gml:name>Ergebnisse</gml:name>
 <resultMeta:path>results</resultMeta:path>
 <resultMeta:childMember>
  <CalculationUnitResultMeta gml:id="CalculationUnitResultMeta12962238875191">
   <gml:description/>
   <gml:name>Demo2D</gml:name>
   <resultMeta:path>CalculationUnit2D121299806933713174</resultMeta:path>
   <resultMeta:childMember>
    <StepResultMeta gml:id="StepResultMeta13544645380104">
     <gml:description>Instationäre Simulation zum Zeitpunkt: 02.01.10 00:00:00 GMT+01:00</gml:description>
     <gml:name>Zeitschritt (02.01.10 00:00:00 GMT+01:00)</gml:name>
     <resultMeta:path>timestep-02.01.2010_00_00_00_000_MEZ</resultMeta:path>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta13544645430451">
       <gml:description>TIN der Fließtiefen</gml:description>
       <gml:name>Fließtiefen</gml:name>
       <resultMeta:path>Tin/tin_DEPTH.gz</resultMeta:path>
       <type>tinDepth</type>
       <minValue>-10.879</minValue>
       <maxValue>8.592</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta13544645430455">
       <gml:description>TIN der Wasserspiegellagen</gml:description>
       <gml:name>Wasserspiegellagen</gml:name>
       <resultMeta:path>Tin/tin_WATERLEVEL.gz</resultMeta:path>
       <type>tinWsp</type>
       <minValue>365.164</minValue>
       <maxValue>371.015</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta135446454304512">
       <gml:description>TIN der tiefengemittelten Fließgeschwindigkeiten</gml:description>
       <gml:name>Geschwindigkeiten</gml:name>
       <resultMeta:path>Tin/tin_VELOCITY.gz</resultMeta:path>
       <type>tinVelo</type>
       <minValue>0.000</minValue>
       <maxValue>6.603</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta135446454304514">
       <gml:description>Modellknoten</gml:description>
       <gml:name>Modellknoten</gml:name>
       <resultMeta:path>results.gz</resultMeta:path>
       <type>nodes</type>
       <minValueVelo>0.000</minValueVelo>
       <maxValueVelo>6.603</maxValueVelo>
       <minValueDepth>-10.879</minValueDepth>
       <maxValueDepth>8.592</maxValueDepth>
       <minValueWaterlevel>365.164</minValueWaterlevel>
       <maxValueWaterlevel>371.015</maxValueWaterlevel>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta135446454306014">
       <gml:description>ASCII Ergebnisdatei(en) von RMA·Kalypso</gml:description>
       <gml:name>RMA-Rohdaten</gml:name>
       <resultMeta:path>original.2d.zip</resultMeta:path>
       <type>coreDataZip</type>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <stepTime>2010-01-01T23:00:00.000Z</stepTime>
     <stepNumber/>
     <isRestart/>
     <type>unsteady</type>
    </StepResultMeta>
   </resultMeta:childMember>
   <resultMeta:childMember>
    <DocumentResultMeta gml:id="DocumentResultMeta13544645430453">
     <gml:description>TIN der Modellhöhen</gml:description>
     <gml:name>Modellhöhen</gml:name>
     <resultMeta:path>model/Tin/tin_TERRAIN.gz</resultMeta:path>
     <type>tinTerrain</type>
     <minValue>359.256</minValue>
     <maxValue>377.769</maxValue>
    </DocumentResultMeta>
   </resultMeta:childMember>
   <simulationStartTime/>
   <simulationEndTime>2012-12-02T16:09:03.326Z</simulationEndTime>
   <calcUnitID>CalculationUnit2D121299806933713174</calcUnitID>
  </CalculationUnitResultMeta>
 </resultMeta:childMember>
</ScenarioResultMeta>
