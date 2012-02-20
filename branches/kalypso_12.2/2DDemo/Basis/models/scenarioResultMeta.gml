<?xml version="1.0" encoding="UTF-8"?>
<resultMeta1d2d:ScenarioResultMeta xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:resultMeta1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResultMeta" xmlns:commonShp="org.kalypso.gml.common" gml:id="root">
 <gml:name>Ergebnisse</gml:name>
 <resultMeta:path xmlns:resultMeta="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase/result/meta">results</resultMeta:path>
 <resultMeta:childMember xmlns:resultMeta="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase/result/meta">
  <resultMeta1d2d:CalculationUnitResultMeta gml:id="CalculationUnitResultMeta12962238875191">
   <gml:description/>
   <gml:name>Demo2D</gml:name>
   <resultMeta:path>CalculationUnit2D121299806933713174</resultMeta:path>
   <resultMeta:childMember>
    <resultMeta1d2d:StepResultMeta gml:id="StepResultMeta12962306640332">
     <gml:description>Instationäre Simulation zum Zeitpunkt: 02.01.10 00:00:00 MEZ</gml:description>
     <gml:name>Zeitschritt (02.01.10 00:00:00 MEZ)</gml:name>
     <resultMeta:path>timestep-02.01.2010_00_00_00_000_MEZ</resultMeta:path>
     <resultMeta:childMember>
      <resultMeta1d2d:DocumentResultMeta gml:id="DocumentResultMeta12962306801780">
       <gml:description>TIN der Fließtiefen</gml:description>
       <gml:name>Fließtiefen</gml:name>
       <resultMeta:path>Tin/tin_DEPTH.gz</resultMeta:path>
       <resultMeta1d2d:type>tinDepth</resultMeta1d2d:type>
       <resultMeta1d2d:minValue>-10.879</resultMeta1d2d:minValue>
       <resultMeta1d2d:maxValue>8.592</resultMeta1d2d:maxValue>
      </resultMeta1d2d:DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <resultMeta1d2d:DocumentResultMeta gml:id="DocumentResultMeta12962306801786">
       <gml:description>TIN der Wasserspiegellagen</gml:description>
       <gml:name>Wasserspiegellagen</gml:name>
       <resultMeta:path>Tin/tin_WATERLEVEL.gz</resultMeta:path>
       <resultMeta1d2d:type>tinWsp</resultMeta1d2d:type>
       <resultMeta1d2d:minValue>365.030</resultMeta1d2d:minValue>
       <resultMeta1d2d:maxValue>371.015</resultMeta1d2d:maxValue>
      </resultMeta1d2d:DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <resultMeta1d2d:DocumentResultMeta gml:id="DocumentResultMeta12962306801783">
       <gml:description>TIN der tiefengemittelten Fließgeschwindigkeiten</gml:description>
       <gml:name>Geschwindigkeiten</gml:name>
       <resultMeta:path>Tin/tin_VELOCITY.gz</resultMeta:path>
       <resultMeta1d2d:type>tinVelo</resultMeta1d2d:type>
       <resultMeta1d2d:minValue>0.000</resultMeta1d2d:minValue>
       <resultMeta1d2d:maxValue>6.606</resultMeta1d2d:maxValue>
      </resultMeta1d2d:DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <resultMeta1d2d:DocumentResultMeta gml:id="DocumentResultMeta12962306801781">
       <gml:description>Modellknoten</gml:description>
       <gml:name>Modellknoten</gml:name>
       <resultMeta:path>results.gz</resultMeta:path>
       <resultMeta1d2d:type>nodes</resultMeta1d2d:type>
       <resultMeta1d2d:minValueVelo>0.000</resultMeta1d2d:minValueVelo>
       <resultMeta1d2d:maxValueVelo>6.606</resultMeta1d2d:maxValueVelo>
       <resultMeta1d2d:minValueDepth>-10.879</resultMeta1d2d:minValueDepth>
       <resultMeta1d2d:maxValueDepth>8.592</resultMeta1d2d:maxValueDepth>
       <resultMeta1d2d:minValueWaterlevel>365.030</resultMeta1d2d:minValueWaterlevel>
       <resultMeta1d2d:maxValueWaterlevel>371.015</resultMeta1d2d:maxValueWaterlevel>
      </resultMeta1d2d:DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <resultMeta1d2d:DocumentResultMeta gml:id="DocumentResultMeta12962306807264">
       <gml:description>ASCII Ergebnisdatei(en) von RMA·Kalypso</gml:description>
       <gml:name>RMA-Rohdaten</gml:name>
       <resultMeta:path>original.2d.zip</resultMeta:path>
       <resultMeta1d2d:type>coreDataZip</resultMeta1d2d:type>
      </resultMeta1d2d:DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta1d2d:stepTime>2010-01-01T23:00:00.000Z</resultMeta1d2d:stepTime>
     <resultMeta1d2d:stepNumber/>
     <resultMeta1d2d:isRestart/>
     <resultMeta1d2d:type>unsteady</resultMeta1d2d:type>
    </resultMeta1d2d:StepResultMeta>
   </resultMeta:childMember>
   <resultMeta:childMember>
    <resultMeta1d2d:DocumentResultMeta gml:id="DocumentResultMeta12962306801785">
     <gml:description>TIN der Modellhöhen</gml:description>
     <gml:name>Modellhöhen</gml:name>
     <resultMeta:path>model/Tin/tin_TERRAIN.gz</resultMeta:path>
     <resultMeta1d2d:type>tinTerrain</resultMeta1d2d:type>
     <resultMeta1d2d:minValue>359.256</resultMeta1d2d:minValue>
     <resultMeta1d2d:maxValue>377.769</resultMeta1d2d:maxValue>
    </resultMeta1d2d:DocumentResultMeta>
   </resultMeta:childMember>
   <resultMeta1d2d:simulationStartTime/>
   <resultMeta1d2d:simulationEndTime>2011-01-28T16:04:40.777Z</resultMeta1d2d:simulationEndTime>
   <resultMeta1d2d:calcUnitID>CalculationUnit2D121299806933713174</resultMeta1d2d:calcUnitID>
  </resultMeta1d2d:CalculationUnitResultMeta>
 </resultMeta:childMember>
</resultMeta1d2d:ScenarioResultMeta>
