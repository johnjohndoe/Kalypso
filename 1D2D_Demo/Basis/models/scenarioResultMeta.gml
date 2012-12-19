<?xml version="1.0" encoding="UTF-8"?><ScenarioResultMeta xmlns:gml="http://www.opengis.net/gml" xmlns:resultMeta="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase/result/meta" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:common="org.kalypso.gml.common" xmlns="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResultMeta" gml:id="root">
 <gml:name>Ergebnisse</gml:name>
 <resultMeta:path>results</resultMeta:path>
 <resultMeta:childMember>
  <CalculationUnitResultMeta gml:id="CalculationUnitResultMeta12125835310821">
   <gml:description>Ergebnisimporte aus dem WSPM-Projekt '1D-stationär: WSPMDemo'</gml:description>
   <gml:name>1D-stationär: WSPMDemo</gml:name>
   <resultMeta:path>restart1dStationary1212583531082</resultMeta:path>
   <resultMeta:childMember>
    <StepResultMeta gml:id="StepResultMeta12125835311292">
     <gml:description>Rechenvariante 'Rechnung 1'</gml:description>
     <gml:name>Rechnung 1</gml:name>
     <resultMeta:path>Rechnung 1</resultMeta:path>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta12125835312231">
       <gml:description>WSPM-Ergebnis: &#13;
WSPMDemo - Rechnung 1</gml:description>
       <gml:name>vektoren</gml:name>
       <resultMeta:path>results.gml</resultMeta:path>
       <type>nodes</type>
       <minValue>0.051</minValue>
       <maxValue>0.717</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <stepTime/>
     <stepNumber/>
     <isRestart>true</isRestart>
     <type>steady</type>
    </StepResultMeta>
   </resultMeta:childMember>
   <resultMeta:childMember>
    <StepResultMeta gml:id="StepResultMeta12125847459334">
     <gml:description>Rechenvariante 'Rechnung 2'</gml:description>
     <gml:name>Rechnung 2</gml:name>
     <resultMeta:path>Rechnung 2</resultMeta:path>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta12125847459339">
       <gml:description>WSPM-Ergebnis: &#13;
WSPMDemo - Rechnung 2</gml:description>
       <gml:name>vektoren</gml:name>
       <resultMeta:path>results.gml</resultMeta:path>
       <type>nodes</type>
       <minValue>0.282</minValue>
       <maxValue>1.796</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <stepTime/>
     <stepNumber/>
     <isRestart>true</isRestart>
     <type>steady</type>
    </StepResultMeta>
   </resultMeta:childMember>
   <resultMeta:childMember>
    <StepResultMeta gml:id="StepResultMeta12127360649530">
     <gml:description>Rechenvariante '250'</gml:description>
     <gml:name>250</gml:name>
     <resultMeta:path>250</resultMeta:path>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta12127360650167">
       <gml:description>WSPM-Ergebnis: &#13;
WSPMDemo - 250</gml:description>
       <gml:name>vektoren</gml:name>
       <resultMeta:path>results.gml</resultMeta:path>
       <type>nodes</type>
       <minValue>0.262</minValue>
       <maxValue>1.573</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <stepTime/>
     <stepNumber/>
     <isRestart>true</isRestart>
     <type>steady</type>
    </StepResultMeta>
   </resultMeta:childMember>
   <simulationStartTime/>
   <simulationEndTime/>
   <calcUnitID>restart1dStationary1212583531082</calcUnitID>
  </CalculationUnitResultMeta>
 </resultMeta:childMember>
 <resultMeta:childMember>
  <CalculationUnitResultMeta gml:id="CalculationUnitResultMeta12962338958266">
   <gml:description/>
   <gml:name>Demo 2D</gml:name>
   <resultMeta:path>CalculationUnit2D121439979923417593</resultMeta:path>
   <resultMeta:childMember>
    <StepResultMeta gml:id="StepResultMeta135446367708029">
     <gml:description>Instationäre Simulation zum Zeitpunkt: 26.01.11 06:00:00 GMT+01:00</gml:description>
     <gml:name>Zeitschritt (26.01.11 06:00:00 GMT+01:00)</gml:name>
     <resultMeta:path>timestep-26.01.2011_06_00_00_000_MEZ</resultMeta:path>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta135446368273558">
       <gml:description>TIN der Fließtiefen</gml:description>
       <gml:name>Fließtiefen</gml:name>
       <resultMeta:path>Tin/tin_DEPTH.gz</resultMeta:path>
       <type>tinDepth</type>
       <minValue>-10.879</minValue>
       <maxValue>7.674</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta135446368273535">
       <gml:description>TIN der Wasserspiegellagen</gml:description>
       <gml:name>Wasserspiegellagen</gml:name>
       <resultMeta:path>Tin/tin_WATERLEVEL.gz</resultMeta:path>
       <type>tinWsp</type>
       <minValue>365.531</minValue>
       <maxValue>371.015</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta135446368273516">
       <gml:description>TIN der tiefengemittelten Fließgeschwindigkeiten</gml:description>
       <gml:name>Geschwindigkeiten</gml:name>
       <resultMeta:path>Tin/tin_VELOCITY.gz</resultMeta:path>
       <type>tinVelo</type>
       <minValue>0.000</minValue>
       <maxValue>1.752</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta135446368273559">
       <gml:description>Modellknoten</gml:description>
       <gml:name>Modellknoten</gml:name>
       <resultMeta:path>results.gz</resultMeta:path>
       <type>nodes</type>
       <minValueVelo>0.000</minValueVelo>
       <maxValueVelo>1.752</maxValueVelo>
       <minValueDepth>-10.879</minValueDepth>
       <maxValueDepth>7.674</maxValueDepth>
       <minValueWaterlevel>365.531</minValueWaterlevel>
       <maxValueWaterlevel>371.015</maxValueWaterlevel>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta135446368275026">
       <gml:description>ASCII Ergebnisdatei(en) von RMA·Kalypso</gml:description>
       <gml:name>RMA-Rohdaten</gml:name>
       <resultMeta:path>original.2d.zip</resultMeta:path>
       <type>coreDataZip</type>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <stepTime>2011-01-26T05:00:00.000Z</stepTime>
     <stepNumber/>
     <isRestart/>
     <type>unsteady</type>
    </StepResultMeta>
   </resultMeta:childMember>
   <resultMeta:childMember>
    <DocumentResultMeta gml:id="DocumentResultMeta135446368273547">
     <gml:description>TIN der Modellhöhen</gml:description>
     <gml:name>Modellhöhen</gml:name>
     <resultMeta:path>model/Tin/tin_TERRAIN.gz</resultMeta:path>
     <type>tinTerrain</type>
     <minValue>359.256</minValue>
     <maxValue>377.769</maxValue>
    </DocumentResultMeta>
   </resultMeta:childMember>
   <simulationStartTime/>
   <simulationEndTime>2012-12-02T15:54:42.796Z</simulationEndTime>
   <calcUnitID>CalculationUnit2D121439979923417593</calcUnitID>
  </CalculationUnitResultMeta>
 </resultMeta:childMember>
 <resultMeta:childMember>
  <CalculationUnitResultMeta gml:id="CalculationUnitResultMeta12962342032615">
   <gml:name>Demo 1D</gml:name>
   <resultMeta:path>CalculationUnit1D12125830336860</resultMeta:path>
   <resultMeta:childMember>
    <StepResultMeta gml:id="StepResultMeta1296648840121148">
     <gml:description>Stationäre Simulation</gml:description>
     <gml:name>Stationär</gml:name>
     <resultMeta:path>steady</resultMeta:path>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta129664884160174">
       <gml:description>1D-Längsschnitt</gml:description>
       <gml:name>Längsschnitt Demo 1D</gml:name>
       <resultMeta:path>lengthSection_CalculationUnit1D12125830336860.gml</resultMeta:path>
       <type>lengthSection</type>
       <minValue>0.000</minValue>
       <maxValue>0.000</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta1296648841601162">
       <gml:description>Modellknoten</gml:description>
       <gml:name>Modellknoten</gml:name>
       <resultMeta:path>results.gz</resultMeta:path>
       <type>nodes</type>
       <minValueVelo>0.284</minValueVelo>
       <maxValueVelo>0.699</maxValueVelo>
       <minValueDepth>4.194</minValueDepth>
       <maxValueDepth>10.215</maxValueDepth>
       <minValueWaterlevel>367.100</minValueWaterlevel>
       <maxValueWaterlevel>367.203</maxValueWaterlevel>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta12966488420070">
       <gml:description>ASCII Ergebnisdatei(en) von RMA·Kalypso</gml:description>
       <gml:name>RMA-Rohdaten</gml:name>
       <resultMeta:path>original.2d.zip</resultMeta:path>
       <type>coreDataZip</type>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <stepTime/>
     <stepNumber/>
     <isRestart>true</isRestart>
     <type>steady</type>
    </StepResultMeta>
   </resultMeta:childMember>
   <simulationStartTime/>
   <simulationEndTime>2011-02-02T12:14:02.084Z</simulationEndTime>
   <calcUnitID>CalculationUnit1D12125830336860</calcUnitID>
  </CalculationUnitResultMeta>
 </resultMeta:childMember>
 <resultMeta:childMember>
  <CalculationUnitResultMeta gml:id="CalculationUnitResultMeta12964809967909">
   <gml:name>Demo 1D2D</gml:name>
   <resultMeta:path>CalculationUnit1D2D12144639393734308</resultMeta:path>
   <resultMeta:childMember>
    <StepResultMeta gml:id="StepResultMeta1296654463000381">
     <gml:description>Instationäre Simulation zum Zeitpunkt: 07.01.11 07:00:00 MEZ</gml:description>
     <gml:name>Zeitschritt (07.01.11 07:00:00 MEZ)</gml:name>
     <resultMeta:path>timestep-07.01.2011_07_00_00_000_MEZ</resultMeta:path>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta1296654467620121">
       <gml:description>1D-Längsschnitt</gml:description>
       <gml:name>Längsschnitt Demo 1D</gml:name>
       <resultMeta:path>lengthSection_CalculationUnit1D12125830336860.gml</resultMeta:path>
       <type>lengthSection</type>
       <minValue>0.000</minValue>
       <maxValue>0.000</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta1296654467620309">
       <gml:description>Modellknoten</gml:description>
       <gml:name>Modellknoten</gml:name>
       <resultMeta:path>results.gz</resultMeta:path>
       <type>nodes</type>
       <minValueVelo>0.000</minValueVelo>
       <maxValueVelo>2.234</maxValueVelo>
       <minValueDepth>-10.879</minValueDepth>
       <maxValueDepth>10.887</maxValueDepth>
       <minValueWaterlevel>365.531</minValueWaterlevel>
       <maxValueWaterlevel>371.015</maxValueWaterlevel>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta129665446820568">
       <gml:description>ASCII Ergebnisdatei(en) von RMA·Kalypso</gml:description>
       <gml:name>RMA-Rohdaten</gml:name>
       <resultMeta:path>original.2d.zip</resultMeta:path>
       <type>coreDataZip</type>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <stepTime>2011-01-07T06:00:00.000Z</stepTime>
     <stepNumber/>
     <isRestart/>
     <type>unsteady</type>
    </StepResultMeta>
   </resultMeta:childMember>
   <resultMeta:childMember>
    <DocumentResultMeta gml:id="DocumentResultMeta1296655327124277">
     <gml:description>Ganglinien des Teilmodells Demo 1D2D</gml:description>
     <gml:name>Ganglinien</gml:name>
     <resultMeta:path>hydrograph/hydrograph.gml</resultMeta:path>
     <type>hydrograph</type>
    </DocumentResultMeta>
   </resultMeta:childMember>
   <resultMeta:childMember>
    <StepResultMeta gml:id="StepResultMeta129665612850329">
     <gml:description>Instationäre Simulation zum Zeitpunkt: 07.01.11 02:00:00 MEZ</gml:description>
     <gml:name>Zeitschritt (07.01.11 02:00:00 MEZ)</gml:name>
     <resultMeta:path>timestep-07.01.2011_02_00_00_000_MEZ</resultMeta:path>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta1296656136829444">
       <gml:description>1D-Längsschnitt</gml:description>
       <gml:name>Längsschnitt Demo 1D</gml:name>
       <resultMeta:path>lengthSection_CalculationUnit1D12125830336860.gml</resultMeta:path>
       <type>lengthSection</type>
       <minValue>0.000</minValue>
       <maxValue>0.000</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta1296656136830481">
       <gml:description>TIN der Fließtiefen</gml:description>
       <gml:name>Fließtiefen</gml:name>
       <resultMeta:path>Tin/tin_DEPTH.gz</resultMeta:path>
       <type>tinDepth</type>
       <minValue>-10.879</minValue>
       <maxValue>10.937</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta1296656136830296">
       <gml:description>TIN der Wasserspiegellagen</gml:description>
       <gml:name>Wasserspiegellagen</gml:name>
       <resultMeta:path>Tin/tin_WATERLEVEL.gz</resultMeta:path>
       <type>tinWsp</type>
       <minValue>365.531</minValue>
       <maxValue>371.015</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta1296656136830224">
       <gml:description>TIN der tiefengemittelten Fließgeschwindigkeiten</gml:description>
       <gml:name>Geschwindigkeiten</gml:name>
       <resultMeta:path>Tin/tin_VELOCITY.gz</resultMeta:path>
       <type>tinVelo</type>
       <minValue>0.000</minValue>
       <maxValue>2.217</maxValue>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta1296656136830550">
       <gml:description>Modellknoten</gml:description>
       <gml:name>Modellknoten</gml:name>
       <resultMeta:path>results.gz</resultMeta:path>
       <type>nodes</type>
       <minValueVelo>0.000</minValueVelo>
       <maxValueVelo>2.217</maxValueVelo>
       <minValueDepth>-10.879</minValueDepth>
       <maxValueDepth>10.937</maxValueDepth>
       <minValueWaterlevel>365.531</minValueWaterlevel>
       <maxValueWaterlevel>371.015</maxValueWaterlevel>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <resultMeta:childMember>
      <DocumentResultMeta gml:id="DocumentResultMeta1296656137405394">
       <gml:description>ASCII Ergebnisdatei(en) von RMA·Kalypso</gml:description>
       <gml:name>RMA-Rohdaten</gml:name>
       <resultMeta:path>original.2d.zip</resultMeta:path>
       <type>coreDataZip</type>
      </DocumentResultMeta>
     </resultMeta:childMember>
     <stepTime>2011-01-07T01:00:00.000Z</stepTime>
     <stepNumber/>
     <isRestart/>
     <type>unsteady</type>
    </StepResultMeta>
   </resultMeta:childMember>
   <resultMeta:childMember>
    <DocumentResultMeta gml:id="DocumentResultMeta1296656136830340">
     <gml:description>TIN der Modellhöhen</gml:description>
     <gml:name>Modellhöhen</gml:name>
     <resultMeta:path>model/Tin/tin_TERRAIN.gz</resultMeta:path>
     <type>tinTerrain</type>
     <minValue>356.958</minValue>
     <maxValue>377.769</maxValue>
    </DocumentResultMeta>
   </resultMeta:childMember>
   <simulationStartTime/>
   <simulationEndTime>2011-02-02T14:15:37.463Z</simulationEndTime>
   <calcUnitID>CalculationUnit1D2D12144639393734308</calcUnitID>
  </CalculationUnitResultMeta>
 </resultMeta:childMember>
</ScenarioResultMeta>
