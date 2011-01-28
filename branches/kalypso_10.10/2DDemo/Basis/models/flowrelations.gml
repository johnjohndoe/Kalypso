<?xml version="1.0" encoding="UTF-8"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:om="http://www.opengis.net/om" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:swe="http://www.opengis.net/swe" xmlns:ns2="http://www.tu-harburg.de/wb/kalypso/schemata/observation" xmlns:sweExt="org.kalypso.swe.ext" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:wspm="org.kalypso.model.wspm" xmlns:commonShp="org.kalypso.gml.common" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:math="org.kalypso.gml.common.math" gml:id="root">
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition12129978927911">
   <gml:name>Abfluss - Zeitreihe</gml:name>
   <simBase:position>
    <gml:Point srsName="EPSG:31467" srsDimension="3">
     <gml:pos>3929981.7796886526 775452.0620866041 0.0</gml:pos>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:hasDirection>true</op1d2d:hasDirection>
   <op1d2d:direction>240</op1d2d:direction>
   <op1d2d:inflowVelocity/>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource12129978927910">
     <gml:name>Abfluss - Zeitreihe</gml:name>
     <om:time/>
     <om:procedure/>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:featureOfInterest/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition12129978952124">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <sweExt:ordinalNumberComponent/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2009-12-31T23%3A00%3A00.000Z 30 null null
2010-01-01T00%3A00%3A00.000Z 50 null null
2010-01-01T01%3A00%3A00.000Z 100 null null
2010-01-01T02%3A00%3A00.000Z 150 null null
2010-01-01T03%3A00%3A00.000Z 200 null null
2010-01-01T04%3A00%3A00.000Z 250 null null
2010-01-01T05%3A00%3A00.000Z 300 null null
2010-01-01T06%3A00%3A00.000Z 350 null null
2010-01-01T07%3A00%3A00.000Z 400 null null
2010-01-01T08%3A00%3A00.000Z 450 null null
2010-01-01T09%3A00%3A00.000Z 500 null null
2010-01-01T10%3A00%3A00.000Z 550 null null
2010-01-01T11%3A00%3A00.000Z 600 null null
2010-01-01T12%3A00%3A00.000Z 600 null null
2010-01-01T13%3A00%3A00.000Z 600 null null
2010-01-01T14%3A00%3A00.000Z 600 null null
2010-01-01T15%3A00%3A00.000Z 600 null null
2010-01-01T16%3A00%3A00.000Z 600 null null
2010-01-01T17%3A00%3A00.000Z 600 null null
2010-01-01T18%3A00%3A00.000Z 600 null null
2010-01-01T19%3A00%3A00.000Z 600 null null
2010-01-01T20%3A00%3A00.000Z 600 null null
2010-01-01T21%3A00%3A00.000Z 600 null null
2010-01-01T22%3A00%3A00.000Z 600 null null
2010-01-01T23%3A00%3A00.000Z 600 null null
]]></om:result>
     <ns2:dataSourceURI/>
    </ns2:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D12129965069019013</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D121299806933713174</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>30.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition12129980257915">
   <gml:name>Wasserstand - Zeitreihe</gml:name>
   <simBase:position>
    <gml:Point srsName="EPSG:31467" srsDimension="3">
     <gml:pos>3929088.2594008152 773754.3590080483 0.0</gml:pos>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:hasDirection/>
   <op1d2d:direction/>
   <op1d2d:inflowVelocity/>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource12129980257914">
     <gml:name>Wasserstand - Zeitreihe</gml:name>
     <om:time/>
     <om:procedure/>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:featureOfInterest/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition121299802588412">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <sweExt:ordinalNumberComponent/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2009-12-31T23%3A00%3A00.000Z 367 null null
2010-01-01T00%3A00%3A00.000Z 366.9 null null
2010-01-01T01%3A00%3A00.000Z 366.8 null null
2010-01-01T02%3A00%3A00.000Z 366.7 null null
2010-01-01T03%3A00%3A00.000Z 366.6 null null
2010-01-01T04%3A00%3A00.000Z 366.5 null null
2010-01-01T05%3A00%3A00.000Z 366.4 null null
2010-01-01T06%3A00%3A00.000Z 366.3 null null
2010-01-01T07%3A00%3A00.000Z 366.2 null null
2010-01-01T08%3A00%3A00.000Z 366.1 null null
2010-01-01T09%3A00%3A00.000Z 366 null null
2010-01-01T10%3A00%3A00.000Z 365.9 null null
2010-01-01T11%3A00%3A00.000Z 365.8 null null
2010-01-01T12%3A00%3A00.000Z 365.7 null null
2010-01-01T13%3A00%3A00.000Z 365.6 null null
2010-01-01T14%3A00%3A00.000Z 365.5 null null
2010-01-01T15%3A00%3A00.000Z 365.4 null null
2010-01-01T16%3A00%3A00.000Z 365.3 null null
2010-01-01T17%3A00%3A00.000Z 365.2 null null
2010-01-01T18%3A00%3A00.000Z 365.1 null null
2010-01-01T19%3A00%3A00.000Z 365 null null
2010-01-01T20%3A00%3A00.000Z 365 null null
2010-01-01T21%3A00%3A00.000Z 365 null null
2010-01-01T22%3A00%3A00.000Z 365 null null
2010-01-01T23%3A00%3A00.000Z 365 null null
]]></om:result>
     <ns2:dataSourceURI/>
    </ns2:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D1212996538682737</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D121299806933713174</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>367.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
</simBase:FlowRelationshipModel>
