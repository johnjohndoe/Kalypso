<?xml version="1.0" encoding="UTF-8"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:wspm="org.kalypso.model.wspm" xmlns:swe="http://www.opengis.net/swe" xmlns:ns2="http://www.tu-harburg.de/wb/kalypso/schemata/observation" xmlns:commonShp="org.kalypso.gml.common" xmlns:math="org.kalypso.gml.common.math" xmlns:om="http://www.opengis.net/om" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition12129978927911">
   <gml:description>Manuell erzeugt am: 09.06.2008 9:51</gml:description>
   <gml:name>Abfluss - Zeitreihe</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929981.7796886526 775452.0620866041 0.0</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:direction>240</op1d2d:direction>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource12129978927910">
     <gml:name>Abfluss - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition12129978952124">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2008-06-09T00%3A00%3A00.000%2B02%3A00 30 null null
2008-06-09T01%3A00%3A00.000%2B02%3A00 50 null null
2008-06-09T02%3A00%3A00.000%2B02%3A00 100 null null
2008-06-09T03%3A00%3A00.000%2B02%3A00 150 null null
2008-06-09T04%3A00%3A00.000%2B02%3A00 200 null null
2008-06-09T05%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T06%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T07%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T08%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T09%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T10%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T11%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T12%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T13%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T14%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T15%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T16%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T17%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T18%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T19%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T20%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T21%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T22%3A00%3A00.000%2B02%3A00 250 null null
2008-06-09T23%3A00%3A00.000%2B02%3A00 250 null null
2008-06-10T00%3A00%3A00.000%2B02%3A00 250.0000 null null
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
   <gml:description>Manuell erzeugt am: 09.06.2008 9:53</gml:description>
   <gml:name>Wasserstand - Zeitreihe</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929088.2594008152 773754.3590080483 0.0</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:direction/>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource12129980257914">
     <gml:name>Wasserstand - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition121299802588412">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2008-06-09T00%3A00%3A00.000%2B02%3A00 367 null null
2008-06-09T01%3A00%3A00.000%2B02%3A00 366.9 null null
2008-06-09T02%3A00%3A00.000%2B02%3A00 366.80 null null
2008-06-09T03%3A00%3A00.000%2B02%3A00 366.7 null null
2008-06-09T04%3A00%3A00.000%2B02%3A00 366.6 null null
2008-06-09T05%3A00%3A00.000%2B02%3A00 366.5 null null
2008-06-09T06%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T07%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T08%3A00%3A00.000%2B02%3A00 366.40 null null
2008-06-09T09%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T10%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T11%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T12%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T13%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T14%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T15%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T16%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T17%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T18%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T19%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T20%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T21%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T22%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-09T23%3A00%3A00.000%2B02%3A00 366.4000 null null
2008-06-10T00%3A00%3A00.000%2B02%3A00 366.4000 null null
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
