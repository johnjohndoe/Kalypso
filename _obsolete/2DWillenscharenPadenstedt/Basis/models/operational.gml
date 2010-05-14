<?xml version="1.0" encoding="WINDOWS-1252"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:swe="http://www.opengis.net/swe" xmlns:ns0="http://www.tu-harburg.de/wb/kalypso/schemata/observation" xmlns:math="org.kalypso.gml.common.math" xmlns:om="http://www.opengis.net/om" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <gml:name>bc</gml:name>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition11829266814898">
   <gml:description>Manuell erzeugt am: 27.06.2007 8:44</gml:description>
   <gml:name>2D - Q - Randbedingung Padenstedt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553931.579419048 5988987.7431585565 0.0</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:direction>175</op1d2d:direction>
   <op1d2d:observation>
    <ns0:ObservationWithSource gml:id="DirectedObservationWithSource11829266818208">
     <gml:name>Abfluss - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11829266818409">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2001-01-01T09%3A00%3A00.000%2B01%3A00 1
2001-01-01T19%3A00%3A00.000%2B01%3A00 1
2001-01-02T05%3A00%3A00.000%2B01%3A00 1
2001-01-02T15%3A00%3A00.000%2B01%3A00 2
2001-01-03T01%3A00%3A00.000%2B01%3A00 3
2001-01-03T11%3A00%3A00.000%2B01%3A00 4
2001-01-03T21%3A00%3A00.000%2B01%3A00 6
2001-01-04T07%3A00%3A00.000%2B01%3A00 8
2001-01-04T17%3A00%3A00.000%2B01%3A00 10
2001-01-05T00%3A00%3A00.000%2B01%3A00 12
2001-01-05T10%3A00%3A00.000%2B01%3A00 17
2001-01-05T20%3A00%3A00.000%2B01%3A00 19.650
2001-01-06T06%3A00%3A00.000%2B01%3A00 19.650
2001-01-06T16%3A00%3A00.000%2B01%3A00 19.650
]]></om:result>
     <ns0:dataSourceURI/>
    </ns0:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>BoundaryLine11834851751719096</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D118348399423422960</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>10.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition118292687788241">
   <gml:description>Manuell erzeugt am: 27.06.2007 8:47</gml:description>
   <gml:name>Abfluss - Zeitreihe</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553520.9015531396 5989132.225287814 0.0</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:direction>260</op1d2d:direction>
   <op1d2d:observation>
    <ns0:ObservationWithSource gml:id="DirectedObservationWithSource11829268778825">
     <gml:name>Abfluss - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition118292687789215">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2001-01-01T09%3A00%3A00.000%2B01%3A00 1
2001-01-01T19%3A00%3A00.000%2B01%3A00 1
2001-01-02T05%3A00%3A00.000%2B01%3A00 1
2001-01-02T15%3A00%3A00.000%2B01%3A00 1
2001-01-03T01%3A00%3A00.000%2B01%3A00 2
2001-01-03T11%3A00%3A00.000%2B01%3A00 3
2001-01-03T21%3A00%3A00.000%2B01%3A00 5
2001-01-04T07%3A00%3A00.000%2B01%3A00 7
2001-01-04T17%3A00%3A00.000%2B01%3A00 8
2001-01-05T00%3A00%3A00.000%2B01%3A00 10
2001-01-05T10%3A00%3A00.000%2B01%3A00 16
2001-01-05T20%3A00%3A00.000%2B01%3A00 19.2
2001-01-06T06%3A00%3A00.000%2B01%3A00 19.2
2001-01-06T16%3A00%3A00.000%2B01%3A00 19.200
]]></om:result>
     <ns0:dataSourceURI/>
    </ns0:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>BoundaryLine11834851360461731</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D118348399423422960</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>10.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition118292704961028">
   <gml:description>Manuell erzeugt am: 27.06.2007 8:50</gml:description>
   <gml:name>2D - H Willenscharen</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552954.345548825 5986891.391847097 0.0</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:direction/>
   <op1d2d:observation>
    <ns0:ObservationWithSource gml:id="ObservationWithSource118292704961012">
     <gml:name>Wasserstand - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition118292704962138">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2001-01-01T09%3A00%3A00.000%2B01%3A00 8.3
2001-01-01T19%3A00%3A00.000%2B01%3A00 8.2
2001-01-02T05%3A00%3A00.000%2B01%3A00 7.75
2001-01-02T15%3A00%3A00.000%2B01%3A00 7.5
2001-01-03T01%3A00%3A00.000%2B01%3A00 7.25
2001-01-03T11%3A00%3A00.000%2B01%3A00 7
2001-01-03T21%3A00%3A00.000%2B01%3A00 6.75
2001-01-04T07%3A00%3A00.000%2B01%3A00 6.5
2001-01-04T17%3A00%3A00.000%2B01%3A00 6.25
2001-01-05T00%3A00%3A00.000%2B01%3A00 6.00
2001-01-05T10%3A00%3A00.000%2B01%3A00 5.75
2001-01-05T20%3A00%3A00.000%2B01%3A00 5.67
2001-01-06T06%3A00%3A00.000%2B01%3A00 5.69
2001-01-06T16%3A00%3A00.000%2B01%3A00 5.67
]]></om:result>
     <ns0:dataSourceURI/>
    </ns0:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>BoundaryLine118348521703124364</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D118348399423422960</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>8.2</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
</simBase:FlowRelationshipModel>
