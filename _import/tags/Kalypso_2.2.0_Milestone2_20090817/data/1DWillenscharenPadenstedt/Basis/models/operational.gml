<?xml version="1.0" encoding="WINDOWS-1252"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:swe="http://www.opengis.net/swe" xmlns:ns0="http://www.tu-harburg.de/wb/kalypso/schemata/observation" xmlns:math="org.kalypso.gml.common.math" xmlns:om="http://www.opengis.net/om" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition11784466928612">
   <gml:description>Manuell erzeugt am: 06.05.2007 12:18</gml:description>
   <gml:name>1D-Randbedingung</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552876.7786 5987232.8608 2.75</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:direction/>
   <op1d2d:observation>
    <ns0:ObservationWithSource gml:id="Timeserie11784466928612">
     <gml:name>Wasserstand - Zeitreihe </gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11784466928618">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2001-01-01T10%3A00%3A00.000%2B01%3A00 3.5
2001-01-01T11%3A00%3A00.000%2B01%3A00 3.4
2001-01-01T12%3A00%3A00.000%2B01%3A00 3.3
2001-01-01T13%3A00%3A00.000%2B01%3A00 3.2
2001-01-01T14%3A00%3A00.000%2B01%3A00 3.1
2001-01-01T15%3A00%3A00.000%2B01%3A00 3.0
]]></om:result>
     <ns0:dataSourceURI/>
    </ns0:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>BoundaryLine1D1183453696187130</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit1D1183450244625149</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>3.5</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition11822431653172">
   <gml:description>Manuell erzeugt am: 19.06.2007 10:52</gml:description>
   <gml:name>Abfluss - Zeitreihe</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553379.3268 5988867.2554 3.14</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:direction>10</op1d2d:direction>
   <op1d2d:observation>
    <ns0:ObservationWithSource gml:id="DirectedObservationWithSource11822431656448">
     <gml:name>Abfluss - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition118224316566011">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2001-01-01T10%3A00%3A00.000%2B01%3A00 5
2001-01-01T11%3A00%3A00.000%2B01%3A00 4.5
2001-01-01T12%3A00%3A00.000%2B01%3A00 4
2001-01-01T13%3A00%3A00.000%2B01%3A00 3.5
2001-01-01T14%3A00%3A00.000%2B01%3A00 3
2001-01-01T15%3A00%3A00.000%2B01%3A00 2.5
]]></om:result>
     <ns0:dataSourceURI/>
    </ns0:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>BoundaryLine1D1183453649156162</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit1D1183450244625149</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>5.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
</simBase:FlowRelationshipModel>
