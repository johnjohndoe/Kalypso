<?xml version="1.0" encoding="WINDOWS-1252"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:swe="http://www.opengis.net/swe" xmlns:ns0="http://www.tu-harburg.de/wb/kalypso/schemata/observation" xmlns:sweExt="org.kalypso.swe.ext" xmlns:om="http://www.opengis.net/om" gml:id="root">
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition11885491130310">
   <gml:description>Manuell erzeugt am: 31.08.2007 10:31</gml:description>
   <gml:name>Importierte Zeitreihe</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553931.579419048 5988987.7431585565 0.0</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:direction>175</op1d2d:direction>
   <op1d2d:observation>
    <ns0:ObservationWithSource gml:id="ObservationWithSource11885491130311">
     <gml:name>Importierte Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11885491135150">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2001-01-01T10%3A00%3A00.000%2B01%3A00 1.0
2001-01-01T20%3A00%3A00.000%2B01%3A00 1.0
2001-01-02T06%3A00%3A00.000%2B01%3A00 2.0
2001-01-02T16%3A00%3A00.000%2B01%3A00 3.0
2001-01-03T02%3A00%3A00.000%2B01%3A00 4.0
2001-01-03T12%3A00%3A00.000%2B01%3A00 6.0
2001-01-03T22%3A00%3A00.000%2B01%3A00 8.0
2001-01-04T08%3A00%3A00.000%2B01%3A00 10.0
2001-01-04T18%3A00%3A00.000%2B01%3A00 12.0
2001-01-05T04%3A00%3A00.000%2B01%3A00 17.0
2001-01-05T14%3A00%3A00.000%2B01%3A00 19.65
2001-01-06T00%3A00%3A00.000%2B01%3A00 19.65
2001-01-06T10%3A00%3A00.000%2B01%3A00 17.5
2001-01-06T20%3A00%3A00.000%2B01%3A00 15.09
2001-01-07T06%3A00%3A00.000%2B01%3A00 12.59
2001-01-07T16%3A00%3A00.000%2B01%3A00 10.09
2001-01-08T02%3A00%3A00.000%2B01%3A00 10.09
2001-01-08T12%3A00%3A00.000%2B01%3A00 7.59
2001-01-08T22%3A00%3A00.000%2B01%3A00 5.09
2001-01-09T08%3A00%3A00.000%2B01%3A00 4.09
2001-01-09T18%3A00%3A00.000%2B01%3A00 3.0
2001-01-10T04%3A00%3A00.000%2B01%3A00 2.0
2001-01-10T14%3A00%3A00.000%2B01%3A00 1.65
2001-01-11T00%3A00%3A00.000%2B01%3A00 1.65
2001-01-11T10%3A00%3A00.000%2B01%3A00 1.65
]]></om:result>
     <ns0:dataSourceURI/>
    </ns0:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D11885485622653637</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D118854986746823292</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>10.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition11885491843592">
   <gml:description>Manuell erzeugt am: 31.08.2007 10:33</gml:description>
   <gml:name>Importierte Zeitreihe</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553520.9015531396 5989132.225287814 0.0</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:direction>260</op1d2d:direction>
   <op1d2d:observation>
    <ns0:ObservationWithSource gml:id="ObservationWithSource11885491843593">
     <gml:name>Importierte Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11885491843755">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2001-01-01T10%3A00%3A00.000%2B01%3A00 1.0
2001-01-01T20%3A00%3A00.000%2B01%3A00 1.0
2001-01-02T06%3A00%3A00.000%2B01%3A00 1.0
2001-01-02T16%3A00%3A00.000%2B01%3A00 2.0
2001-01-03T02%3A00%3A00.000%2B01%3A00 3.0
2001-01-03T12%3A00%3A00.000%2B01%3A00 5.0
2001-01-03T22%3A00%3A00.000%2B01%3A00 7.0
2001-01-04T08%3A00%3A00.000%2B01%3A00 8.0
2001-01-04T18%3A00%3A00.000%2B01%3A00 10.0
2001-01-05T04%3A00%3A00.000%2B01%3A00 16.0
2001-01-05T14%3A00%3A00.000%2B01%3A00 19.2
2001-01-06T00%3A00%3A00.000%2B01%3A00 19.2
2001-01-06T10%3A00%3A00.000%2B01%3A00 17.5
2001-01-06T20%3A00%3A00.000%2B01%3A00 15.01
2001-01-07T06%3A00%3A00.000%2B01%3A00 12.51
2001-01-07T16%3A00%3A00.000%2B01%3A00 9.01
2001-01-08T02%3A00%3A00.000%2B01%3A00 9.01
2001-01-08T12%3A00%3A00.000%2B01%3A00 7.51
2001-01-08T22%3A00%3A00.000%2B01%3A00 5.01
2001-01-09T08%3A00%3A00.000%2B01%3A00 4.01
2001-01-09T18%3A00%3A00.000%2B01%3A00 3.0
2001-01-10T04%3A00%3A00.000%2B01%3A00 2.0
2001-01-10T14%3A00%3A00.000%2B01%3A00 1.48
2001-01-11T00%3A00%3A00.000%2B01%3A00 1.48
2001-01-11T10%3A00%3A00.000%2B01%3A00 1.48
]]></om:result>
     <ns0:dataSourceURI/>
    </ns0:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D118854864512512213</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D118854986746823292</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>10.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition118854972726517">
   <gml:description>Manuell erzeugt am: 31.08.2007 10:42</gml:description>
   <gml:name>Importierte Zeitreihe</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552952.3502193866 5986906.676142531 0.0</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:direction/>
   <op1d2d:observation>
    <ns0:ObservationWithSource gml:id="ObservationWithSource118854972726510">
     <gml:name>Importierte Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11885497272810">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2001-01-01T10%3A00%3A00.000%2B01%3A00 8.0
2001-01-01T20%3A00%3A00.000%2B01%3A00 7.75
2001-01-02T06%3A00%3A00.000%2B01%3A00 7.5
2001-01-02T16%3A00%3A00.000%2B01%3A00 7.25
2001-01-03T02%3A00%3A00.000%2B01%3A00 7.0
2001-01-03T12%3A00%3A00.000%2B01%3A00 6.75
2001-01-03T22%3A00%3A00.000%2B01%3A00 6.5
2001-01-04T08%3A00%3A00.000%2B01%3A00 6.25
2001-01-04T18%3A00%3A00.000%2B01%3A00 6.0
2001-01-05T04%3A00%3A00.000%2B01%3A00 5.75
2001-01-05T14%3A00%3A00.000%2B01%3A00 5.67
2001-01-06T00%3A00%3A00.000%2B01%3A00 5.67
2001-01-06T10%3A00%3A00.000%2B01%3A00 5.3
2001-01-06T20%3A00%3A00.000%2B01%3A00 5.0
2001-01-07T06%3A00%3A00.000%2B01%3A00 4.84
2001-01-07T16%3A00%3A00.000%2B01%3A00 4.84
2001-01-08T02%3A00%3A00.000%2B01%3A00 4.84
2001-01-08T12%3A00%3A00.000%2B01%3A00 4.5
2001-01-08T22%3A00%3A00.000%2B01%3A00 4.0
2001-01-09T08%3A00%3A00.000%2B01%3A00 3.9
2001-01-09T18%3A00%3A00.000%2B01%3A00 3.8
2001-01-10T04%3A00%3A00.000%2B01%3A00 3.69
2001-01-10T14%3A00%3A00.000%2B01%3A00 3.69
2001-01-11T00%3A00%3A00.000%2B01%3A00 3.69
2001-01-11T10%3A00%3A00.000%2B01%3A00 3.69
]]></om:result>
     <ns0:dataSourceURI/>
    </ns0:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D11885497045469423</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D118854986746823292</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>7.5</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
</simBase:FlowRelationshipModel>
