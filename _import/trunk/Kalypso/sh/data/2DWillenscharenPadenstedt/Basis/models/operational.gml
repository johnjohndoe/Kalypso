<?xml version="1.0" encoding="WINDOWS-1252"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:swe="http://www.opengis.net/swe" xmlns:om="http://www.opengis.net/om" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition11783103249870">
   <gml:description>Manuell erzeugt am: 04.05.2007 22:25</gml:description>
   <gml:name>2D - Q - Randbedingung</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gts" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553520.2905955096 5989132.363293992 3.8321888689519565</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:timeserie>
    <op1d2d:Timeserie gml:id="Timeserie11783103249871">
     <gml:name>Abfluss - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11783103256381">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result>&lt;![CDATA[2001-01-01T09:00:00.000+01:00 1
2001-01-01T19:00:00.000+01:00 1
2001-01-02T05:00:00.000+01:00 1
2001-01-02T15:00:00.000+01:00 1
2001-01-03T01:00:00.000+01:00 2
2001-01-03T11:00:00.000+01:00 3
2001-01-03T21:00:00.000+01:00 5
2001-01-04T07:00:00.000+01:00 7
2001-01-04T17:00:00.000+01:00 8
2001-01-05T03:00:00.000+01:00 10
2001-01-05T13:00:00.000+01:00 16
2001-01-05T23:00:00.000+01:00 19.2
2001-01-06T09:00:00.000+01:00 19.2
2001-01-06T19:00:00.000+01:00 19.2
]]&gt;</om:result>
     <op1d2d:dataSourceURI/>
    </op1d2d:Timeserie>
   </op1d2d:timeserie>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition117831042718516">
   <gml:description>Manuell erzeugt am: 04.05.2007 22:27</gml:description>
   <gml:name>2D - Q - Randbedingung</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gts" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553931.607230981 5988987.945857519 6.234411317859789</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:timeserie>
    <op1d2d:Timeserie gml:id="Timeserie117831042718521">
     <gml:name>Abfluss - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition117831042718525">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result>&lt;![CDATA[2001-01-01T09:00:00.000+01:00 1
2001-01-01T19:00:00.000+01:00 1
2001-01-02T05:00:00.000+01:00 1
2001-01-02T15:00:00.000+01:00 2
2001-01-03T01:00:00.000+01:00 3
2001-01-03T11:00:00.000+01:00 4
2001-01-03T21:00:00.000+01:00 6
2001-01-04T07:00:00.000+01:00 8
2001-01-04T17:00:00.000+01:00 10
2001-01-05T03:00:00.000+01:00 12
2001-01-05T13:00:00.000+01:00 17
2001-01-05T23:00:00.000+01:00 19.65
2001-01-06T09:00:00.000+01:00 19.65
2001-01-06T19:00:00.000+01:00 19.65
]]&gt;</om:result>
     <op1d2d:dataSourceURI/>
    </op1d2d:Timeserie>
   </op1d2d:timeserie>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition11783105931002">
   <gml:description>Manuell erzeugt am: 04.05.2007 22:29</gml:description>
   <gml:name>2D - H - Randbedingung</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gts" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552955.0104812603 5986891.54433848 2.7210522337630163</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:timeserie>
    <op1d2d:Timeserie gml:id="Timeserie117831059310037">
     <gml:name>Wasserstand - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition117831059310027">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result>&lt;![CDATA[2001-01-01T09:00:00.000+01:00 8.3
2001-01-01T19:00:00.000+01:00 8.2
2001-01-02T05:00:00.000+01:00 7.75
2001-01-02T15:00:00.000+01:00 7.5
2001-01-03T01:00:00.000+01:00 7.25
2001-01-03T11:00:00.000+01:00 7
2001-01-03T21:00:00.000+01:00 6.75
2001-01-04T07:00:00.000+01:00 6.5
2001-01-04T17:00:00.000+01:00 6.25
2001-01-05T03:00:00.000+01:00 6
2001-01-05T13:00:00.000+01:00 5.75
2001-01-05T23:00:00.000+01:00 5.67
2001-01-06T09:00:00.000+01:00 5.69
2001-01-06T19:00:00.000+01:00 5.67
]]&gt;</om:result>
     <op1d2d:dataSourceURI/>
    </op1d2d:Timeserie>
   </op1d2d:timeserie>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
</simBase:FlowRelationshipModel>
