<?xml version="1.0" encoding="WINDOWS-1252"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:swe="http://www.opengis.net/swe" xmlns:om="http://www.opengis.net/om" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition11784846066911">
   <gml:description>Manuell erzeugt am: 06.05.2007 22:50</gml:description>
   <gml:name>1D-Randbedingung</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">862.5254038 506.0621778 9.1</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:timeserie>
    <op1d2d:Timeserie gml:id="Timeserie11784846066910">
     <gml:name>Wasserstand - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11784846074713">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result>2001-01-01T00:30:00.000+01:00 13
2001-01-01T01:00:00.000+01:00 13
</om:result>
     <op1d2d:dataSourceURI/>
    </op1d2d:Timeserie>
   </op1d2d:timeserie>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition11784847812342">
   <gml:description>Manuell erzeugt am: 06.05.2007 22:53</gml:description>
   <gml:name>2D-Randbedingung</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">83.10254035232033 56.06217784598106 10.0</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:timeserie>
    <op1d2d:Timeserie gml:id="Timeserie11784847812341">
     <gml:name>Abfluss - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11784847812346">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result>2001-01-01T00:30:00.000+01:00 9.82600000000000051159076974727213382720947265625
2001-01-01T01:00:00.000+01:00 9.82600000000000051159076974727213382720947265625
</om:result>
     <op1d2d:dataSourceURI/>
    </op1d2d:Timeserie>
   </op1d2d:timeserie>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
</simBase:FlowRelationshipModel>
