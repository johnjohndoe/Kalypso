<?xml version="1.0" encoding="WINDOWS-1252"?>
<c1d2d:Control xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:c1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dControl" xmlns:swe="http://www.opengis.net/swe" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <c1d2d:Version>NEW</c1d2d:Version>
 <c1d2d:VEGETA>true</c1d2d:VEGETA>
 <c1d2d:IDNOPT>-2</c1d2d:IDNOPT>
 <c1d2d:startsim>2001-01-01T00:30:00.000+01:00</c1d2d:startsim>
 <c1d2d:IEDSW>0</c1d2d:IEDSW>
 <c1d2d:TBFACT>0.2</c1d2d:TBFACT>
 <c1d2d:TBMIN>2000.0</c1d2d:TBMIN>
 <c1d2d:OMEGA>0.0</c1d2d:OMEGA>
 <c1d2d:ELEV>12.0</c1d2d:ELEV>
 <c1d2d:UNOM>0.5</c1d2d:UNOM>
 <c1d2d:UDIR>0.0</c1d2d:UDIR>
 <c1d2d:HMIN>0.0</c1d2d:HMIN>
 <c1d2d:DSET>0.01</c1d2d:DSET>
 <c1d2d:DSETD>0.015</c1d2d:DSETD>
 <c1d2d:NITI>10</c1d2d:NITI>
 <c1d2d:NCYC>2</c1d2d:NCYC>
 <c1d2d:NITN>25</c1d2d:NITN>
 <c1d2d:CONV_1>0.0010</c1d2d:CONV_1>
 <c1d2d:CONV_2>0.0010</c1d2d:CONV_2>
 <c1d2d:CONV_3>0.0010</c1d2d:CONV_3>
 <c1d2d:IDRPT>1</c1d2d:IDRPT>
 <c1d2d:DRFACT>0.05</c1d2d:DRFACT>
 <c1d2d:AC1>1.5</c1d2d:AC1>
 <c1d2d:AC2>0.5</c1d2d:AC2>
 <c1d2d:AC3>0.02</c1d2d:AC3>
 <c1d2d:IACCYC>1</c1d2d:IACCYC>
 <c1d2d:timestepsMember>
  <c1d2d:TimestepsObservation gml:id="Timeserie11773222660150">
   <gml:name>Zeitschritt Definition</gml:name>
   <om:observedProperty xmlns:om="http://www.opengis.net/om" xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
   <om:resultDefinition xmlns:om="http://www.opengis.net/om">
    <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11784870613943">
     <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
     <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#UnderRelaxationFactor"/>
    </sweExt:SortedRecordDefinition>
   </om:resultDefinition>
   <om:result xmlns:om="http://www.opengis.net/om">2001-01-01T00:30:00.000+01:00 1
2001-01-01T01:00:00.000+01:00 1
</om:result>
  </c1d2d:TimestepsObservation>
 </c1d2d:timestepsMember>
</c1d2d:Control>
