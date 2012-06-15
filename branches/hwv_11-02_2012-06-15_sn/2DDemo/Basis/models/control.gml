<?xml version="1.0" encoding="UTF-8"?>
<c1d2d:ControlModelGroup xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:c1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dControl" xmlns:swe="http://www.opengis.net/swe" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <c1d2d:controlModelCollection xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase">
  <c1d2d:ControlModelCollection gml:id="ControlModelCollection12129954658851">
   <c1d2d:activeModelID xlink:href="#ControlModel12129980697592"/>
   <c1d2d:controlModelMember>
    <c1d2d:ControlModel gml:id="ControlModel12129980697592">
     <wb1d2d:calculationUnit xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xlink:href="discretisation.gml#CalculationUnit2D121299806933713174"/>
     <c1d2d:Version>0_0_2_6</c1d2d:Version>
     <c1d2d:VEGETA>true</c1d2d:VEGETA>
     <c1d2d:BEIENT>true</c1d2d:BEIENT>
     <c1d2d:IDNOPT>-1</c1d2d:IDNOPT>
     <c1d2d:startsim/>
     <c1d2d:IEDSW>0</c1d2d:IEDSW>
     <c1d2d:TBFACT>0.2</c1d2d:TBFACT>
     <c1d2d:TBMIN>2.0</c1d2d:TBMIN>
     <c1d2d:_p_bottom/>
     <c1d2d:OMEGA/>
     <c1d2d:ELEV>367.0</c1d2d:ELEV>
     <c1d2d:UNOM>0.5</c1d2d:UNOM>
     <c1d2d:UDIR/>
     <c1d2d:HMIN/>
     <c1d2d:DSET>0.03</c1d2d:DSET>
     <c1d2d:DSETD>0.15</c1d2d:DSETD>
     <c1d2d:_steady>true</c1d2d:_steady>
     <c1d2d:_unsteady>true</c1d2d:_unsteady>
     <c1d2d:NITI>40</c1d2d:NITI>
     <c1d2d:NITN>40</c1d2d:NITN>
     <c1d2d:CONV_1>0.05</c1d2d:CONV_1>
     <c1d2d:CONV_2>0.05</c1d2d:CONV_2>
     <c1d2d:CONV_3>0.01</c1d2d:CONV_3>
     <c1d2d:IDRPT>0</c1d2d:IDRPT>
     <c1d2d:DRFACT>0.05</c1d2d:DRFACT>
     <c1d2d:AC1>1.5</c1d2d:AC1>
     <c1d2d:AC2>0.67</c1d2d:AC2>
     <c1d2d:AC3>0.04</c1d2d:AC3>
     <c1d2d:_restart>false</c1d2d:_restart>
     <c1d2d:IACCYC>1</c1d2d:IACCYC>
     <c1d2d:FNAM3/>
     <c1d2d:_steadyBC>0.3</c1d2d:_steadyBC>
     <c1d2d:timestepsMember>
      <c1d2d:TimestepsObservation gml:id="TimestepsObservation12129980698840">
       <gml:name>Zeitschrittdefinition</gml:name>
       <om:resultDefinition xmlns:om="http://www.opengis.net/om">
        <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition12142937854032">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#UnderRelaxationFactor"/>
         <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
        </sweExt:SortedRecordDefinition>
       </om:resultDefinition>
       <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[1 2008-06-08T22%3A00%3A00.000Z 0.500
2 2008-06-08T23%3A00%3A00.000Z 0.500
3 2008-06-09T00%3A00%3A00.000Z 0.500
4 2008-06-09T01%3A00%3A00.000Z 0.500
5 2008-06-09T02%3A00%3A00.000Z 0.500
6 2008-06-09T03%3A00%3A00.000Z 0.500
7 2008-06-09T04%3A00%3A00.000Z 0.500
8 2008-06-09T05%3A00%3A00.000Z 0.500
9 2008-06-09T06%3A00%3A00.000Z 0.500
10 2008-06-09T07%3A00%3A00.000Z 0.500
11 2008-06-09T08%3A00%3A00.000Z 0.500
12 2008-06-09T09%3A00%3A00.000Z 0.500
13 2008-06-09T10%3A00%3A00.000Z 0.500
14 2008-06-09T11%3A00%3A00.000Z 0.500
15 2008-06-09T12%3A00%3A00.000Z 0.500
16 2008-06-09T13%3A00%3A00.000Z 0.500
]]></om:result>
      </c1d2d:TimestepsObservation>
     </c1d2d:timestepsMember>
    </c1d2d:ControlModel>
   </c1d2d:controlModelMember>
  </c1d2d:ControlModelCollection>
 </c1d2d:controlModelCollection>
</c1d2d:ControlModelGroup>
