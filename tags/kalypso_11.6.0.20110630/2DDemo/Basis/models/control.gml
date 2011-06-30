<?xml version="1.0" encoding="UTF-8"?>
<c1d2d:ControlModelGroup xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:c1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dControl" xmlns:swe="http://www.opengis.net/swe" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <simBase:version xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase">1.0</simBase:version>
 <c1d2d:controlModelCollection>
  <c1d2d:ControlModelCollection gml:id="ControlModelCollection12129954658851">
   <c1d2d:activeModelID xlink:href="#ControlModel12129980697592"/>
   <c1d2d:controlModelMember>
    <c1d2d:ControlModel gml:id="ControlModel12129980697592">
     <wb1d2d:calculationUnit xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xlink:href="discretisation.gml#CalculationUnit2D121299806933713174"/>
     <c1d2d:Version>1.2.0_rc1_win_x64_20100416</c1d2d:Version>
     <c1d2d:SWANCalculation/>
     <c1d2d:SWANVersion/>
     <c1d2d:SWANConstantBoundary/>
     <c1d2d:SWANBoundaryAlg/>
     <c1d2d:SWANINITialValues/>
     <c1d2d:SWANINITialValuesPar/>
     <c1d2d:SWANConstantWind/>
     <c1d2d:SWANCoordCart/>
     <c1d2d:SWANGEN3/>
     <c1d2d:SWANConstantWindPar/>
     <c1d2d:SWANAdditionalResultsPar/>
     <c1d2d:SWANInputCoordFile/>
     <c1d2d:SWANInputAdditionalCmds/>
     <c1d2d:VEGETA>true</c1d2d:VEGETA>
     <c1d2d:BEIENT>true</c1d2d:BEIENT>
     <c1d2d:ICPU>4</c1d2d:ICPU>
     <c1d2d:BUFFSIZ>10000000</c1d2d:BUFFSIZ>
     <c1d2d:MFW>2000</c1d2d:MFW>
     <c1d2d:PERCENT_CHECK/>
     <c1d2d:IDNOPT>-1</c1d2d:IDNOPT>
     <c1d2d:startsim/>
     <c1d2d:IEDSW>13</c1d2d:IEDSW>
     <c1d2d:TBFACT>0.2</c1d2d:TBFACT>
     <c1d2d:TBFACT_ESCUDIER/>
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
       <om:time xmlns:om="http://www.opengis.net/om"/>
       <om:procedure xmlns:om="http://www.opengis.net/om"/>
       <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
       <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
       <om:resultDefinition xmlns:om="http://www.opengis.net/om">
        <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition12962238212971">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#UnderRelaxationFactor"/>
         <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <sweExt:ordinalNumberComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
        </sweExt:SortedRecordDefinition>
       </om:resultDefinition>
       <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[1 2009-12-31T23%3A00%3A00.000Z 0%2C5
2 2010-01-01T00%3A00%3A00.000Z 0%2C5
3 2010-01-01T01%3A00%3A00.000Z 0%2C5
4 2010-01-01T02%3A00%3A00.000Z 0%2C5
5 2010-01-01T03%3A00%3A00.000Z 0%2C5
6 2010-01-01T04%3A00%3A00.000Z 0%2C5
7 2010-01-01T05%3A00%3A00.000Z 0%2C5
8 2010-01-01T06%3A00%3A00.000Z 0%2C5
9 2010-01-01T07%3A00%3A00.000Z 0%2C5
10 2010-01-01T08%3A00%3A00.000Z 0%2C5
11 2010-01-01T09%3A00%3A00.000Z 0%2C5
12 2010-01-01T10%3A00%3A00.000Z 0%2C5
13 2010-01-01T11%3A00%3A00.000Z 0%2C5
14 2010-01-01T12%3A00%3A00.000Z 0%2C5
15 2010-01-01T13%3A00%3A00.000Z 0%2C5
16 2010-01-01T14%3A00%3A00.000Z 0%2C5
17 2010-01-01T15%3A00%3A00.000Z 0%2C5
18 2010-01-01T16%3A00%3A00.000Z 0%2C5
19 2010-01-01T17%3A00%3A00.000Z 0%2C5
20 2010-01-01T18%3A00%3A00.000Z 0%2C5
21 2010-01-01T19%3A00%3A00.000Z 0%2C5
22 2010-01-01T20%3A00%3A00.000Z 0%2C5
23 2010-01-01T21%3A00%3A00.000Z 0%2C5
24 2010-01-01T22%3A00%3A00.000Z 0%2C5
25 2010-01-01T23%3A00%3A00.000Z 0%2C5
]]></om:result>
      </c1d2d:TimestepsObservation>
     </c1d2d:timestepsMember>
    </c1d2d:ControlModel>
   </c1d2d:controlModelMember>
  </c1d2d:ControlModelCollection>
 </c1d2d:controlModelCollection>
</c1d2d:ControlModelGroup>
