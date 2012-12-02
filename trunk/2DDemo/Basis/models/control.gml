<?xml version="1.0" encoding="UTF-8"?><ControlModelGroup xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om" xmlns:swe="http://www.opengis.net/swe" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sweExt="org.kalypso.swe.ext" xmlns="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dControl" gml:id="root">
 <simBase:version>1.0</simBase:version>
 <controlModelCollection>
  <ControlModelCollection gml:id="ControlModelCollection12129954658851">
   <activeModelID xlink:href="#ControlModel12129980697592"/>
   <controlModelMember>
    <ControlModel gml:id="ControlModel12129980697592">
     <wb1d2d:calculationUnit xlink:href="discretisation.gml#CalculationUnit2D121299806933713174"/>
     <Version>1.2.0_rc1_win_x64_20100416</Version>
     <SWANCalculation/>
     <SWANVersion/>
     <SWANConstantBoundary/>
     <SWANBoundaryAlg/>
     <SWANINITialValues/>
     <SWANINITialValuesPar/>
     <SWANConstantWind/>
     <SWANCoordCart/>
     <SWANGEN3/>
     <SWANConstantWindPar/>
     <SWANAdditionalResultsPar/>
     <SWANInputCoordFile/>
     <SWANInputAdditionalCmds/>
     <VEGETA>true</VEGETA>
     <BEIENT>true</BEIENT>
     <HASWINDDRAG/>
     <CHI/>
     <ICPU>4</ICPU>
     <BUFFSIZ>10000000</BUFFSIZ>
     <MFW>2000</MFW>
     <PERCENT_CHECK/>
     <IDNOPT>-1</IDNOPT>
     <startsim/>
     <IEDSW>13</IEDSW>
     <TBFACT>0.2</TBFACT>
     <TBFACT_ESCUDIER/>
     <TBMIN>2.0</TBMIN>
     <_p_bottom/>
     <OMEGA/>
     <ELEV>367.0</ELEV>
     <UNOM>0.5</UNOM>
     <UDIR/>
     <HMIN/>
     <DSET>0.03</DSET>
     <DSETD>0.15</DSETD>
     <_steady>true</_steady>
     <_unsteady>true</_unsteady>
     <NITI>40</NITI>
     <NITN>40</NITN>
     <CONV_1>0.05</CONV_1>
     <CONV_2>0.05</CONV_2>
     <CONV_3>0.01</CONV_3>
     <IDRPT>0</IDRPT>
     <DRFACT>0.05</DRFACT>
     <FIXEDMARSHBOTTOM/>
     <AC1>1.5</AC1>
     <AC2>0.67</AC2>
     <AC3>0.04</AC3>
     <AC4/>
     <MARSHFRICTIONFACTOR/>
     <MARSHFRICTIONDISTR/>
     <_restart>false</_restart>
     <IACCYC>1</IACCYC>
     <FNAM3/>
     <_steadyBC>0.3</_steadyBC>
     <timestepsMember>
      <TimestepsObservation gml:id="TimestepsObservation12129980698840">
       <gml:name>Zeitschrittdefinition</gml:name>
       <om:time/>
       <om:procedure/>
       <om:observedProperty/>
       <om:featureOfInterest/>
       <om:resultDefinition>
        <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition12962238212971">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#UnderRelaxationFactor"/>
         <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <sweExt:ordinalNumberComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
        </sweExt:SortedRecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[1 2009-12-31T23%3A00%3A00.000Z 0%2C5
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
      </TimestepsObservation>
     </timestepsMember>
    </ControlModel>
   </controlModelMember>
  </ControlModelCollection>
 </controlModelCollection>
</ControlModelGroup>
