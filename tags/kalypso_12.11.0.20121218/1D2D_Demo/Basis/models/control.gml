<?xml version="1.0" encoding="UTF-8"?><ControlModelGroup xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om" xmlns:swe="http://www.opengis.net/swe" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sweExt="org.kalypso.swe.ext" xmlns="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dControl" gml:id="root">
 <simBase:version>1.0</simBase:version>
 <controlModelCollection>
  <ControlModelCollection gml:id="ControlModelCollection12125814299811">
   <activeModelID xlink:href="#ControlModel12143997993122"/>
   <controlModelMember>
    <ControlModel gml:id="ControlModel12143997993122">
     <wb1d2d:calculationUnit xlink:href="discretisation.gml#CalculationUnit2D121439979923417593"/>
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
     <BEIENT>false</BEIENT>
     <HASWINDDRAG/>
     <CHI/>
     <ICPU>4</ICPU>
     <BUFFSIZ>5000000</BUFFSIZ>
     <MFW>2000</MFW>
     <PERCENT_CHECK/>
     <IDNOPT>-1</IDNOPT>
     <startsim/>
     <IEDSW>13</IEDSW>
     <TBFACT>0.2</TBFACT>
     <TBFACT_ESCUDIER/>
     <TBMIN>1.8</TBMIN>
     <_p_bottom/>
     <OMEGA>59.0</OMEGA>
     <ELEV>367.0</ELEV>
     <UNOM>0.0</UNOM>
     <UDIR>250.0</UDIR>
     <HMIN>0.0</HMIN>
     <DSET>0.05</DSET>
     <DSETD>0.1</DSETD>
     <_steady>true</_steady>
     <_unsteady>true</_unsteady>
     <NITI>50</NITI>
     <NITN>25</NITN>
     <CONV_1>0.01</CONV_1>
     <CONV_2>0.01</CONV_2>
     <CONV_3>0.01</CONV_3>
     <IDRPT>0</IDRPT>
     <DRFACT>0.05</DRFACT>
     <FIXEDMARSHBOTTOM/>
     <AC1>1.5</AC1>
     <AC2>0.67</AC2>
     <AC3>0.03</AC3>
     <AC4/>
     <MARSHFRICTIONFACTOR/>
     <MARSHFRICTIONDISTR/>
     <_restart>false</_restart>
     <restartInfoMember>
      <RestartInfo gml:id="RestartInfo121447349925834">
       <calculationUnitID>CalculationUnit2D121439979923417593</calculationUnitID>
       <stepResultMetaID>StepResultMeta121440229167121</stepResultMetaID>
       <filePath>results/CalculationUnit2D121439979923417593/steady/results.gml</filePath>
      </RestartInfo>
     </restartInfoMember>
     <IACCYC>6</IACCYC>
     <FNAM3/>
     <_steadyBC>0.5</_steadyBC>
     <timestepsMember>
      <TimestepsObservation gml:id="TimestepsObservation12143997993124">
       <gml:name>Zeitschrittdefinition</gml:name>
       <om:time/>
       <om:procedure/>
       <om:observedProperty/>
       <om:featureOfInterest/>
       <om:resultDefinition>
        <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition122970465774221">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#UnderRelaxationFactor"/>
         <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <sweExt:ordinalNumberComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
        </sweExt:SortedRecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[1 2011-01-24T17%3A00%3A00.000Z 0.8
2 2011-01-24T23%3A00%3A00.000Z 0.8
3 2011-01-25T05%3A00%3A00.000Z 0.8
4 2011-01-25T11%3A00%3A00.000Z 0.8
5 2011-01-25T17%3A00%3A00.000Z 0.8
6 2011-01-25T23%3A00%3A00.000Z 0.8
7 2011-01-26T05%3A00%3A00.000Z 0.8
]]></om:result>
      </TimestepsObservation>
     </timestepsMember>
    </ControlModel>
   </controlModelMember>
   <controlModelMember>
    <ControlModel gml:id="ControlModel12125830338111">
     <gml:description>Demomodell für eine gekoppelte 1D/2D Berechnung/ Demo model to run coupled 1D/2D calculations.</gml:description>
     <wb1d2d:calculationUnit xlink:href="discretisation.gml#CalculationUnit1D12125830336860"/>
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
     <VEGETA>false</VEGETA>
     <BEIENT>false</BEIENT>
     <HASWINDDRAG/>
     <CHI/>
     <ICPU>4</ICPU>
     <BUFFSIZ>5000000</BUFFSIZ>
     <MFW>500</MFW>
     <PERCENT_CHECK/>
     <IDNOPT>-1</IDNOPT>
     <startsim/>
     <IEDSW>13</IEDSW>
     <TBFACT>0.2</TBFACT>
     <TBFACT_ESCUDIER/>
     <TBMIN>2.0</TBMIN>
     <_p_bottom/>
     <OMEGA>59.0</OMEGA>
     <ELEV/>
     <UNOM>0.5</UNOM>
     <UDIR/>
     <HMIN/>
     <DSET>0.05</DSET>
     <DSETD>0.1</DSETD>
     <_steady>true</_steady>
     <_unsteady>false</_unsteady>
     <NITI>35</NITI>
     <NITN>40</NITN>
     <CONV_1>0.01</CONV_1>
     <CONV_2>0.01</CONV_2>
     <CONV_3>0.001</CONV_3>
     <IDRPT>0</IDRPT>
     <DRFACT>0.05</DRFACT>
     <FIXEDMARSHBOTTOM/>
     <AC1>1.5</AC1>
     <AC2>0.67</AC2>
     <AC3>0.04</AC3>
     <AC4/>
     <MARSHFRICTIONFACTOR/>
     <MARSHFRICTIONDISTR/>
     <_restart>true</_restart>
     <restartInfoMember>
      <RestartInfo gml:id="RestartInfo129664880446214">
       <calculationUnitID>restart1dStationary1212583531082</calculationUnitID>
       <stepResultMetaID>StepResultMeta12127360649530</stepResultMetaID>
       <filePath>results/restart1dStationary1212583531082/250/results.gml</filePath>
      </RestartInfo>
     </restartInfoMember>
     <IACCYC>1</IACCYC>
     <FNAM3/>
     <_steadyBC>0.5</_steadyBC>
     <timestepsMember>
      <TimestepsObservation gml:id="TimestepsObservation12125830338730">
       <gml:name>Zeitschrittdefinition</gml:name>
       <om:time/>
       <om:procedure/>
       <om:observedProperty/>
       <om:featureOfInterest/>
       <om:resultDefinition>
        <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition122245239520312">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#UnderRelaxationFactor"/>
         <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <sweExt:ordinalNumberComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
        </sweExt:SortedRecordDefinition>
       </om:resultDefinition>
       <om:result/>
      </TimestepsObservation>
     </timestepsMember>
    </ControlModel>
   </controlModelMember>
   <controlModelMember>
    <ControlModel gml:id="ControlModel121446393945210">
     <gml:description>Demomodell für eine gekoppelte 1D/2D Berechnung/ Demo model to run coupled 1D/2D calculations.</gml:description>
     <wb1d2d:calculationUnit xlink:href="discretisation.gml#CalculationUnit1D2D12144639393734308"/>
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
     <BEIENT>false</BEIENT>
     <HASWINDDRAG/>
     <CHI/>
     <ICPU>4</ICPU>
     <BUFFSIZ>5000000</BUFFSIZ>
     <MFW>5000</MFW>
     <PERCENT_CHECK/>
     <IDNOPT>-1</IDNOPT>
     <startsim/>
     <IEDSW>13</IEDSW>
     <TBFACT>0.2</TBFACT>
     <TBFACT_ESCUDIER/>
     <TBMIN>0.5</TBMIN>
     <_p_bottom/>
     <OMEGA>59.0</OMEGA>
     <ELEV>367.0</ELEV>
     <UNOM>0.5</UNOM>
     <UDIR>250.0</UDIR>
     <HMIN>0.0</HMIN>
     <DSET>0.05</DSET>
     <DSETD>0.1</DSETD>
     <_steady>false</_steady>
     <_unsteady>true</_unsteady>
     <NITI>25</NITI>
     <NITN>25</NITN>
     <CONV_1>0.01</CONV_1>
     <CONV_2>0.01</CONV_2>
     <CONV_3>0.01</CONV_3>
     <IDRPT>0</IDRPT>
     <DRFACT>0.05</DRFACT>
     <FIXEDMARSHBOTTOM/>
     <AC1>3.0</AC1>
     <AC2>0.67</AC2>
     <AC3>0.03</AC3>
     <AC4/>
     <MARSHFRICTIONFACTOR/>
     <MARSHFRICTIONDISTR/>
     <_restart>true</_restart>
     <restartInfoMember>
      <RestartInfo gml:id="RestartInfo12966516472184">
       <calculationUnitID>CalculationUnit1D12125830336860</calculationUnitID>
       <stepResultMetaID>StepResultMeta1296648840121148</stepResultMetaID>
       <filePath>results/CalculationUnit1D12125830336860/steady/results.gz</filePath>
      </RestartInfo>
     </restartInfoMember>
     <restartInfoMember>
      <RestartInfo gml:id="RestartInfo129665164721865">
       <calculationUnitID>CalculationUnit2D121439979923417593</calculationUnitID>
       <stepResultMetaID>StepResultMeta129665079661354</stepResultMetaID>
       <filePath>results/CalculationUnit2D121439979923417593/timestep-26.01.2011_06_00_00_000_MEZ/results.gz</filePath>
      </RestartInfo>
     </restartInfoMember>
     <IACCYC>1</IACCYC>
     <FNAM3/>
     <_steadyBC>1.0</_steadyBC>
     <timestepsMember>
      <TimestepsObservation gml:id="TimestepsObservation121446393945215">
       <gml:name>Zeitschrittdefinition</gml:name>
       <om:time/>
       <om:procedure/>
       <om:observedProperty/>
       <om:featureOfInterest/>
       <om:resultDefinition>
        <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition12965621971374">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#UnderRelaxationFactor"/>
         <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
         <sweExt:ordinalNumberComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber"/>
        </sweExt:SortedRecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[1 2011-01-03T23%3A00%3A00.000Z 0.8
2 2011-01-04T00%3A00%3A00.000Z 0.8
3 2011-01-04T01%3A00%3A00.000Z 0.8
4 2011-01-04T02%3A00%3A00.000Z 0.8
5 2011-01-04T03%3A00%3A00.000Z 0.8
6 2011-01-04T04%3A00%3A00.000Z 0.8
7 2011-01-04T05%3A00%3A00.000Z 0.8
8 2011-01-04T06%3A00%3A00.000Z 0.8
9 2011-01-04T07%3A00%3A00.000Z 0.8
10 2011-01-04T08%3A00%3A00.000Z 0.8
11 2011-01-04T09%3A00%3A00.000Z 0.8
12 2011-01-04T10%3A00%3A00.000Z 0.8
13 2011-01-04T11%3A00%3A00.000Z 0.8
14 2011-01-04T12%3A00%3A00.000Z 0.8
15 2011-01-04T13%3A00%3A00.000Z 0.8
16 2011-01-04T14%3A00%3A00.000Z 0.8
17 2011-01-04T15%3A00%3A00.000Z 0.8
18 2011-01-04T16%3A00%3A00.000Z 0.8
19 2011-01-04T17%3A00%3A00.000Z 0.8
20 2011-01-04T18%3A00%3A00.000Z 0.8
21 2011-01-04T19%3A00%3A00.000Z 0.8
22 2011-01-04T20%3A00%3A00.000Z 0.8
23 2011-01-04T21%3A00%3A00.000Z 0.8
24 2011-01-04T22%3A00%3A00.000Z 0.8
25 2011-01-04T23%3A00%3A00.000Z 0.8
26 2011-01-05T00%3A00%3A00.000Z 0.8
27 2011-01-05T01%3A00%3A00.000Z 0.8
28 2011-01-05T02%3A00%3A00.000Z 0.8
29 2011-01-05T03%3A00%3A00.000Z 0.8
30 2011-01-05T04%3A00%3A00.000Z 0.8
31 2011-01-05T05%3A00%3A00.000Z 0.8
32 2011-01-05T06%3A00%3A00.000Z 0.8
33 2011-01-05T07%3A00%3A00.000Z 0.8
34 2011-01-05T08%3A00%3A00.000Z 0.8
35 2011-01-05T09%3A00%3A00.000Z 0.8
36 2011-01-05T10%3A00%3A00.000Z 0.8
37 2011-01-05T11%3A00%3A00.000Z 0.8
38 2011-01-05T12%3A00%3A00.000Z 0.8
39 2011-01-05T13%3A00%3A00.000Z 0.8
40 2011-01-05T14%3A00%3A00.000Z 0.8
41 2011-01-05T15%3A00%3A00.000Z 0.8
42 2011-01-05T16%3A00%3A00.000Z 0.8
43 2011-01-05T17%3A00%3A00.000Z 0.8
44 2011-01-05T18%3A00%3A00.000Z 0.8
45 2011-01-05T19%3A00%3A00.000Z 0.8
46 2011-01-05T20%3A00%3A00.000Z 0.8
47 2011-01-05T21%3A00%3A00.000Z 0.8
48 2011-01-05T22%3A00%3A00.000Z 0.8
49 2011-01-05T23%3A00%3A00.000Z 0.8
50 2011-01-06T00%3A00%3A00.000Z 0.8
51 2011-01-06T01%3A00%3A00.000Z 0.8
52 2011-01-06T02%3A00%3A00.000Z 0.8
53 2011-01-06T03%3A00%3A00.000Z 0.8
54 2011-01-06T04%3A00%3A00.000Z 0.8
55 2011-01-06T05%3A00%3A00.000Z 0.8
56 2011-01-06T06%3A00%3A00.000Z 0.8
57 2011-01-06T07%3A00%3A00.000Z 0.8
58 2011-01-06T08%3A00%3A00.000Z 0.8
59 2011-01-06T09%3A00%3A00.000Z 0.8
60 2011-01-06T10%3A00%3A00.000Z 0.8
61 2011-01-06T11%3A00%3A00.000Z 0.8
62 2011-01-06T12%3A00%3A00.000Z 0.8
63 2011-01-06T13%3A00%3A00.000Z 0.8
64 2011-01-06T14%3A00%3A00.000Z 0.8
65 2011-01-06T15%3A00%3A00.000Z 0.8
66 2011-01-06T16%3A00%3A00.000Z 0.8
67 2011-01-06T17%3A00%3A00.000Z 0.8
68 2011-01-06T18%3A00%3A00.000Z 0.8
69 2011-01-06T19%3A00%3A00.000Z 0.8
70 2011-01-06T20%3A00%3A00.000Z 0.8
71 2011-01-06T21%3A00%3A00.000Z 0.8
72 2011-01-06T22%3A00%3A00.000Z 0.8
73 2011-01-06T23%3A00%3A00.000Z 0.8
74 2011-01-07T00%3A00%3A00.000Z 0.8
75 2011-01-07T01%3A00%3A00.000Z 0.8
76 2011-01-07T02%3A00%3A00.000Z 0.8
77 2011-01-07T03%3A00%3A00.000Z 0.8
78 2011-01-07T04%3A00%3A00.000Z 0.8
79 2011-01-07T05%3A00%3A00.000Z 0.8
80 2011-01-07T06%3A00%3A00.000Z 0.8
81 2011-01-07T07%3A00%3A00.000Z 0.8
82 2011-01-07T08%3A00%3A00.000Z 0.8
83 2011-01-07T09%3A00%3A00.000Z 0.8
84 2011-01-07T10%3A00%3A00.000Z 0.8
85 2011-01-07T11%3A00%3A00.000Z 0.8
86 2011-01-07T12%3A00%3A00.000Z 0.8
87 2011-01-07T13%3A00%3A00.000Z 0.8
88 2011-01-07T14%3A00%3A00.000Z 0.8
89 2011-01-07T15%3A00%3A00.000Z 0.8
90 2011-01-07T16%3A00%3A00.000Z 0.8
91 2011-01-07T17%3A00%3A00.000Z 0.8
92 2011-01-07T18%3A00%3A00.000Z 0.8
93 2011-01-07T19%3A00%3A00.000Z 0.8
94 2011-01-07T20%3A00%3A00.000Z 0.8
95 2011-01-07T21%3A00%3A00.000Z 0.8
96 2011-01-07T22%3A00%3A00.000Z 0.8
97 2011-01-07T23%3A00%3A00.000Z 0.8
98 2011-01-08T00%3A00%3A00.000Z 0.8
99 2011-01-08T01%3A00%3A00.000Z 0.8
100 2011-01-08T02%3A00%3A00.000Z 0.8
101 2011-01-08T03%3A00%3A00.000Z 0.8
102 2011-01-08T04%3A00%3A00.000Z 0.8
103 2011-01-08T05%3A00%3A00.000Z 0.8
104 2011-01-08T06%3A00%3A00.000Z 0.8
105 2011-01-08T07%3A00%3A00.000Z 0.8
106 2011-01-08T08%3A00%3A00.000Z 0.8
107 2011-01-08T09%3A00%3A00.000Z 0.8
108 2011-01-08T10%3A00%3A00.000Z 0.8
109 2011-01-08T11%3A00%3A00.000Z 0.8
110 2011-01-08T12%3A00%3A00.000Z 0.8
111 2011-01-08T13%3A00%3A00.000Z 0.8
112 2011-01-08T14%3A00%3A00.000Z 0.8
113 2011-01-08T15%3A00%3A00.000Z 0.8
114 2011-01-08T16%3A00%3A00.000Z 0.8
115 2011-01-08T17%3A00%3A00.000Z 0.8
116 2011-01-08T18%3A00%3A00.000Z 0.8
117 2011-01-08T19%3A00%3A00.000Z 0.8
118 2011-01-08T20%3A00%3A00.000Z 0.8
119 2011-01-08T21%3A00%3A00.000Z 0.8
120 2011-01-08T22%3A00%3A00.000Z 0.8
121 2011-01-08T23%3A00%3A00.000Z 0.8
122 2011-01-09T00%3A00%3A00.000Z 0.8
123 2011-01-09T01%3A00%3A00.000Z 0.8
124 2011-01-09T02%3A00%3A00.000Z 0.8
125 2011-01-09T03%3A00%3A00.000Z 0.8
]]></om:result>
      </TimestepsObservation>
     </timestepsMember>
    </ControlModel>
   </controlModelMember>
  </ControlModelCollection>
 </controlModelCollection>
</ControlModelGroup>
