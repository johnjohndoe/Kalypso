<?xml version="1.0" encoding="UTF-8"?>
<QIntervallResultCollection xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns="org.kalypso.model.wspm.tuhh" xmlns:swe="http://www.opengis.net/swe" gml:id="root">
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228281711">
   <gml:description>Gelesen aus: PROF0041.7200.txt</gml:description>
   <gml:name>41.7200</gml:name>
   <station>41.7200</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938274871"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation12943228281710">
     <gml:description>Übernommen aus Datei: PROF0041.7200.txt</gml:description>
     <gml:name>0041.7200</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition12943228282802">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.9 0.8820000 14.58500 10.00000 1.000000 -0.5153966 -0.1479894 -5.184742E-14
363.4 1.385000 29.24280 30.00000 1.000000 1.035381 0.4107093 2.939871E-13
363.8 1.739000 40.88730 50.00000 1.000000 0.7235425 0.05932804 -5.511147E-13
364.1 2.037000 51.48660 70.00000 1.000000 -0.1384753 -0.2193177 4.329870E-13
364.3 2.300000 61.41240 90.00000 1.000000 -0.9727448 -0.2591315 -1.236788E-13
364.6 2.532000 70.59513 110.0000 1.000650 -1.403832 -0.4478578 0.001252701
364.8 2.745000 79.38462 130.0000 1.002060 -1.230097 -0.2034893 -0.002488054
365.0 2.944000 87.92757 150.0000 1.003730 -0.2905745 0.7299203 -0.001982242
365.1 3.115000 95.63239 170.0000 1.006590 1.253032 0.4800827 0.003901883
365.3 3.272000 104.7543 190.0000 1.023300 1.718940 0.2205435 0.002107384
365.4 3.416000 114.9627 210.0000 1.045400 1.393564 -0.1743871 -0.0004256382
365.6 3.552000 125.8650 230.0000 1.069870 0.9277060 -0.2106465 -0.001823189
365.7 3.679000 137.2167 250.0000 1.093900 0.4158000 -0.1824141 -0.001246452
365.8 3.798000 149.0066 270.0000 1.117300 -0.1475636 -0.09941847 -0.00008137276
365.9 3.910000 161.4177 290.0000 1.141000 -0.9509986 0.05538210 -0.0005958534
366.0 4.014000 173.9055 310.0000 1.161400 -1.673728 -0.06441231 -0.0006635831
366.1 4.113000 185.9882 330.0000 1.176800 -1.602646 -0.02979878 0.0009882860
366.2 4.206000 197.7612 350.0000 1.188720 -1.048261 -0.1638178 0.001873164
366.3 4.296000 209.2201 370.0000 1.197680 0.3186968 0.07984584 0.001199460
366.4 4.381000 220.3239 390.0000 1.203940 2.187655 0.1668690 -0.002016493
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282882681">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.960450729214 -15.6095817129 34.9334465021 -7.60339398439 1.15762230367</math:coefficients>
     <math:minRange>0.882</math:minRange>
     <math:maxRange>4.381</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842243">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-12.4387186139 25.1750994088 9.3382852546 -5.23899116495 1.04769929011</math:coefficients>
     <math:minRange>0.882</math:minRange>
     <math:maxRange>4.381</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884220">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.999999999878 3.38093208283E-10 -3.35280550809E-10 1.41724742049E-10 -2.16882082833E-11</math:coefficients>
     <math:minRange>0.882</math:minRange>
     <math:maxRange>2.3</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884230">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>7.90503629804 -8.65763081258 3.61247501773 -0.50155981531</math:coefficients>
     <math:minRange>2.3</math:minRange>
     <math:maxRange>2.532</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842212">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-4.65807324141 7.68312916795 -3.82917106317 0.825708701169 -0.0644333948894</math:coefficients>
     <math:minRange>2.532</math:minRange>
     <math:maxRange>4.381</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228282965">
   <gml:description>Gelesen aus: PROF0041.7400.txt</gml:description>
   <gml:name>41.7400</gml:name>
   <station>41.7400</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938275033"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation12943228282966">
     <gml:description>Übernommen aus Datei: PROF0041.7400.txt</gml:description>
     <gml:name>0041.7400</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition12943228282966">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.9 0.8450000 13.75550 10.00000 1.000000 -0.3652716 -0.1556909 0E-7
363.4 1.388000 27.96750 30.00000 1.000000 0.6801304 0.6554766 0E-7
363.8 1.767000 39.19800 50.00000 1.000000 0.5926606 -0.3154564 0E-7
364.1 2.097000 49.89880 70.00000 1.000000 -0.05032532 -0.6467208 0E-7
364.4 2.396000 60.27510 90.00000 1.000000 -0.7378198 -0.02437126 0E-7
364.7 2.653000 69.75770 110.0000 1.000170 -1.112093 0.1750101 0.0004746234
364.9 2.879000 78.51922 130.0000 1.001480 -0.9480047 0.1428658 -0.001122609
365.1 3.090000 87.07593 150.0000 1.003320 -0.09888466 0.9393233 0.0001186738
365.3 3.266000 95.16632 170.0000 1.011300 0.6775674 0.09874962 0.0005077184
365.5 3.430000 103.9984 190.0000 1.025070 1.134517 -0.3805245 0.0005116123
365.6 3.583000 113.6820 210.0000 1.043920 1.173937 -0.5610195 -0.0001939749
365.8 3.727000 124.0385 230.0000 1.065010 1.047661 -0.3485481 -0.00009941082
365.9 3.861000 134.9115 250.0000 1.086400 0.7631446 -0.05900744 0.0008572732
366.0 3.985000 146.5211 270.0000 1.109130 -0.02092162 0.09268752 -0.00005302030
366.1 4.101000 158.6509 290.0000 1.131220 -1.035671 0.2313620 -0.001921014
366.2 4.209000 170.4819 310.0000 1.147930 -1.580147 0.1754068 -0.001132010
366.3 4.311000 181.9593 330.0000 1.160420 -1.498505 0.1344545 0.0006034061
366.4 4.407000 193.1136 350.0000 1.169520 -0.9157056 -0.02724038 0.001787223
366.5 4.499000 203.9620 370.0000 1.175910 0.3180553 -0.006396414 0.001409216
366.6 4.586000 214.5280 390.0000 1.180340 1.975677 -0.1203606 -0.001747707
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884267">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-4.2588602733 -0.0698606158959 23.4356995471 -4.9941588629 0.86647736183</math:coefficients>
     <math:minRange>0.845</math:minRange>
     <math:maxRange>4.586</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884235">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-4.82107627507 13.4422076471 13.6615887739 -5.59744788042 0.931973704512</math:coefficients>
     <math:minRange>0.845</math:minRange>
     <math:maxRange>4.586</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842103">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.845</math:minRange>
     <math:maxRange>2.396</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842237">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>1.87306626087 -1.03673711054 0.409148359933 -0.0536452094535</math:coefficients>
     <math:minRange>2.396</math:minRange>
     <math:maxRange>2.653</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842105">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-4.53155062242 7.09367186659 -3.34940073768 0.686301706045 -0.0510324419653</math:coefficients>
     <math:minRange>2.653</math:minRange>
     <math:maxRange>4.586</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228282964">
   <gml:description>Gelesen aus: PROF0041.7450.txt</gml:description>
   <gml:name>41.7450</gml:name>
   <station>41.7450</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938275032"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation12943228282969">
     <gml:description>Übernommen aus Datei: PROF0041.7450.txt</gml:description>
     <gml:name>0041.7450</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282831111">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.9 0.8820000 13.51920 10.00000 1.000000 -0.3556327 -0.2989487 3.197442E-14
363.5 1.439000 27.49200 30.00000 1.000000 0.6537195 0.9525188 -9.736656E-14
363.9 1.838000 38.85980 50.00000 1.000000 0.6019924 -0.1248545 5.040413E-14
364.2 2.191000 49.89070 70.00000 1.000000 -0.05841032 -0.5819265 6.372680E-14
364.5 2.501000 60.36070 90.00000 1.000000 -0.8132500 -0.7456950 -1.909584E-14
364.8 2.776000 70.28760 110.0000 1.000000 -1.212605 -0.5007722 -6.961098E-14
365.1 3.026000 79.81440 130.0000 1.000000 -0.8889140 0.6008788 3.796963E-14
365.3 3.244000 88.70914 150.0000 1.001660 0.1208803 1.550955 0.0001073879
365.5 3.419000 97.09625 170.0000 1.011010 0.8402141 0.3930057 0.00002088445
365.6 3.581000 106.3450 190.0000 1.027140 1.147959 -0.3232648 -0.0003923602
365.8 3.732000 116.4027 210.0000 1.047190 1.149026 -0.5596817 -0.0005618666
365.9 3.873000 127.0375 230.0000 1.068020 1.064111 -0.3761139 0.0005646293
366.0 4.003000 138.2155 250.0000 1.088940 0.7348364 -0.1435865 0.001574946
366.2 4.123000 150.1127 270.0000 1.110610 -0.08231197 0.03544588 0.0004125134
366.3 4.234000 162.4569 290.0000 1.131050 -1.174766 0.08523695 -0.001912484
366.4 4.337000 174.3949 310.0000 1.145630 -1.733404 -0.03731138 -0.001391042
366.5 4.434000 185.9100 330.0000 1.155840 -1.640630 -0.1271153 0.0002327727
366.6 4.526000 197.0767 350.0000 1.163000 -0.9417801 -0.1146163 0.001343431
366.6 4.614000 207.9005 370.0000 1.167630 0.4149119 0.1106942 0.001176019
366.7 4.697000 218.4190 390.0000 1.170410 2.174054 0.2051516 -0.001174832
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884219">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-7.93557617337 4.46320678587 21.9706421433 -5.84564371985 1.02361167616</math:coefficients>
     <math:minRange>0.882</math:minRange>
     <math:maxRange>4.697</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884285">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-3.09946862215 7.87764213736 17.0018855261 -6.59797272566 1.01764017495</math:coefficients>
     <math:minRange>0.882</math:minRange>
     <math:maxRange>4.697</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884265">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.00000000001 -1.33787060715E-11 1.12802796469E-11 -3.95965416542E-12 4.94630934528E-13</math:coefficients>
     <math:minRange>0.882</math:minRange>
     <math:maxRange>3.026</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884257">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-6.38317992205 7.26075011249 -2.37995970532 0.260020340135</math:coefficients>
     <math:minRange>3.026</math:minRange>
     <math:maxRange>3.244</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884214">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-1.03596332455 3.42797890743 -1.89314300433 0.42598022663 -0.0334312435014</math:coefficients>
     <math:minRange>3.244</math:minRange>
     <math:maxRange>4.697</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228283271">
   <gml:description>Gelesen aus: PROF0041.8000.txt</gml:description>
   <gml:name>41.8000</gml:name>
   <station>41.8000</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938275181"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282832710">
     <gml:description>Übernommen aus Datei: PROF0041.8000.txt</gml:description>
     <gml:name>0041.8000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282832715">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.9 0.8770000 13.21160 10.00000 1.000000 -0.6817345 -0.6252367 0E-7
363.5 1.449000 26.63940 30.00000 1.000000 1.658579 1.966940 0E-7
363.9 1.854000 37.19970 50.00000 1.000000 0.5940084 -0.4508065 0E-7
364.3 2.233000 48.02640 70.00000 1.000000 -0.8543588 -1.093293 0E-7
364.6 2.565000 58.50510 90.00000 1.000000 -1.738517 -0.7186835 0E-7
364.9 2.841000 67.88917 110.0000 1.000500 -1.623914 -0.6577496 0.003714565
365.1 3.084000 76.69569 130.0000 1.002260 -0.6174156 -0.2017278 -0.009688517
365.3 3.307000 85.22973 150.0000 1.004350 1.239950 1.191700 0.0001030676
365.5 3.494000 93.65366 170.0000 1.014860 2.660684 1.271466 0.009214919
365.7 3.659000 103.9020 190.0000 1.040540 2.018190 0.7469914 0.004337925
365.8 3.811000 114.9035 210.0000 1.066740 0.6679393 0.2458485 -0.002045376
366.0 3.953000 125.6251 230.0000 1.086540 -0.3152518 -0.1103770 -0.004157215
366.1 4.086000 136.0428 250.0000 1.101410 -0.9603228 -0.3973930 -0.004032823
366.2 4.212000 146.1459 270.0000 1.112050 -1.210718 -0.4963258 -0.002361579
366.4 4.331000 155.9112 290.0000 1.119200 -1.130392 -0.5614246 0.0001858244
366.5 4.444000 165.4425 310.0000 1.124600 -0.8203505 -0.5692603 0.002197493
366.6 4.552000 174.8770 330.0000 1.129380 -0.3969017 -0.4618682 0.002927997
366.7 4.656000 184.3369 350.0000 1.134280 0.05619300 -0.1468760 0.002033876
366.8 4.756000 193.8007 370.0000 1.139100 0.5141823 0.2933618 0.00008083508
366.9 4.852000 203.2598 390.0000 1.143800 0.9401491 0.7747146 -0.002510991
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884276">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-30.2972251678 54.2213914958 -14.0761115623 4.38619774236 -0.0210143831016</math:coefficients>
     <math:minRange>0.877</math:minRange>
     <math:maxRange>4.852</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842127">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-29.020533862 66.6594813219 -27.4546270358 6.53738621862 -0.343927898034</math:coefficients>
     <math:minRange>0.877</math:minRange>
     <math:maxRange>4.852</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842201">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.877</math:minRange>
     <math:maxRange>2.565</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842195">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>37.9980641201 -41.7431686712 15.677874358 -1.95992318169</math:coefficients>
     <math:minRange>2.565</math:minRange>
     <math:maxRange>2.841</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884289">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>10.1884254382 -9.21442080774 3.36199578011 -0.529258482707 0.0306162389352</math:coefficients>
     <math:minRange>2.841</math:minRange>
     <math:maxRange>4.852</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282832714">
   <gml:description>Gelesen aus: PROF0041.9000.txt</gml:description>
   <gml:name>41.9000</gml:name>
   <station>41.9000</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938275184"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282832717">
     <gml:description>Übernommen aus Datei: PROF0041.9000.txt</gml:description>
     <gml:name>0041.9000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition12943228283420">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.0 0.9760000 12.81380 10.00000 1.000000 0.3128651 -0.7180877 4.152234E-14
362.7 1.618000 26.33710 30.00000 1.000000 -0.8787863 2.171079 -1.756373E-13
363.1 2.069000 37.25790 50.00000 1.000000 -0.08799616 0.1871842 1.794120E-13
363.5 2.465000 47.78800 70.00000 1.000000 0.5310339 -1.482656 6.772360E-14
363.9 2.811000 57.71500 90.00000 1.000000 0.7103802 -2.750381 -1.767475E-13
364.2 3.161000 68.62450 110.0000 1.000000 0.4355893 -0.4882753 6.439294E-14
364.5 3.466000 79.08921 130.0000 1.000000 -0.1547540 2.212923 -0.00006059732
364.8 3.707000 88.00040 150.0000 1.001360 -0.5893516 2.685823 0.0001624038
364.9 3.899000 95.49810 170.0000 1.004010 -0.7124670 0.7930874 -0.0004064989
365.1 4.082000 103.0021 190.0000 1.007820 -0.5091460 -0.3263720 -0.0003216498
365.3 4.257000 110.5136 210.0000 1.012340 0.1253767 -0.6742185 0.001352824
365.5 4.425000 118.0300 230.0000 1.017330 1.297221 -0.2346683 0.004994447
365.6 4.568000 128.9661 250.0000 1.040170 -1.451763 -1.546132 -0.008280793
365.8 4.714000 137.4254 270.0000 1.047570 -0.6829251 -1.171737 -0.003807412
365.9 4.854000 145.8591 290.0000 1.054170 0.6706951 -0.3439891 0.002876121
366.0 4.989000 154.3052 310.0000 1.060530 2.642485 0.9948306 0.01090116
366.1 5.105000 167.0531 330.0000 1.088170 -0.3000320 0.3576957 -0.003339531
366.3 5.219000 178.0486 350.0000 1.103420 -0.8150496 0.3485900 -0.004701293
366.4 5.327000 188.6547 370.0000 1.114610 -0.6535993 0.1984403 -0.002297771
366.5 5.429000 198.8691 390.0000 1.122390 0.1102238 -0.2131357 0.002928593
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842238">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-26.2216815983 39.7894036092 -5.16029141599 1.59583298553 0.111347832128</math:coefficients>
     <math:minRange>0.976</math:minRange>
     <math:maxRange>5.429</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D12943228288421">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>18.4079838722 -29.7330454049 32.0195536773 -8.04429761359 0.789036667197</math:coefficients>
     <math:minRange>0.976</math:minRange>
     <math:maxRange>5.429</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884217">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.00000000002 -3.72583646953E-11 2.95101133528E-11 -9.77268720693E-12 1.15494290163E-12</math:coefficients>
     <math:minRange>0.976</math:minRange>
     <math:maxRange>3.161</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842184">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-2.0963208559 2.85054014985 -0.873921266957 0.0892184162204</math:coefficients>
     <math:minRange>3.161</math:minRange>
     <math:maxRange>3.466</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842149">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-2.46908762733 3.3638379469 -1.20097973754 0.185568964143 -0.010318504317</math:coefficients>
     <math:minRange>3.466</math:minRange>
     <math:maxRange>5.429</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282835816">
   <gml:description>Gelesen aus: PROF0042.0000.txt</gml:description>
   <gml:name>42.0000</gml:name>
   <station>42.0000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382751810"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282835819">
     <gml:description>Übernommen aus Datei: PROF0042.0000.txt</gml:description>
     <gml:name>0042.0000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282835821">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.0 0.9900000 12.71470 10.00000 1.000000 0.05328541 0.1835407 0E-7
361.7 1.602000 25.41490 30.00000 1.000000 -0.1103314 -0.3477361 0E-7
362.1 2.042000 35.53470 50.00000 1.000000 -0.03832965 -0.4801135 0E-7
362.5 2.429000 45.12380 70.00000 1.000000 0.04170992 0.2728122 0E-7
362.8 2.767000 54.06610 90.00000 1.000000 0.04833215 0.6255612 0E-7
363.1 3.072000 62.54540 110.0000 1.000000 0.04129839 0.6915264 0E-7
363.4 3.350000 70.60900 130.0000 1.000000 0.03588367 0.4029166 0E-7
363.7 3.604000 78.29100 150.0000 1.000000 0.01085683 -0.3367704 0E-7
363.9 3.839000 85.61934 170.0000 1.000000 0.03438368 -1.365330 0.0001660846
364.1 4.076000 93.32019 190.0000 1.000650 0.02960696 -0.9974579 -0.0003343901
364.3 4.300000 100.9377 210.0000 1.001820 -0.03093509 -0.4790933 -0.0001066295
364.6 4.513000 108.4690 230.0000 1.003150 -0.09754442 0.3084476 0.0001941868
364.8 4.715000 115.8406 250.0000 1.004480 -0.1155670 1.305412 0.0003377239
364.9 4.900000 122.8499 270.0000 1.005840 -0.1348642 1.751769 0.0002634325
365.1 5.059000 129.0117 290.0000 1.007520 -0.07800037 0.3468518 -0.0001851591
365.3 5.213000 135.1688 310.0000 1.009450 -0.01214914 -0.6605574 -0.0005397297
365.4 5.364000 141.3203 330.0000 1.011690 0.1423586 -1.021515 -0.0005199539
365.6 5.511000 147.4684 350.0000 1.014140 0.3405790 -0.8589073 0.0003277771
365.7 5.654000 153.8637 370.0000 1.018170 0.3279500 -0.1813609 0.001015406
365.8 5.792000 161.0474 390.0000 1.026270 -0.4885234 0.8400047 -0.0006187487
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842244">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>7.41222877832 -20.6291136079 27.7267316004 -4.49519767104 0.396473150674</math:coefficients>
     <math:minRange>0.99</math:minRange>
     <math:maxRange>5.792</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884277">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-2.19844797226 10.9696838448 4.80995931847 -0.689608130394 0.0638471562727</math:coefficients>
     <math:minRange>0.99</math:minRange>
     <math:maxRange>5.792</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D12943228288425">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.99</math:minRange>
     <math:maxRange>3.604</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842169">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>6.17252969815 -4.20762680228 1.14028752862 -0.10294922709</math:coefficients>
     <math:minRange>3.604</math:minRange>
     <math:maxRange>3.839</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842200">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>4.43886920433 -3.01881100232 0.98837364412 -0.143232102737 0.00777072894466</math:coefficients>
     <math:minRange>3.839</math:minRange>
     <math:maxRange>5.792</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228283740">
   <gml:description>Gelesen aus: PROF0042.0350.txt</gml:description>
   <gml:name>42.0350</gml:name>
   <station>42.0350</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938275348"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282837414">
     <gml:description>Übernommen aus Datei: PROF0042.0350.txt</gml:description>
     <gml:name>0042.0350</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282838925">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.7 0.7450000 14.72490 10.00000 1.000000 0.002344384 -0.2063245 0E-7
361.2 1.232000 29.06390 30.00000 1.000000 0.001173419 0.8491644 0E-7
361.5 1.578000 40.00140 50.00000 1.000000 -0.008023871 -0.1338781 0E-7
361.8 1.868000 49.61060 70.00000 1.000000 -0.01362570 -1.009523 0E-7
362.1 2.135000 58.77860 90.00000 1.000000 0.004240449 -0.6160086 0E-7
362.3 2.378000 67.40380 110.0000 1.000000 0.01888281 0.3266167 0E-7
362.5 2.593000 75.27625 130.0000 1.000170 0.01018245 0.7289096 0.00001277178
362.7 2.785000 82.48283 150.0000 1.001200 -0.0002822210 0.3796539 0.000001263297
362.9 2.968000 89.48213 170.0000 1.002680 0.01226979 0.2923097 -0.00002321915
363.1 3.142000 96.31004 190.0000 1.004410 -0.008784109 0.2115621 -0.00004553770
363.3 3.309000 102.9652 210.0000 1.006220 -0.0009125738 0.1401744 -0.00001771116
363.4 3.470000 109.5439 230.0000 1.007990 -0.03328938 0.005873381 0.00008377393
363.6 3.626000 115.9880 250.0000 1.009800 -0.01698645 -0.2367577 0.0001060215
363.7 3.777000 122.3357 270.0000 1.011640 0.001142992 -0.7412962 -0.000008501092
363.9 3.923000 128.5847 290.0000 1.013320 0.01534723 -1.650105 -0.0001230885
364.0 4.087000 135.7442 310.0000 1.014820 0.02163639 -0.06160261 -0.00005270850
364.2 4.247000 142.8929 330.0000 1.016030 0.001311924 0.9886789 0.000009557675
364.4 4.403000 149.9724 350.0000 1.016960 0.007177237 1.335040 0.000008721961
364.5 4.553000 156.9326 370.0000 1.017390 -0.009598792 0.5710066 0.0001259526
364.7 4.699000 163.8130 390.0000 1.017750 -0.004205976 -1.173494 -0.00007729655
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842205">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-15.9904641049 34.7836680187 -5.41578858439 7.61099373406 -0.879370243559</math:coefficients>
     <math:minRange>0.745</math:minRange>
     <math:maxRange>4.699</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842282">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-4.54774524 23.482018165 3.42547108759 -0.313855593974 0.0306482822497</math:coefficients>
     <math:minRange>0.745</math:minRange>
     <math:maxRange>4.699</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842271">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.745</math:minRange>
     <math:maxRange>2.378</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884258">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.387872293656 0.759517862719 -0.31404425556 0.0432709504873</math:coefficients>
     <math:minRange>2.378</math:minRange>
     <math:maxRange>2.593</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884293">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.1183355353 -0.122917086753 0.0415185742264 -0.00483794760981 1.27451142577E-4</math:coefficients>
     <math:minRange>2.593</math:minRange>
     <math:maxRange>4.699</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228283893">
   <gml:description>Gelesen aus: PROF0042.1000.txt</gml:description>
   <gml:name>42.1000</gml:name>
   <station>42.1000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382753413"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282838915">
     <gml:description>Übernommen aus Datei: PROF0042.1000.txt</gml:description>
     <gml:name>0042.1000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282838911">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.5 0.9210000 12.93560 10.00000 1.000000 0.3113077 1.622266 0E-7
362.1 1.554000 26.94190 30.00000 1.000000 -0.8569948 -3.608991 0E-7
362.6 2.010000 38.47310 50.00000 1.000000 -0.1255949 -1.776822 0E-7
362.9 2.385000 48.87020 70.00000 1.000000 0.4496202 1.100108 0E-7
363.3 2.710000 58.53390 90.00000 1.000000 0.6304500 3.665369 0E-7
363.5 2.964000 66.50178 110.0000 1.001140 0.5177722 2.764708 -0.002166247
363.8 3.196000 74.10661 130.0000 1.004150 0.2365897 1.214181 0.002958200
364.0 3.421000 81.79849 150.0000 1.008030 -0.1540611 -0.01390742 0.002346077
364.2 3.639000 89.55300 170.0000 1.011910 -0.5577986 -0.9874112 -0.0002294932
364.4 3.847000 97.17051 190.0000 1.015630 -0.7992906 -2.014277 -0.002609354
364.6 4.047000 104.7444 210.0000 1.019280 -0.8263912 -2.939118 -0.003617778
364.8 4.250000 112.7138 230.0000 1.023010 -0.5387048 -2.637094 -0.002389073
365.0 4.447000 120.6253 250.0000 1.026610 0.2933544 -1.899024 0.001595978
365.2 4.637000 128.4810 270.0000 1.030110 1.714014 -0.7858413 0.008268398
365.4 4.825000 139.4894 290.0000 1.050080 0.8701194 1.340602 0.001135559
365.6 5.019000 152.2133 310.0000 1.069510 -0.1416828 5.653110 -0.002395085
365.7 5.156000 161.8452 330.0000 1.083140 -0.6363380 3.846744 -0.003433845
365.8 5.277000 170.5486 350.0000 1.093680 -0.5976141 0.6989987 -0.002139315
366.0 5.398000 179.5661 370.0000 1.103360 -0.1793520 -1.647639 0.0004593637
366.1 5.516000 188.9265 390.0000 1.113760 0.3905954 -3.595962 0.002216615
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884264">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>40.8349843861 -75.6954913916 56.136384129 -9.80341053776 0.756574110096</math:coefficients>
     <math:minRange>0.921</math:minRange>
     <math:maxRange>5.516</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842298">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>13.4571491527 -19.082406696 25.5898902524 -6.07737010513 0.564388265546</math:coefficients>
     <math:minRange>0.921</math:minRange>
     <math:maxRange>5.516</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884292">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.921</math:minRange>
     <math:maxRange>2.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884229">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-18.7360055624 21.1844756318 -7.57231536025 0.901289180918</math:coefficients>
     <math:minRange>2.71</math:minRange>
     <math:maxRange>2.964</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842199">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-1.66431848261 2.57077842872 -0.909260468211 0.139082443827 -0.00764466688178</math:coefficients>
     <math:minRange>2.964</math:minRange>
     <math:maxRange>5.516</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282840531">
   <gml:description>Gelesen aus: PROF0042.2000.txt</gml:description>
   <gml:name>42.2000</gml:name>
   <station>42.2000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382754914"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation12943228284059">
     <gml:description>Übernommen aus Datei: PROF0042.2000.txt</gml:description>
     <gml:name>0042.2000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282842015">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.2 0.7710000 14.62290 10.00000 1.000000 0.01384809 -0.3873006 0E-7
361.7 1.293000 29.18890 30.00000 1.000000 -0.02638846 1.081623 0E-7
362.1 1.676000 40.66750 50.00000 1.000000 -0.01434179 0.1416190 0E-7
362.4 1.998000 50.85290 70.00000 1.000000 0.002385070 -0.6828013 0E-7
362.7 2.282000 60.24610 90.00000 1.000000 0.01463587 -0.9743345 0E-7
363.0 2.540000 69.10330 110.0000 1.000000 0.02988616 -0.6240213 0E-7
363.2 2.777000 77.53780 130.0000 1.000000 0.01835320 0.1777508 0E-7
363.4 2.998000 85.63140 150.0000 1.000000 0.01208926 1.383052 0E-7
363.6 3.188000 92.77610 170.0000 1.000270 -0.001678743 0.9663370 -0.00002978565
363.8 3.363000 99.51118 190.0000 1.001340 -0.02458535 0.05521514 0.00007199786
364.0 3.534000 106.2073 210.0000 1.002890 -0.02961426 -0.3826105 -0.00001665421
364.1 3.702000 112.9063 230.0000 1.004570 -0.02885960 -0.3632207 -0.00004001527
364.3 3.865000 119.5037 250.0000 1.006300 -0.007615774 -0.2648300 -0.00001572618
364.5 4.023000 126.0294 270.0000 1.008070 -0.007755921 -0.2374240 0.000009192400
364.6 4.178000 132.5302 290.0000 1.009880 -0.002819387 -0.1624344 0.00002183751
364.8 4.330000 138.9737 310.0000 1.011730 0.03238053 -0.1631078 0.000002986313
364.9 4.482000 145.5406 330.0000 1.013570 0.04106402 0.03970645 0.00003287219
365.1 4.634000 152.2446 350.0000 1.015550 0.008282641 0.3306012 -0.00003256442
365.2 4.784000 158.9276 370.0000 1.017490 0.001303115 0.3187670 -0.00002340505
365.4 4.931000 165.5900 390.0000 1.019440 -0.03056867 -0.2525873 0.00001926451
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842210">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-21.0217965861 45.2036019486 -12.7245726615 7.8864371956 -0.758257781244</math:coefficients>
     <math:minRange>0.771</math:minRange>
     <math:maxRange>4.931</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842130">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-4.41123141673 22.8433819662 2.41232686656 0.0065642599448 -0.00357276193383</math:coefficients>
     <math:minRange>0.771</math:minRange>
     <math:maxRange>4.931</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842289">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.771</math:minRange>
     <math:maxRange>2.998</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884221">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-1.34783573161 2.3214885591 -0.765035062903 0.0840253919956</math:coefficients>
     <math:minRange>2.998</math:minRange>
     <math:maxRange>3.188</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842156">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.25907929643 -0.258545979531 0.09236070668 -0.0141274903024 8.17601941523E-4</math:coefficients>
     <math:minRange>3.188</math:minRange>
     <math:maxRange>4.931</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282842036">
   <gml:description>Gelesen aus: PROF0042.3000.txt</gml:description>
   <gml:name>42.3000</gml:name>
   <station>42.3000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382754926"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282842020">
     <gml:description>Übernommen aus Datei: PROF0042.3000.txt</gml:description>
     <gml:name>0042.3000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282842033">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.7 0.9250000 14.32200 10.00000 1.000000 0.02263203 -0.05384567 0E-7
361.2 1.440000 28.36570 30.00000 1.000000 -0.09620624 -0.1946738 0E-7
361.6 1.818000 39.61360 50.00000 1.000000 0.02096550 0.3463461 0E-7
361.9 2.132000 49.58960 70.00000 1.000000 0.08193916 0.5118878 0E-7
362.2 2.407000 58.77170 90.00000 1.000000 0.09191740 0.2276830 0E-7
362.5 2.654000 67.40160 110.0000 1.000000 0.03188762 -0.5026863 0E-7
362.7 2.891000 75.98010 130.0000 1.000000 -0.03288571 -0.6108729 0E-7
362.9 3.114000 84.35430 150.0000 1.000000 -0.1137821 -0.5729267 0E-7
363.1 3.324000 92.49250 170.0000 1.000000 -0.1629480 -0.4316664 0E-7
363.3 3.523000 100.4220 190.0000 1.000000 -0.1449630 -0.08750607 0E-7
363.5 3.711000 108.1240 210.0000 1.000000 -0.05298631 0.3857806 0E-7
363.7 3.890000 115.6490 230.0000 1.000000 0.1348028 1.144242 0E-7
363.9 4.056000 122.8041 250.0000 1.000070 0.4186297 1.625163 -0.000003217030
364.0 4.200000 129.6832 270.0000 1.003990 0.2410376 0.4529831 0.00001499102
364.1 4.338000 136.6017 290.0000 1.007910 -0.01624805 -0.4881083 -0.00002928546
364.3 4.471000 143.4517 310.0000 1.011410 -0.2046799 -1.094723 0.00003787750
364.4 4.601000 150.2431 330.0000 1.014670 -0.2351335 -1.083214 -0.00004652915
364.5 4.726000 156.9489 350.0000 1.017280 -0.1880583 -0.7770727 0.00004738610
364.7 4.847000 163.5987 370.0000 1.019660 -0.04701686 -0.04977369 -0.00002781378
364.8 4.965000 170.1815 390.0000 1.021650 0.2510962 1.252984 0.000006590782
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884263">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>8.68137272927 -30.4068734064 40.8717381533 -7.77746356767 0.786451306423</math:coefficients>
     <math:minRange>0.925</math:minRange>
     <math:maxRange>4.965</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842181">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-3.9332627223 13.4342644968 8.22138873697 -1.65958007967 0.177920012277</math:coefficients>
     <math:minRange>0.925</math:minRange>
     <math:maxRange>4.965</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842219">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.925</math:minRange>
     <math:maxRange>3.89</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884232">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-55.2162273502 42.772002973 -10.8456526906 0.916532293599</math:coefficients>
     <math:minRange>3.89</math:minRange>
     <math:maxRange>4.056</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842107">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>6.15369830997 -4.60518713002 1.51997554891 -0.220229028059 0.0118779346543</math:coefficients>
     <math:minRange>4.056</math:minRange>
     <math:maxRange>4.965</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282842026">
   <gml:description>Gelesen aus: PROF0042.4000.txt</gml:description>
   <gml:name>42.4000</gml:name>
   <station>42.4000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382756527"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282842022">
     <gml:description>Übernommen aus Datei: PROF0042.4000.txt</gml:description>
     <gml:name>0042.4000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282843633">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.1 0.8420000 13.42820 10.00000 1.000000 0.5720109 0.7103868 0E-7
362.7 1.470000 28.38450 30.00000 1.000000 -1.701348 -2.193867 0E-7
363.2 1.902000 40.56600 50.00000 1.000000 -0.1879928 0.1129175 0E-7
363.5 2.229000 50.81288 70.00000 1.001630 0.8858949 1.045638 -0.002102294
363.8 2.508000 60.30636 90.00000 1.004600 1.206964 1.154026 0.002541580
364.0 2.759000 69.36309 110.0000 1.007410 0.9779020 0.8544455 0.002642618
364.3 2.990000 78.14343 130.0000 1.009900 0.3514639 0.3222634 0.0007403656
364.5 3.206000 86.63605 150.0000 1.011990 -0.3778956 -0.2458940 -0.001628580
364.7 3.410000 94.87006 170.0000 1.013790 -0.9956378 -0.6881337 -0.003591870
364.9 3.601000 102.8596 190.0000 1.014670 -1.418777 -1.174357 -0.003830611
365.1 3.784000 110.6619 210.0000 1.015370 -1.375346 -1.229572 -0.002577364
365.2 3.958000 118.2713 230.0000 1.015530 -0.7766107 -0.9671592 0.0008773588
365.4 4.124000 125.7439 250.0000 1.015730 0.4696350 -0.2883679 0.006201555
365.6 4.282000 133.5052 270.0000 1.019290 2.015350 0.7820158 0.01020265
365.7 4.425000 143.6678 290.0000 1.037050 1.301340 1.164794 0.001559100
365.8 4.556000 154.1345 310.0000 1.053000 0.4944702 1.054965 -0.003880397
366.0 4.679000 164.9032 330.0000 1.067960 -0.2017622 0.8969936 -0.006879037
366.1 4.793000 175.6616 350.0000 1.080580 -0.6478167 0.3873047 -0.006429957
366.2 4.900000 186.1481 370.0000 1.089360 -0.5050996 -0.2770461 -0.001064612
366.3 4.999000 196.4638 390.0000 1.095900 -0.08674438 -1.421353 0.007219499
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884231">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>33.8383523718 -70.7676484878 61.0164540482 -12.3752218326 1.16842453164</math:coefficients>
     <math:minRange>0.842</math:minRange>
     <math:maxRange>4.999</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842215">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>29.3614460122 -53.3717908467 52.2980683256 -13.6923673229 1.34093099177</math:coefficients>
     <math:minRange>0.842</math:minRange>
     <math:maxRange>4.999</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884296">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0</math:coefficients>
     <math:minRange>0.842</math:minRange>
     <math:maxRange>1.902</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884298">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-2.06993081381 4.59820782191 -2.2893025641 0.378731257111</math:coefficients>
     <math:minRange>1.902</math:minRange>
     <math:maxRange>2.229</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842279">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.572263916778 0.404433866638 -0.119779028939 0.0100234415916 4.00624634647E-4</math:coefficients>
     <math:minRange>2.229</math:minRange>
     <math:maxRange>4.999</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282845244">
   <gml:description>Gelesen aus: PROF0042.5000.txt</gml:description>
   <gml:name>42.5000</gml:name>
   <station>42.5000</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938275812"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282845238">
     <gml:description>Übernommen aus Datei: PROF0042.5000.txt</gml:description>
     <gml:name>0042.5000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282845242">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.2 1.054000 12.15310 10.00000 1.000000 4.584419 3.914346 0E-7
362.9 1.747000 25.38990 30.00000 1.000000 -16.95794 -13.74716 0E-7
363.4 2.258000 36.80600 50.00000 1.000000 5.189256 3.005598 0E-7
363.8 2.683000 47.36640 70.00000 1.000000 14.99120 11.61686 0E-7
364.2 3.047000 57.21300 90.00000 1.000000 10.27047 9.248893 0E-7
364.5 3.370000 66.52520 110.0000 1.000000 -1.935969 0.6666412 0E-7
364.8 3.661000 75.42210 130.0000 1.000000 -13.92209 -8.813634 0E-7
365.0 3.928000 83.98140 150.0000 1.000000 -18.88218 -14.32062 0E-7
365.3 4.161000 91.77942 170.0000 1.000420 -12.01623 -13.42325 -0.000006936370
365.5 4.379000 106.3995 190.0000 1.055720 2.956663 -2.654518 0.00004988853
365.6 4.519000 123.8132 210.0000 1.119630 15.63841 4.906024 -0.0001005870
365.8 4.652000 140.7603 230.0000 1.159190 37.33459 18.14451 0.00009485924
365.8 4.737000 164.5729 250.0000 1.229710 44.16300 23.55558 -0.00003722437
365.9 4.771000 241.2727 270.0000 1.426190 -18.85080 14.72208 -1.426190
365.9 4.776000 243.1503 290.0000 1.427390 -18.64363 -3.585242 -1.427390
365.9 4.827000 263.6953 310.0000 1.439270 -16.83254 -5.555111 -1.439270
366.0 4.877000 284.1667 330.0000 1.441280 -13.38219 -6.474354 -1.441280
366.0 4.925000 303.6950 350.0000 1.437490 -7.979989 -6.779742 -1.437490
366.1 4.969000 322.3294 370.0000 1.430610 -1.986957 -7.483307 -1.430610
366.1 5.013000 340.4823 390.0000 1.421240 6.262509 -6.943596 -1.421240
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D12943228288426">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>403.716735421 -771.471436413 503.647159518 -128.649454429 11.7127976892</math:coefficients>
     <math:minRange>1.054</math:minRange>
     <math:maxRange>5.013</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842283">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>545.263185938 -1041.20519421 682.40695737 -178.964265835 16.4958031008</math:coefficients>
     <math:minRange>1.054</math:minRange>
     <math:maxRange>5.013</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842227">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>1.054</math:minRange>
     <math:maxRange>3.928</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842231">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>905.156704958 -677.629835831 169.224106435 -14.081426132</math:coefficients>
     <math:minRange>3.928</math:minRange>
     <math:maxRange>4.161</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884261">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>7313.65723773 -6572.77765547 2213.84751729 -331.193385379 18.5692200941</math:coefficients>
     <math:minRange>4.161</math:minRange>
     <math:maxRange>4.737</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282845241">
   <gml:description>Gelesen aus: PROF0042.6000.txt</gml:description>
   <gml:name>42.6000</gml:name>
   <station>42.6000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382758112"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation12943228284524">
     <gml:description>Übernommen aus Datei: PROF0042.6000.txt</gml:description>
     <gml:name>0042.6000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282845246">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.5 0.9490000 13.47720 10.00000 1.000000 2.115098 5.862078 0E-7
363.2 1.638000 27.55220 30.00000 1.000000 -9.249278 -21.08881 0E-7
363.6 2.111000 38.40870 50.00000 1.000000 4.361520 3.103558 0E-7
364.0 2.498000 48.01703 70.00000 1.000000 8.627109 16.79585 -0.01314936
364.4 2.826000 56.69465 90.00000 1.002180 4.418365 15.70993 0.03807489
364.6 3.122000 65.00140 110.0000 1.004950 -3.469419 4.895269 -0.007658928
364.9 3.403000 73.29199 130.0000 1.008060 -10.05024 -10.10670 -0.03982756
365.2 3.670000 81.52719 150.0000 1.011480 -9.555473 -23.40891 -0.01853867
365.4 3.922000 89.61432 170.0000 1.015120 3.666604 -28.77614 0.05516042
365.9 4.362000 159.2045 190.0000 1.234600 25.83090 20.99895 0.02795214
366.0 4.446000 180.1378 210.0000 1.252610 33.64997 24.26680 0.03817421
366.0 4.511000 252.7020 230.0000 1.345120 -13.56241 25.14061 -0.03832737
366.0 4.514000 253.8844 250.0000 1.344870 -13.50566 6.168480 -0.03748758
366.1 4.551000 269.2852 270.0000 1.340040 -13.10373 -0.6661379 -0.02660294
366.1 4.596000 287.8342 290.0000 1.331340 -11.09770 -3.393889 -0.01386954
366.2 4.645000 308.6270 310.0000 1.318000 -7.765120 -2.929612 -0.0008724475
366.2 4.682000 326.0370 330.0000 1.310010 -5.699983 -6.273476 0.003171284
366.2 4.720000 342.5904 350.0000 1.295960 -1.078714 -8.038134 0.009498149
366.3 4.759000 359.8800 370.0000 1.280560 4.645538 -8.082755 0.01273475
366.3 4.792000 374.2260 390.0000 1.267790 10.82263 -10.17696 0.01156856
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842286">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>516.474532016 -1053.60898555 720.623628224 -192.343343994 18.0724796514</math:coefficients>
     <math:minRange>0.949</math:minRange>
     <math:maxRange>4.792</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842285">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>323.655513072 -681.069944926 500.073343956 -145.234859078 14.8363963443</math:coefficients>
     <math:minRange>0.949</math:minRange>
     <math:maxRange>4.792</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842326">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0</math:coefficients>
     <math:minRange>0.949</math:minRange>
     <math:maxRange>2.111</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842254">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-41.462889792 57.0722460682 -25.4852272814 3.77937941139</math:coefficients>
     <math:minRange>2.111</math:minRange>
     <math:maxRange>2.498</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842122">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-35.0694868469 42.7544415584 -18.6252096137 3.52988124464 -0.245135616263</math:coefficients>
     <math:minRange>2.498</math:minRange>
     <math:maxRange>4.792</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282846720">
   <gml:description>Gelesen aus: PROF0042.7000.txt</gml:description>
   <gml:name>42.7000</gml:name>
   <station>42.7000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382759635"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282846740">
     <gml:description>Übernommen aus Datei: PROF0042.7000.txt</gml:description>
     <gml:name>0042.7000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282846737">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.7 1.006000 18.02812 10.00000 1.131440 1.476686 3.749618 -0.002692650
363.5 1.793000 36.55740 30.00000 1.201490 -8.996003 -17.20698 0.01137292
364.0 2.328000 50.86790 50.00000 1.227850 9.279176 9.518662 -0.003832233
364.4 2.765000 63.39690 70.00000 1.245060 9.272881 16.42989 -0.01146487
364.8 3.152000 75.13910 90.00000 1.259280 -3.053519 6.012617 -0.008591010
365.2 3.508000 86.52100 110.0000 1.271360 -15.93225 -11.25056 0.003520144
365.5 3.847000 97.80260 130.0000 1.282270 -15.48803 -22.19459 0.02092011
365.8 4.161000 108.7094 150.0000 1.292220 12.40980 -13.21358 0.03744502
366.0 4.342000 133.6423 170.0000 1.391260 29.59409 -0.4518934 -0.04855879
366.2 4.527000 152.7728 190.0000 1.414250 73.91539 30.38619 -0.06175048
366.3 4.615000 296.0989 210.0000 1.465220 -30.54209 42.11672 -0.1098201
366.3 4.615000 296.0989 230.0000 1.465240 -30.54209 22.11672 -0.1098401
366.3 4.623000 299.2853 250.0000 1.461210 -29.88690 5.271986 -0.1056136
366.3 4.644000 307.1909 270.0000 1.451090 -27.45362 -6.220702 -0.09503502
366.4 4.685000 323.3604 290.0000 1.430330 -22.34798 -8.647394 -0.07362773
366.4 4.727000 339.9387 310.0000 1.407900 -15.58311 -9.270568 -0.05089502
366.5 4.775000 358.5838 330.0000 1.382480 -5.543572 -5.337159 -0.02560787
366.5 4.805000 370.6874 350.0000 1.366050 1.411678 -9.367199 -0.009536642
366.5 4.849000 387.8970 370.0000 1.343900 13.79207 -4.475231 0.01167974
366.6 4.880000 400.0698 390.0000 1.329270 23.67531 -5.849819 0.02534613
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884281">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>574.089141858 -1134.90553518 756.278919753 -199.37311259 18.5286497431</math:coefficients>
     <math:minRange>1.006</math:minRange>
     <math:maxRange>4.88</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842185">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>455.356014866 -918.640267215 646.478898024 -180.88232756 17.768469104</math:coefficients>
     <math:minRange>1.006</math:minRange>
     <math:maxRange>4.88</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D12943228288422">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.006</math:minRange>
     <math:maxRange>1.006</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842235">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.006</math:minRange>
     <math:maxRange>1.006</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842120">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.645488776575 0.837215020242 -0.452447815914 0.105852079678 -0.00864575791862</math:coefficients>
     <math:minRange>1.006</math:minRange>
     <math:maxRange>4.88</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282846715">
   <gml:description>Gelesen aus: PROF0042.8000.txt</gml:description>
   <gml:name>42.8000</gml:name>
   <station>42.8000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382759625"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282846741">
     <gml:description>Übernommen aus Datei: PROF0042.8000.txt</gml:description>
     <gml:name>0042.8000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282846736">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.2 0.9470000 14.72951 10.00000 1.021190 1.098141 0.6430093 0.003799264
362.8 1.573000 30.53333 30.00000 1.019430 -2.766356 -1.561351 -0.009735557
363.3 1.991000 42.58449 50.00000 1.017780 -0.8155434 -0.6660604 -0.002438569
363.6 2.331000 53.27350 70.00000 1.017690 0.9802876 0.5649924 0.003239388
363.9 2.622000 63.11320 90.00000 1.017740 1.781073 1.382815 0.005955619
364.1 2.872000 71.92380 110.0000 1.017250 1.810347 1.091492 0.006512610
364.4 3.101000 80.23970 130.0000 1.017050 1.304079 0.5979571 0.004838460
364.6 3.315000 88.18090 150.0000 1.017020 0.4800078 0.1059761 0.001755514
364.8 3.516000 95.81950 170.0000 1.017280 -0.5024263 -0.3942259 -0.002104551
365.0 3.706000 103.1996 190.0000 1.017510 -1.448096 -0.8454398 -0.005694642
365.1 3.887000 110.3353 210.0000 1.017270 -2.134424 -1.135352 -0.007897310
365.3 4.060000 117.2814 230.0000 1.017150 -2.420137 -1.220131 -0.008658327
365.5 4.225000 124.0644 250.0000 1.017170 -2.189196 -1.158370 -0.007418422
365.6 4.385000 130.7030 270.0000 1.017260 -1.194309 -0.6192709 -0.003536008
365.8 4.539000 137.2085 290.0000 1.017390 0.6374077 0.2494262 0.003508247
365.9 4.689000 143.6081 310.0000 1.017590 3.510388 1.704906 0.01425434
366.1 4.834000 149.9055 330.0000 1.017860 7.473569 3.595186 0.02906832
366.2 4.942000 166.4374 350.0000 1.070300 -0.4536496 0.6906017 -0.008773475
366.3 5.054000 178.1063 370.0000 1.091550 -2.214881 -0.8184513 -0.01142823
366.4 5.162000 189.4374 390.0000 1.107020 -2.936282 -2.207710 -0.005246677
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842206">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>28.1727398738 -55.6346359949 45.837653952 -7.6433938243 0.671439578704</math:coefficients>
     <math:minRange>0.947</math:minRange>
     <math:maxRange>5.162</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842147">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>49.6815115926 -89.1376317084 71.6707801427 -17.6336528343 1.56708184407</math:coefficients>
     <math:minRange>0.947</math:minRange>
     <math:maxRange>5.162</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842111">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>0.947</math:minRange>
     <math:maxRange>0.947</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842117">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>0.947</math:minRange>
     <math:maxRange>0.947</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842323">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.21035476772 -0.369669491961 0.236962787523 -0.0615526697581 0.00556591713304</math:coefficients>
     <math:minRange>0.947</math:minRange>
     <math:maxRange>5.162</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282848313">
   <gml:description>Gelesen aus: PROF0042.8950.txt</gml:description>
   <gml:name>42.8950</gml:name>
   <station>42.8950</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382759617"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282848356">
     <gml:description>Übernommen aus Datei: PROF0042.8950.txt</gml:description>
     <gml:name>0042.8950</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282848364">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.0 1.001000 13.76730 10.00000 1.000000 0.8598895 0.04470658 5.440093E-14
362.6 1.609000 28.71130 30.00000 1.000000 -2.046669 0.1283176 -2.543521E-13
363.0 2.030000 40.64630 50.00000 1.000000 -0.6837788 -0.2878393 3.057554E-13
363.4 2.372000 51.34090 70.00000 1.000000 0.6387282 -0.4201325 5.462297E-14
363.7 2.667000 61.25950 90.00000 1.000000 1.343945 -0.1874107 -2.837730E-13
363.9 2.930000 70.62610 110.0000 1.000000 1.435676 0.4014488 1.230127E-13
364.2 3.165000 79.42972 130.0000 1.000000 1.039746 0.9088805 0.003110207
364.4 3.369000 87.33852 150.0000 1.000970 0.4435999 0.3674515 -0.005335691
364.6 3.562000 95.06409 170.0000 1.002490 -0.2666499 0.2185441 -0.002719027
364.7 3.742000 102.4553 190.0000 1.004230 -0.9269898 0.01034958 0.002125332
364.9 3.909000 109.4930 210.0000 1.006520 -1.448362 -0.5165174 0.004570679
365.1 4.070000 116.4412 230.0000 1.009000 -1.752140 -0.7272317 0.003829311
365.2 4.225000 123.2695 250.0000 1.010860 -1.729142 -0.7327188 0.0009835576
365.4 4.374000 129.9932 270.0000 1.012640 -1.325807 -0.6420848 -0.003090757
365.5 4.519000 136.6469 290.0000 1.014440 -0.4123082 -0.2753966 -0.006373942
365.7 4.660000 143.2293 310.0000 1.016190 1.085162 0.2958662 -0.006117236
365.8 4.797000 149.7531 330.0000 1.018000 3.217127 1.001344 0.0005900388
365.9 4.931000 156.2169 350.0000 1.019720 6.111936 1.930822 0.01725456
366.0 5.045000 171.6418 370.0000 1.069820 -0.5595499 0.2578450 -0.006125825
366.1 5.154000 185.2389 390.0000 1.104430 -5.024413 -1.776245 -0.002701204
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842182">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-8.12651888306 12.4997031239 3.14090210735 2.67045722455 -0.165976129071</math:coefficients>
     <math:minRange>1.001</math:minRange>
     <math:maxRange>5.154</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884297">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>38.139420296 -66.3256051078 54.4786557187 -12.7906508983 1.1166204136</math:coefficients>
     <math:minRange>1.001</math:minRange>
     <math:maxRange>5.154</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842342">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.00000000004 -8.94335594347E-11 7.31505675025E-11 -2.52573061006E-11 3.13461744878E-12</math:coefficients>
     <math:minRange>1.001</math:minRange>
     <math:maxRange>2.93</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842145">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>56.5004084021 -55.2671499806 18.3303560531 -2.02482407057</math:coefficients>
     <math:minRange>2.93</math:minRange>
     <math:maxRange>3.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884299">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>21.8780239714 -21.4187130512 8.1728173511 -1.3750955084 0.0861340311722</math:coefficients>
     <math:minRange>3.165</math:minRange>
     <math:maxRange>5.154</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282848359">
   <gml:description>Gelesen aus: PROF0042.9000.txt</gml:description>
   <gml:name>42.9000</gml:name>
   <station>42.9000</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938275969"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282848331">
     <gml:description>Übernommen aus Datei: PROF0042.9000.txt</gml:description>
     <gml:name>0042.9000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282848330">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.8 0.8620000 14.81270 10.00000 1.000000 0.01145171 -0.1236492 0E-7
362.4 1.379000 29.99000 30.00000 1.000000 -0.02818303 0.3730416 0E-7
362.7 1.749000 42.06170 50.00000 1.000000 -0.005391919 0.06505718 0E-7
363.0 2.054000 52.77990 70.00000 1.000000 0.009504417 -0.2383167 0E-7
363.3 2.319000 62.64300 90.00000 1.000000 0.01537132 -0.4339141 0E-7
363.5 2.555000 71.84130 110.0000 1.000000 0.02006501 -0.6073052 0E-7
363.8 2.783000 81.09790 130.0000 1.000000 0.01551859 0.5613185 0E-7
364.0 2.984000 89.56263 150.0000 1.000210 -0.005678854 0.8975627 -0.00002814875
364.1 3.162000 97.27104 170.0000 1.001120 -0.01728709 0.2349012 0.00006099233
364.3 3.332000 104.8013 190.0000 1.002390 -0.008385098 -0.1216586 -0.00001498546
364.5 3.494000 112.1719 210.0000 1.003670 -0.02713797 -0.3283215 0.000003271163
364.6 3.650000 119.3771 230.0000 1.005050 -0.001325642 -0.2744580 -0.00001622544
364.8 3.798000 126.3908 250.0000 1.006470 -0.02039778 -0.3475154 -0.00006712770
364.9 3.942000 133.2904 270.0000 1.007790 0.008669066 -0.1181112 0.000008095845
365.1 4.081000 140.0906 290.0000 1.009170 0.009399701 0.1928424 0.00003296738
365.2 4.214000 146.6859 310.0000 1.010510 0.02326496 0.3442033 0.00009670095
365.3 4.340000 153.0414 330.0000 1.012010 0.01889019 0.07751262 -0.000008769518
365.4 4.463000 159.3333 350.0000 1.013510 0.009722834 -0.06086531 -0.00007185597
365.6 4.583000 165.5623 370.0000 1.014970 -0.01260552 -0.1194819 -0.00003991627
365.7 4.701000 171.7414 390.0000 1.016460 -0.01546488 0.02715748 0.00004500143
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842108">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-9.40892170402 13.7834822955 8.18133244375 2.20034706471 -0.153066381321</math:coefficients>
     <math:minRange>0.862</math:minRange>
     <math:maxRange>4.701</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842190">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-5.6242008443 20.1252666204 4.25837375482 -0.0973829276239 -0.00255779513536</math:coefficients>
     <math:minRange>0.862</math:minRange>
     <math:maxRange>4.701</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884250">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.862</math:minRange>
     <math:maxRange>2.783</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282884252">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-0.639177008124 1.73899780268 -0.614806459336 0.0724336393461</math:coefficients>
     <math:minRange>2.783</math:minRange>
     <math:maxRange>2.984</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842325">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.17742127909 -0.193178419732 0.0754498627781 -0.0126986177033 8.17122164793E-4</math:coefficients>
     <math:minRange>2.984</math:minRange>
     <math:maxRange>4.701</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282848342">
   <gml:description>Gelesen aus: PROF0043.0000.txt</gml:description>
   <gml:name>43.0000</gml:name>
   <station>43.0000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382759633"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation12943228284837">
     <gml:description>Übernommen aus Datei: PROF0043.0000.txt</gml:description>
     <gml:name>0043.0000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282849858">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.7 1.034000 13.66160 10.00000 1.000000 3.346207 0.04668203 -3.808065E-14
362.3 1.612000 27.72010 30.00000 1.000000 -6.593629 0.06081905 1.236788E-13
362.7 2.027000 38.97660 50.00000 1.000000 -3.290021 -0.2192133 -7.571721E-14
363.0 2.369000 48.99930 70.00000 1.000000 0.9194182 -0.2854789 -7.849277E-14
363.3 2.667000 58.27210 90.00000 1.000000 3.731469 -0.08884435 3.019807E-14
363.6 2.934000 67.00070 110.0000 1.000000 4.787326 0.2812592 9.103829E-14
363.8 3.178000 75.31650 130.0000 1.000000 4.313225 0.7788831 -5.195844E-14
364.1 3.393000 82.91951 150.0000 1.000300 2.799862 0.3155594 0.01114256
364.3 3.595000 90.20541 170.0000 1.001130 0.7291300 -0.09252862 -0.01743194
364.5 3.787000 97.31854 190.0000 1.002210 -1.603941 -0.3874852 -0.01056872
364.6 3.972000 104.2864 210.0000 1.003430 -3.789883 -0.4334858 0.005098079
364.8 4.150000 111.1253 230.0000 1.004710 -5.482152 -0.3719559 0.01543822
365.0 4.322000 117.8440 250.0000 1.005810 -6.305803 -0.2184314 0.01520690
365.2 4.488000 124.4566 270.0000 1.006850 -5.937579 -0.08823126 0.004931752
365.3 4.650000 130.9789 290.0000 1.007930 -3.973527 0.1633152 -0.01062973
365.5 4.807000 137.4141 310.0000 1.009090 -0.1471712 0.3237652 -0.02331027
365.6 4.959000 143.7736 330.0000 1.010150 5.799349 0.3055316 -0.02269742
365.8 5.108000 150.0637 350.0000 1.011160 14.30248 0.2978712 0.003050346
365.9 5.253000 156.2663 370.0000 1.011830 25.55652 0.1011630 0.06704296
366.1 5.393000 231.1577 390.0000 1.230670 -29.16128 -0.4891942 -0.03727274
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842175">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-9.02883311295 11.239717841 4.6376522529 2.48548307609 -0.220844835162</math:coefficients>
     <math:minRange>1.034</math:minRange>
     <math:maxRange>5.393</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828842207">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>129.921708838 -230.103809312 150.623004376 -35.678540176 2.98910961175</math:coefficients>
     <math:minRange>1.034</math:minRange>
     <math:maxRange>5.393</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857125">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.99999999999 2.28613419593E-11 -1.75904667927E-11 5.69412381574E-12 -6.60770729289E-13</math:coefficients>
     <math:minRange>1.034</math:minRange>
     <math:maxRange>3.178</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885798">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>293.339395161 -269.399691341 82.7041967532 -8.45796044121</math:coefficients>
     <math:minRange>3.178</math:minRange>
     <math:maxRange>3.393</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857304">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>78.0117460795 -74.1551469686 26.5762174908 -4.2016887305 0.247298190252</math:coefficients>
     <math:minRange>3.393</math:minRange>
     <math:maxRange>5.393</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282849871">
   <gml:description>Gelesen aus: PROF0043.1000.txt</gml:description>
   <gml:name>43.1000</gml:name>
   <station>43.1000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382761214"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282849828">
     <gml:description>Übernommen aus Datei: PROF0043.1000.txt</gml:description>
     <gml:name>0043.1000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282849826">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.3 1.178000 15.41850 10.00000 1.000000 0.01330813 0.2652626 0E-7
361.9 1.691000 30.68780 30.00000 1.000000 -0.03365798 -0.4918111 0E-7
362.2 2.052000 42.25990 50.00000 1.000000 -0.01139369 -0.3975831 0E-7
362.5 2.353000 52.41100 70.00000 1.000000 0.02133523 0.08470108 0E-7
362.8 2.615000 61.64645 90.00000 1.000000 0.01893836 0.4623506 -0.00006054077
363.0 2.848000 70.13098 110.0000 1.000720 0.03007159 0.4515784 0.00007976727
363.2 3.063000 78.22202 130.0000 1.001770 0.01685440 0.3720587 0.00004195336
363.4 3.264000 86.02551 150.0000 1.002830 -0.02461595 0.2170129 0.00003164642
363.6 3.454000 93.55471 170.0000 1.003890 -0.02524230 0.01767247 0.000003328941
363.8 3.634000 100.8568 190.0000 1.004970 -0.01809879 -0.3223612 -0.00009319914
364.0 3.805000 107.9744 210.0000 1.006020 -0.02649118 -0.8571848 -0.0002162807
364.1 3.975000 115.2059 230.0000 1.006740 -0.02420302 -0.7639174 -0.00002307486
364.3 4.143000 122.4705 250.0000 1.007420 0.02997669 -0.1958978 0.0002001692
364.5 4.304000 129.6546 270.0000 1.008370 0.02605914 0.1668167 0.0001335597
364.6 4.460000 136.7670 290.0000 1.009370 0.03386319 0.4972869 0.00002949326
364.8 4.610000 143.8033 310.0000 1.010310 0.002659119 0.5974371 0.00001581421
364.9 4.754000 150.6839 330.0000 1.011360 0.0007274581 0.3992681 -0.00005459550
365.1 4.893000 157.5056 350.0000 1.012500 -0.03105386 -0.01790269 -0.0001327690
365.2 5.031000 164.3820 370.0000 1.013650 -0.01347670 -0.1205134 -0.00008044975
365.3 5.165000 171.2011 390.0000 1.014790 0.01444018 -0.3642738 0.0001251774
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857257">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>16.0288845984 -39.2122259416 32.2525228553 -2.82023372259 0.146589758961</math:coefficients>
     <math:minRange>1.178</math:minRange>
     <math:maxRange>5.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857309">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-12.7457873768 19.1133580994 4.65507820072 -0.548667806461 0.0515067629968</math:coefficients>
     <math:minRange>1.178</math:minRange>
     <math:maxRange>5.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857114">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>1.178</math:minRange>
     <math:maxRange>2.353</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857342">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.272399636342 0.894818590279 -0.366328052943 0.0499175112945</math:coefficients>
     <math:minRange>2.353</math:minRange>
     <math:maxRange>2.615</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857151">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.08082907229 -0.0982510698135 0.0419637545872 -0.00751852171355 5.03091669307E-4</math:coefficients>
     <math:minRange>2.615</math:minRange>
     <math:maxRange>5.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282849868">
   <gml:description>Gelesen aus: PROF0043.2000.txt</gml:description>
   <gml:name>43.2000</gml:name>
   <station>43.2000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382761225"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282849849">
     <gml:description>Übernommen aus Datei: PROF0043.2000.txt</gml:description>
     <gml:name>0043.2000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282851428">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.1 0.9950000 14.57610 10.00000 1.000000 0.02750268 0.4543449 0E-7
361.6 1.494000 28.50650 30.00000 1.000000 -0.07130244 -0.8309928 0E-7
362.0 1.866000 39.54800 50.00000 1.000000 -0.0005372979 -0.5142730 0E-7
362.3 2.179000 49.35070 70.00000 1.000000 0.02932030 0.1347574 0E-7
362.5 2.453000 58.29520 90.00000 1.000000 0.03636928 0.5450252 0E-7
362.8 2.701000 66.68180 110.0000 1.000000 0.02251679 0.7031834 0E-7
363.0 2.930000 74.65060 130.0000 1.000000 0.01267702 0.6171637 0E-7
363.2 3.143000 82.26460 150.0000 1.000000 0.0002416538 0.1830201 0E-7
363.4 3.347000 89.75850 170.0000 1.000000 -0.02932774 -0.1860125 0E-7
363.6 3.544000 97.16840 190.0000 1.000000 -0.05433451 -0.4331332 0E-7
363.8 3.735000 104.4620 210.0000 1.000000 -0.01521735 -0.5545984 0E-7
364.0 3.918000 111.6530 230.0000 1.000000 -0.01342135 -0.8449320 0E-7
364.2 4.098000 118.8710 250.0000 1.000000 0.01149363 -0.8239544 0E-7
364.4 4.281000 126.3980 270.0000 1.000000 0.02865497 0.1844295 0E-7
364.5 4.453000 133.6680 290.0000 1.000000 0.02513584 0.5457522 0E-7
364.7 4.618000 140.8240 310.0000 1.000000 0.009664776 0.6583208 0E-7
364.9 4.777000 147.8820 330.0000 1.000000 -0.0007796197 0.5988987 0E-7
365.0 4.930000 154.8400 350.0000 1.000000 -0.01364406 0.3294437 0E-7
365.2 5.078000 161.7010 370.0000 1.000000 0.004908282 -0.05307047 0E-7
365.3 5.220000 168.4740 390.0000 1.000000 -0.009920867 -0.7133726 0E-7
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885776">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>11.3234747225 -32.0738761695 35.5757619662 -4.5362691417 0.297962653102</math:coefficients>
     <math:minRange>0.995</math:minRange>
     <math:maxRange>5.22</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D12943228288573">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-8.4966376466 19.653827732 4.02887537275 -0.498248572696 0.0477544977465</math:coefficients>
     <math:minRange>0.995</math:minRange>
     <math:maxRange>5.22</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885769">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.995</math:minRange>
     <math:maxRange>5.22</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857138">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>5.22</math:minRange>
     <math:maxRange>5.22</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857323">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>5.22</math:minRange>
     <math:maxRange>5.22</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228285145">
   <gml:description>Gelesen aus: PROF0043.3000.txt</gml:description>
   <gml:name>43.3000</gml:name>
   <station>43.3000</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938276278"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282851479">
     <gml:description>Übernommen aus Datei: PROF0043.3000.txt</gml:description>
     <gml:name>0043.3000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282851449">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.0 0.7120000 13.23580 10.00000 1.000000 0.03299187 0.5724512 4.973799E-14
361.6 1.324000 26.67420 30.00000 1.000000 -0.07810004 -1.281481 -7.738254E-14
362.1 1.765000 37.33680 50.00000 1.000000 -0.02601169 -0.5575925 -3.830269E-14
362.4 2.132000 46.81160 70.00000 1.000000 0.03956296 0.4160357 9.325873E-15
362.8 2.453000 55.56990 90.00000 1.000000 0.06208302 1.036871 3.685940E-14
363.1 2.741000 63.80790 110.0000 1.000000 0.04197463 1.105638 4.196643E-14
363.3 3.006000 71.69160 130.0000 1.000000 0.007078801 0.7700297 2.997602E-14
363.6 3.252000 79.25190 150.0000 1.000000 -0.01232028 0.01286825 8.215650E-15
363.8 3.484000 86.64050 170.0000 1.000000 -0.05296136 -0.9637700 -1.576517E-14
364.0 3.712000 94.09160 190.0000 1.000000 -0.04538900 -1.406247 -3.452794E-14
364.2 3.932000 101.5010 210.0000 1.000000 -0.01880937 -1.667517 -3.852474E-14
364.5 4.153000 109.1830 230.0000 1.000000 0.02387789 -0.9005758 -1.720846E-14
364.7 4.378000 117.3450 250.0000 1.000000 0.01098855 1.316765 4.463097E-14
364.9 4.575000 124.7383 270.0000 1.000100 0.008558640 1.765684 0.00002242089
365.1 4.750000 131.5449 290.0000 1.000660 -0.01575261 0.8020024 -0.00009315727
365.2 4.919000 138.3186 310.0000 1.001500 -0.03188006 0.04723982 0.0001129091
365.4 5.083000 145.0607 330.0000 1.002520 -0.007153335 -0.3853445 0.00003221653
365.6 5.242000 151.7674 350.0000 1.003650 0.05671561 -0.4940227 -0.0001798394
365.7 5.396000 158.4497 370.0000 1.004890 0.1422242 -0.2832955 0.0001427874
365.9 5.544000 165.4410 390.0000 1.008310 -0.1376785 0.09426205 -0.00003733722
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857178">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>11.069425678 -21.1736788846 32.2983314136 -5.27591906086 0.426283456043</math:coefficients>
     <math:minRange>0.712</math:minRange>
     <math:maxRange>5.544</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857120">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.544676579313 15.3860892764 3.82967721038 -0.514342689722 0.0522849039086</math:coefficients>
     <math:minRange>0.712</math:minRange>
     <math:maxRange>5.544</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857281">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 -1.93353892845E-12 1.36053226221E-12 -3.78154163826E-13 3.62045275862E-14</math:coefficients>
     <math:minRange>0.712</math:minRange>
     <math:maxRange>4.378</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857173">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>8.3082686458 -4.92285373328 1.10501536667 -0.0826540335105</math:coefficients>
     <math:minRange>4.378</math:minRange>
     <math:maxRange>4.575</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857319">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>25.5186590878 -19.7472419305 5.95763239865 -0.798142326699 0.040074605884</math:coefficients>
     <math:minRange>4.575</math:minRange>
     <math:maxRange>5.544</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282851436">
   <gml:description>Gelesen aus: PROF0043.4000.txt</gml:description>
   <gml:name>43.4000</gml:name>
   <station>43.4000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382762719"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282851424">
     <gml:description>Übernommen aus Datei: PROF0043.4000.txt</gml:description>
     <gml:name>0043.4000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282853069">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.0 0.8810000 13.66210 10.00000 1.000000 2.240827 0.05935231 0E-7
361.6 1.508000 27.60860 30.00000 1.000000 -4.209393 -0.2605508 0E-7
362.1 1.949000 38.42660 50.00000 1.000000 -2.310764 -0.07965821 0E-7
362.5 2.315000 48.00310 70.00000 1.000000 0.3107137 0.4304938 0E-7
362.8 2.630000 56.72992 90.00000 1.000000 2.115224 0.6542167 0.007170481
363.0 2.901000 64.57381 110.0000 1.000960 2.905829 -0.1633048 -0.008506721
363.3 3.156000 72.22545 130.0000 1.002240 2.863638 -0.7089879 -0.007726136
363.5 3.406000 79.98522 150.0000 1.003800 2.085545 -0.4127858 -0.0009857453
363.8 3.643000 87.59626 170.0000 1.005490 0.7761698 -0.1477175 0.005427950
364.0 3.869000 95.05209 190.0000 1.007250 -0.7571887 0.01672592 0.008554158
364.2 4.086000 102.4126 210.0000 1.009000 -2.254387 0.06655292 0.007676231
364.4 4.298000 109.8084 230.0000 1.010750 -3.450861 0.2249038 0.003344890
364.6 4.507000 117.3694 250.0000 1.012740 -4.093591 0.5693495 -0.003171248
364.8 4.710000 124.9528 270.0000 1.015380 -3.806611 0.6988202 -0.009785615
365.0 4.905000 132.5637 290.0000 1.018400 -2.391516 0.3026682 -0.01322012
365.2 5.097000 140.2905 310.0000 1.021760 0.5888870 -0.2116850 -0.01008201
365.4 5.284000 148.0979 330.0000 1.025300 5.374465 -1.129069 0.003442136
365.6 5.467000 155.9577 350.0000 1.029000 12.34519 -2.423508 0.03127249
365.9 5.748000 198.1152 370.0000 1.157540 -1.138343 6.199541 -0.008361059
366.0 5.848000 216.3927 390.0000 1.201110 -7.193828 -3.685357 -0.005049680
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857244">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.04257020014 -2.27228420027 14.1728624502 0.107802111488 -0.0920834437266</math:coefficients>
     <math:minRange>0.881</math:minRange>
     <math:maxRange>5.848</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857327">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>52.7967332262 -90.3173459261 68.0095139743 -15.9528619855 1.32460333743</math:coefficients>
     <math:minRange>0.881</math:minRange>
     <math:maxRange>5.848</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885727">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.881</math:minRange>
     <math:maxRange>2.315</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857187">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>22.3104043601 -26.366275719 10.8494499668 -1.48446075862</math:coefficients>
     <math:minRange>2.315</math:minRange>
     <math:maxRange>2.63</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857349">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>6.17078469556 -5.57923799163 2.20880601128 -0.380552348169 0.0241305326888</math:coefficients>
     <math:minRange>2.63</math:minRange>
     <math:maxRange>5.848</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282853024">
   <gml:description>Gelesen aus: PROF0043.5000.txt</gml:description>
   <gml:name>43.5000</gml:name>
   <station>43.5000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382762758"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282853082">
     <gml:description>Übernommen aus Datei: PROF0043.5000.txt</gml:description>
     <gml:name>0043.5000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282853049">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.1 1.063000 12.67660 10.00000 1.000000 1.413344 0.6722751 0E-7
361.7 1.705000 26.05290 30.00000 1.000000 -3.012305 -1.557683 0E-7
362.1 2.161000 36.88740 50.00000 1.000000 -1.259719 -0.5747582 0E-7
362.5 2.543000 46.83770 70.00000 1.000000 0.7538663 0.4252258 0E-7
362.9 2.889000 56.53180 90.00000 1.000000 1.984836 1.433788 0E-7
363.2 3.195000 65.66120 110.0000 1.000000 2.206926 1.385623 0E-7
363.5 3.475000 74.39800 130.0000 1.000000 1.705378 0.8071714 0E-7
363.7 3.732000 82.82480 150.0000 1.000000 0.6651102 -0.2304418 0E-7
364.0 3.972000 90.98900 170.0000 1.000000 -0.5638160 -1.322650 0E-7
364.2 4.199000 98.97580 190.0000 1.000000 -1.720760 -2.120914 0E-7
364.4 4.417000 106.9240 210.0000 1.000000 -2.590048 -2.198055 0E-7
364.6 4.644000 115.5430 230.0000 1.000000 -2.951620 0.4305732 0E-7
364.8 4.843000 123.4154 250.0000 1.000080 -2.512786 2.165796 -0.0003972964
365.0 5.005000 130.0853 270.0000 1.000680 -1.440268 1.394608 0.001328329
365.1 5.149000 136.1632 290.0000 1.002080 0.2704733 -0.1955747 -0.0003926004
365.3 5.291000 142.3355 310.0000 1.003970 2.766297 -0.6844611 -0.002461932
365.4 5.431000 148.6551 330.0000 1.006870 6.104219 0.01207424 0.001606526
365.6 5.565000 158.5781 350.0000 1.029210 6.605221 1.315660 0.003144612
365.7 5.680000 174.6047 370.0000 1.081380 0.5584879 0.8685144 -0.004507411
365.8 5.775000 193.1839 390.0000 1.136870 -8.982836 -2.026773 0.001679772
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857350">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>36.6668783675 -72.2328225303 55.5896626128 -10.9989114431 0.928637022999</math:coefficients>
     <math:minRange>1.063</math:minRange>
     <math:maxRange>5.775</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857179">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>53.7196713518 -88.1885557298 60.9662273999 -13.4436268339 1.07505861038</math:coefficients>
     <math:minRange>1.063</math:minRange>
     <math:maxRange>5.775</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857150">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>1.063</math:minRange>
     <math:maxRange>4.644</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857322">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-56.6768683173 36.7129520824 -7.78788658558 0.550553694116</math:coefficients>
     <math:minRange>4.644</math:minRange>
     <math:maxRange>4.843</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885759">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>307.47234948 -245.534514886 73.6981811464 -9.82244290721 0.490484859397</math:coefficients>
     <math:minRange>4.843</math:minRange>
     <math:maxRange>5.775</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228285307">
   <gml:description>Gelesen aus: PROF0043.6000.txt</gml:description>
   <gml:name>43.6000</gml:name>
   <station>43.6000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382762762"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282853089">
     <gml:description>Übernommen aus Datei: PROF0043.6000.txt</gml:description>
     <gml:name>0043.6000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282853057">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.2 0.8660000 13.47120 10.00000 1.000000 2.603367 2.975973 0E-7
361.8 1.452000 27.12350 30.00000 1.000000 -4.910031 -5.203334 0E-7
362.2 1.885000 38.29260 50.00000 1.000000 -2.535813 -3.461979 0E-7
362.6 2.255000 48.59090 70.00000 1.000000 0.7232415 0.1912619 0E-7
362.9 2.577000 58.07010 90.00000 1.000000 2.892369 2.974482 0E-7
363.2 2.867000 67.04620 110.0000 1.000000 3.631155 4.488889 0E-7
363.4 3.133000 75.62210 130.0000 1.000000 3.148170 4.727047 0E-7
363.7 3.380000 83.87710 150.0000 1.000000 1.797072 3.864875 0E-7
363.9 3.611000 91.84420 170.0000 1.000000 -0.006791293 2.118505 0E-7
364.1 3.807000 98.79760 190.0000 1.000390 -1.685272 -2.008757 0.008197680
364.3 3.995000 105.6069 210.0000 1.001300 -3.109893 -6.304975 -0.01707816
364.5 4.190000 112.8717 230.0000 1.002470 -4.106044 -9.326457 -0.002516018
364.7 4.384000 120.2880 250.0000 1.003850 -4.180162 -11.49243 0.01710215
365.1 4.749000 139.6453 270.0000 1.005800 -4.982180 5.689207 0.009633200
365.2 4.927000 148.1833 290.0000 1.006910 -1.284336 6.137544 -0.01187987
365.4 5.095000 156.5671 310.0000 1.007900 4.416190 7.231363 -0.02312500
365.6 5.242000 164.0778 330.0000 1.008740 11.62972 7.368277 -0.005718848
365.7 5.365000 170.5629 350.0000 1.009940 19.44649 5.595074 0.04485030
365.8 5.442000 198.5427 370.0000 1.103450 1.439609 -2.288354 0.008204879
365.8 5.497000 232.5498 390.0000 1.194430 -24.92686 -13.27621 -0.02767032
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857139">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>61.5766643894 -120.488290315 89.1655738811 -18.4056050201 1.46799492689</math:coefficients>
     <math:minRange>0.866</math:minRange>
     <math:maxRange>5.497</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857367">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>66.5785595765 -121.979637461 91.5602609126 -22.537848417 1.95877184163</math:coefficients>
     <math:minRange>0.866</math:minRange>
     <math:maxRange>5.497</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857232">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.866</math:minRange>
     <math:maxRange>3.611</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885796">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>520.309443242 -423.269827981 114.954326836 -10.4026459769</math:coefficients>
     <math:minRange>3.611</math:minRange>
     <math:maxRange>3.807</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857216">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>193.786260366 -172.576177919 57.643158408 -8.51466927051 0.46933999505</math:coefficients>
     <math:minRange>3.807</math:minRange>
     <math:maxRange>5.497</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282854578">
   <gml:description>Gelesen aus: PROF0043.7000.txt</gml:description>
   <gml:name>43.7000</gml:name>
   <station>43.7000</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938276435"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282854528">
     <gml:description>Übernommen aus Datei: PROF0043.7000.txt</gml:description>
     <gml:name>0043.7000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282854513">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.0 0.8480000 13.31970 10.00000 1.000000 0.2382242 0.8278455 0E-7
361.6 1.437000 26.76390 30.00000 1.000000 -0.5392974 -1.682516 0E-7
362.0 1.876000 37.63400 50.00000 1.000000 -0.1555034 -0.8730111 0E-7
362.4 2.246000 47.32320 70.00000 1.000000 0.2232700 0.3905487 0E-7
362.7 2.574000 56.33130 90.00000 1.000000 0.3726311 1.486418 0E-7
363.0 2.862000 64.57887 110.0000 1.000330 0.2977076 1.504415 0.0008528182
363.3 3.124000 72.32247 130.0000 1.001700 0.1217733 0.8527668 -0.0009749603
363.5 3.370000 79.84102 150.0000 1.003320 -0.1131218 0.01736057 -0.001023456
363.7 3.604000 87.17479 170.0000 1.005060 -0.2728440 -0.7916571 -0.0003313974
364.0 3.827000 94.36654 190.0000 1.006900 -0.3048295 -1.531032 0.0005665550
364.2 4.043000 101.6171 210.0000 1.009210 -0.2083677 -1.844530 0.001179803
364.4 4.260000 109.1403 230.0000 1.011570 0.1865539 -0.8887306 0.002156055
364.6 4.467000 116.5665 250.0000 1.013490 0.9510406 0.3760224 0.004128518
364.8 4.650000 126.6002 270.0000 1.028810 -1.204763 0.3689389 -0.006753122
365.0 4.825000 134.3920 290.0000 1.032350 -0.7967176 0.6817548 -0.004706033
365.1 4.995000 142.1488 310.0000 1.034720 0.1385310 1.674269 0.0001195681
365.3 5.141000 149.5308 330.0000 1.039660 0.8737820 0.8082685 0.003192657
365.4 5.281000 157.8086 350.0000 1.048970 1.015527 0.2142491 0.003565900
365.5 5.413000 167.0930 370.0000 1.062840 0.3002937 -0.4525074 0.0009940345
365.7 5.538000 177.2503 390.0000 1.079790 -1.123890 -1.138874 -0.002966939
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857186">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>17.7444193146 -37.8752755213 40.9634633142 -7.52091981355 0.639955775328</math:coefficients>
     <math:minRange>0.848</math:minRange>
     <math:maxRange>5.538</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885716">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>5.56662955192 -1.4324295589 15.8615970164 -3.93248003327 0.382672800327</math:coefficients>
     <math:minRange>0.848</math:minRange>
     <math:maxRange>5.538</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857148">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.848</math:minRange>
     <math:maxRange>2.574</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885711">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>4.74693522391 -4.18756060988 1.55713436256 -0.192618466888</math:coefficients>
     <math:minRange>2.574</math:minRange>
     <math:maxRange>2.862</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857106">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.9485400295 -1.00512398419 0.39491817608 -0.0685414909789 0.00449103237115</math:coefficients>
     <math:minRange>2.862</math:minRange>
     <math:maxRange>5.538</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282854556">
   <gml:description>Gelesen aus: PROF0043.8000.txt</gml:description>
   <gml:name>43.8000</gml:name>
   <station>43.8000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382764341"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282854556">
     <gml:description>Übernommen aus Datei: PROF0043.8000.txt</gml:description>
     <gml:name>0043.8000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition12943228285458">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.7 0.8610000 13.81870 10.00000 1.000000 1.502678 2.746794 0E-7
361.4 1.521000 27.51380 30.00000 1.000000 -3.407434 -5.564920 0E-7
361.8 1.984000 38.16140 50.00000 1.000000 -1.239851 -2.829900 0E-7
362.2 2.363000 47.53530 70.00000 1.000000 0.9718125 0.7306964 0E-7
362.5 2.693000 56.14540 90.00000 1.000000 2.196762 3.283732 0E-7
362.8 2.988000 64.20350 110.0000 1.000000 2.372004 4.345251 0E-7
363.1 3.258000 71.86080 130.0000 1.000000 1.749202 4.081980 0E-7
363.4 3.508000 79.19830 150.0000 1.000000 0.6256721 2.692081 0E-7
363.6 3.741000 86.27580 170.0000 1.000000 -0.7020925 0.4019891 0E-7
363.8 3.963000 93.17310 190.0000 1.000000 -1.885400 -2.239213 0E-7
364.0 4.183000 100.2490 210.0000 1.000000 -2.741175 -4.342835 0E-7
364.2 4.394000 107.3090 230.0000 1.000000 -2.946387 -6.252807 0E-7
364.5 4.599000 114.4270 250.0000 1.000000 -2.174448 -7.493203 0E-7
364.6 4.795000 121.4730 270.0000 1.000000 -0.1397262 -8.131664 0E-7
365.0 5.187000 139.6208 290.0000 1.013420 6.153931 16.29119 -0.000007209004
365.2 5.300000 150.8760 310.0000 1.042960 3.934126 10.88947 0.00002244132
365.3 5.406000 162.7072 330.0000 1.070660 1.557030 5.463528 -0.00001307143
365.4 5.511000 175.3508 350.0000 1.095850 -0.6954225 0.8233204 -0.00002423042
365.5 5.608000 187.3705 370.0000 1.116190 -2.127308 -4.097179 0.00003582027
365.5 5.689000 197.8726 390.0000 1.131140 -3.003974 -10.79832 -0.00001375074
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885780">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>63.9541886537 -121.730173406 86.2863259748 -17.4036295173 1.35520567063</math:coefficients>
     <math:minRange>0.861</math:minRange>
     <math:maxRange>5.689</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857262">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>44.795702582 -76.7122971004 61.2752707838 -14.9847864158 1.30062699458</math:coefficients>
     <math:minRange>0.861</math:minRange>
     <math:maxRange>5.689</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857129">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.861</math:minRange>
     <math:maxRange>4.795</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857236">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-139.235089996 85.9150013762 -17.5373675529 1.19270853635</math:coefficients>
     <math:minRange>4.795</math:minRange>
     <math:maxRange>5.187</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857311">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>340.277159513 -246.427550307 66.8665645111 -8.03825407114 0.361525734347</math:coefficients>
     <math:minRange>5.187</math:minRange>
     <math:maxRange>5.689</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228285618">
   <gml:description>Gelesen aus: PROF0043.9000.txt</gml:description>
   <gml:name>43.9000</gml:name>
   <station>43.9000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382764319"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282856156">
     <gml:description>Übernommen aus Datei: PROF0043.9000.txt</gml:description>
     <gml:name>0043.9000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282856121">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.9 1.104000 12.82890 10.00000 1.000000 5.067984 0.6793305 0E-7
361.7 1.904000 26.36020 30.00000 1.000000 -14.04575 -2.016714 0E-7
362.2 2.438000 36.86170 50.00000 1.000000 -2.768237 -0.1433454 0E-7
362.6 2.869000 46.17010 70.00000 1.000000 6.339568 1.134356 0E-7
363.0 3.239000 54.74300 90.00000 1.000000 9.969817 1.361846 0E-7
363.3 3.569000 62.82700 110.0000 1.000000 8.918562 0.8689638 0E-7
363.6 3.867000 70.51590 130.0000 1.000000 4.781925 -0.1588026 0E-7
363.9 4.140000 77.88570 150.0000 1.000000 -0.7106035 -1.378600 0E-7
364.2 4.435000 86.21200 170.0000 1.000000 -6.810274 0.8365788 0E-7
364.4 4.667000 93.37355 190.0000 1.001340 -10.51642 0.07657396 -0.003630084
364.6 4.874000 100.4795 210.0000 1.007040 -11.94097 -1.137922 0.008060323
364.8 5.070000 108.5420 230.0000 1.019700 -11.17784 -1.677806 0.002050101
365.0 5.256000 117.2920 250.0000 1.035660 -7.324678 -1.456521 -0.006550953
365.2 5.431000 126.6844 270.0000 1.053690 -0.04463319 -0.6404995 -0.009522853
365.3 5.593000 136.8145 290.0000 1.073670 10.35852 0.3804656 -0.003533060
365.5 5.741000 148.0366 310.0000 1.097060 22.97152 1.227367 0.01019655
365.6 5.877000 160.0225 330.0000 1.120170 37.81668 1.907551 0.03457528
365.8 6.043000 254.5888 350.0000 1.279000 -16.69003 9.322462 -0.04673850
365.9 6.102000 268.4114 370.0000 1.278470 -14.13525 -0.3148345 -0.01311575
365.9 6.165000 283.1602 390.0000 1.275820 -10.05989 -8.870448 0.02820895
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857204">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>45.7893554662 -78.5806419118 52.3472777847 -9.91816234883 0.798993067388</math:coefficients>
     <math:minRange>1.104</math:minRange>
     <math:maxRange>6.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857300">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>257.63800263 -423.779702653 240.060030164 -52.3234207656 3.99030172556</math:coefficients>
     <math:minRange>1.104</math:minRange>
     <math:maxRange>6.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857124">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>1.104</math:minRange>
     <math:maxRange>4.435</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857242">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-257.688190461 171.907489122 -38.0673288921 2.80895185502</math:coefficients>
     <math:minRange>4.435</math:minRange>
     <math:maxRange>4.667</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857348">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-54.3398673983 38.1467472428 -9.64821586954 1.05200387495 -0.0410707348932</math:coefficients>
     <math:minRange>4.667</math:minRange>
     <math:maxRange>6.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282856135">
   <gml:description>Gelesen aus: PROF0043.9820.txt</gml:description>
   <gml:name>43.9820</gml:name>
   <station>43.9820</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938276438"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282856169">
     <gml:description>Übernommen aus Datei: PROF0043.9820.txt</gml:description>
     <gml:name>0043.9820</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282856122">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.8 0.9630000 12.93030 10.00000 1.000000 0.4708818 0.4577544 0E-7
361.5 1.659000 26.57780 30.00000 1.000000 -1.033342 -1.059914 0E-7
362.0 2.146000 37.37780 50.00000 1.000000 -0.4265101 -0.2961830 0E-7
362.4 2.535000 46.73115 70.00000 1.000870 0.2405301 0.2650912 0.001185169
362.7 2.870000 55.34116 90.00000 1.003520 0.6235126 0.5156791 -0.001847312
363.0 3.172000 63.53931 110.0000 1.006300 0.7168137 0.6357605 -0.001168021
363.3 3.449000 71.41960 130.0000 1.008950 0.5732425 0.5559139 0.0003268602
363.6 3.708000 79.05444 150.0000 1.011390 0.3082954 0.3978084 0.001395824
363.8 3.951000 86.48729 170.0000 1.013700 -0.04285246 0.05328727 0.001474012
364.0 4.180000 93.73608 190.0000 1.015310 -0.4072254 -0.5166587 0.001143589
364.3 4.401000 100.9393 210.0000 1.016800 -0.6990076 -0.9344163 0.0001052714
364.5 4.617000 108.2281 230.0000 1.018030 -0.8951490 -1.002229 -0.001100918
364.7 4.828000 115.5824 250.0000 1.019240 -0.9063156 -0.7638392 -0.002182079
364.9 5.035000 123.0523 270.0000 1.020510 -0.6733557 -0.1557320 -0.002565695
365.1 5.234000 130.4111 290.0000 1.021550 -0.05791891 0.3795858 -0.001281838
365.3 5.426000 137.6773 310.0000 1.022650 1.002050 0.8879113 0.002057188
365.5 5.610000 144.8721 330.0000 1.023840 2.470023 1.206961 0.008003397
365.6 5.781000 155.6026 350.0000 1.044960 0.4813150 0.6977701 -0.003142264
365.8 5.944000 165.7732 370.0000 1.059190 -0.6533886 -0.1558829 -0.004097369
366.0 6.101000 175.6412 390.0000 1.070400 -1.091599 -1.168667 0.001694187
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857301">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>11.943783668 -20.7926682288 21.970739139 -2.18420820713 0.131333810139</math:coefficients>
     <math:minRange>0.963</math:minRange>
     <math:maxRange>6.101</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885736">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>12.3812685476 -13.0389716471 17.8656446225 -3.62835226532 0.28920595455</math:coefficients>
     <math:minRange>0.963</math:minRange>
     <math:maxRange>6.101</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857259">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0</math:coefficients>
     <math:minRange>0.963</math:minRange>
     <math:maxRange>2.146</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857378">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>2.78323531715 -2.34068792779 1.01980551055 -0.147388896042</math:coefficients>
     <math:minRange>2.146</math:minRange>
     <math:maxRange>2.535</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857167">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.80871277036 -0.897233712726 0.361249548432 -0.0623896556848 0.00394021196546</math:coefficients>
     <math:minRange>2.535</math:minRange>
     <math:maxRange>6.101</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282857632">
   <gml:description>Gelesen aus: PROF0044.0000.txt</gml:description>
   <gml:name>44.0000</gml:name>
   <station>44.0000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382765978"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282857655">
     <gml:description>Übernommen aus Datei: PROF0044.0000.txt</gml:description>
     <gml:name>0044.0000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition12943228285766">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.9 0.9300000 12.82930 10.00000 1.000000 6.690548 1.344965 0E-7
361.5 1.602000 26.46880 30.00000 1.000000 -14.72388 -2.905914 0E-7
362.0 2.076000 37.43660 50.00000 1.000000 -5.981889 -1.463444 0E-7
362.4 2.464000 47.25670 70.00000 1.000000 3.541333 0.6593954 0E-7
362.7 2.800000 56.35123 90.00000 1.000000 9.105735 2.433823 -0.00001553206
363.0 3.081000 64.38634 110.0000 1.001110 10.40393 2.231971 0.00004335630
363.3 3.340000 72.14924 130.0000 1.003110 8.602042 1.654466 -0.000001405837
363.5 3.582000 79.69128 150.0000 1.005370 4.620543 0.7979178 -0.00002293045
363.7 3.809000 87.05007 170.0000 1.007680 -0.5656031 -0.3445401 -0.0001001189
363.9 4.024000 94.23495 190.0000 1.009630 -5.945677 -1.623062 0.00003690264
364.1 4.228000 101.2779 210.0000 1.011410 -10.60301 -3.008980 0.0001236334
364.4 4.438000 108.6911 230.0000 1.013210 -13.76389 -2.880361 0.00007643262
364.6 4.645000 116.2529 250.0000 1.014990 -14.04967 -1.953628 -0.0001629096
364.8 4.843000 123.6734 270.0000 1.016200 -10.25847 -0.7897456 -0.00006135872
365.0 5.033000 130.9844 290.0000 1.017230 -1.502674 0.7166859 0.00005444507
365.1 5.216000 138.2042 310.0000 1.018290 13.05304 2.684185 0.00004810464
365.3 5.393000 145.3434 330.0000 1.019380 34.22640 5.246309 -0.000001195433
365.5 5.564000 152.4108 350.0000 1.020500 62.62520 8.415085 -0.00001742377
365.6 5.672000 280.0253 370.0000 1.464700 -37.85669 3.809838 -1.464700
365.6 5.680000 281.9542 390.0000 1.468420 -37.61731 -15.02497 -1.468420
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857164">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>33.0256929933 -59.4994505691 45.4054130929 -7.51233035439 0.568428996358</math:coefficients>
     <math:minRange>0.93</math:minRange>
     <math:maxRange>5.68</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857290">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>209.369826298 -383.45208583 241.960164973 -57.2224291723 4.70071153871</math:coefficients>
     <math:minRange>0.93</math:minRange>
     <math:maxRange>5.68</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857307">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.93</math:minRange>
     <math:maxRange>2.464</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857128">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.721769431319 0.324906246652 -0.126240966914 0.0163177262481</math:coefficients>
     <math:minRange>2.464</math:minRange>
     <math:maxRange>2.8</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885772">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.22433880454 -0.232295104916 0.0851975704498 -0.0130710495156 7.33071568609E-4</math:coefficients>
     <math:minRange>2.8</math:minRange>
     <math:maxRange>5.564</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282857687">
   <gml:description>Gelesen aus: PROF0044.1000.txt</gml:description>
   <gml:name>44.1000</gml:name>
   <station>44.1000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382765975"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828576110">
     <gml:description>Übernommen aus Datei: PROF0044.1000.txt</gml:description>
     <gml:name>0044.1000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition12943228285763">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.8 0.7110000 14.93990 10.00000 1.000000 0.001431881 -0.07345395 0E-7
361.3 1.190000 29.17520 30.00000 1.000000 -0.003451624 0.02650580 0E-7
361.7 1.545000 40.46030 50.00000 1.000000 -0.006263096 0.1021482 0E-7
362.0 1.844000 50.42750 70.00000 1.000000 0.01442979 0.1909856 0E-7
362.2 2.107000 59.58630 90.00000 1.000000 0.006744631 0.1472706 0E-7
362.5 2.345000 68.18200 110.0000 1.000000 -0.01381413 -0.01458245 0E-7
362.7 2.565000 76.34390 130.0000 1.000000 -0.001930604 -0.2118569 0E-7
362.9 2.769000 84.14410 150.0000 1.000000 -0.01174171 -0.6136158 0E-7
363.1 2.967000 91.87850 170.0000 1.000000 0.008206903 -0.4668757 0E-7
363.3 3.157000 99.49420 190.0000 1.000000 0.01124675 -0.09749371 0E-7
363.5 3.338000 106.9170 210.0000 1.000000 0.007056219 0.2513555 0E-7
363.6 3.511000 114.1600 230.0000 1.000000 0.0003240411 0.5659625 0E-7
363.8 3.677000 121.2470 250.0000 1.000000 -0.01046837 0.8602599 0E-7
364.0 3.834000 128.0629 270.0000 1.000000 -0.01510650 0.7851061 0.000001826825
364.1 3.980000 134.4795 290.0000 1.000180 0.004959049 -0.03946828 -0.000007557800
364.3 4.121000 140.7812 310.0000 1.000460 0.01285189 -0.9197251 0.000009451294
364.4 4.265000 147.3392 330.0000 1.000850 -0.007669403 -0.7726380 4.100009E-7
364.5 4.406000 153.8268 350.0000 1.001280 -0.002979289 -0.4254693 -0.00001048284
364.7 4.544000 160.2489 370.0000 1.001710 0.01547980 0.09567976 0.000008590568
364.8 4.678000 166.6087 390.0000 1.002210 -0.009306223 0.6099052 -0.000002238045
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885789">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-3.31167630829 3.30442160522 22.8215411435 -1.91420610721 0.156620700634</math:coefficients>
     <math:minRange>0.711</math:minRange>
     <math:maxRange>4.678</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857287">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-4.08459730885 25.0059291954 2.45574474241 0.0173932623299 -0.00379018177883</math:coefficients>
     <math:minRange>0.711</math:minRange>
     <math:maxRange>4.678</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857279">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.711</math:minRange>
     <math:maxRange>3.677</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885721">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-0.0269950387753 0.826731382217 -0.221799149155 0.0198313606291</math:coefficients>
     <math:minRange>3.677</math:minRange>
     <math:maxRange>3.834</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857119">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.92359751674 -0.846020822484 0.289825762455 -0.0441114016142 0.00252582614055</math:coefficients>
     <math:minRange>3.834</math:minRange>
     <math:maxRange>4.678</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282857616">
   <gml:description>Gelesen aus: PROF0044.2000.txt</gml:description>
   <gml:name>44.2000</gml:name>
   <station>44.2000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382765954"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation12943228285762">
     <gml:description>Übernommen aus Datei: PROF0044.2000.txt</gml:description>
     <gml:name>0044.2000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828592114">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.2 0.9610000 12.22720 10.00000 1.000000 2.842720 1.158835 1.532108E-14
361.9 1.619000 25.03470 30.00000 1.000000 -6.306236 -2.539439 -3.352874E-14
362.4 2.101000 35.78200 50.00000 1.000000 -2.319575 -1.033025 -7.993606E-15
362.8 2.509000 45.80920 70.00000 1.000000 2.066791 0.8191312 1.443290E-14
363.1 2.868000 55.30170 90.00000 1.000000 4.444410 1.908762 2.198242E-14
363.5 3.188000 64.30150 110.0000 1.000000 4.606687 1.969047 1.643130E-14
363.7 3.480000 72.94670 130.0000 1.000000 3.095150 1.426282 3.552714E-15
364.0 3.742000 81.08180 150.0000 1.000000 0.6546513 0.07855841 -1.021405E-14
364.3 3.987000 88.94810 170.0000 1.000000 -2.026561 -1.131668 -1.931788E-14
364.5 4.216000 96.57060 190.0000 1.000000 -4.375773 -2.037289 -1.931788E-14
364.7 4.432000 103.9740 210.0000 1.000000 -5.836501 -2.306247 -5.662137E-15
364.9 4.637000 111.2090 230.0000 1.000000 -5.954685 -1.670977 2.531308E-14
365.1 4.817000 117.7346 250.0000 1.000280 -4.521077 -1.710399 -0.0003392176
365.3 4.991000 124.1769 270.0000 1.000980 -1.258749 -0.4929781 0.0006452430
365.4 5.151000 130.3848 290.0000 1.002960 3.635122 0.9344716 0.002108183
365.6 5.302000 138.2817 310.0000 1.016140 8.521406 3.065148 -0.005915928
365.7 5.441000 147.8008 330.0000 1.036060 13.08521 5.266166 -0.002065058
365.8 5.569000 158.6804 350.0000 1.060670 17.41954 7.420522 0.03758185
365.9 5.621000 192.7362 370.0000 1.185970 -9.778496 -3.075967 -0.04239461
366.0 5.700000 212.1768 390.0000 1.231760 -17.99404 -8.048932 0.01037953
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857375">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>39.1299306789 -76.4896383723 60.5546174103 -12.7767908522 1.11554061451</math:coefficients>
     <math:minRange>0.961</math:minRange>
     <math:maxRange>5.7</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D12943228288571">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>96.4097767223 -170.177621696 112.094207785 -26.0758479813 2.13613955677</math:coefficients>
     <math:minRange>0.961</math:minRange>
     <math:maxRange>5.7</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857211">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 -1.20606957717E-12 7.60218272825E-13 -1.93247530714E-13 1.71091769691E-14</math:coefficients>
     <math:minRange>0.961</math:minRange>
     <math:maxRange>4.637</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857340">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>122.528780939 -77.6542867035 16.5372212897 -1.17373138829</math:coefficients>
     <math:minRange>4.637</math:minRange>
     <math:maxRange>4.817</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857215">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>997.943372749 -788.236501822 233.634452936 -30.768663148 1.51914014184</math:coefficients>
     <math:minRange>4.817</math:minRange>
     <math:maxRange>5.7</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282859265">
   <gml:description>Gelesen aus: PROF0044.3000.txt</gml:description>
   <gml:name>44.3000</gml:name>
   <station>44.3000</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938276741"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828592109">
     <gml:description>Übernommen aus Datei: PROF0044.3000.txt</gml:description>
     <gml:name>0044.3000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828592104">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.5 0.8900000 12.96210 10.00000 1.000000 2.791190 1.321166 3.308465E-14
362.1 1.494000 26.26100 30.00000 1.000000 -6.053389 -2.759566 -5.473400E-14
362.5 1.948000 37.43840 50.00000 1.000000 -2.310024 -1.164430 -1.598721E-14
362.9 2.328000 47.50200 70.00000 1.000000 1.913391 0.8014434 1.842970E-14
363.2 2.657000 56.77420 90.00000 1.000000 4.173700 1.802152 2.953193E-14
363.5 2.952000 65.50760 110.0000 1.000000 4.387219 1.926363 2.065015E-14
363.8 3.224000 73.90770 130.0000 1.000000 3.003925 1.578155 1.554312E-15
364.0 3.468000 81.75880 150.0000 1.000000 0.7123006 0.2536439 -1.687539E-14
364.3 3.701000 89.45440 170.0000 1.000000 -1.863918 -0.7592654 -2.564615E-14
364.5 3.926000 97.13590 190.0000 1.000000 -4.185117 -1.069002 -1.443290E-14
364.7 4.138000 104.6240 210.0000 1.000000 -5.584204 -0.9838769 2.575717E-14
364.9 4.318000 111.1445 230.0000 1.000300 -5.576913 -2.497661 -0.002767661
365.1 4.490000 117.5402 250.0000 1.001180 -4.051129 -3.362419 0.009076610
365.2 4.658000 123.9203 270.0000 1.002470 -0.5947545 -3.048350 -0.005597108
365.4 4.828000 130.7241 290.0000 1.005310 5.226077 -0.6010627 -0.008626424
365.6 5.023000 143.0960 310.0000 1.034150 11.44137 7.856633 0.005709881
365.7 5.144000 156.3495 330.0000 1.073030 12.35115 7.164848 0.01915832
365.8 5.254000 186.0095 350.0000 1.169800 -2.421002 5.935287 -0.01757426
365.9 5.312000 197.3804 370.0000 1.194690 -5.097752 -3.667524 -0.008425182
366.0 5.392000 213.5665 390.0000 1.223630 -8.262118 -8.726533 0.009045828
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857117">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>35.8790474912 -73.8587346732 63.0630561741 -13.5351815554 1.22091439695</math:coefficients>
     <math:minRange>0.89</math:minRange>
     <math:maxRange>5.392</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885741">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>90.9329995801 -171.804142915 123.604547443 -31.0669814392 2.74149315654</math:coefficients>
     <math:minRange>0.89</math:minRange>
     <math:maxRange>5.392</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857376">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 -2.19596692941E-12 1.49046331148E-12 -4.109004855E-13 3.96795342383E-14</math:coefficients>
     <math:minRange>0.89</math:minRange>
     <math:maxRange>4.138</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857402">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-649.322227714 464.622427598 -110.625688263 8.77795187326</math:coefficients>
     <math:minRange>4.138</math:minRange>
     <math:maxRange>4.318</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857162">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-521.305555751 428.598676881 -131.303513048 17.7913587686 -0.8991805285</math:coefficients>
     <math:minRange>4.318</math:minRange>
     <math:maxRange>5.392</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228285929">
   <gml:description>Gelesen aus: PROF0044.3900.txt</gml:description>
   <gml:name>44.3900</gml:name>
   <station>44.3900</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938276745"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282859218">
     <gml:description>Übernommen aus Datei: PROF0044.3900.txt</gml:description>
     <gml:name>0044.3900</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition12943228285929">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.1 0.6360000 16.40640 10.00000 1.000000 0.4185665 -1.563231 0E-7
362.4 0.9800000 41.37420 30.00000 1.000000 -1.097359 4.608225 0E-7
362.6 1.148000 50.88450 50.00000 1.000000 -0.1103292 -0.8351260 0E-7
362.8 1.352000 62.62050 70.00000 1.000000 0.4203354 -1.102122 0E-7
363.0 1.535000 73.32650 90.00000 1.000000 0.4878894 -1.247620 0E-7
363.1 1.703000 83.28070 110.0000 1.000000 0.3833913 -1.115588 0E-7
363.3 1.859000 92.67350 130.0000 1.000000 0.1967994 -0.7636036 0E-7
363.4 2.005000 101.6100 150.0000 1.000000 -0.01168859 -0.2962971 0E-7
363.6 2.142000 110.1290 170.0000 1.000000 -0.2065427 0.1005800 0E-7
363.7 2.274000 118.3470 190.0000 1.000000 -0.2611374 0.7653690 0E-7
363.8 2.399000 126.2750 210.0000 1.000000 -0.3205924 1.159005 0E-7
364.0 2.517000 133.8030 230.0000 1.000000 -0.2971754 1.050258 0E-7
364.1 2.633000 141.2530 250.0000 1.000000 -0.2126709 1.105974 0E-7
364.2 2.746000 148.5150 270.0000 1.000000 -0.03992493 1.010807 0E-7
364.3 2.855000 155.6210 290.0000 1.000000 0.09998047 0.4415520 0E-7
364.4 2.961000 162.5840 310.0000 1.000000 0.2358948 -0.5605184 0E-7
364.5 3.064000 169.4130 330.0000 1.000000 0.3345681 -2.119552 0E-7
364.6 3.164000 176.1220 350.0000 1.000000 0.3579001 -4.348871 0E-7
364.8 3.325000 187.2500 370.0000 1.000000 0.02056387 3.681061 0E-7
364.9 3.422000 194.1000 390.0000 1.000000 -0.3984684 0.02969870 0E-7
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857404">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-36.8894456769 79.1535941739 -25.7843017874 23.2408433361 -3.45166347741</math:coefficients>
     <math:minRange>0.636</math:minRange>
     <math:maxRange>3.422</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885784">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-39.478896931 107.893455807 -37.5259677351 11.939654118 -1.27651482388</math:coefficients>
     <math:minRange>0.636</math:minRange>
     <math:maxRange>3.422</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857289">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.636</math:minRange>
     <math:maxRange>3.422</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885747">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>3.422</math:minRange>
     <math:maxRange>3.422</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885722">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>3.422</math:minRange>
     <math:maxRange>3.422</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282860893">
   <gml:description>Gelesen aus: PROF0044.4000.txt</gml:description>
   <gml:name>44.4000</gml:name>
   <station>44.4000</station>
   <profileMember xlink:href="project:/modell.gml#Profile12119938276743"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282860864">
     <gml:description>Übernommen aus Datei: PROF0044.4000.txt</gml:description>
     <gml:name>0044.4000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282860880">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.1 0.5630000 17.39420 10.00000 1.000000 0.3354920 1.725726 0E-7
362.5 0.9450000 33.60150 30.00000 1.000000 -0.6773716 -3.060276 0E-7
362.7 1.230000 46.30740 50.00000 1.000000 -0.3140741 -1.914817 0E-7
363.0 1.472000 57.42460 70.00000 1.000000 0.1803882 0.1544928 0E-7
363.2 1.688000 67.61760 90.00000 1.000000 0.4816552 1.889166 0E-7
363.4 1.884000 77.12290 110.0000 1.000000 0.4973515 2.685456 0E-7
363.6 2.067000 86.11990 130.0000 1.000000 0.3636903 2.733587 0E-7
363.8 2.239000 94.73480 150.0000 1.000000 0.08050594 2.020536 0E-7
363.9 2.398000 102.8020 170.0000 1.000000 -0.2312236 0.2360530 0E-7
364.1 2.547000 110.4450 190.0000 1.000000 -0.4920132 -2.283035 0E-7
364.2 2.691000 117.8870 210.0000 1.000000 -0.6150165 -4.888651 0E-7
364.3 2.829000 125.1290 230.0000 1.000000 -0.5860749 -7.602095 0E-7
364.6 3.047000 136.9140 250.0000 1.000000 -0.1288101 1.418865 0E-7
364.7 3.212000 146.3890 270.0000 1.000000 0.5226239 5.295710 0E-7
364.8 3.328000 153.2593 290.0000 1.000290 1.354099 3.373370 0.000009297331
365.0 3.437000 162.4428 310.0000 1.012060 -0.06258401 1.539901 -0.00005292306
365.1 3.541000 170.5936 330.0000 1.018140 -0.2482754 0.1147738 0.0001210073
365.2 3.638000 178.6167 350.0000 1.024240 -0.2868063 -1.309181 -0.0001345357
365.3 3.732000 186.8492 370.0000 1.030910 -0.2073843 -2.005920 0.00007119603
365.3 3.831000 196.0427 390.0000 1.038130 0.03382816 -0.1236590 -0.00001404184
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857268">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>35.2792084065 -108.768019706 140.821613555 -41.706602146 4.87230660632</math:coefficients>
     <math:minRange>0.563</math:minRange>
     <math:maxRange>3.831</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857384">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>5.50373771934 5.50674912729 35.4722550991 -12.837121076 1.7207150821</math:coefficients>
     <math:minRange>0.563</math:minRange>
     <math:maxRange>3.831</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857229">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.563</math:minRange>
     <math:maxRange>3.212</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885787">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-386.259066336 357.565524327 -110.034883321 11.2855953638</math:coefficients>
     <math:minRange>3.212</math:minRange>
     <math:maxRange>3.328</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857269">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-210.069740012 232.748681774 -96.2102874118 17.6661167045 -1.21544958758</math:coefficients>
     <math:minRange>3.328</math:minRange>
     <math:maxRange>3.831</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282860836">
   <gml:description>Gelesen aus: PROF0044.4050.txt</gml:description>
   <gml:name>44.4050</gml:name>
   <station>44.4050</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382767488"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282860879">
     <gml:description>Übernommen aus Datei: PROF0044.4050.txt</gml:description>
     <gml:name>0044.4050</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828608123">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.2 0.6030000 16.45700 10.00000 1.000000 -0.004113504 0.2637860 0E-7
362.6 1.018000 32.06410 30.00000 1.000000 -0.3219295 -0.8153534 0E-7
362.9 1.333000 44.62270 50.00000 1.000000 0.7079821 -0.7386854 0E-7
363.2 1.638000 59.38670 70.00000 1.000000 -0.04029402 2.505275 0E-7
363.4 1.856000 69.77800 90.00000 1.000000 -0.1027049 1.207693 0E-7
363.6 2.053000 79.36240 110.0000 1.000000 -0.1706260 -0.4022760 0E-7
363.8 2.237000 88.51370 130.0000 1.000000 -0.2517058 -1.881391 0E-7
364.0 2.407000 97.12440 150.0000 1.000000 -0.2701061 -3.513012 0E-7
364.2 2.617000 108.1260 170.0000 1.000000 -0.2512384 1.049300 0E-7
364.3 2.779000 116.9140 190.0000 1.000000 -0.1062525 1.606137 0E-7
364.5 2.925000 125.0578 210.0000 1.000120 0.2342157 1.503252 0.0001755472
364.6 3.056000 132.5683 230.0000 1.000760 0.7773600 0.5985120 -0.001373999
364.7 3.183000 140.0667 250.0000 1.002350 1.571214 0.3603569 0.003543700
364.9 3.298000 150.0982 270.0000 1.017140 -0.4646641 -0.5729947 -0.002927363
365.0 3.411000 158.7588 290.0000 1.023200 -0.7451313 -0.6467712 -0.0008453475
365.1 3.519000 167.1461 310.0000 1.027630 -0.5690854 -0.3984976 0.001409478
365.2 3.619000 175.4518 330.0000 1.033200 -0.4011532 -0.5120003 0.0009144233
365.3 3.715000 183.9616 350.0000 1.038880 -0.2275396 -0.3104877 -0.0004402297
365.4 3.806000 192.4557 370.0000 1.043900 0.05665164 -0.08399573 -0.0009767329
365.5 3.895000 201.0820 390.0000 1.048360 0.5791209 0.7811529 0.0005205236
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857352">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>4.36498212726 -17.767177481 53.1017878513 -13.4628427492 1.93580784933</math:coefficients>
     <math:minRange>0.603</math:minRange>
     <math:maxRange>3.895</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857198">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>4.67056687429 4.27049528424 31.1275599888 -10.4870761797 1.42428422145</math:coefficients>
     <math:minRange>0.603</math:minRange>
     <math:maxRange>3.895</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885724">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.603</math:minRange>
     <math:maxRange>2.779</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857231">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>60.6963808707 -63.3349021342 22.3915322677 -2.63794261765</math:coefficients>
     <math:minRange>2.779</math:minRange>
     <math:maxRange>2.925</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857328">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>37.1070700478 -41.987395721 18.2107252553 -3.49421105174 0.250626831565</math:coefficients>
     <math:minRange>2.925</math:minRange>
     <math:maxRange>3.895</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828608123">
   <gml:description>Gelesen aus: PROF0044.4250.txt</gml:description>
   <gml:name>44.4250</gml:name>
   <station>44.4250</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382767443"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282860859">
     <gml:description>Übernommen aus Datei: PROF0044.4250.txt</gml:description>
     <gml:name>0044.4250</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282860867">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.2 0.6010000 16.23980 10.00000 1.000000 -0.05009438 0.9488623 4.818368E-14
362.6 1.023000 32.22670 30.00000 1.000000 -0.1529373 -1.937947 -1.181277E-13
362.9 1.340000 45.03200 50.00000 1.000000 0.2726697 -0.9188658 6.883383E-15
363.2 1.608000 56.48570 70.00000 1.000000 0.3944661 0.5114333 7.416290E-14
363.4 1.845000 67.08170 90.00000 1.000000 0.2157719 1.536531 5.662137E-14
363.7 2.055000 76.86330 110.0000 1.000000 -0.1701130 1.529469 -5.440093E-15
363.8 2.245000 85.90180 130.0000 1.000000 -0.4918976 0.6763865 -6.017409E-14
364.0 2.428000 94.81640 150.0000 1.000000 -0.7010425 0.1382864 -5.850875E-14
364.2 2.603000 103.5790 170.0000 1.000000 -0.7155033 -0.1678287 5.706546E-14
364.4 2.768000 112.0453 190.0000 1.000000 -0.4048183 -0.4358275 0.001538171
364.5 2.913000 119.6690 210.0000 1.000290 0.2413179 -1.971280 -0.003914937
364.7 3.052000 127.0850 230.0000 1.000960 1.366345 -3.090791 -0.0007007044
364.8 3.185000 134.3790 250.0000 1.001840 2.925810 -3.751082 0.007428767
365.0 3.366000 151.2142 270.0000 1.029740 -0.5642529 4.950079 -0.003985371
365.1 3.471000 160.0692 290.0000 1.037760 -0.8716781 3.067164 -0.001892909
365.2 3.570000 168.9666 310.0000 1.045300 -1.087183 1.278041 -0.0003884429
365.3 3.666000 177.9384 330.0000 1.051860 -1.000591 0.09529184 0.0008684554
365.4 3.758000 186.8729 350.0000 1.057630 -0.6052055 -0.6962717 0.001354950
365.4 3.847000 195.7650 370.0000 1.062840 0.1873462 -0.9195875 0.0008049106
365.5 3.932000 204.6456 390.0000 1.067780 1.211589 -0.8420646 -0.001112890
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885710">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>21.7795764254 -65.4284801992 93.6105619186 -26.4893416005 3.29532133326</math:coefficients>
     <math:minRange>0.601</math:minRange>
     <math:maxRange>3.932</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857260">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>1.4517436051 12.2509842719 25.7956230184 -9.84158946515 1.48809161702</math:coefficients>
     <math:minRange>0.601</math:minRange>
     <math:maxRange>3.932</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857316">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 -9.28606327938E-12 9.93941000743E-12 -4.33323515065E-12 6.62501329346E-13</math:coefficients>
     <math:minRange>0.601</math:minRange>
     <math:maxRange>2.603</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885794">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>66.9102978801 -74.315021143 27.9167682832 -3.49388886614</math:coefficients>
     <math:minRange>2.603</math:minRange>
     <math:maxRange>2.768</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857133">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>10.9995250533 -10.7690630267 4.23846252922 -0.721554133002 0.0449563013047</math:coefficients>
     <math:minRange>2.768</math:minRange>
     <math:maxRange>3.932</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282862312">
   <gml:description>Gelesen aus: PROF0044.5000.txt</gml:description>
   <gml:name>44.5000</gml:name>
   <station>44.5000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382769078"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282862319">
     <gml:description>Übernommen aus Datei: PROF0044.5000.txt</gml:description>
     <gml:name>0044.5000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282862319">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[362.4 0.5660000 15.65390 10.00000 1.000000 2.906146 0.3301524 6.838974E-14
362.8 1.011000 30.26760 30.00000 1.000000 -5.849688 -0.8214021 -1.809664E-13
363.1 1.344000 41.78590 50.00000 1.000000 -2.563379 -0.08442818 1.043610E-14
363.4 1.628000 52.13670 70.00000 1.000000 1.479420 0.4639471 1.201261E-13
363.7 1.881000 61.80210 90.00000 1.000000 3.886280 0.3994701 9.858780E-14
363.9 2.118000 71.20850 110.0000 1.000000 4.324609 0.3018947 -5.773160E-15
364.1 2.336000 80.17930 130.0000 1.000000 3.068889 -0.2742940 -1.061373E-13
364.3 2.547000 89.18780 150.0000 1.000000 0.6670716 -0.2388477 -1.071365E-13
364.5 2.749000 98.12550 170.0000 1.000000 -2.059692 0.3989540 1.034728E-13
364.7 2.928000 106.3675 190.0000 1.000090 -4.110588 0.2150144 0.001927676
364.9 3.087000 113.8589 210.0000 1.000820 -4.826757 -0.6746431 -0.005988823
365.0 3.239000 121.2521 230.0000 1.001970 -3.907911 -0.7905446 0.002659108
365.2 3.383000 130.0988 250.0000 1.013880 -2.530426 -0.2203695 0.006486649
365.3 3.512000 141.5242 270.0000 1.037840 -2.250013 -0.1233245 -0.002227383
365.4 3.631000 152.3126 290.0000 1.053450 0.4120170 0.01682735 -0.004296395
365.5 3.745000 162.6709 310.0000 1.064410 5.795157 0.9333543 -0.002636189
365.6 3.847000 172.1577 330.0000 1.072610 13.14914 1.143151 0.001207588
365.7 3.941000 181.5303 350.0000 1.079220 21.92049 1.140415 0.007814858
365.8 4.015000 249.2135 370.0000 1.105050 -29.51077 -2.115327 -0.004947089
365.8 4.014000 249.0397 390.0000 1.104910 -249.0397 -390.0000 -1.104910
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885779">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>11.2696360638 -36.1227161035 72.2275943444 -21.6412983112 2.83999578203</math:coefficients>
     <math:minRange>0.566</math:minRange>
     <math:maxRange>4.015</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885782">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>65.8896979549 -167.543104957 182.227660787 -64.4853076112 7.93734323324</math:coefficients>
     <math:minRange>0.566</math:minRange>
     <math:maxRange>4.015</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857108">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 -1.12998590034E-11 1.20424237076E-11 -5.15915923307E-12 7.68304063319E-13</math:coefficients>
     <math:minRange>0.566</math:minRange>
     <math:maxRange>2.749</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857209">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>106.391586571 -112.508176828 40.0152383841 -4.74154600959</math:coefficients>
     <math:minRange>2.749</math:minRange>
     <math:maxRange>2.928</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885730">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>44.8110172006 -49.7540207472 21.0836342938 -3.95683566153 0.278131267446</math:coefficients>
     <math:minRange>2.928</math:minRange>
     <math:maxRange>4.015</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282862327">
   <gml:description>Gelesen aus: PROF0044.6000.txt</gml:description>
   <gml:name>44.6000</gml:name>
   <station>44.6000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382769011"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282862330">
     <gml:description>Übernommen aus Datei: PROF0044.6000.txt</gml:description>
     <gml:name>0044.6000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282862347">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.4 0.6950000 15.15110 10.00000 1.000000 1.274629 -0.5230754 -6.783463E-14
361.9 1.168000 29.87450 30.00000 1.000000 -2.372659 1.419961 1.467715E-13
362.3 1.520000 41.66880 50.00000 1.000000 -1.253674 0.2349953 1.687539E-14
362.6 1.821000 52.26890 70.00000 1.000000 0.3465025 -0.5335051 -7.593925E-14
362.8 2.082000 61.89420 90.00000 1.000000 1.346839 -0.9734605 -8.359979E-14
363.1 2.313000 70.63420 110.0000 1.000000 1.717518 -1.256336 -3.363976E-14
363.3 2.528000 78.93280 130.0000 1.000000 1.548304 -0.8741205 3.486100E-14
363.5 2.729000 86.87710 150.0000 1.000000 0.9214246 -0.1384996 7.949197E-14
363.7 2.920000 94.53410 170.0000 1.000000 0.06500944 0.9238037 5.706546E-14
363.8 3.102000 101.9490 190.0000 1.000000 -0.8568295 2.042409 -7.560619E-14
364.0 3.268000 108.8308 210.0000 1.000150 -1.635252 2.007998 -0.001179261
364.2 3.426000 115.4757 230.0000 1.000630 -2.130619 1.444174 0.002186141
364.3 3.581000 122.1239 250.0000 1.001340 -2.229085 0.7218379 0.0009326607
364.5 3.737000 128.9084 270.0000 1.002200 -1.684039 0.1120974 -0.001509580
364.6 3.889000 135.6113 290.0000 1.003140 -0.2944496 -1.242984 -0.002591313
364.8 4.036000 142.2342 310.0000 1.004120 2.048352 -3.685502 -0.0008390206
364.9 4.179000 148.7862 330.0000 1.005140 5.542382 -7.294496 0.004340000
365.4 4.616000 197.3966 350.0000 1.051630 -1.422739 15.85851 -0.001921127
365.4 4.708000 208.4817 370.0000 1.061710 -1.075501 3.077454 -0.0009536583
365.5 4.788000 218.1145 390.0000 1.068830 0.1438853 -11.32126 0.001535158
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857321">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-28.1347940757 69.1114170769 -31.9834209288 16.2693459486 -1.8583710106</math:coefficients>
     <math:minRange>0.695</math:minRange>
     <math:maxRange>4.788</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857243">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>29.1606964554 -56.2844403714 67.5847296859 -20.1347839522 2.12975376816</math:coefficients>
     <math:minRange>0.695</math:minRange>
     <math:maxRange>4.788</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857134">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.999999999997 8.82518179245E-12 -7.99491419276E-12 2.94491604068E-12 -3.79989776873E-13</math:coefficients>
     <math:minRange>0.695</math:minRange>
     <math:maxRange>3.102</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857181">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-66.8893715239 64.4353571705 -20.3783629657 2.14748853019</math:coefficients>
     <math:minRange>3.102</math:minRange>
     <math:maxRange>3.268</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857239">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-13.9420171032 14.9451207092 -5.53995585953 0.90026172042 -0.0539594885481</math:coefficients>
     <math:minRange>3.268</math:minRange>
     <math:maxRange>4.788</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828639153">
   <gml:description>Gelesen aus: PROF0044.7000.txt</gml:description>
   <gml:name>44.7000</gml:name>
   <station>44.7000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382769024"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282863982">
     <gml:description>Übernommen aus Datei: PROF0044.7000.txt</gml:description>
     <gml:name>0044.7000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282863922">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.5 0.7850000 13.78500 10.00000 1.000000 0.1047698 -0.3505299 0E-7
361.1 1.348000 28.32770 30.00000 1.000000 -0.1870370 0.8669856 0E-7
361.5 1.736000 39.26520 50.00000 1.000000 -0.1218820 0.04349125 0E-7
361.8 2.073000 49.26920 70.00000 1.000000 0.01950889 -0.1852529 0E-7
362.1 2.371000 58.52010 90.00000 1.000000 0.1135140 -0.2595152 0E-7
362.4 2.638000 67.14460 110.0000 1.000000 0.1224231 -0.4566541 0E-7
362.6 2.884000 75.29760 130.0000 1.000000 0.1277557 -0.5901977 0E-7
362.8 3.118000 83.25680 150.0000 1.000000 0.1093026 -0.2673302 0E-7
363.1 3.338000 90.95930 170.0000 1.000000 0.03992334 0.08582371 0E-7
363.3 3.547000 98.43950 190.0000 1.000000 -0.02608002 0.5161884 0E-7
363.5 3.745000 105.7280 210.0000 1.000000 -0.1286446 0.8305826 0E-7
363.7 3.935000 112.8480 230.0000 1.000000 -0.1869544 1.176505 0E-7
363.8 4.113000 119.6482 250.0000 1.000000 -0.2053734 0.9704884 -0.000005779273
364.0 4.277000 126.0515 270.0000 1.000350 -0.2005472 -0.1864394 -0.00001718452
364.2 4.437000 132.3788 290.0000 1.000910 -0.1111008 -1.263777 0.00009954323
364.3 4.593000 138.6428 310.0000 1.001630 0.05582110 -2.322032 -0.00004230932
364.5 4.761000 145.5490 330.0000 1.002530 0.2896157 -1.404470 -0.0002549876
364.7 4.949000 153.5203 350.0000 1.003630 0.6027770 2.606669 0.0005213898
364.8 5.095000 160.5992 370.0000 1.008150 0.1962547 1.666965 -0.0004226018
364.9 5.222000 167.4000 390.0000 1.013440 -0.6140465 -1.477499 0.0001219295
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857428">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-7.85075611328 14.0815726816 9.79318091458 0.931590410559 -0.103376785034</math:coefficients>
     <math:minRange>0.785</math:minRange>
     <math:maxRange>5.222</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857261">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-1.01257324189 14.1707739491 7.09331847035 -1.31847339015 0.118502183852</math:coefficients>
     <math:minRange>0.785</math:minRange>
     <math:maxRange>5.222</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857121">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.785</math:minRange>
     <math:maxRange>3.935</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857432">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>2.61428366093 -1.21364449992 0.30408568925 -0.0253916458949</math:coefficients>
     <math:minRange>3.935</math:minRange>
     <math:maxRange>4.113</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857203">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>15.3279057535 -12.8407075006 4.3102573655 -0.642407163644 0.0358803265879</math:coefficients>
     <math:minRange>4.113</math:minRange>
     <math:maxRange>5.222</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282863954">
   <gml:description>Gelesen aus: PROF0044.8000.txt</gml:description>
   <gml:name>44.8000</gml:name>
   <station>44.8000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382770654"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282863975">
     <gml:description>Übernommen aus Datei: PROF0044.8000.txt</gml:description>
     <gml:name>0044.8000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282863958">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.3 0.7240000 13.65460 10.00000 1.000000 -0.09553602 -0.09502174 0E-7
360.9 1.291000 27.98910 30.00000 1.000000 0.3196542 0.03813126 0E-7
361.3 1.701000 40.24900 50.00000 1.000000 -0.1274424 0.5133466 0E-7
361.6 2.025000 50.20530 70.00000 1.000000 -0.1321464 -0.09413388 0E-7
361.9 2.316000 59.54160 90.00000 1.000000 -0.09429301 -0.3892355 0E-7
362.2 2.585000 68.48270 110.0000 1.000000 -0.01692723 -0.3140291 0E-7
362.4 2.835000 77.11810 130.0000 1.000000 0.02630787 -0.08652198 0E-7
362.7 3.068000 85.43080 150.0000 1.000000 0.05742387 0.1165312 0E-7
362.9 3.286000 93.44300 170.0000 1.000000 0.07535688 0.2014648 0E-7
363.1 3.490000 101.1820 190.0000 1.000000 0.04861607 0.04225247 0E-7
363.3 3.685000 108.7590 210.0000 1.000000 0.02569724 -0.03163029 0E-7
363.5 3.873000 116.2260 230.0000 1.000000 0.01419706 0.08758543 0E-7
363.7 4.053000 123.5380 250.0000 1.000000 0.003847741 0.2081846 0E-7
363.8 4.225000 130.7110 270.0000 1.000000 -0.03775017 0.2414649 0E-7
364.0 4.390000 137.7140 290.0000 1.000000 -0.05285353 0.2242672 0E-7
364.1 4.546000 144.4370 310.0000 1.000000 -0.03205823 -0.1709808 0E-7
364.3 4.696000 151.0550 330.0000 1.000000 -0.03520994 -0.6451288 0E-7
364.4 4.847000 157.8250 350.0000 1.000000 -0.01203528 -0.2967376 0E-7
364.6 4.993000 164.4870 370.0000 1.000000 0.02740778 0.06260367 0E-7
364.7 5.134000 171.0780 390.0000 1.000000 0.03774362 0.3875876 0E-7
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885760">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.0842497665802 -0.229512006769 20.3606369514 -1.92721451977 0.166408450053</math:coefficients>
     <math:minRange>0.724</math:minRange>
     <math:maxRange>5.134</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857389">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-2.15186014663 18.9625240188 4.06134601474 -0.412564275609 0.0355443453511</math:coefficients>
     <math:minRange>0.724</math:minRange>
     <math:maxRange>5.134</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885766">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.724</math:minRange>
     <math:maxRange>5.134</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857131">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>5.134</math:minRange>
     <math:maxRange>5.134</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857142">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>5.134</math:minRange>
     <math:maxRange>5.134</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult12943228286395">
   <gml:description>Gelesen aus: PROF0044.9000.txt</gml:description>
   <gml:name>44.9000</gml:name>
   <station>44.9000</station>
   <profileMember xlink:href="project:/modell.gml#Profile1211993827706106"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282863961">
     <gml:description>Übernommen aus Datei: PROF0044.9000.txt</gml:description>
     <gml:name>0044.9000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828654114">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.8 0.7720000 14.35470 10.00000 1.000000 0.4762954 -1.046540 0E-7
361.3 1.290000 29.16150 30.00000 1.000000 -0.7316820 2.172862 0E-7
361.6 1.653000 40.30460 50.00000 1.000000 -0.6312295 1.252702 0E-7
362.0 1.965000 50.37320 70.00000 1.000000 -0.2152740 -0.1947530 0E-7
362.2 2.244000 59.69280 90.00000 1.000000 0.3550620 -1.724352 0E-7
362.5 2.498000 68.50920 110.0000 1.000000 0.9099885 -3.221787 0E-7
362.7 2.740000 77.16870 130.0000 1.000000 1.488710 -3.979039 0E-7
363.1 3.069000 91.67500 150.0000 1.000000 0.04702792 4.908205 0E-7
363.2 3.262000 99.99810 170.0000 1.000000 -0.3089875 3.417174 0E-7
363.4 3.448000 108.1810 190.0000 1.000000 -0.5676839 2.414553 0E-7
363.6 3.618000 115.8560 210.0000 1.000000 -0.7580711 0.8178633 0E-7
363.8 3.785000 123.4880 230.0000 1.000000 -0.7792794 -0.09660523 0E-7
363.9 3.944000 130.9570 250.0000 1.000000 -0.7303567 -0.9628785 0E-7
364.1 4.092000 137.9929 270.0000 1.000090 -0.4980791 -2.279629 0.0004840475
364.2 4.233000 144.8175 290.0000 1.000380 -0.1251594 -3.671638 -0.001914011
364.4 4.370000 151.5517 310.0000 1.000800 0.4186547 -4.813353 0.002233033
364.6 4.579000 162.2626 330.0000 1.001770 1.422414 5.482813 0.0001596086
364.7 4.697000 168.8021 350.0000 1.003510 1.862179 3.427906 -0.003276374
364.8 4.814000 175.4016 370.0000 1.005350 2.471714 1.836907 0.003627395
364.9 4.903000 187.6708 390.0000 1.033350 -4.106242 -3.740409 -0.001313699
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857210">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-18.8439070822 31.874431611 4.48227846166 1.11498858984 0.0167045696442</math:coefficients>
     <math:minRange>0.772</math:minRange>
     <math:maxRange>4.903</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857270">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>2.82126152298 6.92990300769 13.177763088 -2.81019605602 0.278952381941</math:coefficients>
     <math:minRange>0.772</math:minRange>
     <math:maxRange>4.903</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282885726">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.772</math:minRange>
     <math:maxRange>3.944</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857118">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>275.569254762 -206.233314187 51.6267178903 -4.30722124045</math:coefficients>
     <math:minRange>3.944</math:minRange>
     <math:maxRange>4.092</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857453">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>487.50414899 -440.325675587 149.282650971 -22.4689884201 1.26682630272</math:coefficients>
     <math:minRange>4.092</math:minRange>
     <math:maxRange>4.903</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828654143">
   <gml:description>Gelesen aus: PROF0045.0000.txt</gml:description>
   <gml:name>45.0000</gml:name>
   <station>45.0000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382770642"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282865437">
     <gml:description>Übernommen aus Datei: PROF0045.0000.txt</gml:description>
     <gml:name>0045.0000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828654164">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.7 0.8800000 13.16600 10.00000 1.000000 0.8373833 1.556985 0E-7
361.3 1.491000 26.97330 30.00000 1.000000 -2.169542 -3.328530 0E-7
361.8 1.923000 37.64190 50.00000 1.000000 -0.5081180 -1.374138 0E-7
362.1 2.292000 47.20330 70.00000 1.000000 1.084449 1.015543 0E-7
362.5 2.619000 56.06130 90.00000 1.000000 1.731952 2.180002 0E-7
362.8 2.920000 64.53760 110.0000 1.000000 1.455055 2.178192 0E-7
363.1 3.206000 72.88300 130.0000 1.000000 0.5080204 1.531302 0E-7
363.3 3.482000 81.33220 150.0000 1.000000 -0.8468926 0.7359007 0E-7
363.6 3.740000 89.56180 170.0000 1.000000 -2.032144 -0.4351062 0E-7
363.8 3.972000 97.26351 190.0000 1.000280 -2.575548 -2.295021 0.00002304896
364.0 4.191000 104.8193 210.0000 1.000970 -2.168731 -3.602688 0.002108484
364.3 4.401000 112.2590 230.0000 1.001840 -0.3770891 -3.704588 -0.006148482
364.5 4.601000 119.5657 250.0000 1.002790 3.055530 -2.404172 -0.001854749
364.6 4.793000 126.7785 270.0000 1.003780 8.429455 0.7563126 0.02106786
364.8 4.980000 151.7045 290.0000 1.079600 -1.603116 6.430771 -0.01655344
365.0 5.098000 163.7618 310.0000 1.098000 -2.692379 4.483425 -0.008286814
365.1 5.209000 175.1859 330.0000 1.109630 -2.543729 2.943814 0.002079210
365.2 5.301000 184.9588 350.0000 1.116360 -1.714836 -0.5667265 0.007615626
365.3 5.396000 195.3015 370.0000 1.120890 -0.06878602 -2.313945 0.006151547
365.3 5.486000 205.4401 390.0000 1.123190 2.199067 -3.787333 -0.006202290
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857359">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>56.0795986941 -117.168387008 92.9300756777 -21.2736762954 1.86416525697</math:coefficients>
     <math:minRange>0.88</math:minRange>
     <math:maxRange>5.486</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828857180">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>35.4345782636 -64.9792339489 58.695609693 -15.5268410575 1.46367341918</math:coefficients>
     <math:minRange>0.88</math:minRange>
     <math:maxRange>5.486</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873471">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.88</math:minRange>
     <math:maxRange>3.74</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887387">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-93.1907904271 74.1029277357 -19.4256004019 1.696754346</math:coefficients>
     <math:minRange>3.74</math:minRange>
     <math:maxRange>3.972</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D12943228288735">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-132.941762666 117.752912867 -38.6011575554 5.58930058391 -0.301422154578</math:coefficients>
     <math:minRange>3.972</math:minRange>
     <math:maxRange>5.486</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282865419">
   <gml:description>Gelesen aus: PROF0045.1000.txt</gml:description>
   <gml:name>45.1000</gml:name>
   <station>45.1000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382770620"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282865461">
     <gml:description>Übernommen aus Datei: PROF0045.1000.txt</gml:description>
     <gml:name>0045.1000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828654145">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.0 0.7470000 13.75630 10.00000 1.000000 0.8716244 -0.6307018 0E-7
361.6 1.326000 28.97830 30.00000 1.000000 -1.927572 1.529744 0E-7
362.0 1.711000 40.37570 50.00000 1.000000 -0.8195785 0.6171496 0E-7
362.3 2.030000 50.47680 70.00000 1.000000 0.4204718 -0.4904365 0E-7
362.6 2.311000 59.79200 90.00000 1.000000 1.242480 -1.371315 0E-7
362.8 2.575000 68.98540 110.0000 1.000000 1.453895 -1.222823 0E-7
363.1 2.820000 77.83410 130.0000 1.000000 1.173860 -0.6099178 0E-7
363.3 3.045000 86.27850 150.0000 1.000000 0.5187620 -0.06752032 0E-7
363.5 3.256000 94.45450 170.0000 1.000000 -0.3028795 0.5698335 0E-7
363.7 3.456000 102.3990 190.0000 1.000000 -1.074427 1.318024 0E-7
363.9 3.645000 110.1450 210.0000 1.000000 -1.682468 1.940123 0E-7
364.1 3.820000 117.4751 230.0000 1.000000 -1.911121 1.866862 -0.001474951
364.2 3.979000 124.3222 250.0000 1.000450 -1.730436 0.6221308 0.004043037
364.4 4.134000 131.0836 270.0000 1.001130 -0.9517817 -0.5410527 -0.0007409133
364.5 4.284000 137.7652 290.0000 1.001980 0.4728309 -1.834411 -0.004515797
364.7 4.430000 144.3755 310.0000 1.002930 2.680233 -3.218870 -0.001863430
364.8 4.571000 150.9219 330.0000 1.003950 5.679508 -4.901884 0.008010871
365.1 4.826000 179.7955 350.0000 1.046950 -2.813660 8.924902 -0.005468139
365.2 4.926000 187.7967 370.0000 1.048740 -1.536471 2.385832 0.001112531
365.3 5.020000 195.5003 390.0000 1.050430 0.2367298 -4.885669 0.0008967925
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873388">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-11.3907826297 24.676699594 1.7266570513 3.52519155543 -0.341451553429</math:coefficients>
     <math:minRange>0.747</math:minRange>
     <math:maxRange>5.02</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873159">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>23.6157115986 -41.0330696198 47.7169177739 -12.8474592887 1.261144677</math:coefficients>
     <math:minRange>0.747</math:minRange>
     <math:maxRange>5.02</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873427">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.747</math:minRange>
     <math:maxRange>3.645</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887340">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-197.047386618 160.345334712 -43.2616474979 3.88960268522</math:coefficients>
     <math:minRange>3.645</math:minRange>
     <math:maxRange>3.82</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873107">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-124.954520757 115.750409058 -39.7223601413 6.03145115859 -0.34178911472</math:coefficients>
     <math:minRange>3.82</math:minRange>
     <math:maxRange>5.02</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282867010">
   <gml:description>Gelesen aus: PROF0045.2000.txt</gml:description>
   <gml:name>45.2000</gml:name>
   <station>45.2000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382770624"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282867023">
     <gml:description>Übernommen aus Datei: PROF0045.2000.txt</gml:description>
     <gml:name>0045.2000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828670101">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.3 1.268000 12.72300 10.00000 1.000000 0.1371739 -1.601067 0E-7
360.9 1.926000 28.43210 30.00000 1.000000 -0.2994125 4.075943 0E-7
361.4 2.372000 41.72140 50.00000 1.000000 -0.2602002 2.067810 0E-7
361.7 2.694000 52.47530 70.00000 1.000000 -0.1035696 -1.357220 0E-7
362.0 2.962000 61.78370 90.00000 1.000000 0.4082860 -4.266452 0E-7
362.2 3.205000 70.51370 110.0000 1.000000 1.116080 -5.799153 0E-7
362.5 3.518000 84.77701 130.0000 1.000130 -0.3008216 2.371218 -0.00002669959
362.7 3.705000 92.84138 150.0000 1.000790 -0.3444598 1.529899 0.00007234504
362.9 3.886000 100.7649 170.0000 1.001740 -0.2726915 1.635944 -0.000001781609
363.0 4.047000 108.0270 190.0000 1.002740 -0.2394797 0.7031187 -0.00008939625
363.2 4.208000 115.4171 210.0000 1.003700 -0.1679699 0.7527075 -0.00001454117
363.4 4.363000 122.6755 230.0000 1.004750 -0.09088934 0.8366227 0.00002947001
363.5 4.517000 130.0408 250.0000 1.005870 -0.02459751 1.383123 0.00006012538
363.7 4.666000 137.2924 270.0000 1.007020 0.04662334 1.645604 0.00004226275
363.8 4.810000 144.4481 290.0000 1.008160 0.08942186 1.401620 -0.00003520290
363.9 4.950000 151.5156 310.0000 1.009120 0.1327951 0.5847751 -0.00004694270
364.1 5.085000 158.4862 330.0000 1.009920 0.1227715 -1.125871 -0.00007013586
364.2 5.217000 165.3367 350.0000 1.010330 0.1756517 -3.627729 0.00008704608
364.3 5.345000 172.1073 370.0000 1.010720 0.1908647 -7.207558 0.000001824034
364.6 5.626000 187.8265 390.0000 1.010210 -0.3155766 5.996666 -0.000008373209
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873287">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-121.758753131 190.744139386 -98.0915374293 24.967670529 -1.89318343588</math:coefficients>
     <math:minRange>1.268</math:minRange>
     <math:maxRange>5.626</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873180">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>2.29410727753 -3.44934965649 10.4327646303 -0.956246061544 0.0446061863812</math:coefficients>
     <math:minRange>1.268</math:minRange>
     <math:maxRange>5.626</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873413">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>1.268</math:minRange>
     <math:maxRange>3.205</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873481">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-0.124514691519 1.0244499987 -0.310861822969 0.0314178326259</math:coefficients>
     <math:minRange>3.205</math:minRange>
     <math:maxRange>3.518</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873110">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.780968633342 0.227726596595 -0.089703815374 0.015626324047 -9.93467074753E-4</math:coefficients>
     <math:minRange>3.518</math:minRange>
     <math:maxRange>5.626</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282867024">
   <gml:description>Gelesen aus: PROF0045.3000.txt</gml:description>
   <gml:name>45.3000</gml:name>
   <station>45.3000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382770644"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282867029">
     <gml:description>Übernommen aus Datei: PROF0045.3000.txt</gml:description>
     <gml:name>0045.3000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828670175">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[359.5 0.9870000 14.19860 10.00000 1.000000 -0.01826741 0.03606418 0E-7
360.0 1.526000 29.76850 30.00000 1.000000 0.05343164 -0.003604405 0E-7
360.4 1.886000 40.93740 50.00000 1.000000 0.003456838 -0.09091197 0E-7
360.7 2.185000 50.57180 70.00000 1.000000 -0.03884951 -0.1192978 0E-7
361.0 2.451000 59.33960 90.00000 1.000000 -0.02349118 -0.002397507 0E-7
361.2 2.693000 67.51630 110.0000 1.000000 -0.01701559 0.06858581 0E-7
361.4 2.918000 75.25290 130.0000 1.000000 0.01311697 0.1500731 0E-7
361.6 3.129000 82.67850 150.0000 1.000000 0.006983635 0.1713986 0E-7
361.8 3.329000 89.82040 170.0000 1.000000 0.01876964 0.1817076 0E-7
362.0 3.518000 96.71240 190.0000 1.000000 -0.003786678 0.002203547 0E-7
362.2 3.700000 103.4030 210.0000 1.000000 0.02293601 -0.07606361 0E-7
362.4 3.874000 109.9270 230.0000 1.000000 0.01756066 -0.2561440 0E-7
362.6 4.043000 116.3700 250.0000 1.000000 -0.0004811668 -0.2674199 0E-7
362.7 4.206000 122.6760 270.0000 1.000000 -0.01852540 -0.2799427 0E-7
362.9 4.364000 128.8600 290.0000 1.000000 -0.01810338 -0.2187065 0E-7
363.0 4.520000 135.0360 310.0000 1.000000 0.002635214 0.2786600 0E-7
363.2 4.670000 141.0901 330.0000 1.000000 -0.003739943 0.6631321 -3.995082E-11
363.3 4.809000 146.7845 350.0000 1.000120 -0.01159424 0.1717601 1.208322E-10
363.5 4.945000 152.4194 370.0000 1.000380 -0.002011521 -0.1364448 -1.218843E-10
363.6 5.078000 158.0016 390.0000 1.000750 0.01697541 -0.2726517 4.100076E-11
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873474">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>13.1189581114 -34.8330783482 36.3970329114 -4.66029313385 0.338657221627</math:coefficients>
     <math:minRange>0.987</math:minRange>
     <math:maxRange>5.078</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887341">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-10.4740795446 21.80031002 3.64892050083 -0.471650120846 0.0382872143639</math:coefficients>
     <math:minRange>0.987</math:minRange>
     <math:maxRange>5.078</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873322">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.987</math:minRange>
     <math:maxRange>4.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873381">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-0.18413564869 0.777516056493 -0.170155170952 0.0124110261982</math:coefficients>
     <math:minRange>4.52</math:minRange>
     <math:maxRange>4.67</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873165">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.23881938594 -0.133433804515 0.0242336610344 -0.00141577279597</math:coefficients>
     <math:minRange>4.67</math:minRange>
     <math:maxRange>5.078</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282868693">
   <gml:description>Gelesen aus: PROF0045.4000.txt</gml:description>
   <gml:name>45.4000</gml:name>
   <station>45.4000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382772119"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282868698">
     <gml:description>Übernommen aus Datei: PROF0045.4000.txt</gml:description>
     <gml:name>0045.4000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282868688">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[359.5 0.9660000 15.93020 10.00000 1.000000 -0.001209181 -0.1880053 0E-7
359.9 1.405000 31.51850 30.00000 1.000000 0.004910610 0.3714311 0E-7
360.2 1.722000 43.55820 50.00000 1.000000 -0.006123019 0.1375631 0E-7
360.5 1.989000 54.14230 70.00000 1.000000 0.004590832 -0.06169420 0E-7
360.7 2.225000 63.84530 90.00000 1.000000 -0.0001923934 -0.1757052 0E-7
360.9 2.440000 72.94380 110.0000 1.000000 -0.003140619 -0.1145067 0E-7
361.1 2.637000 81.48290 130.0000 1.000000 0.001686730 -0.1015211 0E-7
361.3 2.818000 89.51600 150.0000 1.000000 -0.009911984 -0.3321839 0E-7
361.5 2.990000 97.28790 170.0000 1.000000 -0.01083589 -0.3111981 0E-7
361.7 3.154000 104.7960 190.0000 1.000000 0.02240645 -0.09132125 0E-7
361.8 3.311000 112.1370 210.0000 1.000000 0.01934431 0.3093722 0E-7
362.0 3.461000 119.2750 230.0000 1.000000 -0.001103063 0.7649145 0E-7
362.1 3.603000 126.1231 250.0000 1.000000 -0.01605724 1.008322 -0.000002263093
362.2 3.732000 132.4016 270.0000 1.000220 -0.007466546 0.1486588 0.000006516871
362.4 3.858000 138.6042 290.0000 1.000640 0.003764255 -0.4578132 -8.912259E-7
362.5 3.982000 144.8144 310.0000 1.001220 -0.02126731 -0.6895208 -0.00001177994
362.6 4.104000 150.9432 330.0000 1.001890 0.003224329 -0.5701740 0.000004892669
362.7 4.223000 156.9936 350.0000 1.002630 0.01969082 -0.2963329 0.00001494733
362.8 4.339000 162.9761 370.0000 1.003420 0.01292082 0.09250755 -0.00001619833
363.0 4.452000 168.8844 390.0000 1.004110 -0.01523191 0.5572074 0.000004775708
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887399">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-6.30278047529 -5.05215841123 23.3721323905 -1.0163802565 0.116572386513</math:coefficients>
     <math:minRange>0.966</math:minRange>
     <math:maxRange>4.452</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873139">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-13.3964161496 26.38026006 4.45221259056 -0.369225456348 0.0233093053849</math:coefficients>
     <math:minRange>0.966</math:minRange>
     <math:maxRange>4.452</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873419">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.966</math:minRange>
     <math:maxRange>3.461</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873108">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-1.18438008711 1.8681765176 -0.532484569131 0.050581694516</math:coefficients>
     <math:minRange>3.461</math:minRange>
     <math:maxRange>3.603</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873386">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.690022973253 0.3842264556 -0.171360547932 0.0327606782975 -0.00226773792984</math:coefficients>
     <math:minRange>3.603</math:minRange>
     <math:maxRange>4.452</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282868692">
   <gml:description>Gelesen aus: PROF0045.5000.txt</gml:description>
   <gml:name>45.5000</gml:name>
   <station>45.5000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382772149"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282868670">
     <gml:description>Übernommen aus Datei: PROF0045.5000.txt</gml:description>
     <gml:name>0045.5000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828686138">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[357.7 0.7340000 16.10210 10.00000 1.000000 -0.03258498 -0.2816523 0E-7
358.1 1.175000 32.79500 30.00000 1.000000 0.1318809 0.8737452 0E-7
358.4 1.465000 45.16430 50.00000 1.000000 -0.06922369 0.01593914 0E-7
358.7 1.710000 55.96800 70.00000 1.000000 -0.06892919 -0.4965722 0E-7
358.9 1.927000 65.84210 90.00000 1.000000 -0.04643606 -0.6845687 0E-7
359.1 2.126000 75.06860 110.0000 1.000000 0.02690524 -0.4163378 0E-7
359.3 2.310000 83.81140 130.0000 1.000000 0.04304112 0.09290012 0E-7
359.4 2.483000 92.17320 150.0000 1.000000 0.03553110 0.9015025 0E-7
359.6 2.636000 99.65070 170.0000 1.000000 0.02929124 0.5116689 0E-7
359.7 2.781000 106.8350 190.0000 1.000000 -0.01150098 0.1176398 0E-7
359.9 2.921000 113.7710 210.0000 1.000000 0.001660693 -0.03450497 0E-7
360.0 3.055000 120.4900 230.0000 1.000000 -0.02232649 -0.2177475 0E-7
360.1 3.185000 127.0210 250.0000 1.000000 -0.02004634 -0.2519247 0E-7
360.3 3.311000 133.3860 270.0000 1.000000 -0.01850078 -0.2298445 0E-7
360.4 3.434000 139.6030 290.0000 1.000000 0.01141891 -0.07341320 0E-7
360.5 3.553000 145.6860 310.0000 1.000000 0.002415336 -0.02322749 0E-7
360.6 3.669000 151.6440 330.0000 1.000000 -0.005286197 0.01638048 0E-7
360.7 3.783000 157.4920 350.0000 1.000000 0.02408312 0.1614638 0E-7
360.9 3.893000 163.2370 370.0000 1.000000 -0.02012533 -0.0007826131 0E-7
361.0 4.002000 168.8880 390.0000 1.000000 0.008732377 0.01933592 0E-7
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873415">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-5.79788570815 5.91488827135 19.122593243 2.47043209201 -0.360476260767</math:coefficients>
     <math:minRange>0.734</math:minRange>
     <math:maxRange>4.002</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873318">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-5.84769359978 23.2764362089 10.2435563238 -1.83685593626 0.137484572556</math:coefficients>
     <math:minRange>0.734</math:minRange>
     <math:maxRange>4.002</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873283">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.734</math:minRange>
     <math:maxRange>4.002</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873317">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>4.002</math:minRange>
     <math:maxRange>4.002</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887375">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>4.002</math:minRange>
     <math:maxRange>4.002</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282868654">
   <gml:description>Gelesen aus: PROF0045.6000.txt</gml:description>
   <gml:name>45.6000</gml:name>
   <station>45.6000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382772111"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828686122">
     <gml:description>Übernommen aus Datei: PROF0045.6000.txt</gml:description>
     <gml:name>0045.6000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282870143">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[358.3 0.8910000 13.33550 10.00000 1.000000 0.01069155 0.2335066 0E-7
358.9 1.477000 28.38370 30.00000 1.000000 -0.04279927 -0.5638110 0E-7
359.2 1.856000 40.30500 50.00000 1.000000 -0.01052516 -0.3618472 0E-7
359.5 2.161000 50.79540 70.00000 1.000000 0.06767805 0.1514629 0E-7
359.8 2.425000 60.47930 90.00000 1.000000 0.06633632 0.8569612 0E-7
360.0 2.651000 69.17300 110.0000 1.000000 -0.02072046 0.6781893 0E-7
360.2 2.854000 77.15240 130.0000 1.000000 -0.06275659 0.07235427 0E-7
360.4 3.045000 84.77500 150.0000 1.000000 -0.06592166 -0.3538997 0E-7
360.6 3.228000 92.13410 170.0000 1.000000 -0.005457066 -0.4369922 0E-7
360.8 3.402000 99.28710 190.0000 1.000000 -0.007483541 -0.4830038 0E-7
360.9 3.570000 106.2330 210.0000 1.000000 0.03274676 -0.3128751 0E-7
361.1 3.731000 113.0000 230.0000 1.000000 0.03241505 -0.1827989 0E-7
361.3 3.887000 119.6090 250.0000 1.000000 0.04582613 0.03827517 0E-7
361.4 4.037000 126.0800 270.0000 1.000000 0.004851887 0.1175618 0E-7
361.6 4.183000 132.4120 290.0000 1.000000 -0.007047131 0.2330624 0E-7
361.7 4.325000 138.6230 310.0000 1.000000 -0.008634931 0.3127949 0E-7
361.8 4.463000 144.7300 330.0000 1.000000 -0.01680812 0.2895638 0E-7
362.0 4.597000 150.7440 350.0000 1.000000 -0.04194362 0.1008607 0E-7
362.1 4.729000 156.6690 370.0000 1.000000 0.003834520 -0.007179547 0E-7
362.2 4.857000 162.5120 390.0000 1.000000 0.02571728 -0.3821855 0E-7
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873439">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>10.3426505772 -20.4868666259 23.1126742264 -0.222986930802 -0.0735098431143</math:coefficients>
     <math:minRange>0.891</math:minRange>
     <math:maxRange>4.857</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873272">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>2.56981275721 1.48278686009 13.7815779397 -2.2343517371 0.150333093362</math:coefficients>
     <math:minRange>0.891</math:minRange>
     <math:maxRange>4.857</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873450">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.891</math:minRange>
     <math:maxRange>4.857</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873387">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>4.857</math:minRange>
     <math:maxRange>4.857</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873162">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>4.857</math:minRange>
     <math:maxRange>4.857</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282870170">
   <gml:description>Gelesen aus: PROF0045.7000.txt</gml:description>
   <gml:name>45.7000</gml:name>
   <station>45.7000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382773738"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282870195">
     <gml:description>Übernommen aus Datei: PROF0045.7000.txt</gml:description>
     <gml:name>0045.7000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828701160">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[359.3 0.7120000 14.65030 10.00000 1.000000 0.09150604 0.1473493 -1.226796E-13
359.8 1.223000 30.62740 30.00000 1.000000 -0.2916065 -0.1590234 1.461054E-13
360.2 1.571000 43.92500 50.00000 1.000000 -0.006931662 -0.4670576 1.083578E-13
360.4 1.846000 55.86560 70.00000 1.000000 0.1984371 -0.2682830 3.730349E-14
360.7 2.077000 66.89150 90.00000 1.000000 0.2063262 0.1941638 -1.987299E-14
360.9 2.280000 77.21310 110.0000 1.000000 0.1311583 0.9205846 -5.540013E-14
361.1 2.456000 86.65480 130.0000 1.000000 -0.05662168 0.9860484 -7.072121E-14
361.2 2.611000 95.11860 150.0000 1.000000 -0.1148083 0.2503425 -7.072121E-14
361.4 2.757000 103.2500 170.0000 1.000000 -0.1302398 -0.2750398 -5.973000E-14
361.5 2.895000 111.1040 190.0000 1.000000 -0.1519761 -0.7273009 -4.130030E-14
361.6 3.028000 118.7210 210.0000 1.000000 -0.08549124 -0.8911817 -1.765255E-14
361.8 3.157000 126.2170 230.0000 1.000000 -0.01351246 -0.7747755 7.549517E-15
361.9 3.283000 133.6800 250.0000 1.000000 0.01584225 -0.3555246 3.153033E-14
362.0 3.405000 140.9770 270.0000 1.000000 0.05976131 0.07502359 5.129230E-14
362.1 3.523000 148.1430 290.0000 1.000000 0.06803368 0.3852916 6.306067E-14
362.2 3.638000 155.1770 310.0000 1.000000 0.09013723 0.6302183 6.394885E-14
362.3 3.750000 162.1090 330.0000 1.000000 0.08615155 0.7053030 5.129230E-14
362.5 3.859000 168.9370 350.0000 1.000000 0.04943147 0.5124106 2.176037E-14
362.6 3.965000 175.6660 370.0000 1.000000 -0.03254197 -0.03950885 -2.620126E-14
362.7 4.069000 182.3060 390.0000 1.000000 -0.1130553 -0.8490401 -9.636736E-14
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887391">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-10.0517426498 31.5121079658 -13.1149992724 13.2760943746 -1.48209573625</math:coefficients>
     <math:minRange>0.712</math:minRange>
     <math:maxRange>4.069</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873234">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>3.90457562035 4.73919734852 16.0982883982 -2.00546672001 0.100596947569</math:coefficients>
     <math:minRange>0.712</math:minRange>
     <math:maxRange>4.069</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873310">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.999999999998 4.50499088517E-12 -3.28622220978E-12 9.59310066789E-13 -9.72568865856E-14</math:coefficients>
     <math:minRange>0.712</math:minRange>
     <math:maxRange>4.069</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887338">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>4.069</math:minRange>
     <math:maxRange>4.069</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873247">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>4.069</math:minRange>
     <math:maxRange>4.069</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828701162">
   <gml:description>Gelesen aus: PROF0045.8000.txt</gml:description>
   <gml:name>45.8000</gml:name>
   <station>45.8000</station>
   <profileMember xlink:href="project:/modell.gml#Profile1211993827737108"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828701186">
     <gml:description>Übernommen aus Datei: PROF0045.8000.txt</gml:description>
     <gml:name>0045.8000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282870193">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[359.7 0.7520000 15.25830 10.00000 1.000000 -0.02978107 0.2876434 0E-7
360.1 1.226000 30.97860 30.00000 1.000000 0.07001850 -0.8030858 0E-7
360.5 1.561000 43.81040 50.00000 1.000000 0.03317145 -0.09858619 0E-7
360.7 1.830000 54.95280 70.00000 1.000000 -0.04111713 0.3170981 0E-7
361.0 2.066000 65.17180 90.00000 1.000000 -0.04749131 0.7600121 0E-7
361.2 2.272000 74.44230 110.0000 1.000000 -0.06171078 0.4294369 0E-7
361.4 2.463000 83.23550 130.0000 1.000000 -0.01266212 0.1387934 0E-7
361.6 2.641000 91.65310 150.0000 1.000000 0.02035548 -0.2323806 0E-7
361.7 2.808000 99.76520 170.0000 1.000000 0.01211425 -0.7046794 0E-7
361.9 2.970000 107.7430 190.0000 1.000000 0.05282258 -0.7135825 0E-7
362.0 3.126000 115.6080 210.0000 1.000000 0.05357993 -0.4578894 0E-7
362.2 3.275000 123.2860 230.0000 1.000000 0.02057341 -0.1541421 0E-7
362.3 3.419000 130.7990 250.0000 1.000000 0.02049177 0.3979619 0E-7
362.5 3.557000 138.1630 270.0000 1.000000 -0.02584265 0.9952448 0E-7
362.6 3.683000 144.9569 290.0000 1.000080 -0.03415078 0.6134227 -1.520079E-7
362.7 3.803000 151.4968 310.0000 1.000410 -0.01540200 0.05710205 8.052103E-7
362.8 3.919000 157.9594 330.0000 1.000900 -0.04454372 -0.4048307 -0.000001685407
362.9 4.033000 164.3520 350.0000 1.001510 -0.02085915 -0.4539614 0.000001762565
363.1 4.144000 170.6763 370.0000 1.002210 -0.003766402 -0.2753144 -9.223727E-7
363.2 4.253000 176.9407 390.0000 1.002960 0.05419974 0.3017370 1.920118E-7
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873373">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>12.2828669607 -34.347561014 47.4156564803 -7.58019510709 0.762817912511</math:coefficients>
     <math:minRange>0.752</math:minRange>
     <math:maxRange>4.253</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873509">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-2.56728242844 16.3371567994 10.9968147703 -1.7758269454 0.146040888242</math:coefficients>
     <math:minRange>0.752</math:minRange>
     <math:maxRange>4.253</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873511">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.752</math:minRange>
     <math:maxRange>3.557</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873341">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-0.945453207975 1.6393760073 -0.460484242131 0.0431150986468</math:coefficients>
     <math:minRange>3.557</math:minRange>
     <math:maxRange>3.683</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887374">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.02499803528 0.0483056467181 -0.0449661459005 0.0115956334795 -9.3578391846E-4</math:coefficients>
     <math:minRange>3.683</math:minRange>
     <math:maxRange>4.253</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828717197">
   <gml:description>Gelesen aus: PROF0045.9000.txt</gml:description>
   <gml:name>45.9000</gml:name>
   <station>45.9000</station>
   <profileMember xlink:href="project:/modell.gml#Profile1211993827737110"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828717171">
     <gml:description>Übernommen aus Datei: PROF0045.9000.txt</gml:description>
     <gml:name>0045.9000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828717107">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.1 0.7890000 15.20450 10.00000 1.000000 0.4705329 2.470585 -1.199041E-14
360.6 1.265000 30.61730 30.00000 1.000000 -2.188284 -6.900212 5.107026E-14
360.9 1.611000 43.14190 50.00000 1.000000 0.8842598 -0.8774462 -4.185541E-14
361.2 1.900000 54.34520 70.00000 1.000000 2.331646 4.106335 -3.563816E-14
361.4 2.148000 64.50110 90.00000 1.000000 1.716854 5.376053 2.198242E-14
361.7 2.369000 73.93910 110.0000 1.000000 -0.1070821 3.817486 4.907186E-14
361.9 2.572000 82.92280 130.0000 1.000000 -2.202759 0.8889284 -3.175238E-14
362.1 2.756000 91.31843 150.0000 1.000000 -3.614837 -2.552090 -0.0008798344
362.2 2.918000 98.89422 170.0000 1.000700 -3.644063 -6.067636 0.002440165
362.4 3.072000 106.2785 190.0000 1.001760 -1.832224 -7.654126 -0.0004430602
362.5 3.220000 113.5212 210.0000 1.003050 2.438089 -6.307912 -0.002544231
362.7 3.362000 120.6232 230.0000 1.004460 9.674300 -1.300245 -0.001003975
362.9 3.589000 132.6476 250.0000 1.006580 29.50407 31.07935 0.009295837
363.0 3.702000 198.7491 270.0000 1.026090 -15.57461 44.38450 -0.003120306
363.0 3.701000 198.6596 290.0000 1.026090 -15.68858 24.06525 -0.003176179
363.0 3.701000 198.6112 310.0000 1.026090 -15.64018 4.065247 -0.003176179
363.0 3.704000 198.8820 330.0000 1.026100 -15.29959 -14.97562 -0.003019156
363.0 3.710000 199.4955 350.0000 1.026170 -14.68149 -33.04490 -0.002760645
363.1 3.820000 210.7490 370.0000 1.026300 -1.164265 -14.55424 0.001239973
363.2 3.911000 220.1932 390.0000 1.025810 13.28945 2.111180 0.0007952328
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873112">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>172.602562579 -438.188316482 388.062318352 -125.863483611 15.0746417211</math:coefficients>
     <math:minRange>0.789</math:minRange>
     <math:maxRange>3.911</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887392">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>81.1606732168 -206.86777232 210.341405719 -75.1685279797 9.57738193243</math:coefficients>
     <math:minRange>0.789</math:minRange>
     <math:maxRange>3.911</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887363">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.999999999996 1.14589814648E-11 -1.12742395865E-11 4.63769983599E-12 -6.80554649581E-13</math:coefficients>
     <math:minRange>0.789</math:minRange>
     <math:maxRange>2.572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873369">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-38.8765047667 45.4147695628 -17.2306711848 2.17780805552</math:coefficients>
     <math:minRange>2.572</math:minRange>
     <math:maxRange>2.756</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873467">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-25.5265356672 32.6610805451 -14.9886602678 3.03716026121 -0.229132525136</math:coefficients>
     <math:minRange>2.756</math:minRange>
     <math:maxRange>3.911</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282871777">
   <gml:description>Gelesen aus: PROF0046.0000.txt</gml:description>
   <gml:name>46.0000</gml:name>
   <station>46.0000</station>
   <profileMember xlink:href="project:/modell.gml#Profile1211993827737144"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282871719">
     <gml:description>Übernommen aus Datei: PROF0046.0000.txt</gml:description>
     <gml:name>0046.0000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828717140">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.4 0.5590000 16.58190 10.00000 1.000000 -0.7658111 1.783275 0E-7
360.8 0.9870000 33.60890 30.00000 1.000000 1.317625 -7.477250 0E-7
361.2 1.294000 47.44420 50.00000 1.000000 1.779662 3.231144 0E-7
361.4 1.537000 59.22230 70.00000 1.000000 0.3100812 7.094016 0E-7
361.6 1.740000 69.50544 90.00000 1.000610 -1.720285 3.750862 -0.0004868304
361.8 1.928000 79.44449 110.0000 1.001920 -3.523929 -3.144427 0.001351618
362.0 2.136000 90.92087 130.0000 1.003540 -4.085271 -8.636532 -0.0006906057
362.2 2.373000 105.3221 150.0000 1.004850 -1.039801 -5.539479 -0.001209277
362.4 2.520000 115.3079 170.0000 1.006670 4.092151 -3.005989 -0.0001016786
362.5 2.640000 123.9367 190.0000 1.009960 11.17098 2.937791 0.0006190787
362.6 2.755000 132.4536 210.0000 1.013380 21.22187 16.36211 0.002222496
362.7 2.864000 140.8353 230.0000 1.016640 34.21663 37.78596 0.004418201
362.7 2.885000 198.4616 250.0000 1.023660 -18.81780 27.02930 -0.001515610
362.7 2.891000 199.1841 270.0000 1.024000 -18.19850 9.751174 -0.001544884
362.8 2.894000 199.4651 290.0000 1.024110 -17.80357 -8.874172 -0.001499524
362.7 2.887000 198.7327 310.0000 1.023810 -18.64309 -32.06746 -0.001562038
362.8 2.895000 199.6525 330.0000 1.024190 -17.76490 -48.41391 -0.001527742
362.8 2.968000 207.9429 350.0000 1.027390 -8.505220 -31.93667 -0.0009906259
362.9 3.040000 216.3212 370.0000 1.029870 2.579440 -9.948157 -0.00001764827
363.0 3.110000 224.5235 390.0000 1.031780 15.53664 17.25096 0.0009730318
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873389">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>190.293581031 -658.403813945 782.330132995 -344.720933972 54.1649318002</math:coefficients>
     <math:minRange>0.559</math:minRange>
     <math:maxRange>3.11</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873102">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>13.7805782178 -45.3390486015 120.788172825 -66.4986012223 12.8199695979</math:coefficients>
     <math:minRange>0.559</math:minRange>
     <math:maxRange>3.11</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873198">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.559</math:minRange>
     <math:maxRange>1.537</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873462">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-2.37904527977 6.34297398004 -3.96261810722 0.823765477444</math:coefficients>
     <math:minRange>1.537</math:minRange>
     <math:maxRange>1.74</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873432">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-0.764560557353 3.03300093365 -1.91862144566 0.528267929872 -0.0531124293823</math:coefficients>
     <math:minRange>1.74</math:minRange>
     <math:maxRange>3.11</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828732176">
   <gml:description>Gelesen aus: PROF0046.0160.txt</gml:description>
   <gml:name>46.0160</gml:name>
   <station>46.0160</station>
   <profileMember xlink:href="project:/modell.gml#Profile1211993827737136"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828732202">
     <gml:description>Übernommen aus Datei: PROF0046.0160.txt</gml:description>
     <gml:name>0046.0160</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828732153">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.3 0.5850000 16.83060 10.00000 1.000000 1.535214 4.865260 0E-7
360.7 0.9990000 34.61930 30.00000 1.000000 -5.351371 -15.72339 0E-7
361.0 1.288000 48.50740 50.00000 1.000000 0.6343288 0.1218155 0E-7
361.2 1.520000 60.34700 70.00000 1.000000 4.005979 10.71838 0E-7
361.4 1.716000 70.72900 90.00000 1.000320 3.989883 12.34728 -0.0001888023
361.6 1.889000 80.18159 110.0000 1.001340 1.698439 7.251979 0.0002865999
361.8 2.048000 89.03142 130.0000 1.002590 -1.565727 -1.653988 0.0003120384
361.9 2.196000 97.48552 150.0000 1.003760 -4.670238 -11.80919 -0.00005590753
362.1 2.337000 105.6162 170.0000 1.004930 -6.341344 -20.55110 -0.0007593387
362.3 2.577000 121.2406 190.0000 1.005810 -3.528130 -8.589642 -0.0005084113
362.4 2.717000 131.3458 210.0000 1.006520 4.593286 3.788857 0.0007017909
362.5 2.819000 139.0797 230.0000 1.008130 15.20541 16.99827 0.001728084
362.6 2.917000 146.6514 250.0000 1.010250 30.31076 38.64766 0.003590810
362.7 2.941000 205.3470 270.0000 1.017980 -21.96764 30.52763 -0.002894949
362.7 2.937000 204.9427 290.0000 1.017840 -22.65817 8.498346 -0.002970476
362.7 2.937000 204.9393 310.0000 1.017840 -22.65471 -11.50165 -0.002970476
362.7 2.939000 205.1798 330.0000 1.017920 -22.34909 -30.48951 -0.002943128
362.7 2.958000 207.3194 350.0000 1.018810 -19.17197 -40.62252 -0.002771266
362.7 3.020000 214.6930 370.0000 1.021340 -7.500928 -25.10132 -0.001268628
362.8 3.086000 222.4259 390.0000 1.023610 8.124046 -1.215987 0.001827980
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873377">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>336.069099597 -1093.14458671 1196.84140452 -499.284139302 73.8929449755</math:coefficients>
     <math:minRange>0.585</math:minRange>
     <math:maxRange>3.086</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873344">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>130.610376598 -416.91494838 501.889307075 -220.194928738 33.9401163222</math:coefficients>
     <math:minRange>0.585</math:minRange>
     <math:maxRange>3.086</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873315">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.585</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873356">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.377070472802 1.18725481752 -0.753318067724 0.159111280638</math:coefficients>
     <math:minRange>1.52</math:minRange>
     <math:maxRange>1.716</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873171">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.45498870834 -0.921091767984 0.68343965041 -0.220743837169 0.0263714090501</math:coefficients>
     <math:minRange>1.716</math:minRange>
     <math:maxRange>3.086</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828732123">
   <gml:description>Gelesen aus: PROF0046.1000.txt</gml:description>
   <gml:name>46.1000</gml:name>
   <station>46.1000</station>
   <profileMember xlink:href="project:/modell.gml#Profile1211993827737112"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828732130">
     <gml:description>Übernommen aus Datei: PROF0046.1000.txt</gml:description>
     <gml:name>0046.1000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282873235">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[359.4 0.8770000 13.72710 10.00000 1.000000 -1.984919 1.466254 0E-7
360.0 1.473000 29.87480 30.00000 1.000000 6.079444 -5.952403 0E-7
360.3 1.844000 41.82250 50.00000 1.000000 1.372501 1.113812 0E-7
360.7 2.146000 52.07690 70.00000 1.000000 -3.095891 4.737309 0E-7
360.9 2.412000 61.49610 90.00000 1.000000 -5.568738 4.502209 0E-7
361.2 2.656000 70.45470 110.0000 1.000000 -5.504070 1.776944 0E-7
361.4 2.873000 78.66673 130.0000 1.000500 -2.902993 -2.550999 -0.0001493052
361.6 3.075000 86.55755 150.0000 1.001670 2.029947 -6.239372 0.0005283437
361.8 3.267000 94.23310 170.0000 1.003110 9.255349 -7.466438 -0.0004813009
362.0 3.449000 101.7225 190.0000 1.004510 18.47622 -4.850336 -0.00008596431
362.4 3.856000 176.3336 210.0000 1.017870 -9.348581 54.12463 0.0003382465
362.4 3.855000 176.2381 230.0000 1.017820 -9.383989 33.86545 0.0003391327
362.4 3.858000 176.4991 250.0000 1.017970 -9.251978 14.64414 0.0003366273
362.4 3.856000 176.3011 270.0000 1.017850 -9.316039 -5.875372 0.0003582465
362.4 3.855000 176.2697 290.0000 1.017830 -9.415519 -26.13455 0.0003291327
362.4 3.920000 182.5312 310.0000 1.021450 -7.006385 -28.46387 -0.000004642161
362.5 4.005000 191.0941 330.0000 1.026310 -3.741849 -22.66382 -0.0003828596
362.6 4.092000 200.0810 350.0000 1.030780 -0.05961621 -12.77578 -0.0002756290
362.7 4.170000 208.2481 370.0000 1.034350 3.604792 -2.680945 0.000003089364
362.8 4.249000 216.6322 390.0000 1.037560 7.646766 11.27867 0.0001733944
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873156">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>216.079513866 -499.878703552 399.25156503 -120.314303273 13.2861506186</math:coefficients>
     <math:minRange>0.877</math:minRange>
     <math:maxRange>4.249</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873326">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-102.631964983 216.215034815 -122.460241438 29.894789892 -2.06831848939</math:coefficients>
     <math:minRange>0.877</math:minRange>
     <math:maxRange>4.249</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873257">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.877</math:minRange>
     <math:maxRange>2.656</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873172">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-6.06875748594 7.80155588905 -2.86853254861 0.351372026564</math:coefficients>
     <math:minRange>2.656</math:minRange>
     <math:maxRange>2.873</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887321">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-5.1038059911 7.08077294552 -3.05110611215 0.577675822475 -0.0404186696354</math:coefficients>
     <math:minRange>2.873</math:minRange>
     <math:maxRange>4.249</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282874894">
   <gml:description>Gelesen aus: PROF0046.2000.txt</gml:description>
   <gml:name>46.2000</gml:name>
   <station>46.2000</station>
   <profileMember xlink:href="project:/modell.gml#Profile1211993827737114"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828748174">
     <gml:description>Übernommen aus Datei: PROF0046.2000.txt</gml:description>
     <gml:name>0046.2000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828748138">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[359.0 0.8150000 15.29630 10.00000 1.000000 1.891307 3.051352 0E-7
359.5 1.286000 30.79730 30.00000 1.000000 -4.971363 -7.512491 0E-7
359.8 1.616000 43.22870 50.00000 1.000000 -1.078496 -2.118370 0E-7
360.1 1.884000 54.14840 70.00000 1.000000 2.202564 2.815891 0E-7
360.3 2.116000 63.96670 90.00000 1.000000 3.619426 5.064565 0E-7
360.5 2.328000 73.18360 110.0000 1.000000 3.283843 5.071508 0E-7
360.7 2.524000 81.94530 130.0000 1.000000 1.609901 3.198976 0E-7
360.9 2.708000 90.34800 150.0000 1.000000 -0.7431457 0.2311074 0E-7
361.1 2.882000 98.45280 170.0000 1.000000 -3.090877 -3.047165 0E-7
361.2 3.047000 106.2980 190.0000 1.000000 -4.750976 -5.871979 0E-7
361.4 3.206000 113.9720 210.0000 1.000000 -5.016663 -7.223063 0E-7
361.6 3.357000 121.4360 230.0000 1.000000 -3.311980 -6.664494 0E-7
361.7 3.503000 128.7360 250.0000 1.000000 1.076351 -3.162302 0E-7
361.8 3.644000 135.8900 270.0000 1.000000 8.708436 3.929875 0E-7
362.0 3.780000 142.9020 290.0000 1.000000 20.08049 15.17392 0E-7
362.1 3.891000 191.1429 310.0000 1.039210 -9.521908 25.14614 -0.000001230683
362.1 3.896000 191.6230 330.0000 1.039380 -9.076262 6.601850 0.000001417385
362.1 3.889000 190.9930 350.0000 1.039150 -9.740103 -15.43347 -0.000008207348
362.1 3.943000 196.2431 370.0000 1.041200 -4.605145 -19.23804 -2.226239E-7
362.2 4.016000 203.5348 390.0000 1.043330 3.694492 -15.44728 3.592212E-8
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873309">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>179.894699843 -434.971823048 363.534948763 -109.294151193 12.1382905503</math:coefficients>
     <math:minRange>0.815</math:minRange>
     <math:maxRange>4.016</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873437">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>126.515471372 -299.096689367 265.799167632 -85.7915753321 9.81014719624</math:coefficients>
     <math:minRange>0.815</math:minRange>
     <math:maxRange>4.016</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873193">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.815</math:minRange>
     <math:maxRange>3.78</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873245">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>3081.74550264 -2410.10013099 628.35030391 -54.5950600152</math:coefficients>
     <math:minRange>3.78</math:minRange>
     <math:maxRange>3.891</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873301">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>78.2458653047 -59.0010327609 15.0195736065 -1.27362274661</math:coefficients>
     <math:minRange>3.891</math:minRange>
     <math:maxRange>4.016</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282874849">
   <gml:description>Gelesen aus: PROF0046.3000.txt</gml:description>
   <gml:name>46.3000</gml:name>
   <station>46.3000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382775255"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282874815">
     <gml:description>Übernommen aus Datei: PROF0046.3000.txt</gml:description>
     <gml:name>0046.3000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282874833">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[359.7 0.7140000 14.80470 10.00000 1.000000 1.581818 4.055693 0E-7
360.2 1.222000 30.51350 30.00000 1.000000 -7.581710 -17.15098 0E-7
360.6 1.581000 43.43000 50.00000 1.000000 5.100940 7.567998 0E-7
360.9 1.864000 54.59950 70.00000 1.000000 7.357171 15.75797 0E-7
361.1 2.096000 64.28347 90.00000 1.000690 2.269306 9.072543 -0.0002698669
361.3 2.305000 73.37084 110.0000 1.002250 -5.111702 -3.935047 0.0008703581
361.5 2.497000 82.06719 130.0000 1.003820 -10.11198 -16.21355 -0.0002550684
361.7 2.682000 90.76700 150.0000 1.005430 -8.088712 -20.30108 -0.001343207
361.9 2.859000 99.36201 170.0000 1.006610 6.112974 -8.764899 -0.0001855402
362.0 3.027000 107.7302 190.0000 1.007520 37.03581 25.09735 0.004359625
362.2 3.178000 212.1278 210.0000 1.022300 -11.61708 81.84797 -0.001562144
362.2 3.180000 212.3253 230.0000 1.022320 -10.92023 63.08326 -0.001434364
362.2 3.176000 211.9369 250.0000 1.022280 -12.31612 40.61890 -0.001689063
362.2 3.182000 212.5910 270.0000 1.022350 -10.28720 24.32480 -0.001315719
362.2 3.182000 212.5781 290.0000 1.022350 -10.27430 4.324795 -0.001315719
362.2 3.179000 212.2278 310.0000 1.022310 -11.27052 -17.53516 -0.001498362
362.2 3.182000 212.5809 330.0000 1.022350 -10.27705 -35.67520 -0.001315719
362.2 3.183000 212.6913 350.0000 1.022360 -9.936461 -55.05209 -0.001251071
362.2 3.193000 213.6817 370.0000 1.022460 -6.354787 -68.73432 -0.0005926006
362.3 3.262000 221.4992 390.0000 1.024740 20.55184 -40.65563 0.002979599
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873194">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>573.733214808 -1624.1836669 1561.49691424 -596.376013718 80.8883488845</math:coefficients>
     <math:minRange>0.714</math:minRange>
     <math:maxRange>3.262</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887312">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>325.489301069 -929.47178836 934.145287769 -371.534793857 52.1490030954</math:coefficients>
     <math:minRange>0.714</math:minRange>
     <math:maxRange>3.262</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873446">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.714</math:minRange>
     <math:maxRange>1.864</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873271">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-1.49935632259 3.88709310799 -2.012670079 0.34692234946</math:coefficients>
     <math:minRange>1.864</math:minRange>
     <math:maxRange>2.096</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873228">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.356483543879 0.739801464231 -0.265571901318 0.0228682250876 0.00256226983894</math:coefficients>
     <math:minRange>2.096</math:minRange>
     <math:maxRange>3.262</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828764200">
   <gml:description>Gelesen aus: PROF0046.4000.txt</gml:description>
   <gml:name>46.4000</gml:name>
   <station>46.4000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382775228"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828764155">
     <gml:description>Übernommen aus Datei: PROF0046.4000.txt</gml:description>
     <gml:name>0046.4000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282876492">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[359.8 0.6800000 14.83300 10.00000 1.000000 2.657266 3.718245 0E-7
360.3 1.185000 30.29670 30.00000 1.000000 -10.98569 -14.43374 0E-7
360.6 1.546000 42.90740 50.00000 1.000000 5.007629 4.572078 0E-7
360.9 1.853000 55.35250 70.00000 1.000000 10.07759 13.72790 0E-7
361.2 2.086000 65.26680 90.00000 1.000000 5.738110 8.947955 0E-7
361.4 2.296000 74.55874 110.0000 1.000000 -2.471066 -0.7530446 -0.0001632700
361.6 2.482000 83.10399 130.0000 1.000560 -9.799758 -10.74108 0.0004813769
361.7 2.651000 91.10462 150.0000 1.001620 -12.63717 -16.74380 0.00004359091
361.9 2.814000 99.03549 170.0000 1.002910 -7.707374 -14.20545 -0.0009961870
362.1 2.976000 107.0827 190.0000 1.004170 9.510263 2.281824 -0.0004969764
362.2 3.130000 114.9678 210.0000 1.005450 42.26498 35.59477 0.004470858
362.3 3.249000 216.7597 230.0000 1.023300 -13.51403 73.10796 -0.002718619
362.3 3.248000 216.7100 250.0000 1.023300 -13.91267 52.55578 -0.002837100
362.3 3.248000 216.6446 270.0000 1.023300 -13.84727 32.55578 -0.002837100
362.3 3.251000 216.9692 290.0000 1.023320 -12.82333 14.21605 -0.002499992
362.3 3.247000 216.5706 310.0000 1.023290 -14.22063 -7.995163 -0.002945030
362.3 3.248000 216.6408 330.0000 1.023300 -13.84345 -27.44422 -0.002837100
362.3 3.254000 217.3273 350.0000 1.023350 -11.82278 -44.11245 -0.002167866
362.3 3.248000 216.6842 370.0000 1.023300 -13.88687 -67.44422 -0.002837100
362.4 3.303000 222.6566 390.0000 1.023800 6.505368 -55.17722 0.004047085
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873452">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>388.936093021 -1123.11897329 1099.33627798 -419.912776263 57.0775827864</math:coefficients>
     <math:minRange>0.68</math:minRange>
     <math:maxRange>3.303</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873514">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>330.87852285 -949.55530978 947.135717197 -370.588077192 50.8786157205</math:coefficients>
     <math:minRange>0.68</math:minRange>
     <math:maxRange>3.303</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887382">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.68</math:minRange>
     <math:maxRange>2.086</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887370">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-0.428035295976 1.98411138331 -0.917775997225 0.14132246886</math:coefficients>
     <math:minRange>2.086</math:minRange>
     <math:maxRange>2.296</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873227">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>5.17829422689 -6.62878834703 3.92491152649 -1.02834666527 0.100660257558</math:coefficients>
     <math:minRange>2.296</math:minRange>
     <math:maxRange>3.303</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828764129">
   <gml:description>Gelesen aus: PROF0046.5000.txt</gml:description>
   <gml:name>46.5000</gml:name>
   <station>46.5000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382775292"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828764201">
     <gml:description>Übernommen aus Datei: PROF0046.5000.txt</gml:description>
     <gml:name>0046.5000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828764215">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[359.9 0.6820000 14.84080 10.00000 1.000000 3.167225 4.192083 0E-7
360.4 1.184000 30.19750 30.00000 1.000000 -11.42345 -14.69563 0E-7
360.7 1.548000 42.81440 50.00000 1.000000 2.549070 2.634599 0E-7
361.0 1.838000 53.65330 70.00000 1.000000 9.378640 11.50179 0E-7
361.3 2.092000 63.69500 90.00000 1.000000 7.823707 10.65396 0E-7
361.5 2.314000 72.89107 110.0000 1.000000 1.230092 3.078506 0.0002909454
361.7 2.509000 81.22732 130.0000 1.001100 -6.227804 -6.828994 -0.001113943
361.9 2.692000 89.37148 150.0000 1.002620 -11.68533 -14.63598 0.0009993744
362.0 2.871000 97.56878 170.0000 1.004380 -11.80871 -15.98124 0.0008934309
362.2 3.041000 105.5711 190.0000 1.006070 -3.070219 -7.466202 -0.001288564
362.4 3.203000 113.4094 210.0000 1.007510 17.53342 14.35140 -0.001567929
362.5 3.358000 121.0904 230.0000 1.008420 52.77722 52.65112 0.006327276
362.6 3.418000 209.6697 250.0000 1.024630 -14.08809 61.13239 -0.002665384
362.6 3.425000 210.3576 270.0000 1.024810 -12.03505 44.69573 -0.001818664
362.6 3.418000 209.6879 290.0000 1.024630 -14.10623 21.13239 -0.002665384
362.6 3.423000 210.1465 310.0000 1.024750 -12.61162 3.672340 -0.002056268
362.6 3.420000 209.8535 330.0000 1.024670 -13.49326 -17.85480 -0.002416276
362.6 3.427000 210.5515 350.0000 1.024860 -11.43757 -34.27663 -0.001567620
362.6 3.428000 210.6352 370.0000 1.024880 -11.12419 -53.76121 -0.001435801
362.6 3.459000 213.8434 390.0000 1.025650 -1.558953 -57.24570 0.002946881
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873433">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>334.959936659 -943.843669248 898.926380804 -329.269589401 42.8510241345</math:coefficients>
     <math:minRange>0.682</math:minRange>
     <math:maxRange>3.459</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887328">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>278.043762307 -773.628786647 750.31036154 -281.878692319 37.0146680501</math:coefficients>
     <math:minRange>0.682</math:minRange>
     <math:maxRange>3.459</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873311">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.682</math:minRange>
     <math:maxRange>2.092</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873454">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>8.16654848876 -9.93725871845 4.58768654596 -0.705106663725</math:coefficients>
     <math:minRange>2.092</math:minRange>
     <math:maxRange>2.314</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873331">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>12.6406670714 -16.9672367014 9.21850588453 -2.21324980443 0.198236752002</math:coefficients>
     <math:minRange>2.314</math:minRange>
     <math:maxRange>3.459</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828779196">
   <gml:description>Gelesen aus: PROF0046.6000.txt</gml:description>
   <gml:name>46.6000</gml:name>
   <station>46.6000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382775293"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282877935">
     <gml:description>Übernommen aus Datei: PROF0046.6000.txt</gml:description>
     <gml:name>0046.6000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828779166">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[360.1 0.7860000 14.60790 10.00000 1.000000 2.694758 0.3355864 0E-7
360.6 1.296000 30.16090 30.00000 1.000000 -6.890186 -0.9004235 0E-7
360.9 1.640000 42.61000 50.00000 1.000000 -1.958160 0.06183737 0E-7
361.2 1.913000 53.19880 70.00000 1.000000 2.419430 0.1366572 0E-7
361.5 2.156000 63.01320 90.00000 1.000000 4.714907 0.2345214 0E-7
361.7 2.375000 72.22620 110.0000 1.000000 4.868634 0.1462954 0E-7
361.9 2.579000 81.14710 130.0000 1.000000 3.497590 0.3822793 0E-7
362.1 2.769000 89.71880 150.0000 1.000000 1.478296 0.9285765 0E-7
362.3 2.948000 98.00680 170.0000 1.000000 -0.2895281 2.063357 0E-7
362.4 3.079000 110.3832 190.0000 1.024560 -7.112875 -1.185803 0.0004651763
362.5 3.218000 119.0246 210.0000 1.027530 -8.552430 -2.037262 -0.001400823
362.7 3.351000 127.3658 230.0000 1.029150 -8.139587 -2.202795 0.0005724791
362.8 3.478000 135.4400 250.0000 1.029770 -5.536159 -1.693407 0.001261050
362.9 3.599000 143.2322 270.0000 1.029090 -0.4473636 -0.5453542 0.0002314761
363.0 3.715000 150.8162 290.0000 1.028060 7.377811 1.374302 -0.001622523
363.1 3.825000 158.0428 310.0000 1.027290 18.07477 3.808622 -0.001509378
363.2 3.927000 164.9740 330.0000 1.027680 31.12564 6.186239 0.003446014
363.3 4.013000 234.5562 350.0000 1.044670 -18.74634 6.332635 -0.0008204090
363.3 4.014000 234.6349 370.0000 1.044680 -18.57921 -13.42586 -0.0006230633
363.3 4.012000 234.4631 390.0000 1.044660 -234.4631 -390.0000 -1.044660
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873531">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>30.9605787681 -81.873762399 87.1106221252 -23.0144352792 2.84725359075</math:coefficients>
     <math:minRange>0.786</math:minRange>
     <math:maxRange>4.014</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873491">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>146.075624141 -344.264784387 297.659700026 -95.1524932656 10.8235848836</math:coefficients>
     <math:minRange>0.786</math:minRange>
     <math:maxRange>4.014</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873334">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0 0.0 0.0</math:coefficients>
     <math:minRange>0.786</math:minRange>
     <math:maxRange>2.948</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873163">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>643.263167896 -640.20675093 212.625985167 -23.5284374973</math:coefficients>
     <math:minRange>2.948</math:minRange>
     <math:maxRange>3.079</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873376">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>67.7358403914 -77.8843280123 33.990627264 -6.57210628146 0.475029410832</math:coefficients>
     <math:minRange>3.079</math:minRange>
     <math:maxRange>4.014</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828779164">
   <gml:description>Gelesen aus: PROF0046.7000.txt</gml:description>
   <gml:name>46.7000</gml:name>
   <station>46.7000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382775252"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828779150">
     <gml:description>Übernommen aus Datei: PROF0046.7000.txt</gml:description>
     <gml:name>0046.7000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282877979">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[361.9 0.7480000 16.10130 10.00000 1.000000 0.1610357 0.2158891 0E-7
362.4 1.210000 33.69810 30.00000 1.000000 -1.289431 -1.694164 0E-7
362.7 1.525000 46.94870 50.00000 1.000000 7.126612 6.466389 0E-7
362.8 1.607000 66.88580 70.00000 1.020110 -6.076875 -4.234316 0.001481447
363.0 1.779000 78.86464 90.00000 1.022410 -2.735490 -2.188017 -0.005014431
363.1 1.933000 89.70942 110.0000 1.023110 1.297321 0.3983315 0.001095502
363.3 2.074000 99.87852 130.0000 1.023450 5.515752 3.376846 0.01077900
363.4 2.162000 116.2728 150.0000 1.045520 -1.603012 -1.188821 -0.004750119
363.5 2.271000 127.7977 170.0000 1.052290 -1.391590 -0.9231132 -0.004219854
363.6 2.374000 138.7661 190.0000 1.055390 -1.077810 -0.6181130 -0.001948862
363.7 2.472000 149.2066 210.0000 1.056850 -0.6646739 -0.2670396 -0.0001005339
363.8 2.565000 159.2449 230.0000 1.057040 -0.3404376 -0.03082069 0.001069210
363.9 2.654000 168.8774 250.0000 1.056680 -0.03670847 0.1724478 0.001144506
363.9 2.739000 178.0841 270.0000 1.054950 0.2320409 0.2293763 0.001329041
364.0 2.821000 186.9924 290.0000 1.053090 0.4240789 0.2819603 0.0007947025
364.1 2.900000 195.6446 310.0000 1.051250 0.4776097 0.2536779 -0.0001615189
364.2 2.976000 204.0358 330.0000 1.049180 0.3837099 0.07162791 -0.0008341752
364.2 3.050000 212.2078 350.0000 1.047180 0.1989763 -0.06151552 -0.001111338
364.3 3.122000 220.1918 370.0000 1.045330 -0.1168357 -0.1902168 -0.0006284964
364.4 3.193000 228.0062 390.0000 1.043610 -0.4842710 -0.07040947 0.001075922
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873524">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>47.9003475566 -112.336724478 88.9894689369 -8.81363686464 0.773160696464</math:coefficients>
     <math:minRange>0.748</math:minRange>
     <math:maxRange>3.193</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873529">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>46.9891196381 -100.431695098 90.1541425524 -14.9368846077 0.657225992587</math:coefficients>
     <math:minRange>0.748</math:minRange>
     <math:maxRange>3.193</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887385">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0</math:coefficients>
     <math:minRange>0.748</math:minRange>
     <math:maxRange>1.525</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873261">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>344.255130319 -659.123563818 421.633530208 -89.8480628241</math:coefficients>
     <math:minRange>1.525</math:minRange>
     <math:maxRange>1.607</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873103">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>4.20703739354 -5.61278351731 3.58814853143 -0.985640737494 0.0987384941088</math:coefficients>
     <math:minRange>1.607</math:minRange>
     <math:maxRange>3.193</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult129432282879545">
   <gml:description>Gelesen aus: PROF0046.8000.txt</gml:description>
   <gml:name>46.8000</gml:name>
   <station>46.8000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382775222"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828795178">
     <gml:description>Übernommen aus Datei: PROF0046.8000.txt</gml:description>
     <gml:name>0046.8000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828795111">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[363.4 0.4640000 21.18490 10.00000 1.011520 -0.2302110 0.08201920 0.0001077299
363.7 0.7490000 45.08981 30.00000 1.042020 0.7467739 -0.2621776 -0.000004639725
363.9 0.9380000 64.73718 50.00000 1.046420 -0.1312115 0.004344362 -0.0002570992
364.1 1.090000 80.87250 70.00000 1.044860 -0.3664138 0.1786801 -0.0006678382
364.2 1.221000 94.87600 90.00000 1.040460 -0.3050140 0.1867147 -0.00005981066
364.3 1.338000 107.4952 110.0000 1.035540 -0.1910877 0.01576918 0.0007417636
364.4 1.446000 119.1759 130.0000 1.031450 -0.03814281 -0.1030971 0.0009505605
364.5 1.548000 130.1707 150.0000 1.028480 0.1767270 0.01636881 0.0005123792
364.6 1.643000 140.6012 170.0000 1.026210 0.1964125 -0.1076130 0.00002169695
364.7 1.734000 150.5893 190.0000 1.024510 0.2194172 -0.06833791 -0.0004502044
364.8 1.821000 160.1824 210.0000 1.023170 0.1984298 -0.01765940 -0.0007203745
364.9 1.904000 169.4361 230.0000 1.022060 0.08375834 -0.09420355 -0.0007347022
365.0 1.985000 178.3875 250.0000 1.021070 0.06835412 0.08204920 -0.0005030523
365.0 2.062000 187.0834 270.0000 1.020260 -0.1034848 -0.07913486 -0.0001777461
365.1 2.138000 195.5428 290.0000 1.019630 -0.1049827 0.1214360 0.0001027884
365.2 2.211000 203.7956 310.0000 1.018900 -0.1738014 0.09552186 0.0005057406
365.3 2.282000 211.8408 330.0000 1.018220 -0.1831794 0.05201968 0.0007515999
365.3 2.351000 219.7105 350.0000 1.017620 -0.1508588 -0.05889818 0.0006871510
365.4 2.419000 227.4249 370.0000 1.017130 0.03343140 0.01433864 0.0001456045
365.5 2.485000 234.9977 390.0000 1.016720 0.2550837 -0.05814002 -0.0009515473
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873248">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>9.65397474895 -45.3657171608 105.687912157 -13.322976751 1.17539775214</math:coefficients>
     <math:minRange>0.464</math:minRange>
     <math:maxRange>2.485</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873185">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-4.06430808897 24.6481811933 76.111389594 -30.1482154326 4.47636042454</math:coefficients>
     <math:minRange>0.464</math:minRange>
     <math:maxRange>2.485</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873476">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>0.464</math:minRange>
     <math:maxRange>0.464</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873209">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>0.464</math:minRange>
     <math:maxRange>0.464</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873151">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.848811836479 0.567451703932 -0.565088919534 0.227081615528 -0.0324722026778</math:coefficients>
     <math:minRange>0.464</math:minRange>
     <math:maxRange>2.485</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828795167">
   <gml:description>Gelesen aus: PROF0046.9000.txt</gml:description>
   <gml:name>46.9000</gml:name>
   <station>46.9000</station>
   <profileMember xlink:href="project:/modell.gml#Profile1211993827768146"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828795159">
     <gml:description>Übernommen aus Datei: PROF0046.9000.txt</gml:description>
     <gml:name>0046.9000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition12943228287959">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[363.3 0.5610000 18.45670 10.00000 1.000000 -0.4074792 -0.1837585 0E-7
363.7 0.9510000 37.31960 30.00000 1.000000 2.005865 1.088609 0E-7
363.9 1.139000 57.19841 50.00000 1.022700 -0.8915310 -0.7500154 0.0005638716
364.1 1.306000 74.39796 70.00000 1.052960 -0.9823594 -0.5050807 -0.001612948
364.2 1.448000 89.52280 90.00000 1.060310 -0.6818907 -0.3414042 0.001008777
364.3 1.576000 103.3397 110.0000 1.062720 -0.2389522 0.09486625 0.0002107989
364.4 1.692000 115.9795 130.0000 1.061050 0.1520680 0.4230780 -0.0005328705
364.5 1.797000 127.5442 150.0000 1.055950 0.3745751 0.2746370 0.0005346746
364.6 1.896000 138.4559 170.0000 1.051460 0.5214509 0.2306615 0.0004731858
364.7 1.989000 148.8187 190.0000 1.047480 0.4805042 0.05343443 0.00006680205
364.8 2.078000 158.7126 210.0000 1.043840 0.4063126 -0.01600549 -0.0002401982
364.9 2.163000 168.2430 230.0000 1.040630 0.2173360 -0.1176839 -0.0003669573
365.0 2.245000 177.4075 250.0000 1.038020 0.05685250 -0.1336637 -0.0004794413
365.1 2.324000 186.3644 270.0000 1.035700 -0.1985593 -0.1588768 -0.0003087940
365.1 2.401000 195.0326 290.0000 1.033710 -0.3199029 -0.01689334 -0.00001959757
365.2 2.475000 203.4850 310.0000 1.032100 -0.4532125 -0.04146988 0.0002188924
365.3 2.547000 211.7401 330.0000 1.030700 -0.4677777 -0.03122541 0.0003880358
365.4 2.617000 219.7885 350.0000 1.029330 -0.3166697 -0.03951048 0.0004837005
365.4 2.686000 227.6695 370.0000 1.028090 0.1184077 0.1794528 0.0001868897
365.5 2.752000 235.3869 390.0000 1.026900 0.6249626 -0.009151343 -0.0005748219
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887356">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>21.0370439132 -66.7811751232 86.4862430494 -5.68411051685 0.282463501877</math:coefficients>
     <math:minRange>0.561</math:minRange>
     <math:maxRange>2.752</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873339">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>50.9646370419 -158.271496421 217.324160867 -76.4835383307 9.91660974907</math:coefficients>
     <math:minRange>0.561</math:minRange>
     <math:maxRange>2.752</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873105">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0</math:coefficients>
     <math:minRange>0.561</math:minRange>
     <math:maxRange>0.951</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873128">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>1.96246624504 -2.34648279941 1.74216033527 -0.356444452651</math:coefficients>
     <math:minRange>0.951</math:minRange>
     <math:maxRange>1.139</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873371">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-0.386624054219 2.83667242272 -2.01734885556 0.618144654919 -0.0697151587664</math:coefficients>
     <math:minRange>1.139</math:minRange>
     <math:maxRange>2.752</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828795173">
   <gml:description>Gelesen aus: PROF0047.0000.txt</gml:description>
   <gml:name>47.0000</gml:name>
   <station>47.0000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382776837"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828795155">
     <gml:description>Übernommen aus Datei: PROF0047.0000.txt</gml:description>
     <gml:name>0047.0000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828810152">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[363.2 0.6540000 18.66320 10.00000 1.000000 -0.5468744 -0.4248047 0.0002708049
363.6 1.025000 37.25935 30.00000 1.002060 3.817322 2.348518 0.003602659
363.7 1.195000 58.47490 50.00000 1.025500 -3.637119 -1.576567 -0.01100814
363.9 1.373000 72.36230 70.00000 1.022900 -1.486882 -0.9679341 0.002082297
364.1 1.525000 85.05075 90.00000 1.025200 0.5724663 -0.3214577 0.008434188
364.2 1.659000 98.18366 110.0000 1.036460 1.079799 0.2020868 0.003839085
364.3 1.778000 111.2508 130.0000 1.046840 0.5291930 0.2481755 -0.001749083
364.4 1.888000 123.3865 150.0000 1.051220 0.2365457 0.2980257 -0.002835576
364.5 1.990000 134.8342 170.0000 1.053360 -0.03837335 0.1923588 -0.002987568
364.6 2.087000 145.6769 190.0000 1.053800 -0.1162854 0.2727718 -0.002518107
364.7 2.179000 155.9585 210.0000 1.052640 -0.08412020 0.3630135 -0.001367649
364.8 2.265000 165.7219 230.0000 1.050250 -0.1299399 0.06204527 0.0002840230
364.9 2.348000 175.1425 250.0000 1.047900 -0.1122058 -0.08466800 0.001310175
365.0 2.428000 184.2538 270.0000 1.045760 -0.07763203 -0.1672868 0.001676574
365.0 2.505000 193.0963 290.0000 1.043830 -0.07625537 -0.2725560 0.001518421
365.1 2.580000 201.7052 310.0000 1.042090 -0.03441626 -0.2119328 0.0009533824
365.2 2.652000 210.0838 330.0000 1.040340 -0.07470664 -0.3202135 0.0003309485
365.3 2.723000 218.2643 350.0000 1.038780 0.0005473232 -0.09724260 -0.0005038224
365.3 2.792000 226.2371 370.0000 1.037330 0.08474946 0.1339469 -0.001332607
365.4 2.859000 234.0861 390.0000 1.035850 0.09418797 0.3237206 -0.001916902
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873146">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>10.9871226615 -43.1288396238 63.1032226214 -0.741177493343 0.0623296337164</math:coefficients>
     <math:minRange>0.654</math:minRange>
     <math:maxRange>2.859</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873217">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>13.4939844411 -39.8102092627 84.6793755355 -21.2349566699 2.0742716967</math:coefficients>
     <math:minRange>0.654</math:minRange>
     <math:maxRange>2.859</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873127">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>0.654</math:minRange>
     <math:maxRange>0.654</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873237">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>0.654</math:minRange>
     <math:maxRange>0.654</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873405">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.11162801193 -0.365030862598 0.384894468853 -0.14500423491 0.0180875136354</math:coefficients>
     <math:minRange>0.654</math:minRange>
     <math:maxRange>2.792</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828810159">
   <gml:description>Gelesen aus: PROF0047.1000.txt</gml:description>
   <gml:name>47.1000</gml:name>
   <station>47.1000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382776834"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation129432282881069">
     <gml:description>Übernommen aus Datei: PROF0047.1000.txt</gml:description>
     <gml:name>0047.1000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828810118">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[363.1 0.6780000 18.33180 10.00000 1.000000 0.5837931 -0.02334632 0E-7
363.4 1.045000 36.86130 30.00000 1.000000 -1.739403 0.1807851 0E-7
363.7 1.308000 51.13970 50.00000 1.000000 -0.4991303 -0.2816872 0E-7
363.9 1.512000 64.41397 70.00000 1.018800 1.585333 -0.08032311 -0.002197489
364.1 1.676000 78.90548 90.00000 1.045700 1.766784 0.1134485 0.004971928
364.2 1.814000 93.75344 110.0000 1.071730 0.8687936 0.1401086 0.0009148299
364.3 1.935000 108.2428 130.0000 1.089170 -0.2496199 0.1140136 -0.002950740
364.4 2.044000 121.8539 150.0000 1.096980 -0.9882826 0.09597741 -0.003208241
364.5 2.143000 134.4422 170.0000 1.098190 -1.291586 -0.1016406 -0.001273454
364.6 2.236000 146.2739 190.0000 1.096650 -1.154097 -0.06568379 0.0002538844
364.7 2.323000 157.4064 210.0000 1.093020 -0.7965775 -0.07710583 0.001592449
364.8 2.405000 167.9955 230.0000 1.088620 -0.3757869 -0.1546651 0.002172713
364.9 2.484000 178.1350 250.0000 1.084120 0.1801625 0.008156806 0.001852909
365.0 2.559000 187.9015 270.0000 1.079670 0.5744143 0.002941106 0.001083736
365.0 2.631000 197.3540 290.0000 1.075630 0.8123986 -0.04969420 -0.00008234976
365.1 2.701000 206.5333 310.0000 1.071950 0.9262860 0.01936258 -0.001248841
365.2 2.769000 215.4587 330.0000 1.068490 0.8409804 0.1208903 -0.001891303
365.2 2.835000 224.1603 350.0000 1.065390 0.4778428 0.1692875 -0.001799846
365.3 2.899000 232.6551 370.0000 1.062450 -0.2211193 0.08319269 -0.0004639644
365.4 2.961000 240.9551 390.0000 1.059780 -1.301185 -0.2140182 0.002273779
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873308">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-40.551680442 120.337968728 -104.323462859 58.9975795334 -7.06313634691</math:coefficients>
     <math:minRange>0.678</math:minRange>
     <math:maxRange>2.961</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873115">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-17.7261762464 85.5211462347 -75.8958019072 48.7134201963 -7.74119296703</math:coefficients>
     <math:minRange>0.678</math:minRange>
     <math:maxRange>2.961</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873506">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0 0.0</math:coefficients>
     <math:minRange>0.678</math:minRange>
     <math:maxRange>1.308</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873235">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-2.50911248219 8.13787318934 -6.29000248807 1.62038465544</math:coefficients>
     <math:minRange>1.308</math:minRange>
     <math:maxRange>1.512</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873117">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.12976362098 -1.10318716863 1.31438290481 -0.522584777701 0.0681880678829</math:coefficients>
     <math:minRange>1.512</math:minRange>
     <math:maxRange>2.961</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828810142">
   <gml:description>Gelesen aus: PROF0047.2000.txt</gml:description>
   <gml:name>47.2000</gml:name>
   <station>47.2000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382776891"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828810164">
     <gml:description>Übernommen aus Datei: PROF0047.2000.txt</gml:description>
     <gml:name>0047.2000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition1294322828826169">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[363.1 0.5640000 19.18910 10.00000 1.000000 0.07154021 -0.4559480 -0.0003598326
363.4 0.9110000 37.25601 30.00000 1.000660 -0.4649840 1.504332 -0.001974754
363.7 1.160000 51.04402 50.00000 1.004140 0.6863948 0.1341271 0.01001561
363.9 1.358000 65.68587 70.00000 1.033710 0.2199060 -1.313016 -0.003822166
364.1 1.522000 80.00606 90.00000 1.051740 -0.5461596 -2.613923 -0.008947732
364.2 1.698000 96.33565 110.0000 1.057110 -0.3956784 1.381769 -0.002185515
364.4 1.828000 109.4190 130.0000 1.060730 -0.02515562 1.968552 0.001370461
364.5 1.932000 120.7325 150.0000 1.062800 0.1751448 0.2867956 0.003732985
364.6 2.035000 132.5277 170.0000 1.065420 0.3971580 0.09537477 0.004283351
364.7 2.130000 144.1818 190.0000 1.068420 0.3210755 -0.1430978 0.003128311
364.8 2.220000 155.8427 210.0000 1.072150 0.01631122 -0.09548346 0.0002301136
364.8 2.304000 166.9489 230.0000 1.073670 -0.1998143 -0.2246637 -0.001252924
364.9 2.384000 177.5857 250.0000 1.073570 -0.2514755 -0.2702514 -0.001700594
365.0 2.460000 187.7588 270.0000 1.072740 -0.2214035 -0.3984987 -0.001827629
365.1 2.534000 197.6519 290.0000 1.071220 -0.08726072 -0.2120387 -0.001549773
365.1 2.605000 207.1802 310.0000 1.069440 0.04698380 -0.08840821 -0.001146196
365.2 2.674000 216.4133 330.0000 1.067360 0.2002499 0.1540656 -0.0004699955
365.3 2.740000 225.3661 350.0000 1.064950 0.1809032 0.1249932 0.0006427835
365.3 2.804000 234.0916 370.0000 1.062650 0.03681708 0.03458711 0.001833492
365.4 2.867000 242.6191 390.0000 1.060450 -0.1605529 0.1307328 0.003192429
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873297">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-38.5572384554 121.976263451 -93.6562360134 54.3125354851 -6.38088873464</math:coefficients>
     <math:minRange>0.564</math:minRange>
     <math:maxRange>2.867</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873221">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-12.9210798762 74.5164033356 -48.4141695871 33.9755775577 -5.34273579079</math:coefficients>
     <math:minRange>0.564</math:minRange>
     <math:maxRange>2.867</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873581">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>0.564</math:minRange>
     <math:maxRange>0.564</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873275">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>0.564</math:minRange>
     <math:maxRange>0.564</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873535">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.12780445914 -0.440449017513 0.470966784772 -0.17736848006 0.0223086171485</math:coefficients>
     <math:minRange>0.564</math:minRange>
     <math:maxRange>2.804</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
 <resultMember>
  <QIntervallResult gml:id="QIntervallResult1294322828826225">
   <gml:description>Gelesen aus: PROF0047.3000.txt</gml:description>
   <gml:name>47.3000</gml:name>
   <station>47.3000</station>
   <profileMember xlink:href="project:/modell.gml#Profile121199382776822"/>
   <slope>0.00100</slope>
   <pointsMember>
    <WPointsObservation gml:id="WPointsObservation1294322828826250">
     <gml:description>Übernommen aus Datei: PROF0047.3000.txt</gml:description>
     <gml:name>0047.3000</gml:name>
     <om:time xmlns:om="http://www.opengis.net/om"/>
     <om:procedure xmlns:om="http://www.opengis.net/om"/>
     <om:observedProperty xmlns:om="http://www.opengis.net/om"/>
     <om:featureOfInterest xmlns:om="http://www.opengis.net/om"/>
     <om:resultDefinition xmlns:om="http://www.opengis.net/om">
      <swe:RecordDefinition gml:id="RecordDefinition129432282882653">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha"/>
      </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result xmlns:om="http://www.opengis.net/om"><![CDATA[363.1 0.6170000 17.89320 10.00000 1.000000 0.4158526 -0.1697027 0E-7
363.5 1.002000 34.99320 30.00000 1.000000 -1.761120 0.6022172 0E-7
363.8 1.279000 48.14247 50.00000 1.001670 1.058523 0.1107245 -0.001424396
364.0 1.491000 62.64274 70.00000 1.040540 1.428584 -0.5870124 0.004967532
364.2 1.661000 77.51450 90.00000 1.065930 -0.01769136 -1.734215 -0.003339137
364.3 1.836000 93.11610 110.0000 1.070350 -0.5349621 1.178391 -0.001575810
364.5 1.969000 105.3598 130.0000 1.069790 -0.5372937 1.121015 -0.001002444
364.6 2.085000 116.4522 150.0000 1.066120 -0.4569875 0.3578783 0.0009007119
364.7 2.194000 127.0880 170.0000 1.063350 -0.2118108 0.02005740 0.001348159
364.8 2.295000 137.3716 190.0000 1.061420 -0.1136015 -0.3874505 0.001054102
364.9 2.392000 147.3470 210.0000 1.060010 0.1254735 -0.3375804 0.0005589554
365.0 2.484000 157.1131 230.0000 1.059260 0.2442840 -0.2229266 -0.0001416175
365.1 2.571000 166.6282 250.0000 1.058560 0.2323131 -0.2410325 -0.0004537156
365.2 2.655000 175.9666 270.0000 1.058150 0.1939937 -0.08910184 -0.0007251064
365.2 2.736000 185.0923 290.0000 1.057640 0.1355459 0.1130970 -0.0006898375
365.3 2.813000 193.9357 310.0000 1.056840 -0.01332945 -0.01164379 -0.0003072867
365.4 2.889000 202.5392 330.0000 1.055780 0.02249611 0.2145526 0.0002054390
365.5 2.962000 210.9193 350.0000 1.054640 -0.01834870 0.1796241 0.0005021863
365.5 3.033000 219.0964 370.0000 1.053400 -0.05823989 0.06990127 0.0004135791
365.6 3.102000 227.0936 390.0000 1.052110 -0.1336804 -0.1867920 -0.0002913141
]]></om:result>
    </WPointsObservation>
   </pointsMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D129432282887344">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-29.2534468549 89.4067243392 -65.3855362781 40.415145813 -4.70291943155</math:coefficients>
     <math:minRange>0.617</math:minRange>
     <math:maxRange>3.102</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873219">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>14.4167936016 -15.3273962925 36.5498966238 -2.24001306378 -0.267280639627</math:coefficients>
     <math:minRange>0.617</math:minRange>
     <math:maxRange>3.102</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873243">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0 0.0</math:coefficients>
     <math:minRange>0.617</math:minRange>
     <math:maxRange>1.002</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873504">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-3.88979874308 13.5857321279 -12.5063355873 3.81039796294</math:coefficients>
     <math:minRange>1.002</math:minRange>
     <math:maxRange>1.279</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
   <polynomialMember>
    <math:Polynomial1D xmlns:math="org.kalypso.gml.common.math" gml:id="Polynomial1D1294322828873563">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-0.750977802509 3.07735146276 -1.91647462504 0.521443282095 -0.0525588364512</math:coefficients>
     <math:minRange>1.279</math:minRange>
     <math:maxRange>3.102</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </polynomialMember>
  </QIntervallResult>
 </resultMember>
</QIntervallResultCollection>
