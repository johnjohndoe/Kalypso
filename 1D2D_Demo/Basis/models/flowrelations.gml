<?xml version="1.0" encoding="UTF-8"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:wspm="org.kalypso.model.wspm" xmlns:swe="http://www.opengis.net/swe" xmlns:ns2="http://www.tu-harburg.de/wb/kalypso/schemata/observation" xmlns:math="org.kalypso.gml.common.math" xmlns:commonShp="org.kalypso.gml.common" xmlns:om="http://www.opengis.net/om" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777110315">
   <gml:description>Gelesen aus: PROF0044.4050.txt</gml:description>
   <gml:name>44.4050</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929971.777 775460.254 361.563</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296156696">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296156226">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296156728">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296156178">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296156755">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296156833">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222961562">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296156121">
     <gml:description>Übernommen aus Datei: PROF0044.4050.txt</gml:description>
     <gml:name>0044.4050</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296156344">
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
     <om:result><![CDATA[362.0 0.4190000 10.02600 1.000000 1.000000 -1.587489 -5.329071E-15 0E-7
365.1 3.573000 171.5450 101.0000 1.036870 19.73393 0E-7 0E-7
366.4 4.804000 294.8530 201.0000 1.067540 -9.626069 0.1304035 0.001000192
367.3 5.709000 397.5260 301.0000 1.062170 -20.84465 2.830576 -0.01233566
368.0 6.464000 490.3440 401.0000 1.058600 -19.88864 -2.756358 -0.009180584
368.7 7.127000 577.2590 501.0000 1.057170 -10.30437 -6.873770 -0.001387383
369.3 7.724000 659.7730 601.0000 1.056220 5.695955 -6.509139 0.007977453
369.8 8.270000 738.7280 701.0000 1.055440 26.42565 -0.8986430 0.01702817
370.3 8.777000 815.5330 801.0000 1.055920 50.14664 10.32272 0.02367718
370.7 9.180000 935.3840 901.0000 1.090690 15.41995 8.372353 -0.006302513
371.1 9.551000 1029.566 1001.000 1.100950 3.452830 6.481773 -0.01309953
371.5 9.892000 1116.292 1101.000 1.101780 -4.667707 3.427712 -0.01168542
371.8 10.21200 1197.889 1201.000 1.099990 -10.05729 0.3540105 -0.008680561
372.1 10.51800 1275.840 1301.000 1.097540 -13.12010 -1.571901 -0.005907507
372.4 10.81100 1350.402 1401.000 1.094410 -14.32075 -2.765313 -0.003239330
372.7 11.09100 1421.824 1501.000 1.090760 -14.30724 -3.928219 -0.0007173374
372.9 11.36100 1490.657 1601.000 1.087040 -13.20255 -4.618339 0.001313188
373.2 11.62300 1557.206 1701.000 1.083380 -11.06909 -4.592152 0.002814835
373.4 11.87700 1621.851 1801.000 1.079980 -8.527467 -4.289229 0.003689615
373.7 12.12400 1684.793 1901.000 1.076860 -5.729013 -3.723739 0.004007896
373.9 12.36500 1746.366 2001.000 1.074160 -2.931616 -2.849758 0.003713811
374.2 12.60200 1806.519 2101.000 1.071670 0.2790925 -1.132417 0.003082873
374.4 12.83300 1865.349 2201.000 1.069370 3.114136 0.2751274 0.002250779
374.6 13.05900 1922.990 2301.000 1.067250 5.563253 1.511306 0.001301768
374.8 13.28100 1979.497 2401.000 1.065290 7.704390 2.760536 0.0003268241
375.1 13.49900 2034.954 2501.000 1.063500 9.329119 3.779602 -0.0006015798
375.3 13.71200 2089.439 2601.000 1.061830 9.987981 3.860284 -0.001343155
375.5 13.92300 2143.023 2701.000 1.060290 10.27772 4.194603 -0.001863598
375.7 14.13000 2195.744 2801.000 1.058870 9.544259 3.629861 -0.002055831
375.9 14.33400 2247.658 2901.000 1.057550 7.890998 2.443585 -0.001835551
376.1 14.53500 2298.817 3001.000 1.056330 5.175390 0.4468165 -0.001130631
376.3 14.73300 2349.244 3101.000 1.055190 1.291317 -2.542439 0.0001478397
376.5 14.92600 2398.508 3200.000 1.054150 -3.856071 -6.682679 0.002032411
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156976">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-12.2847178186 31.7057704502</math:coefficients>
     <math:minRange>0.419</math:minRange>
     <math:maxRange>3.573</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156912">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>1261.12544708 -839.013125744 188.145451798 -12.3702619695</math:coefficients>
     <math:minRange>3.573</math:minRange>
     <math:maxRange>4.804</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156474">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-905.668660977 477.7081161 -82.5949560495 7.35675935379 -0.183218097608</math:coefficients>
     <math:minRange>4.804</math:minRange>
     <math:maxRange>14.926</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156167">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-23.5840191398 82.1941417748 -14.9185670272 2.79039852042 -0.0959808894142</math:coefficients>
     <math:minRange>0.419</math:minRange>
     <math:maxRange>14.926</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171492">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.926</math:minRange>
     <math:maxRange>14.926</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171849">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.926</math:minRange>
     <math:maxRange>14.926</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229617188">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.99510192454 0.011689917565</math:coefficients>
     <math:minRange>0.419</math:minRange>
     <math:maxRange>3.573</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222961717">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>4.17770212865 -2.40196880039 0.603165137462 -0.0495198307556</math:coefficients>
     <math:minRange>3.573</math:minRange>
     <math:maxRange>4.804</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229617160">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>2.03369105232 -0.45678357413 0.0748729177835 -0.00508592298787 1.22337935292E-4</math:coefficients>
     <math:minRange>4.804</math:minRange>
     <math:maxRange>14.926</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>44.4050</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775051245"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777110436">
   <gml:description>Gelesen aus: PROF0044.4250.txt</gml:description>
   <gml:name>44.4250</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929976.168 775469.786 361.601</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296234946">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation12225222962341064">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296234842">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296234254">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions12225222962341082">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296234991">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222962341176">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296234716">
     <gml:description>Übernommen aus Datei: PROF0044.4250.txt</gml:description>
     <gml:name>0044.4250</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296234900">
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
     <om:result><![CDATA[362.0 0.4130000 9.874000 1.000000 1.000000 -8.881784E-15 7.105427E-15 -4.218847E-15
365.2 3.633000 174.8090 101.0000 1.059060 0E-7 0E-7 1.754152E-14
366.4 4.815000 304.3660 201.0000 1.093920 0.8514946 0.1444885 -1.554312E-15
367.3 5.714000 413.0590 301.0000 1.099370 -0.9669325 0.4161658 0.001440789
368.0 6.375000 548.1360 401.0000 1.149830 -24.82967 -4.601018 -0.02453957
368.6 6.985000 657.0260 501.0000 1.162830 -12.33504 -2.643121 -0.009631204
369.1 7.540000 758.9120 601.0000 1.173210 8.250900 0.9758640 0.008012362
369.7 8.054000 855.5740 701.0000 1.182430 32.72515 5.393976 0.02575654
370.1 8.535000 948.1750 801.0000 1.190670 58.40572 9.904222 0.04278069
370.5 8.913000 1127.949 901.0000 1.277160 -26.03270 -3.667607 -0.02427140
370.9 9.334000 1231.861 1001.000 1.293000 -22.11134 -3.233309 -0.01925036
371.3 9.739000 1331.856 1101.000 1.306710 -17.39455 -2.702420 -0.01395197
371.7 10.13100 1428.617 1201.000 1.319160 -12.43731 -1.948596 -0.009202841
372.1 10.51100 1522.389 1301.000 1.330260 -7.675970 -1.208037 -0.004899186
372.5 10.88000 1613.612 1401.000 1.340500 -3.596185 -0.6283471 -0.001466179
372.8 11.24000 1702.433 1501.000 1.349790 -0.02373987 0.001246688 0.001318154
373.2 11.59100 1789.151 1601.000 1.358370 2.632225 0.4031448 0.003299063
373.5 11.93400 1873.926 1701.000 1.366350 4.424811 0.6248192 0.004506070
373.9 12.26600 1955.904 1801.000 1.372250 5.474095 -0.4407519 0.006479266
374.2 12.60100 2038.425 1901.000 1.380970 5.973080 1.262064 0.004760251
374.5 12.92400 2118.302 2001.000 1.387570 5.465248 1.070056 0.004104910
374.8 13.24200 2196.784 2101.000 1.393790 4.552460 0.9854355 0.003073818
375.2 13.55500 2273.957 2201.000 1.399670 3.283540 0.9156599 0.001781760
375.5 13.85600 2348.317 2301.000 1.403110 1.630003 -1.508277 0.002399264
375.8 14.16600 2425.021 2401.000 1.410710 -0.3262283 0.4990790 -0.001233225
376.1 14.46400 2498.502 2501.000 1.415820 -1.898893 0.01222803 -0.002571256
376.4 14.75900 2571.197 2601.000 1.420670 -3.156946 -0.05979063 -0.003557985
376.6 15.04900 2642.853 2701.000 1.425310 -4.126681 -0.4341902 -0.004098634
376.9 15.33600 2713.672 2801.000 1.429760 -4.310223 -0.4588175 -0.004013136
377.2 15.61900 2783.619 2901.000 1.434020 -3.698599 -0.5056152 -0.003144128
377.5 15.89900 2852.720 3001.000 1.438110 -1.843938 -0.2413641 -0.001320669
377.8 16.17500 2921.015 3101.000 1.442040 1.186078 -0.04112945 0.001596511
378.0 16.44600 2987.895 3200.000 1.445790 5.949698 0.7025223 0.005756776
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234852">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-11.8260869565 31.0559006211</math:coefficients>
     <math:minRange>0.413</math:minRange>
     <math:maxRange>3.633</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501237">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>2720.22371149 -1915.9042086 450.837409553 -33.5595075287</math:coefficients>
     <math:minRange>3.633</math:minRange>
     <math:maxRange>4.815</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250798">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>445.839020607 -226.949446818 44.3481451025 -1.76877561585 0.0322631605081</math:coefficients>
     <math:minRange>4.815</math:minRange>
     <math:maxRange>16.446</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250386">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-11.2807065217 51.2220496894</math:coefficients>
     <math:minRange>0.413</math:minRange>
     <math:maxRange>3.633</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250699">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>4481.35431579 -3217.99140222 778.577317385 -60.3072354689</math:coefficients>
     <math:minRange>3.633</math:minRange>
     <math:maxRange>4.815</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229625024">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>1379.52675262 -657.786713314 119.791532833 -6.80978928214 0.141115235523</math:coefficients>
     <math:minRange>4.815</math:minRange>
     <math:maxRange>16.446</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250362">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.99622567179 0.00809263285771 0.00253311469331</math:coefficients>
     <math:minRange>0.413</math:minRange>
     <math:maxRange>4.815</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250213">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-7.61046273069 4.96698206929 -0.943542937683 0.0596932173773</math:coefficients>
     <math:minRange>4.815</math:minRange>
     <math:maxRange>5.714</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250806">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.69056128803 -0.32948869366 0.0584709331301 -0.0037878712035 8.49454678014E-5</math:coefficients>
     <math:minRange>5.714</math:minRange>
     <math:maxRange>16.446</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>44.4250</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775084134"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777110624">
   <gml:description>Gelesen aus: PROF0044.5000.txt</gml:description>
   <gml:name>44.5000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930019.466 775539.966 361.792</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296250940">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296250849">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition12225222962501105">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296250321">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296250744">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296250148">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222962501266">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296250799">
     <gml:description>Übernommen aus Datei: PROF0044.5000.txt</gml:description>
     <gml:name>0044.5000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296250605">
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
     <om:result><![CDATA[362.2 0.3700000 9.514000 1.000000 1.000000 0E-7 -1.776357E-15 0E-7
365.6 3.808000 168.4960 101.0000 1.083860 0E-7 0E-7 0E-7
366.3 4.525000 353.4410 201.0000 1.159080 0.9762198 0.6344622 0.0003888208
366.9 5.126000 492.8770 301.0000 1.131630 -16.32509 -8.212686 -0.007713787
367.4 5.609000 610.9900 401.0000 1.104440 -11.75957 -6.928872 -0.002393878
367.8 6.027000 714.2620 501.0000 1.082250 4.120891 -0.7842622 0.004737322
368.2 6.407000 808.7340 601.0000 1.067310 25.64380 9.568503 0.008520781
368.6 6.758000 896.6170 701.0000 1.056600 49.56319 22.51909 0.01067620
368.8 6.964000 1037.869 801.0000 1.071400 -24.57416 -6.497276 -0.008490332
369.0 7.241000 1124.973 901.0000 1.064180 -20.21454 -5.797350 -0.006499169
369.3 7.504000 1208.028 1001.000 1.057830 -15.57320 -4.804972 -0.004533890
369.5 7.755000 1287.538 1101.000 1.052170 -10.97347 -3.700345 -0.002620209
369.8 7.995000 1363.701 1201.000 1.047340 -6.597041 -2.847625 -0.001039775
370.0 8.226000 1437.183 1301.000 1.043210 -2.641180 -2.023292 0.0002173791
370.2 8.449000 1508.288 1401.000 1.039670 0.8038050 -1.281136 0.001181133
370.5 8.666000 1577.305 1501.000 1.036660 4.051196 -0.1006334 0.001839417
370.7 8.876000 1644.856 1601.000 1.034240 6.122402 0.7207996 0.002106564
370.9 9.079000 1711.724 1701.000 1.032640 6.247019 0.8570476 0.001724351
371.1 9.278000 1777.716 1801.000 1.031530 5.639436 1.542117 0.0009746600
371.3 9.471000 1842.076 1901.000 1.030390 4.446725 1.553769 0.0003835912
371.5 9.660000 1904.779 2001.000 1.029080 3.411119 1.730996 0.00006497902
371.6 9.843000 1965.998 2101.000 1.027680 1.778943 0.8285589 -0.00004864773
371.8 10.02400 2025.929 2201.000 1.026350 0.7341517 0.8708661 -0.0001512879
372.0 10.20000 2084.773 2301.000 1.025180 -0.8185310 0.08254321 -0.0003074398
372.2 10.37400 2142.509 2401.000 1.024070 -1.795004 0.04318052 -0.0004361584
372.3 10.54400 2199.220 2501.000 1.023020 -2.841427 -0.4862776 -0.0005177189
372.5 10.71200 2254.957 2601.000 1.022030 -3.262327 -0.4216022 -0.0005587757
372.7 10.87600 2309.777 2701.000 1.021100 -3.682423 -1.036276 -0.0005396847
372.8 11.03900 2363.767 2801.000 1.020220 -3.098432 -0.5759516 -0.0004581209
373.0 11.19900 2416.954 2901.000 1.019390 -2.107857 -0.3189822 -0.0002938261
373.1 11.35600 2469.380 3001.000 1.018610 -0.6661511 -0.3397065 -0.00003820004
373.3 11.51100 2521.094 3101.000 1.017880 1.610689 -0.05839054 0.0003152605
373.5 11.66300 2571.609 3200.000 1.017190 4.947270 0.8224663 0.0007886977
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229625074">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-9.7620709715 29.0866783013</math:coefficients>
     <math:minRange>0.37</math:minRange>
     <math:maxRange>3.808</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250260">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>19370.0975875 -13934.9428527 3324.66189138 -261.055604083</math:coefficients>
     <math:minRange>3.808</math:minRange>
     <math:maxRange>4.525</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229625072">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>1265.54272783 -687.865200797 125.450518366 -6.33046796166 0.158701509515</math:coefficients>
     <math:minRange>4.525</math:minRange>
     <math:maxRange>11.663</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250544">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-7.5957556719 46.242582897</math:coefficients>
     <math:minRange>0.37</math:minRange>
     <math:maxRange>3.808</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250539">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>43003.4454406 -31096.5190217 7458.1865461 -589.822030156</math:coefficients>
     <math:minRange>3.808</math:minRange>
     <math:maxRange>4.525</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229625075">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>2041.32854022 -1226.77155345 269.404104492 -20.4208621016 0.572569367473</math:coefficients>
     <math:minRange>4.525</math:minRange>
     <math:maxRange>11.663</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250486">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.990974927283 0.0243920884235</math:coefficients>
     <math:minRange>0.37</math:minRange>
     <math:maxRange>3.808</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250989">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>35.1208783989 -25.0812041888 6.12477372604 -0.49515855102</math:coefficients>
     <math:minRange>3.808</math:minRange>
     <math:maxRange>4.525</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250590">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>2.02168888105 -0.372611950406 0.0554479992896 -0.00381466422613 1.00067972968E-4</math:coefficients>
     <math:minRange>4.525</math:minRange>
     <math:maxRange>11.663</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>44.5000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile121268077511736"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777110477">
   <gml:description>Gelesen aus: PROF0044.6000.txt</gml:description>
   <gml:name>44.6000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930053.367 775633.404 360.74</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState12225222962651033">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation12225222962651300">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296265885">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296265769">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296265737">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296265216">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222962651130">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296265817">
     <gml:description>Übernommen aus Datei: PROF0044.6000.txt</gml:description>
     <gml:name>0044.6000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296265596">
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
     <om:result><![CDATA[361.2 0.4560000 8.509000 1.000000 1.000000 -1.776357E-14 1.776357E-15 0E-7
364.8 4.104000 145.3430 101.0000 1.005370 0E-7 0E-7 0E-7
366.4 5.622000 321.8040 201.0000 1.073900 -0.5787439 0.03451771 -0.00007556962
367.2 6.438000 436.0840 301.0000 1.075190 8.446595 -0.6730978 0.006097078
367.8 7.087000 551.9920 401.0000 1.089070 1.903569 0.5349471 -0.006920175
368.4 7.632000 653.4070 501.0000 1.083710 -1.681653 -0.001185446 -0.003275990
368.9 8.119000 745.3860 601.0000 1.077720 -2.613519 -0.4061297 -0.0001226037
369.3 8.567000 831.4170 701.0000 1.072910 -2.517610 -0.1830688 0.001365099
369.7 8.983000 912.0540 801.0000 1.068420 -1.601724 0.01515942 0.002413926
370.1 9.373000 988.7370 901.0000 1.065030 -0.7784969 -0.005642669 0.002446098
370.5 9.744000 1062.468 1001.000 1.062640 -0.08131398 0.3704792 0.001653366
370.8 10.09700 1133.316 1101.000 1.060560 0.3352474 0.5115743 0.0008091705
371.2 10.43400 1201.635 1201.000 1.058580 0.3195028 0.2182914 0.0001590377
371.5 10.75900 1267.544 1301.000 1.056790 0.4260190 0.07552185 -0.0003910536
371.8 11.07300 1331.242 1401.000 1.055080 0.5658923 -0.08022345 -0.0007271754
372.1 11.37800 1392.966 1501.000 1.053520 0.8461049 -0.004778776 -0.0009329809
372.4 11.67300 1452.962 1601.000 1.052140 0.7777122 -0.3770565 -0.001043356
372.7 11.96200 1511.400 1701.000 1.050900 0.9804921 -0.1337645 -0.001054244
373.0 12.24300 1568.432 1801.000 1.049790 0.8869065 -0.2120632 -0.0009669090
373.3 12.51800 1624.172 1901.000 1.048810 0.7892534 -0.1309051 -0.0008134349
373.5 12.78700 1678.735 2001.000 1.047930 0.5812945 -0.08330258 -0.0005903378
373.8 13.05000 1732.196 2101.000 1.047150 0.2035614 -0.2521138 -0.0003268373
374.0 13.30900 1784.648 2201.000 1.046460 -0.01297566 -0.02951430 -0.00004673130
374.3 13.56300 1836.154 2301.000 1.045840 -0.3085719 0.06442928 0.0002406886
374.6 13.81200 1886.744 2401.000 1.045280 -0.6853184 -0.1075616 0.0005131012
374.8 14.05800 1936.540 2501.000 1.044790 -0.8311772 0.1453614 0.0007256481
375.0 14.30000 1985.553 2601.000 1.044340 -0.9257784 0.3193731 0.0008763981
375.3 14.53800 2033.834 2701.000 1.043950 -0.9856738 0.3128504 0.0009121578
375.5 14.77200 2081.409 2801.000 1.043590 -1.001610 0.02861153 0.0008300863
375.7 15.00400 2128.329 2901.000 1.043280 -0.5800707 0.2433090 0.0005725056
376.0 15.23200 2174.628 3001.000 1.042990 -0.1245993 0.02056995 0.0001407094
376.2 15.45700 2220.354 3101.000 1.042740 0.5592125 -0.2799936 -0.0005205578
376.4 15.67800 2265.070 3200.000 1.042510 1.738681 -0.1762178 -0.001418126
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962651132">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-11.5 27.4122807018</math:coefficients>
     <math:minRange>0.456</math:minRange>
     <math:maxRange>4.104</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265270">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>517.841542313 -248.750869708 40.2974485755 -1.08054590353</math:coefficients>
     <math:minRange>4.104</math:minRange>
     <math:maxRange>5.622</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265341">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>526.799580111 -240.118115787 36.6550002422 -0.805799811722 0.00882322284921</math:coefficients>
     <math:minRange>5.622</math:minRange>
     <math:maxRange>15.678</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962651124">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-8.59525 37.5093201754</math:coefficients>
     <math:minRange>0.456</math:minRange>
     <math:maxRange>4.104</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229626557">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>3101.89158824 -1876.9537849 378.94213484 -23.6677354049</math:coefficients>
     <math:minRange>4.104</math:minRange>
     <math:maxRange>5.622</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229626588">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>360.072736725 -224.930988612 53.3764105967 -2.93266770383 0.0598298084124</math:coefficients>
     <math:minRange>5.622</math:minRange>
     <math:maxRange>15.678</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962651093">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.99932875 0.00147203947368</math:coefficients>
     <math:minRange>0.456</math:minRange>
     <math:maxRange>4.104</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962651154">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>4.53157883468 -2.26209489166 0.473947296981 -0.0321916177295</math:coefficients>
     <math:minRange>4.104</math:minRange>
     <math:maxRange>5.622</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265656">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.548239874441 0.217120529196 -0.0312111134388 0.00186624834974 -4.02424072601E-5</math:coefficients>
     <math:minRange>5.622</math:minRange>
     <math:maxRange>15.678</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>44.6000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775134182"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777110688">
   <gml:description>Gelesen aus: PROF0044.7000.txt</gml:description>
   <gml:name>44.7000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930078.452 775730.197 359.728</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296203446">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296203496">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296203776">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296203473">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions12225222962031070">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296203911">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296203436">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296203305">
     <gml:description>Übernommen aus Datei: PROF0044.7000.txt</gml:description>
     <gml:name>0044.7000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296203198">
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
     <om:result><![CDATA[360.2 0.4880000 7.415000 1.000000 1.000000 -7.993606E-15 0E-7 0E-7
364.4 4.664000 141.5360 101.0000 1.002230 0E-7 0E-7 0E-7
366.1 6.387000 274.4130 201.0000 1.110740 -0.4182112 -0.1098778 0.001774513
367.1 7.385000 412.9120 301.0000 1.169020 7.390015 1.994786 -0.02431132
367.8 8.080000 533.5180 401.0000 1.158830 0.4932407 0.2061590 -0.01078513
368.4 8.668000 637.8650 501.0000 1.141310 -1.885684 -0.3976154 0.001496582
368.9 9.187000 731.7020 601.0000 1.125770 -2.291700 -0.8549952 0.008420217
369.4 9.661000 818.7150 701.0000 1.114300 -1.781581 -0.6815353 0.01007890
369.8 10.09800 900.0670 801.0000 1.105250 -1.007168 -0.7949734 0.009312724
370.2 10.51100 977.8910 901.0000 1.098360 -0.2538284 0.01372139 0.006849843
370.6 10.89900 1052.062 1001.000 1.092920 0.02461197 0.2080735 0.003852782
371.0 11.26900 1123.225 1101.000 1.088470 0.2582103 0.5556928 0.0008618367
371.4 11.62200 1191.618 1201.000 1.084660 0.2221169 0.5376166 -0.001681778
371.7 11.96100 1257.272 1301.000 1.081060 0.3422386 0.3514830 -0.003372291
372.0 12.28800 1320.722 1401.000 1.078010 0.3880998 0.06837103 -0.004599216
372.3 12.60600 1382.203 1501.000 1.075350 0.6517658 0.1998101 -0.005287553
372.6 12.91400 1441.892 1601.000 1.073010 0.7268324 0.08489691 -0.005428575
372.9 13.21400 1499.993 1701.000 1.070980 0.7775261 0.06244903 -0.005121153
373.2 13.50600 1556.656 1801.000 1.069190 0.6456773 -0.1313383 -0.004398738
373.5 13.79200 1611.973 1901.000 1.067620 0.6271053 -0.03615632 -0.003359049
373.8 14.07100 1666.100 2001.000 1.066230 0.3801984 -0.2003002 -0.002077129
374.1 14.34500 1719.110 2101.000 1.065010 0.2315404 -0.08430198 -0.0006653400
374.3 14.61300 1771.104 2201.000 1.063930 -0.09508685 -0.2091539 0.0007816749
374.6 14.87700 1822.088 2301.000 1.062970 -0.1987916 0.03321690 0.002160417
374.9 15.13500 1872.195 2401.000 1.062120 -0.5739386 -0.2468210 0.003350931
375.1 15.39000 1921.459 2501.000 1.061350 -0.6497504 -0.004042911 0.004259305
375.4 15.64000 1969.924 2601.000 1.060670 -0.8277027 -0.1237661 0.004746658
375.6 15.88700 2017.692 2701.000 1.060070 -0.7936645 0.09817730 0.004694006
375.9 16.13000 2064.767 2801.000 1.059520 -0.7138733 0.1745720 0.004005869
376.1 16.36900 2111.133 2901.000 1.059030 -0.5412427 0.01846328 0.002550370
376.3 16.60500 2156.885 3001.000 1.058590 -0.1438093 -0.02705810 0.0002065361
376.6 16.83800 2202.034 3101.000 1.058200 0.5010329 -0.02512183 -0.003150811
376.8 17.06600 2246.167 3200.000 1.057850 1.443299 0.08871354 -0.007586670
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203211">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-10.6858237548 23.9463601533</math:coefficients>
     <math:minRange>0.488</math:minRange>
     <math:maxRange>4.664</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203207">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>1064.3434841 -538.096546832 92.7530896477 -4.64547974438</math:coefficients>
     <math:minRange>4.664</math:minRange>
     <math:maxRange>6.387</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203881">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>916.398314583 -337.463203429 41.3560785808 -1.0346507618 0.013446666995</math:coefficients>
     <math:minRange>6.387</math:minRange>
     <math:maxRange>17.066</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203622">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-8.25814367816 32.1170977011</math:coefficients>
     <math:minRange>0.488</math:minRange>
     <math:maxRange>4.664</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229620395">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>-50.7527800062 145.468177768 -42.7462608141 4.37315014374</math:coefficients>
     <math:minRange>4.664</math:minRange>
     <math:maxRange>6.387</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203457">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>361.196850289 -237.319997948 48.6480667302 -2.4195337889 0.0447271240597</math:coefficients>
     <math:minRange>6.387</math:minRange>
     <math:maxRange>17.066</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203389">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.99973940613 5.34003831418E-4</math:coefficients>
     <math:minRange>0.488</math:minRange>
     <math:maxRange>4.664</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203637">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>5.28535349993 -2.38273214656 0.430943280813 -0.025078235913</math:coefficients>
     <math:minRange>4.664</math:minRange>
     <math:maxRange>6.387</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962031056">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-1.19968284823 0.861251888225 -0.113095229797 0.00627442633587 -1.26094783881E-4</math:coefficients>
     <math:minRange>6.387</math:minRange>
     <math:maxRange>17.066</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>44.7000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775150724"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777110188">
   <gml:description>Gelesen aus: PROF0044.8000.txt</gml:description>
   <gml:name>44.8000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930112.938 775823.897 359.599</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296234755">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296234135">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition12225222962341135">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter122252229623418">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions12225222962341158">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval12225222962341174">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222962341251">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296234169">
     <gml:description>Übernommen aus Datei: PROF0044.8000.txt</gml:description>
     <gml:name>0044.8000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296234820">
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
     <om:result><![CDATA[360.0 0.4360000 7.488000 1.000000 1.000000 -8.881784E-16 -7.105427E-15 2.886580E-15
364.2 4.617000 147.5550 101.0000 1.000000 0E-7 0E-7 2.635669E-13
366.0 6.356000 274.2460 201.0000 1.081250 -0.6874311 0.002131852 -1.193934E-12
366.9 7.300000 396.0660 301.0000 1.107810 10.51695 -0.6323083 1.581402E-12
367.6 7.989000 515.1690 401.0000 1.118250 2.671906 0.9889641 -8.215650E-14
368.1 8.548000 616.2270 501.0000 1.102090 -1.600484 0.2410684 0.00009449973
368.6 9.043000 707.3550 601.0000 1.087700 -3.324251 0.01164702 -0.001146456
369.1 9.492000 790.9220 701.0000 1.074840 -3.516296 -0.2883574 -0.0004392283
369.5 9.908000 868.9960 801.0000 1.064500 -2.901008 -0.4325784 0.0001638577
369.9 10.29900 942.7960 901.0000 1.056270 -1.826671 -0.2671503 0.0004282593
370.3 10.66900 1013.163 1001.000 1.049590 -0.7807261 -0.04946913 0.0005124844
370.6 11.02000 1080.468 1101.000 1.044240 -0.02366960 -0.2563786 0.0003596765
371.0 11.35800 1145.444 1201.000 1.039900 0.6936160 0.05237831 0.00001921056
371.3 11.68300 1208.164 1301.000 1.036120 1.178873 0.2880412 -0.0001912993
371.6 11.99600 1268.710 1401.000 1.032680 1.467212 0.2358251 -0.0001735914
371.9 12.29900 1327.401 1501.000 1.029720 1.582095 0.09986604 -0.0001763684
372.2 12.59400 1384.445 1601.000 1.027110 1.679135 0.1963803 -0.0001564767
372.5 12.88000 1439.947 1701.000 1.024800 1.452207 -0.1185448 -0.0001109427
372.8 13.16000 1494.091 1801.000 1.022750 1.301950 -0.05730549 -0.00006882364
373.0 13.43400 1546.978 1901.000 1.020930 1.140804 0.1768880 -0.00003565054
373.3 13.70100 1598.719 2001.000 1.019300 0.6875870 0.01493948 0.000003889814
373.6 13.96300 1649.403 2101.000 1.017840 0.2655225 0.02488326 0.00003211315
373.8 14.22000 1699.105 2201.000 1.016530 -0.1682355 0.05984490 0.00004858669
374.1 14.47200 1747.900 2301.000 1.015340 -0.6527018 -0.01918742 0.00006607350
374.3 14.72000 1795.839 2401.000 1.014270 -1.008278 0.06307109 0.00006600270
374.6 14.96300 1842.985 2501.000 1.013300 -1.449023 -0.2119389 0.00006117371
374.8 15.20300 1889.369 2601.000 1.012420 -1.578646 -0.1229821 0.00004452732
375.0 15.43900 1935.057 2701.000 1.011620 -1.611215 -0.1720692 0.00002293857
375.3 15.67200 1980.078 2801.000 1.010880 -1.336654 -0.01685096 0.000007560308
375.5 15.90100 2024.468 2901.000 1.010210 -0.9396696 -0.1646466 -0.00001239355
375.7 16.12800 2068.266 3001.000 1.009590 -0.02036645 0.1914859 -0.00002548214
375.9 16.35100 2111.480 3101.000 1.009020 1.066606 0.1051136 -0.00002764997
376.2 16.56900 2153.738 3200.000 1.008510 2.532881 0.04231615 -0.00002798982
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234579">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-9.42812724229 23.9177230328</math:coefficients>
     <math:minRange>0.436</math:minRange>
     <math:maxRange>4.617</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234371">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>920.278231168 -460.309732551 78.9164419312 -3.82309168197</math:coefficients>
     <math:minRange>4.617</math:minRange>
     <math:maxRange>6.356</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962341211">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>1096.24871472 -408.299313384 50.3065699431 -1.42414223251 0.0203823861125</math:coefficients>
     <math:minRange>6.356</math:minRange>
     <math:maxRange>16.569</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234901">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-7.11836498445 33.5008371203</math:coefficients>
     <math:minRange>0.436</math:minRange>
     <math:maxRange>4.617</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234723">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>-205.96710385 226.054452962 -55.4257686118 4.9921438162</math:coefficients>
     <math:minRange>4.617</math:minRange>
     <math:maxRange>6.356</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962341225">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>731.993317537 -379.834579947 68.3898332033 -3.58634967218 0.0697363067162</math:coefficients>
     <math:minRange>6.356</math:minRange>
     <math:maxRange>16.569</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234902">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.06645969504 -0.177925241567 0.0613729702951 -0.00675569171396 2.45690535858E-4</math:coefficients>
     <math:minRange>0.436</math:minRange>
     <math:maxRange>7.989</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962341172">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-62.0867493261 22.6762849114 -2.7072631647 0.107538398282</math:coefficients>
     <math:minRange>7.989</math:minRange>
     <math:maxRange>8.548</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234123">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>2.14692633842 -0.26171699685 0.0234401765229 -9.62438064565E-4 1.51355286344E-5</math:coefficients>
     <math:minRange>8.548</math:minRange>
     <math:maxRange>16.569</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>44.8000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775166780"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777110398">
   <gml:description>Gelesen aus: PROF0044.9000.txt</gml:description>
   <gml:name>44.9000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930150.319 775915.808 359.986</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296093275">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296093255">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296093252">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296093600">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296093862">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval122252229609392">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296093368">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296093380">
     <gml:description>Übernommen aus Datei: PROF0044.9000.txt</gml:description>
     <gml:name>0044.9000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296093682">
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
     <om:result><![CDATA[360.5 0.5030000 7.901000 1.000000 1.000000 -7.105427E-15 -7.105427E-15 0E-7
364.4 4.453000 155.7060 101.0000 1.001220 0E-7 0E-7 0E-7
365.9 5.906000 288.3160 201.0000 1.081130 -0.4328647 -0.02038133 0.0007272742
366.8 6.821000 425.8980 301.0000 1.143720 6.822814 0.3702800 -0.004698989
367.5 7.542000 556.1810 401.0000 1.177210 0.6632803 0.4198158 -0.009922050
368.1 8.157000 669.7870 501.0000 1.184750 -1.592028 -0.3268568 -0.002176529
368.7 8.710000 773.7820 601.0000 1.188490 -2.033046 -0.5744107 0.002553418
369.2 9.218000 870.7520 701.0000 1.190740 -1.596258 -0.4840272 0.004772611
369.7 9.691000 962.2410 801.0000 1.192220 -0.8387894 -0.2493922 0.005361865
370.1 10.13600 1049.406 901.0000 1.193730 -0.1542108 0.05904843 0.004502515
370.5 10.55900 1133.204 1001.000 1.195500 0.2834500 0.6502326 0.002587760
370.9 10.96000 1213.592 1101.000 1.196530 0.2354528 0.5958388 0.001028001
371.3 11.34400 1290.917 1201.000 1.197110 0.1574228 0.3767127 -0.0002011449
371.7 11.71400 1365.561 1301.000 1.197570 0.1343175 0.1921120 -0.001259783
372.1 12.07200 1437.720 1401.000 1.197870 0.2778507 0.1159270 -0.002000383
372.4 12.41800 1507.667 1501.000 1.198110 0.2481120 -0.2579677 -0.002459095
372.7 12.75600 1575.677 1601.000 1.198360 0.5332029 -0.1068095 -0.002676987
373.1 13.08400 1641.937 1701.000 1.198620 0.5132719 -0.3071458 -0.002648279
373.4 13.40500 1706.615 1801.000 1.198910 0.6100844 -0.2024558 -0.002404472
373.7 13.71800 1769.852 1901.000 1.199220 0.4746384 -0.3239360 -0.001965619
374.0 14.02500 1831.754 2001.000 1.199560 0.4058395 -0.2306275 -0.001377261
374.3 14.32600 1892.431 2101.000 1.199930 0.3014355 -0.08920337 -0.0006880271
374.6 14.62100 1951.974 2201.000 1.200310 0.08351863 -0.05808817 0.00006543768
374.9 14.91100 2010.458 2301.000 1.200710 -0.1046169 0.06037058 0.0008146905
375.2 15.19600 2067.946 2401.000 1.201120 -0.3054369 0.1404284 0.001502957
375.5 15.47600 2124.510 2501.000 1.201550 -0.5676569 0.06210084 0.002050045
375.7 15.75200 2180.207 2601.000 1.201990 -0.7216256 0.07595683 0.002396254
376.0 16.02400 2235.071 2701.000 1.202430 -0.7737430 0.08839763 0.002475585
376.3 16.29300 2289.172 2801.000 1.202880 -0.5628445 0.3851108 0.002200649
376.5 16.55700 2342.536 2901.000 1.203330 -0.4886623 0.1343063 0.001502235
376.8 16.81800 2395.203 3001.000 1.203790 -0.1556061 0.008971945 0.0002906285
377.1 17.07600 2447.205 3101.000 1.204250 0.4374530 -0.05601146 -0.001507320
377.3 17.32800 2498.075 3200.000 1.204700 1.175298 -0.3056278 -0.003936905
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229609358">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-11.7341772152 25.3164556962</math:coefficients>
     <math:minRange>0.503</math:minRange>
     <math:maxRange>4.453</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229609310">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>1657.53818967 -906.553881453 165.988472964 -9.18549000229</math:coefficients>
     <math:minRange>4.453</math:minRange>
     <math:maxRange>5.906</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296093115">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>389.416570595 -171.829816103 26.3920481516 -0.481260056923 0.00407324216255</math:coefficients>
     <math:minRange>5.906</math:minRange>
     <math:maxRange>17.328</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296093102">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-10.9207506329 37.4189873418</math:coefficients>
     <math:minRange>0.503</math:minRange>
     <math:maxRange>4.453</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296093653">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>458.298823002 -129.034590561 3.77104522413 2.23355100611</math:coefficients>
     <math:minRange>4.453</math:minRange>
     <math:maxRange>5.906</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296093314">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>14.489450412 -102.450266272 34.0088196457 -1.67517213196 0.0306613033391</math:coefficients>
     <math:minRange>5.906</math:minRange>
     <math:maxRange>17.328</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296093129">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.999844643038 3.08860759494E-4</math:coefficients>
     <math:minRange>0.503</math:minRange>
     <math:maxRange>4.453</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296093741">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>3.56478767361 -1.45846808324 0.267133115169 -0.0154706218101</math:coefficients>
     <math:minRange>4.453</math:minRange>
     <math:maxRange>5.906</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296093806">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-0.492026939383 0.546789925958 -0.065290335855 0.00340508422092 -6.53783969908E-5</math:coefficients>
     <math:minRange>5.906</math:minRange>
     <math:maxRange>17.328</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>44.9000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775166107"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777110276">
   <gml:description>Gelesen aus: PROF0045.0000.txt</gml:description>
   <gml:name>45.0000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930202.681 776001.955 359.854</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296265504">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296265910">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition122252229626599">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296265150">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296265993">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval122252229626593">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222962651108">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation12225222962651276">
     <gml:description>Übernommen aus Datei: PROF0045.0000.txt</gml:description>
     <gml:name>0045.0000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296265854">
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
     <om:result><![CDATA[360.7 0.8540000 12.63400 1.000000 1.000000 -3.552714E-15 0E-7 0E-7
365.0 5.165000 170.6360 101.0000 1.125030 0E-7 0E-7 0E-7
366.2 6.342000 320.8420 201.0000 1.129930 -0.03079415 -0.07679042 0.0006455984
367.0 7.101000 448.5940 301.0000 1.109090 0.7063241 1.054916 -0.009185556
367.6 7.701000 554.7000 401.0000 1.081960 -0.2224291 0.2410245 -0.002020310
368.1 8.224000 648.6120 501.0000 1.063750 -0.2921133 -0.1419135 0.001563788
368.6 8.696000 734.5740 601.0000 1.051550 -0.1026642 -0.1635259 0.002580178
369.0 9.129000 814.5730 701.0000 1.042730 -0.03572168 -0.3566835 0.002672690
369.4 9.534000 890.2060 801.0000 1.036600 -0.02154323 -0.2379504 0.001848938
369.8 9.915000 961.9910 901.0000 1.031720 -0.07567902 -0.2702812 0.001159210
370.1 10.27700 1030.503 1001.000 1.027830 -0.005451113 -0.2467499 0.0005507618
370.5 10.62400 1096.347 1101.000 1.024840 0.2187735 0.1172782 -0.0001135712
370.8 10.95600 1159.828 1201.000 1.022390 0.1989095 0.1471366 -0.0006161694
371.1 11.27600 1221.184 1301.000 1.020250 0.1988594 0.1782220 -0.0008704685
371.4 11.58500 1280.705 1401.000 1.018500 0.06636601 0.06350979 -0.001057585
371.7 11.88500 1338.617 1501.000 1.017060 -0.08164628 0.06840948 -0.001187045
372.0 12.17700 1394.925 1601.000 1.015770 -0.08942595 0.2142863 -0.001167524
372.3 12.46100 1449.730 1701.000 1.014570 -0.08295089 0.2225813 -0.0009938682
372.6 12.73800 1503.204 1801.000 1.013490 -0.06065255 0.1967664 -0.0007471826
372.9 13.00800 1555.475 1901.000 1.012540 -0.1653881 -0.08461747 -0.0004785387
373.1 13.27300 1606.627 2001.000 1.011690 -0.1058440 -0.06789208 -0.0001980933
373.4 13.53300 1656.740 2101.000 1.010930 0.02986063 0.09525544 0.00007388865
373.6 13.78700 1705.894 2201.000 1.010240 -0.03727874 -0.1359470 0.0003319187
373.9 14.03700 1754.158 2301.000 1.009620 0.006532180 -0.1130223 0.0005484933
374.1 14.28300 1801.572 2401.000 1.009060 0.1192112 0.05478977 0.0007123560
374.4 14.52400 1848.221 2501.000 1.008560 0.02196292 -0.1544040 0.0008061436
374.6 14.76200 1894.116 2601.000 1.008110 0.09067753 -0.01220307 0.0008194689
374.9 14.99600 1939.314 2701.000 1.007690 0.07677914 -0.02494556 0.0007588186
375.1 15.22600 1983.871 2801.000 1.007320 -0.07326772 -0.2843240 0.0005908742
375.3 15.45400 2027.790 2901.000 1.006980 0.02683570 0.007567642 0.0003182889
375.5 15.67800 2071.127 3001.000 1.006680 -0.06126005 -0.09562562 -0.00007486071
375.8 15.90000 2113.891 3101.000 1.006400 0.04394664 0.2337834 -0.0005849074
376.0 16.11600 2155.702 3200.000 1.006140 -0.04536939 0.1088818 -0.001204925
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265815">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-18.8097889121 23.1964741359</math:coefficients>
     <math:minRange>0.854</math:minRange>
     <math:maxRange>5.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265147">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>5280.7982905 -2644.05585963 436.847999355 -23.058214133</math:coefficients>
     <math:minRange>5.165</math:minRange>
     <math:maxRange>6.342</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265119">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>679.895945018 -301.041418603 41.7260939247 -1.06607549313 0.0147764210174</math:coefficients>
     <math:minRange>6.342</math:minRange>
     <math:maxRange>16.116</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265325">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-18.6658626769 36.6508930643</math:coefficients>
     <math:minRange>0.854</math:minRange>
     <math:maxRange>5.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962651444">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>8519.22184952 -4290.64033145 715.488781636 -38.2811916629</math:coefficients>
     <math:minRange>5.165</math:minRange>
     <math:maxRange>6.342</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265776">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>-377.844962579 29.6836443621 17.2155695968 -0.80206355934 0.0139499211373</math:coefficients>
     <math:minRange>6.342</math:minRange>
     <math:maxRange>16.116</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265574">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.975231820923 0.0290025516122</math:coefficients>
     <math:minRange>0.854</math:minRange>
     <math:maxRange>5.165</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265117">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>3.49086253118 -1.42857866376 0.281510607794 -0.0181231164179</math:coefficients>
     <math:minRange>5.165</math:minRange>
     <math:maxRange>6.342</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229626560">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.72580661049 -0.15564798643 0.011820717302 -3.41262497064E-4 2.16203030406E-6</math:coefficients>
     <math:minRange>6.342</math:minRange>
     <math:maxRange>16.116</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.0000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775199397"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777126810">
   <gml:description>Gelesen aus: PROF0045.1000.txt</gml:description>
   <gml:name>45.1000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930234.182 776097.545 360.251</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296234188">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296234945">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296234429">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296234282">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296234287">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296234472">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296234232">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296234659">
     <gml:description>Übernommen aus Datei: PROF0045.1000.txt</gml:description>
     <gml:name>0045.1000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296234360">
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
     <om:result><![CDATA[360.7 0.4700000 7.647000 1.000000 1.000000 -1.776357E-15 1.776357E-15 0E-7
364.7 4.497000 147.4880 101.0000 1.003900 0E-7 0E-7 0E-7
366.0 5.791000 321.9700 201.0000 1.114230 0.1186680 0.005431171 -0.00002878210
366.8 6.545000 437.2690 301.0000 1.081980 -0.8409553 -0.01085230 -0.000005804905
367.4 7.174000 535.9650 401.0000 1.062080 -0.8999689 0.05423512 0.0001535136
368.0 7.726000 624.3250 501.0000 1.048770 -0.5653368 -0.1006457 0.0004495394
368.5 8.227000 705.9100 601.0000 1.040070 -0.1289203 -0.05751832 0.0001658875
368.9 8.689000 782.1650 701.0000 1.033660 0.3875683 -0.08809573 0.0002246041
369.4 9.121000 854.5260 801.0000 1.029200 0.6849020 -0.09649215 0.00009845818
369.8 9.529000 923.7310 901.0000 1.026180 0.7856406 -0.02850985 -0.0002543950
370.2 9.917000 990.3160 1001.000 1.023920 0.6522003 0.08460647 -0.0005217993
370.5 10.28700 1054.365 1101.000 1.021830 0.4046195 -0.05699669 -0.0003648317
370.9 10.64400 1116.311 1201.000 1.020320 0.3673126 0.2629404 -0.0003876490
371.2 10.98700 1176.361 1301.000 1.019030 0.07718811 0.2408202 -0.0003471026
371.6 11.31900 1234.707 1401.000 1.017900 -0.2029237 0.2934840 -0.0002827736
371.9 11.64000 1291.335 1501.000 1.016710 -0.5135855 0.03634159 -0.00003742161
372.2 11.95300 1346.425 1601.000 1.015630 -0.5527118 0.08065014 0.0001664829
372.5 12.25700 1400.100 1701.000 1.014630 -0.6556237 -0.1695325 0.0003326013
372.8 12.55400 1452.489 1801.000 1.013700 -0.6295823 -0.3071907 0.0004473497
373.1 12.84500 1503.712 1901.000 1.012840 -0.4433092 -0.1968650 0.0004986241
373.4 13.13000 1553.853 2001.000 1.012060 -0.2022277 -0.01828189 0.0004739823
373.7 13.40800 1603.004 2101.000 1.011340 -0.1935291 -0.3049463 0.0003995180
373.9 13.68200 1651.236 2201.000 1.010690 0.02676187 -0.1335372 0.0002643989
374.2 13.95100 1698.607 2301.000 1.010090 0.2105984 0.004811342 0.0001029295
374.5 14.21500 1745.166 2401.000 1.009550 0.2978023 -0.02000307 -0.00008045386
374.7 14.47500 1790.982 2501.000 1.009050 0.3865484 0.05550465 -0.0002515357
375.0 14.73100 1836.084 2601.000 1.008600 0.4396848 0.1299640 -0.0004011831
375.2 14.98300 1880.518 2701.000 1.008180 0.4044312 0.1059858 -0.0004887031
375.5 15.23200 1924.324 2801.000 1.007800 0.4110974 0.2951388 -0.0005036426
375.7 15.47700 1967.531 2901.000 1.007450 0.2499499 0.2131597 -0.0004110241
376.0 15.71900 2010.169 3001.000 1.007120 0.06288280 0.1889276 -0.0001777942
376.2 15.95700 2052.264 3101.000 1.006830 -0.3543970 -0.2665895 0.0002007755
376.4 16.19100 2093.428 3200.000 1.006560 -0.6154616 -0.2339623 0.0007677061
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234415">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-10.6712192699 24.8323814254</math:coefficients>
     <math:minRange>0.47</math:minRange>
     <math:maxRange>4.497</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234464">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>1644.81351755 -855.7493198 146.046397466 -7.1364190097</math:coefficients>
     <math:minRange>4.497</math:minRange>
     <math:maxRange>5.791</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234596">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>263.014676925 -152.873203224 27.0139972678 -0.443527830546 0.00309614874626</math:coefficients>
     <math:minRange>5.791</math:minRange>
     <math:maxRange>16.191</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234263">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-8.67414973926 34.7258505091</math:coefficients>
     <math:minRange>0.47</math:minRange>
     <math:maxRange>4.497</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234607">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>7612.44187117 -4400.29846164 841.875736727 -51.7035828509</math:coefficients>
     <math:minRange>4.497</math:minRange>
     <math:maxRange>5.791</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234769">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>-338.219244423 68.7938780377 9.82057357553 -0.375503283025 0.00489720814946</math:coefficients>
     <math:minRange>5.791</math:minRange>
     <math:maxRange>16.191</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234821">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.999544822448 9.6846287559E-4</math:coefficients>
     <math:minRange>0.47</math:minRange>
     <math:maxRange>4.497</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234689">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>17.6600072648 -10.0581522969 2.00219189866 -0.131015669277</math:coefficients>
     <math:minRange>4.497</math:minRange>
     <math:maxRange>5.791</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234699">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.96975458507 -0.291313481709 0.0338321036026 -0.0017590810752 3.42179479254E-5</math:coefficients>
     <math:minRange>5.791</math:minRange>
     <math:maxRange>16.191</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.1000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775216194"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777126114">
   <gml:description>Gelesen aus: PROF0045.2000.txt</gml:description>
   <gml:name>45.2000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930230.109 776205.698 358.997</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296187313">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296187939">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296187496">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter12225222961871047">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296187352">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296187378">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296187301">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296187233">
     <gml:description>Übernommen aus Datei: PROF0045.2000.txt</gml:description>
     <gml:name>0045.2000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296187167">
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
     <om:result><![CDATA[359.9 0.8730000 6.138000 1.000000 1.000000 -1.776357E-15 0E-7 0E-7
364.0 5.018000 154.9920 101.0000 1.012380 0E-7 0E-7 0E-7
365.6 6.597000 301.8760 201.0000 1.103020 0.05727882 -0.1248165 0.0007624099
366.4 7.442000 421.1110 301.0000 1.093830 0.01149083 1.455751 -0.009956514
367.1 8.119000 520.5450 401.0000 1.072680 -0.5504889 0.6537932 -0.002718814
367.7 8.712000 609.5970 501.0000 1.058690 -0.6563310 0.1909100 0.0005869622
368.2 9.246000 691.3170 601.0000 1.048430 -0.5084525 -0.3235528 0.002413874
368.7 9.737000 767.7470 701.0000 1.040990 -0.2649031 -0.8020753 0.003071232
369.2 10.19700 840.2970 801.0000 1.035860 0.1605921 -0.7227495 0.002663957
369.6 10.63000 909.5750 901.0000 1.031950 0.5207759 -0.5973061 0.002054314
370.0 11.04200 976.3710 1001.000 1.029120 0.7793335 -0.1051841 0.001176561
370.4 11.43300 1040.505 1101.000 1.026720 0.9454546 -0.1174058 0.0005612833
370.8 11.80900 1102.930 1201.000 1.024900 0.9147958 0.1951056 -0.00008690544
371.2 12.17000 1163.603 1301.000 1.023730 0.6198107 0.2724996 -0.0009192898
371.5 12.51900 1222.802 1401.000 1.022760 0.1903008 0.4669491 -0.001573287
371.9 12.85600 1280.371 1501.000 1.021700 -0.2972987 0.3712256 -0.001820190
372.2 13.18400 1336.486 1601.000 1.020680 -0.5785972 0.5309899 -0.001854951
372.5 13.50200 1391.114 1701.000 1.019650 -0.8495825 0.3419765 -0.001671561
372.8 13.81200 1444.365 1801.000 1.018630 -0.9306582 0.1605142 -0.001338142
373.1 14.11500 1496.378 1901.000 1.017650 -0.8342163 0.08621137 -0.0009244624
373.4 14.41100 1547.248 2001.000 1.016710 -0.6955222 -0.08165774 -0.0004648159
373.7 14.70100 1597.070 2101.000 1.015820 -0.4735100 -0.1867664 -0.000002270834
374.0 14.98500 1645.918 2201.000 1.014990 -0.2748980 -0.3916706 0.0004242906
374.3 15.26500 1693.871 2301.000 1.014220 0.1369752 -0.1293775 0.0007851971
374.5 15.53900 1740.978 2401.000 1.013500 0.3403637 -0.2463534 0.001068405
374.8 15.80900 1787.316 2501.000 1.012830 0.5784911 -0.1390335 0.001247436
375.1 16.07400 1832.914 2601.000 1.012210 0.6267408 -0.2895529 0.001302284
375.3 16.33600 1877.821 2701.000 1.011640 0.7588754 -0.04297712 0.001207776
375.6 16.59400 1922.078 2801.000 1.011100 0.7413954 0.1315341 0.0009662873
375.8 16.84800 1965.727 2901.000 1.010600 0.5138898 0.1420252 0.0005496522
376.1 17.09800 2008.774 3001.000 1.010140 0.05362853 -0.1001540 -0.00005884011
376.3 17.34600 2051.281 3101.000 1.009720 -0.3778761 0.1329101 -0.0008852861
376.6 17.58800 2092.834 3200.000 1.009320 -1.058810 0.1419538 -0.001903462
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187667">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-20.0615199035 24.1254523522</math:coefficients>
     <math:minRange>0.873</math:minRange>
     <math:maxRange>5.018</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187943">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>408.90377175 -117.287974652 5.25526512098 1.17382109208</math:coefficients>
     <math:minRange>5.018</math:minRange>
     <math:maxRange>6.597</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187933">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>348.689218509 -160.052796009 22.3572961371 -0.224131508688 -3.14318897497E-4</math:coefficients>
     <math:minRange>6.597</math:minRange>
     <math:maxRange>17.588</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187831">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-25.2129148372 35.9117008444</math:coefficients>
     <math:minRange>0.873</math:minRange>
     <math:maxRange>5.018</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187336">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>1686.87300515 -767.84646976 116.370990311 -4.82044483301</math:coefficients>
     <math:minRange>5.018</math:minRange>
     <math:maxRange>6.597</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229618790">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>-450.723779217 89.8683810211 3.49928217032 0.0551321621079 -0.004394614807</math:coefficients>
     <math:minRange>6.597</math:minRange>
     <math:maxRange>17.588</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187839">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.997392583836 0.00298673100121</math:coefficients>
     <math:minRange>0.873</math:minRange>
     <math:maxRange>5.018</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187431">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>11.0665554073 -5.40384603405 0.955328658975 -0.055345404563</math:coefficients>
     <math:minRange>5.018</math:minRange>
     <math:maxRange>6.597</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187938">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.32887782783 -0.0372934706447 -6.96289523039E-4 2.24891925884E-4 -7.04055270509E-6</math:coefficients>
     <math:minRange>6.597</math:minRange>
     <math:maxRange>17.588</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.2000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775216835"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777126232">
   <gml:description>Gelesen aus: PROF0045.3000.txt</gml:description>
   <gml:name>45.3000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930247.241 776305.667 358.516</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState12225222962651259">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296265860">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296265153">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter12225222962651394">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296265679">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval12225222962651449">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296265565">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation12225222962651435">
     <gml:description>Übernommen aus Datei: PROF0045.3000.txt</gml:description>
     <gml:name>0045.3000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296265180">
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
     <om:result><![CDATA[359.2 0.6570000 7.110000 1.000000 1.000000 9.228174E-13 9.194867E-13 -9.992007E-16
363.1 4.591000 137.9010 101.0000 1.000000 -2.728484E-12 -1.875833E-12 2.442491E-15
365.3 6.790000 278.3070 201.0000 1.114010 2.273737E-13 8.526513E-14 -2.220446E-16
366.2 7.662000 405.5090 301.0000 1.122140 0.06907401 -0.09651846 0.0004241817
366.9 8.342000 512.9850 401.0000 1.103690 -0.2947215 0.9706208 -0.004850223
367.4 8.921000 606.5050 501.0000 1.083950 -0.5154530 0.6261002 -0.001936768
368.0 9.439000 691.4920 601.0000 1.069300 -0.4221543 0.1189919 0.00001152815
368.4 9.914000 770.6960 701.0000 1.058190 -0.3040409 -0.2618695 0.001229271
368.9 10.35600 845.3190 801.0000 1.049920 -0.03061770 -0.5107327 0.001652469
369.3 10.77300 916.6510 901.0000 1.043980 0.2192216 -0.2939468 0.001270304
369.7 11.16600 984.7800 1001.000 1.038940 0.3339911 -0.5218689 0.001215663
370.1 11.54200 1050.482 1101.000 1.035090 0.5967306 -0.3205780 0.0008948563
370.4 11.90200 1114.181 1201.000 1.032220 0.6356791 -0.1135676 0.0003473838
370.8 12.24800 1175.973 1301.000 1.029840 0.6021358 0.06336723 -0.00008543364
371.1 12.58300 1236.465 1401.000 1.027990 0.3362914 0.6044394 -0.0005699991
371.4 12.90400 1294.864 1501.000 1.026320 0.01071370 0.1641459 -0.0008228941
371.7 13.21700 1352.029 1601.000 1.024850 -0.2180037 0.2268588 -0.0009716367
372.0 13.52100 1407.795 1701.000 1.023510 -0.4239064 0.1868382 -0.0009971139
372.3 13.81800 1462.346 1801.000 1.022280 -0.4774882 0.4425084 -0.0009364840
372.6 14.10600 1515.607 1901.000 1.021070 -0.7186893 0.07238341 -0.0007336809
372.9 14.38800 1567.658 2001.000 1.019920 -0.7186499 -0.1085767 -0.0004749964
373.2 14.66500 1618.611 2101.000 1.018820 -0.4422501 0.08782783 -0.0001829564
373.5 14.93500 1668.540 2201.000 1.017800 -0.3687240 -0.2319972 0.00009221948
373.7 15.20100 1717.536 2301.000 1.016830 -0.06602954 -0.1130510 0.0003496365
374.0 15.46200 1765.646 2401.000 1.015940 0.2036918 -0.05693306 0.0005424478
374.2 15.71800 1812.935 2501.000 1.015110 0.3476053 -0.1944676 0.0006735408
374.5 15.97000 1859.463 2601.000 1.014330 0.4651053 -0.2515809 0.0007352470
374.7 16.21900 1905.265 2701.000 1.013610 0.6815101 0.07555338 0.0007012814
375.0 16.46300 1950.393 2801.000 1.012940 0.5524859 -0.1176966 0.0005762440
375.2 16.70400 1994.871 2901.000 1.012320 0.4017530 -0.1087682 0.0003437216
375.5 16.94200 2038.748 3001.000 1.011740 0.1592249 0.02759081 0.000004397644
375.7 17.17700 2082.040 3101.000 1.011200 -0.2110158 0.2189116 -0.0004498788
375.9 17.40600 2124.362 3200.000 1.010710 -0.8869874 0.09164441 -0.001021600
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265417">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-5.83686520269 8.25770265608 3.27014439427</math:coefficients>
     <math:minRange>0.657</math:minRange>
     <math:maxRange>6.79</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962811206">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>22048.7281503 -8958.88926705 1209.45703021 -53.5951678892</math:coefficients>
     <math:minRange>6.79</math:minRange>
     <math:maxRange>7.662</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296281911">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>503.986646083 -204.095103587 25.1819118538 -0.261309998741 -2.98317465591E-5</math:coefficients>
     <math:minRange>7.662</math:minRange>
     <math:maxRange>17.406</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296281338">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.318417941653 7.05883901996 4.98999145657</math:coefficients>
     <math:minRange>0.657</math:minRange>
     <math:maxRange>6.79</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962811462">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>32114.620515 -13182.8001634 1800.39167942 -80.9161290798</math:coefficients>
     <math:minRange>6.79</math:minRange>
     <math:maxRange>7.662</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962811011">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>-652.233858479 129.175910686 -0.489370203298 0.283675438131 -0.00893798416055</math:coefficients>
     <math:minRange>7.662</math:minRange>
     <math:maxRange>17.406</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229628130">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.02549866328 -0.0443648051043 0.0084536595092</math:coefficients>
     <math:minRange>0.657</math:minRange>
     <math:maxRange>6.79</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296281635">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-8.39432175011 3.49045339472 -0.419780920788 0.0164888256548</math:coefficients>
     <math:minRange>6.79</math:minRange>
     <math:maxRange>7.662</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296281773">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.86500815709 -0.176308199512 0.0133196449686 -4.17421958313E-4 4.13255745311E-6</math:coefficients>
     <math:minRange>7.662</math:minRange>
     <math:maxRange>17.406</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.3000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775232853"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777143404">
   <gml:description>Gelesen aus: PROF0045.4000.txt</gml:description>
   <gml:name>45.4000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930306.198 776372.15 358.504</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState12225222962341056">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296234550">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition122252229623484">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296234462">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296234211">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval12225222962341179">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296234228">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation12225222962341171">
     <gml:description>Übernommen aus Datei: PROF0045.4000.txt</gml:description>
     <gml:name>0045.4000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296234127">
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
     <om:result><![CDATA[359.3 0.7570000 9.263000 1.000000 1.000000 7.825649 4.182993 0E-7
362.5 4.039000 147.6960 101.0000 1.001690 -41.48579 -16.15528 0E-7
364.2 5.737000 251.5580 201.0000 1.025690 10.74674 -10.02009 0.0001539479
365.5 6.955000 376.4780 301.0000 1.086270 36.62769 16.09202 0.005813120
366.2 7.686000 497.8780 401.0000 1.119380 19.37170 15.43077 -0.01201509
366.8 8.284000 602.0400 501.0000 1.116540 7.210542 10.87198 -0.005910152
367.3 8.813000 695.6680 601.0000 1.108600 -0.3855541 6.306999 0.00005254461
367.8 9.295000 782.0010 701.0000 1.100490 -4.866991 2.432570 0.003654532
368.2 9.739000 862.8050 801.0000 1.093110 -7.612530 -1.211297 0.005440751
368.7 10.15600 939.5150 901.0000 1.087260 -8.904361 -3.881188 0.005364057
369.1 10.55100 1012.914 1001.000 1.082630 -9.166887 -5.603754 0.004195410
369.4 10.92700 1083.474 1101.000 1.078790 -8.735330 -6.602785 0.002646820
369.8 11.28600 1151.576 1201.000 1.075540 -7.947661 -7.179403 0.001081484
370.1 11.63100 1217.631 1301.000 1.072900 -6.906763 -7.198488 -0.0004577341
370.5 11.96400 1281.946 1401.000 1.070850 -5.744967 -6.655112 -0.001929720
370.8 12.28600 1344.416 1501.000 1.068890 -4.338813 -5.747115 -0.002841185
371.1 12.59800 1405.212 1601.000 1.067100 -2.812015 -4.608259 -0.003307660
371.4 12.90000 1464.489 1701.000 1.065460 -1.449498 -3.651435 -0.003359186
371.7 13.19600 1522.474 1801.000 1.064090 0.2163520 -1.877077 -0.003197649
372.0 13.48300 1579.190 1901.000 1.062890 1.463660 -0.6265933 -0.002781978
372.3 13.76400 1634.782 2001.000 1.061700 2.669917 0.8599960 -0.002041917
372.5 14.03900 1689.264 2101.000 1.060630 3.748526 2.333599 -0.001170989
372.8 14.30800 1742.669 2201.000 1.059620 4.602131 3.554558 -0.0001980652
373.1 14.57200 1795.045 2301.000 1.058640 5.327074 4.679630 0.0008153400
373.3 14.83100 1846.445 2401.000 1.057700 5.818843 5.507980 0.001766264
373.6 15.08600 1896.973 2501.000 1.056840 6.126773 6.244660 0.002520197
373.8 15.33600 1946.666 2601.000 1.056030 5.972882 6.317757 0.003012917
374.1 15.58200 1995.577 2701.000 1.055280 5.465646 5.957832 0.003139263
374.3 15.82500 2043.754 2801.000 1.054580 4.717332 5.420776 0.002809948
374.6 16.06400 2091.242 2901.000 1.053920 3.455073 4.153611 0.001943433
374.8 16.30000 2138.087 3001.000 1.053310 1.797063 2.430916 0.0004287978
375.0 16.53300 2184.303 3101.000 1.052750 -0.2995252 0.1242811 -0.001830610
375.3 16.76100 2229.471 3200.000 1.052220 -2.813365 -2.735633 -0.004874529
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962341151">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-13.7748943888 30.2383546095 -8.26227292913 1.88609292188 -0.0488540852934</math:coefficients>
     <math:minRange>0.757</math:minRange>
     <math:maxRange>16.761</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234244">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.761</math:minRange>
     <math:maxRange>16.761</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234605">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.761</math:minRange>
     <math:maxRange>16.761</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234196">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>41.4906099335 -44.1730892252 15.9529528535 -0.237276632878 -0.00556075407872</math:coefficients>
     <math:minRange>0.757</math:minRange>
     <math:maxRange>16.761</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234128">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.761</math:minRange>
     <math:maxRange>16.761</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234429">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.761</math:minRange>
     <math:maxRange>16.761</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296234179">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.99961019805 5.1492992078E-4</math:coefficients>
     <math:minRange>0.757</math:minRange>
     <math:maxRange>4.039</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962341113">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-0.594728014076 1.0908507302 -0.246710563481 0.0184426184782</math:coefficients>
     <math:minRange>4.039</math:minRange>
     <math:maxRange>5.737</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229623439">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-0.9033997869 0.721797630962 -0.0933252349025 0.0051479642898 -1.03513108104E-4</math:coefficients>
     <math:minRange>5.737</math:minRange>
     <math:maxRange>16.761</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.4000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775249227"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation121268077714364">
   <gml:description>Gelesen aus: PROF0045.5000.txt</gml:description>
   <gml:name>45.5000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930406.571 776381.501 356.958</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296203428">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296203486">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition12225222962031112">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter122252229620395">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296203990">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296203306">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296203770">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296203899">
     <gml:description>Übernommen aus Datei: PROF0045.5000.txt</gml:description>
     <gml:name>0045.5000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296203592">
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
     <om:result><![CDATA[357.6 0.6250000 12.47600 1.000000 1.000000 1.966605E-11 -4.496706 -7.771561E-16
360.6 3.608000 148.4920 101.0000 1.000000 -1.130616E-10 27.99651 4.352074E-14
362.2 5.202000 233.0220 201.0000 1.000000 1.901981E-10 -17.39902 -1.161293E-13
363.5 6.566000 310.1310 301.0000 1.000000 -1.331841E-10 -29.94390 1.081357E-13
364.6 7.690000 382.1470 401.0000 1.006960 4.547474E-12 -12.17321 -4.440892E-15
365.6 8.593000 536.7120 501.0000 1.124450 -0.1153454 19.48201 0.0005091093
366.1 9.135000 643.2710 601.0000 1.133890 1.092529 16.05625 -0.004324552
366.6 9.611000 740.0250 701.0000 1.130990 0.7507871 12.30046 -0.004062860
367.0 10.03900 828.7490 801.0000 1.121750 0.1194125 8.207774 -0.0009457289
367.4 10.43100 910.9580 901.0000 1.111810 -0.2546769 3.952631 0.001303548
367.8 10.80100 989.3370 1001.000 1.102530 -0.3965462 1.312116 0.002221544
368.1 11.14800 1063.656 1101.000 1.094050 -0.4799658 -1.201234 0.002399542
368.4 11.47700 1134.765 1201.000 1.086200 -0.4502651 -3.262886 0.002335446
368.8 11.79200 1203.469 1301.000 1.079620 -0.3715390 -4.501563 0.001564507
369.1 12.09500 1270.003 1401.000 1.073910 -0.1400069 -4.943530 0.0006023193
369.3 12.38600 1334.573 1501.000 1.068870 -0.04814799 -5.177040 -0.0002670347
369.6 12.66500 1396.920 1601.000 1.064300 0.08181792 -5.768393 -0.0008205221
369.9 12.93700 1458.097 1701.000 1.060370 0.2526246 -5.382390 -0.001326557
370.2 13.20400 1518.525 1801.000 1.056890 0.4501175 -3.582139 -0.001644863
370.4 13.46100 1577.220 1901.000 1.053820 0.4742493 -2.620559 -0.001714965
370.7 13.70900 1634.365 2001.000 1.051040 0.3163594 -2.505130 -0.001508854
370.9 13.95300 1690.702 2101.000 1.048570 0.3439543 -1.504549 -0.001173470
371.1 14.19200 1746.326 2201.000 1.046470 0.2007392 -0.2494047 -0.0008351154
371.4 14.42800 1801.500 2301.000 1.044510 0.06041801 1.909755 -0.0003608626
371.6 14.65600 1855.132 2401.000 1.042610 -0.1817334 2.577381 0.0002688185
371.8 14.88000 1907.861 2501.000 1.040840 -0.2585922 3.284781 0.0008665588
372.1 15.10000 1959.720 2601.000 1.039180 -0.2266945 3.855228 0.001359357
372.3 15.31500 2010.782 2701.000 1.037620 -0.4184112 3.650604 0.001669374
372.5 15.52800 2061.148 2801.000 1.036210 -0.2451643 3.909411 0.001634314
372.7 15.73600 2110.685 2901.000 1.034810 -0.3049390 3.071821 0.001320988
372.9 15.94100 2159.454 3001.000 1.033450 -0.2021066 1.940097 0.0005929648
373.1 16.14400 2207.483 3101.000 1.032160 0.2578879 0.8755829 -0.0006868140
373.3 16.34100 2254.374 3200.000 1.030930 0.5006558 -1.208845 -0.002539958
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203663">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-66.487644039 117.260605307 -28.3930826785 3.30062210067 -0.0767335283207</math:coefficients>
     <math:minRange>0.625</math:minRange>
     <math:maxRange>16.341</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218768">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.341</math:minRange>
     <math:maxRange>16.341</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218262">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.341</math:minRange>
     <math:maxRange>16.341</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218197">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-5.75065242201 24.1710267104 8.74908111083 -1.26656724268 0.0745238515711</math:coefficients>
     <math:minRange>0.625</math:minRange>
     <math:maxRange>7.69</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218246">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>53626.6804 -19402.7294452 2336.05541945 -92.7589616549</math:coefficients>
     <math:minRange>7.69</math:minRange>
     <math:maxRange>8.593</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962181094">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>-936.170214475 157.146085332 -0.263542385454 0.29530700232 -0.00834567465059</math:coefficients>
     <math:minRange>8.593</math:minRange>
     <math:maxRange>16.341</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218664">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.00664699203 -0.0147675912542 0.0074411828921 -0.00138087545766 8.62993223972E-5</math:coefficients>
     <math:minRange>0.625</math:minRange>
     <math:maxRange>7.69</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218349">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>153.27567662 -56.4370910152 6.95187021837 -0.284490596891</math:coefficients>
     <math:minRange>7.69</math:minRange>
     <math:maxRange>8.593</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218920">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-3.63319271029 1.58720618736 -0.192429114318 0.0100578997324 -1.93238184099E-4</math:coefficients>
     <math:minRange>8.593</math:minRange>
     <math:maxRange>16.341</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.5000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775265388"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation12126807771431017">
   <gml:description>Gelesen aus: PROF0045.6000.txt</gml:description>
   <gml:name>45.6000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930498.498 776418.028 357.376</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296171580">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296171673">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296171623">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296171795">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296171818">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296171856">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222961713">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296171621">
     <gml:description>Übernommen aus Datei: PROF0045.6000.txt</gml:description>
     <gml:name>0045.6000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296171587">
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
     <om:result><![CDATA[358.0 0.5760000 7.064000 1.000000 1.000000 -1.065814E-13 -1.776357E-15 5.551115E-15
361.8 4.390000 141.4940 101.0000 1.000000 -1.705303E-13 0E-7 -1.454392E-14
363.7 6.292000 231.8150 201.0000 1.001270 2.842171E-14 -0.1685321 1.110223E-15
364.9 7.561000 355.1440 301.0000 1.106270 -0.04389293 3.097100 0.001165171
365.7 8.309000 469.9530 401.0000 1.126970 0.1158848 0.6100956 -0.01383690
366.3 8.924000 567.2660 501.0000 1.114910 0.7079692 -0.6333346 -0.005281091
366.8 9.464000 656.0420 601.0000 1.102800 0.09826800 -0.9568676 -0.0003507890
367.3 9.952000 737.3870 701.0000 1.090370 -0.002907096 -1.002165 0.003487820
367.8 10.40200 813.5790 801.0000 1.080010 -0.1020376 -0.9045927 0.004939276
368.2 10.82300 885.7200 901.0000 1.071510 -0.1404238 -0.6552713 0.004784998
368.6 11.22100 954.6860 1001.000 1.064640 -0.2147423 -0.2307137 0.003566573
369.0 11.59900 1020.843 1101.000 1.058780 -0.3535569 0.06364400 0.002094347
369.3 11.96000 1084.402 1201.000 1.053600 -0.3825292 0.1836361 0.0007753856
369.7 12.30700 1145.765 1301.000 1.049120 -0.2811312 0.3051039 -0.0004019519
370.0 12.64200 1205.235 1401.000 1.045310 -0.07946614 0.4874447 -0.001424703
370.3 12.96500 1263.031 1501.000 1.041990 -0.06302740 0.2862332 -0.002134380
370.7 13.28000 1319.342 1601.000 1.039140 0.2428776 0.5839716 -0.002601716
371.0 13.58500 1374.338 1701.000 1.036690 0.2661061 0.4439399 -0.002793420
371.3 13.88300 1428.102 1801.000 1.034530 0.4288778 0.5814767 -0.002697357
371.5 14.17300 1480.726 1901.000 1.032600 0.4289382 0.4364302 -0.002325329
371.8 14.45600 1532.307 2001.000 1.030860 0.3254889 0.1427074 -0.001730100
372.1 14.73400 1583.011 2101.000 1.029380 0.2963803 0.2417789 -0.001077415
372.4 15.00600 1632.836 2201.000 1.028080 0.1446850 0.2309975 -0.0003749992
372.6 15.27200 1681.712 2301.000 1.026760 -0.07437958 -0.03566436 0.0004825944
372.9 15.53300 1729.669 2401.000 1.025500 -0.2201187 -0.3111905 0.001319086
373.2 15.78900 1776.785 2501.000 1.024310 -0.3805698 -0.7067907 0.002030403
373.4 16.04200 1823.127 2601.000 1.023180 -0.2637758 -0.5298638 0.002527737
373.7 16.29000 1868.723 2701.000 1.022120 -0.2716182 -0.6538774 0.002711630
373.9 16.53400 1913.637 2801.000 1.021130 -0.2900686 -0.7642336 0.002486997
374.2 16.77500 1957.893 2901.000 1.020200 -0.1634154 -0.5165211 0.001765065
374.4 17.01300 2001.538 3001.000 1.019320 0.05812109 0.03667045 0.0004603655
374.6 17.24700 2044.595 3101.000 1.018490 0.1645096 0.4127128 -0.001508025
374.9 17.47600 2086.676 3200.000 1.017720 0.3568045 1.105400 -0.004215472
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171840">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-14.1022548506 26.2191924489</math:coefficients>
     <math:minRange>0.576</math:minRange>
     <math:maxRange>4.39</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171725">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>1272.10923899 -721.871985375 140.597343298 -8.41204809597</math:coefficients>
     <math:minRange>4.39</math:minRange>
     <math:maxRange>6.292</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222961711041">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>1250.30762253 -431.989692344 51.0889533925 -1.57720025666 0.0248212876228</math:coefficients>
     <math:minRange>6.292</math:minRange>
     <math:maxRange>17.476</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171447">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-7.82283117075 24.6116781927 2.1415187709</math:coefficients>
     <math:minRange>0.576</math:minRange>
     <math:maxRange>6.292</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171189">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>-94.340692498 240.453243719 -59.9105738521 4.75736694711</math:coefficients>
     <math:minRange>6.292</math:minRange>
     <math:maxRange>7.561</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187679">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>-366.233053872 17.1320375366 13.8572540523 -0.520241224882 0.00748777279073</math:coefficients>
     <math:minRange>7.561</math:minRange>
     <math:maxRange>17.476</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187262">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0002953847 -5.80106462295E-4 1.16815638802E-4</math:coefficients>
     <math:minRange>0.576</math:minRange>
     <math:maxRange>6.292</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187784">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>31.3882552605 -13.337951378 1.93684391554 -0.0929073645956</math:coefficients>
     <math:minRange>6.292</math:minRange>
     <math:maxRange>7.561</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187581">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>-1.00233856288 0.739859306475 -0.09258453845 0.00489434662294 -9.39206564999E-5</math:coefficients>
     <math:minRange>7.561</math:minRange>
     <math:maxRange>17.476</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.6000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775282444"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation121268077714383">
   <gml:description>Gelesen aus: PROF0045.7000.txt</gml:description>
   <gml:name>45.7000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930553.005 776500.844 358.597</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296171931">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296171478">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296171864">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296171486">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296171913">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296171503">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296171541">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296171331">
     <gml:description>Übernommen aus Datei: PROF0045.7000.txt</gml:description>
     <gml:name>0045.7000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296171472">
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
     <om:result><![CDATA[359.1 0.4580000 8.190000 1.000000 1.000000 3.442932 5.329071E-15 -4.440892E-16
362.3 3.691000 158.4390 101.0000 1.000000 -23.75679 0E-7 2.664535E-15
363.8 5.227000 266.9510 201.0000 1.015980 16.61230 -0.1181229 -2.220446E-16
364.7 6.126000 378.0880 301.0000 1.068860 12.65850 1.104382 0.0004829100
365.4 6.825000 476.5450 401.0000 1.074440 6.022302 0.8583863 -0.004778274
366.0 7.424000 564.9520 501.0000 1.069690 1.416323 0.3172041 -0.002519238
366.6 7.960000 646.8100 601.0000 1.064260 -1.935588 -0.1657250 -0.0007803836
367.0 8.452000 723.0890 701.0000 1.058650 -3.565655 -0.3641101 0.0006747770
367.5 8.909000 795.0350 801.0000 1.053420 -4.205720 -0.6299749 0.001691836
367.9 9.340000 863.6890 901.0000 1.049120 -4.070309 -0.5335046 0.001919709
368.3 9.748000 929.6720 1001.000 1.045650 -3.713829 -0.5152193 0.001597444
368.7 10.13700 993.1780 1101.000 1.042350 -2.991883 -0.4986459 0.001445270
369.1 10.51000 1054.769 1201.000 1.039720 -2.206807 -0.4200029 0.0009894100
369.5 10.87000 1114.716 1301.000 1.037670 -1.308649 -0.03106952 0.0003195829
369.8 11.21700 1173.130 1401.000 1.035880 -0.5537709 0.1968281 -0.0002405756
370.1 11.55300 1230.228 1501.000 1.034380 0.06259547 0.4300796 -0.0007461909
370.5 11.87900 1286.088 1601.000 1.033040 0.5381616 0.6230122 -0.001094070
370.8 12.19500 1340.882 1701.000 1.031960 0.6207453 0.4654179 -0.001413190
371.1 12.50400 1394.435 1801.000 1.030840 0.9414517 0.6498931 -0.001449271
371.4 12.80500 1446.897 1901.000 1.029770 1.122551 0.6348726 -0.001323916
371.7 13.09800 1498.337 2001.000 1.028720 1.048839 0.2024265 -0.001046769
372.0 13.38600 1548.915 2101.000 1.027810 1.048687 0.1981818 -0.0007854584
372.3 13.66800 1598.469 2201.000 1.026810 1.077361 0.1309008 -0.0003479957
372.5 13.94400 1647.083 2301.000 1.025810 1.024112 -0.1476968 0.0001338653
372.8 14.21500 1694.838 2401.000 1.024830 0.9615613 -0.4080550 0.0005964067
373.1 14.48200 1741.775 2501.000 1.023880 1.006028 -0.3887244 0.0009856220
373.3 14.74400 1787.949 2601.000 1.022960 0.9109302 -0.5666866 0.001261609
373.6 15.00200 1833.421 2701.000 1.022090 0.7782079 -0.6595211 0.001360312
373.9 15.25700 1878.230 2801.000 1.021240 0.7335752 -0.3566064 0.001265997
374.1 15.50800 1922.412 2901.000 1.020440 0.5564847 -0.1251823 0.0009100620
374.4 15.75500 1965.996 3001.000 1.019670 0.2111343 -0.04546310 0.0002724422
374.6 15.99900 2009.017 3101.000 1.018950 -0.1679273 0.2188117 -0.0007130575
374.8 16.23800 2051.081 3200.000 1.018260 -0.5397326 0.7707740 -0.002049235
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171319">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-13.1664089081 30.9310238169</math:coefficients>
     <math:minRange>0.458</math:minRange>
     <math:maxRange>3.691</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171723">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>411.981603796 -222.919651959 43.9301563575 -1.72353743403</math:coefficients>
     <math:minRange>3.691</math:minRange>
     <math:maxRange>5.227</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171420">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>315.869393306 -153.32366047 28.0817451732 -0.609328618686 0.00832862878718</math:coefficients>
     <math:minRange>5.227</math:minRange>
     <math:maxRange>16.238</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171573">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>20.5766886449 -27.815817788 18.4222983849 -0.717027838967 0.0099844011832</math:coefficients>
     <math:minRange>0.458</math:minRange>
     <math:maxRange>16.238</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171457">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.238</math:minRange>
     <math:maxRange>16.238</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171192">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.238</math:minRange>
     <math:maxRange>16.238</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171302">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0036878034 -0.00905110642954 0.0021815151674</math:coefficients>
     <math:minRange>0.458</math:minRange>
     <math:maxRange>5.227</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171846">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>23.3274121895 -11.9783328787 2.13074827619 -0.125453632157</math:coefficients>
     <math:minRange>5.227</math:minRange>
     <math:maxRange>6.126</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171567">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.635390863809 0.183069178027 -0.026990693733 0.00163247959127 -3.54505506487E-5</math:coefficients>
     <math:minRange>6.126</math:minRange>
     <math:maxRange>16.238</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.7000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775298669"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777143648">
   <gml:description>Gelesen aus: PROF0045.8000.txt</gml:description>
   <gml:name>45.8000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930562.354 776599.847 358.91</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296187161">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation122252229618792">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition122252229618783">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter12225222961871017">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296187343">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval12225222961871002">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296187257">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296187574">
     <gml:description>Übernommen aus Datei: PROF0045.8000.txt</gml:description>
     <gml:name>0045.8000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296187702">
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
     <om:result><![CDATA[359.4 0.5290000 8.860000 1.000000 1.000000 0.7687588 1.776357E-15 0E-7
362.8 3.858000 154.5370 101.0000 1.000670 -7.793538 0E-7 0E-7
364.2 5.335000 278.9580 201.0000 1.069160 9.151153 -0.09886414 0.0007042519
365.1 6.237000 387.8110 301.0000 1.076950 3.991479 1.085123 -0.008423927
365.9 6.962000 482.9550 401.0000 1.068290 -0.05990976 0.5885078 -0.003759187
366.5 7.593000 568.5720 501.0000 1.059150 -1.634315 0.06209804 0.0005764734
367.1 8.162000 648.3930 601.0000 1.052700 -2.409978 -0.1860894 0.002095524
367.6 8.684000 723.2310 701.0000 1.047310 -2.384546 -0.4575930 0.002755885
368.1 9.171000 794.2990 801.0000 1.043000 -1.856463 -0.4625038 0.002671787
368.5 9.629000 862.2090 901.0000 1.039300 -1.098114 -0.4110463 0.002389186
369.0 10.06300 927.6970 1001.000 1.036680 -0.4834853 -0.3233778 0.001460442
369.4 10.47700 991.0010 1101.000 1.034350 0.07632362 -0.1457779 0.0006705253
369.8 10.87400 1052.443 1201.000 1.032510 0.5117474 0.1550066 -0.0001988225
370.2 11.25400 1112.055 1301.000 1.030770 0.6262500 -0.02131357 -0.0007707126
370.5 11.62200 1170.055 1401.000 1.029280 0.8619117 0.1480396 -0.001247391
370.9 11.97800 1226.574 1501.000 1.027900 0.9920260 0.2702114 -0.001515114
371.2 12.32300 1281.763 1601.000 1.026670 0.9471507 0.2676559 -0.001650692
371.6 12.65900 1335.828 1701.000 1.025680 0.7801934 0.4174043 -0.001786064
371.9 12.98600 1388.681 1801.000 1.024620 0.5309876 0.4557258 -0.001646750
372.2 13.30400 1440.345 1901.000 1.023550 0.1373574 0.1314680 -0.001330309
372.5 13.61600 1490.841 2001.000 1.022470 0.03173725 0.1781837 -0.0008806755
372.8 13.92100 1540.269 2101.000 1.021390 -0.07084829 0.09865503 -0.0003423729
373.1 14.21900 1588.740 2201.000 1.020370 -0.3002419 -0.2865600 0.0001874407
373.4 14.51300 1636.302 2301.000 1.019400 -0.2327619 -0.1175708 0.0006772755
373.7 14.80100 1683.037 2401.000 1.018470 -0.2847487 -0.2036830 0.001104063
374.0 15.08400 1728.991 2501.000 1.017600 -0.3476457 -0.3304026 0.001410256
374.3 15.36300 1774.212 2601.000 1.016770 -0.3120933 -0.2547240 0.001578048
374.5 15.63800 1818.749 2701.000 1.016000 -0.2296740 -0.07425841 0.001551577
374.8 15.90800 1862.639 2801.000 1.015270 -0.3004366 -0.2557599 0.001319514
375.1 16.17500 1905.919 2901.000 1.014580 -0.2359334 -0.1509175 0.0008404451
375.3 16.43900 1948.625 3001.000 1.013940 -0.06977771 0.1675659 0.00006840960
375.6 16.69900 1990.779 3101.000 1.013340 0.01714880 0.2417385 -0.001015034
375.9 16.95300 2032.000 3200.000 1.012780 0.08503556 0.2051079 -0.002423817
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187739">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-14.8906578552 30.039050766</math:coefficients>
     <math:minRange>0.529</math:minRange>
     <math:maxRange>3.858</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187963">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>625.659242353 -351.069224731 68.4607985449 -3.29511835701</math:coefficients>
     <math:minRange>3.858</math:minRange>
     <math:maxRange>5.335</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229618754">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>149.356795486 -82.5233449156 17.9393333453 -0.114002654655 -0.00182214046765</math:coefficients>
     <math:minRange>5.335</math:minRange>
     <math:maxRange>16.953</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187613">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>18.039308713 -25.1815922141 17.9498404786 -0.767081408187 0.0123435168886</math:coefficients>
     <math:minRange>0.529</math:minRange>
     <math:maxRange>16.953</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187490">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.953</math:minRange>
     <math:maxRange>16.953</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187164">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.953</math:minRange>
     <math:maxRange>16.953</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187660">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.999893532592 2.01261640132E-4</math:coefficients>
     <math:minRange>0.529</math:minRange>
     <math:maxRange>3.858</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222961871073">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>4.82148454742 -2.60913783776 0.582423537291 -0.0422068473657</math:coefficients>
     <math:minRange>3.858</math:minRange>
     <math:maxRange>5.335</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296187355">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.863855469324 0.0966505792611 -0.0153793049399 9.55119225922E-4 -2.08910781386E-5</math:coefficients>
     <math:minRange>5.335</math:minRange>
     <math:maxRange>16.953</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.8000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775315731"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation12126807771591030">
   <gml:description>Gelesen aus: PROF0045.9000.txt</gml:description>
   <gml:name>45.9000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930544.969 776695.513 359.301</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296171242">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296171513">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296171739">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296171507">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296171233">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval122252229617167">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296171815">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296171807">
     <gml:description>Übernommen aus Datei: PROF0045.9000.txt</gml:description>
     <gml:name>0045.9000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296171176">
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
     <om:result><![CDATA[359.9 0.5570000 8.632000 1.000000 1.000000 -1.708619 1.065814E-14 0E-7
363.0 3.700000 198.5580 101.0000 1.028960 6.618531 0E-7 0E-7
364.0 4.708000 308.1500 201.0000 1.030080 2.370199 -0.09824658 0.0001598905
364.8 5.523000 407.1350 301.0000 1.033660 -0.8038176 0.7897356 -0.002034688
365.5 6.209000 495.8020 401.0000 1.031650 -2.694898 0.6014262 -0.00009162950
366.1 6.818000 577.6090 501.0000 1.030910 -3.490771 0.3898972 -0.00005387812
366.7 7.372000 654.2960 601.0000 1.029900 -3.701458 0.1386316 -0.00002211779
367.2 7.883000 726.4570 701.0000 1.028530 -3.295980 -0.3993025 0.0002804838
367.7 8.363000 795.2120 801.0000 1.027160 -2.352237 -0.5534208 0.0005865856
368.1 8.817000 861.2060 901.0000 1.026340 -1.233049 -0.4771582 0.0004033006
368.6 9.249000 924.8440 1001.000 1.025810 -0.07292378 -0.2306896 0.00002113584
369.0 9.660000 986.1780 1101.000 1.024650 0.9843160 -0.3734027 0.0003784082
369.4 10.05500 1045.845 1201.000 1.023910 1.880431 -0.3358018 0.0004237542
369.7 10.43600 1104.087 1301.000 1.023560 2.548623 -0.1129919 0.0001848047
370.1 10.80400 1161.003 1401.000 1.023340 2.942789 0.1264871 -0.00008436726
370.5 11.16000 1216.585 1501.000 1.022930 3.146870 0.2729283 -0.00007344524
370.8 11.50500 1271.133 1601.000 1.022750 2.955348 0.2705640 -0.0002145219
371.1 11.84100 1324.705 1701.000 1.022660 2.578340 0.4152093 -0.0003818390
371.5 12.16800 1377.336 1801.000 1.022600 1.945062 0.4596085 -0.0005288379
371.8 12.48700 1428.985 1901.000 1.022420 1.227851 0.4843063 -0.0005204294
372.1 12.79800 1479.669 2001.000 1.022240 0.3887276 0.2843322 -0.0004913042
372.4 13.10300 1529.490 2101.000 1.022050 -0.3683561 0.3259302 -0.0004472886
372.7 13.40100 1578.380 2201.000 1.021700 -1.146316 0.1226052 -0.0002528189
373.0 13.69300 1626.339 2301.000 1.021300 -1.790815 -0.1436598 -0.00003323505
373.3 13.98000 1673.441 2401.000 1.020860 -2.216802 -0.2592634 0.0001859813
373.6 14.26200 1719.722 2501.000 1.020380 -2.460442 -0.3384435 0.0003899813
373.8 14.53900 1765.247 2601.000 1.019880 -2.585354 -0.4918561 0.0005442468
374.1 14.81200 1810.064 2701.000 1.019380 -2.472042 -0.4573027 0.0006129945
374.4 15.08100 1854.215 2801.000 1.018880 -2.156873 -0.3213366 0.0005815809
374.6 15.34600 1897.741 2901.000 1.018380 -1.674387 -0.1680885 0.0004357619
374.9 15.60700 1940.670 3001.000 1.017900 -1.045542 -0.07932025 0.0001418067
375.2 15.86500 1983.032 3101.000 1.017430 -0.1247054 0.2570898 -0.0003072839
375.4 16.11700 2024.442 3200.000 1.016970 0.9760374 0.5892595 -0.0009122657
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171454">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-16.7219217308 31.8167356029</math:coefficients>
     <math:minRange>0.557</math:minRange>
     <math:maxRange>3.7</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171549">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>4358.53427198 -3068.39010634 717.00234107 -53.703688385</math:coefficients>
     <math:minRange>3.7</math:minRange>
     <math:maxRange>4.708</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171605">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>16.3409831122 -36.503920848 16.7369506016 -0.140396116516 1.89713824831E-4</math:coefficients>
     <math:minRange>4.708</math:minRange>
     <math:maxRange>16.117</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222961714">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-1.30142255224 5.99678345473 16.1804600536 -0.791998029719 0.0154545585349</math:coefficients>
     <math:minRange>0.557</math:minRange>
     <math:maxRange>16.117</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171691">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.117</math:minRange>
     <math:maxRange>16.117</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171873">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.117</math:minRange>
     <math:maxRange>16.117</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171164">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.994867731467 0.00921412663061</math:coefficients>
     <math:minRange>0.557</math:minRange>
     <math:maxRange>3.7</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171110">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.278212076127 0.525898889605 -0.122242533235 0.00944509332766</math:coefficients>
     <math:minRange>3.7</math:minRange>
     <math:maxRange>4.708</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296171487">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.951368645151 0.037517990483 -0.00609444309597 4.02411206505E-4 -9.50899291542E-6</math:coefficients>
     <math:minRange>4.708</math:minRange>
     <math:maxRange>16.117</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>45.9000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775331457"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777159233">
   <gml:description>Gelesen aus: PROF0046.0000.txt</gml:description>
   <gml:name>46.0000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930542.955 776796.046 359.857</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296218917">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296218483">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296218287">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter122252229621844">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296218373">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296218353">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222962181168">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296218593">
     <gml:description>Übernommen aus Datei: PROF0046.0000.txt</gml:description>
     <gml:name>0046.0000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296218826">
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
     <om:result><![CDATA[360.2 0.3680000 9.930000 1.000000 1.000000 -1.350706 -3.552714E-15 2.664535E-15
362.7 2.885000 198.5060 101.0000 1.027130 4.686046 0E-7 -1.043610E-14
363.7 3.856000 314.1400 201.0000 1.037330 -0.05067293 -0.02093448 8.881784E-16
364.5 4.629000 412.3390 301.0000 1.034110 0.6395484 -0.01367030 0.0001585376
365.1 5.283000 504.2100 401.0000 1.037940 -1.445493 0.3891511 -0.002325549
365.7 5.856000 586.8990 501.0000 1.035210 -1.549014 -0.06157442 0.0004357018
366.2 6.380000 665.3760 601.0000 1.034710 -1.746036 0.2253601 0.0002895429
366.7 6.861000 739.6430 701.0000 1.034210 -2.146816 -0.09911223 -0.0001960010
367.2 7.313000 810.2300 801.0000 1.032820 -1.793109 0.08646730 0.00004824407
367.6 7.738000 877.5440 901.0000 1.031580 -1.221880 -0.05193090 0.0001039399
368.0 8.142000 942.3030 1001.000 1.030210 -0.5165865 -0.1224690 0.0003147925
368.4 8.527000 1004.812 1101.000 1.028940 0.1070457 -0.4264330 0.0004948603
368.8 8.898000 1065.538 1201.000 1.028040 0.8294596 -0.2929143 0.0003902481
369.1 9.255000 1124.698 1301.000 1.027410 1.304123 -0.2264217 0.0001180023
369.5 9.601000 1182.373 1401.000 1.026770 1.850356 0.1835589 -0.00004172160
369.8 9.934000 1238.614 1501.000 1.025970 1.999701 -0.03487582 0.00006614214
370.1 10.25800 1293.772 1601.000 1.025460 2.014792 -0.02678855 -0.00002117529
370.4 10.57400 1347.928 1701.000 1.025130 1.939173 0.2561068 -0.0002023652
370.7 10.88100 1401.164 1801.000 1.024930 1.482333 0.2480506 -0.0004357469
371.0 11.18100 1453.436 1901.000 1.024670 1.002725 0.3751348 -0.0005460462
371.3 11.47300 1504.616 2001.000 1.024240 0.4330217 0.1000619 -0.0004352236
371.6 11.76000 1554.913 2101.000 1.023840 0.06796585 0.2747326 -0.0003205216
371.9 12.04000 1604.327 2201.000 1.023420 -0.4520480 0.04641533 -0.0001645208
372.2 12.31500 1652.921 2301.000 1.022970 -0.8495271 -0.02132408 0.00002577804
372.4 12.58500 1700.784 2401.000 1.022570 -1.217942 -0.05128601 0.0001550035
372.7 12.85000 1747.960 2501.000 1.022190 -1.603847 -0.1620868 0.0002376295
373.0 13.11100 1794.408 2601.000 1.021770 -1.789670 -0.08166021 0.0003168153
373.2 13.36700 1840.099 2701.000 1.021300 -1.922428 -0.2948631 0.0003883284
373.5 13.62000 1885.117 2801.000 1.020820 -1.726864 -0.1111143 0.0003933000
373.7 13.86800 1929.470 2901.000 1.020330 -1.564326 -0.4085306 0.0003201228
374.0 14.11400 1973.214 3001.000 1.019840 -0.9463661 -0.05592204 0.0001362419
374.2 14.35600 2016.383 3101.000 1.019360 -0.2599593 0.06691351 -0.0001789386
374.4 14.59300 2058.570 3200.000 1.018890 0.7246249 0.4585002 -0.0006351849
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218442">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-13.6205800556 39.7298371077</math:coefficients>
     <math:minRange>0.368</math:minRange>
     <math:maxRange>2.885</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218156">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>2235.76828692 -1962.41570217 577.205729838 -53.1977897944</math:coefficients>
     <math:minRange>2.885</math:minRange>
     <math:maxRange>3.856</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218264">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>26.6958179206 -29.4441514565 20.3814361608 -0.275789435403 0.00264959694295</math:coefficients>
     <math:minRange>3.856</math:minRange>
     <math:maxRange>14.593</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218389">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-5.69525810119 33.0346842672 15.9488693445 -0.850936250498 0.0183226680133</math:coefficients>
     <math:minRange>0.368</math:minRange>
     <math:maxRange>14.593</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218234">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.593</math:minRange>
     <math:maxRange>14.593</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218777">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.593</math:minRange>
     <math:maxRange>14.593</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218427">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.995950014878 0.0110343100375 -7.85752321574E-5</math:coefficients>
     <math:minRange>0.368</math:minRange>
     <math:maxRange>3.856</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218328">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-1.77990269139 1.990919464 -0.466918270858 0.0363263915798</math:coefficients>
     <math:minRange>3.856</math:minRange>
     <math:maxRange>4.629</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962181079">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.933890099308 0.049675186237 -0.00844803342955 5.86548782483E-4 -1.46479048216E-5</math:coefficients>
     <math:minRange>4.629</math:minRange>
     <math:maxRange>14.593</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.0000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775331336"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation12126807771591075">
   <gml:description>Gelesen aus: PROF0046.0160.txt</gml:description>
   <gml:name>46.0160</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930559.811 776812.645 359.723</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296265393">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296265609">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition12225222962651319">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296265453">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296265116">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval12225222962651111">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222962651214">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation122252229626548">
     <gml:description>Übernommen aus Datei: PROF0046.0160.txt</gml:description>
     <gml:name>0046.0160</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296265339">
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
     <om:result><![CDATA[360.1 0.3940000 9.998000 1.000000 1.000000 -1.399748 7.105427E-15 0E-7
362.7 2.938000 205.0530 101.0000 1.019820 4.177460 0E-7 0E-7
363.5 3.808000 313.3770 201.0000 1.043340 2.059632 -0.09294482 0.0003928652
364.3 4.557000 417.2410 301.0000 1.054980 0.7579298 0.5281979 -0.002508609
364.9 5.184000 512.4500 401.0000 1.060170 -2.055294 1.014795 -0.004851444
365.5 5.731000 598.0400 501.0000 1.056810 -2.958798 0.3635819 -0.001416391
366.0 6.229000 677.2270 601.0000 1.052060 -2.239355 0.004038032 0.002014945
366.4 6.688000 752.9630 701.0000 1.049570 -2.300869 -0.5999177 0.002493784
366.8 7.121000 825.4520 801.0000 1.047670 -1.872100 -0.3418840 0.002061588
367.3 7.528000 894.5970 901.0000 1.045830 -1.310674 -0.4785542 0.001512765
367.6 7.916000 961.1430 1001.000 1.044110 -0.4942063 -0.3001788 0.0009141048
368.0 8.286000 1025.313 1101.000 1.042300 0.2911607 -0.3250479 0.0005727367
368.4 8.641000 1087.541 1201.000 1.040740 0.9593881 -0.3922269 0.0001919939
368.7 8.984000 1148.164 1301.000 1.039560 1.574811 -0.1641096 -0.0003414231
369.0 9.316000 1207.310 1401.000 1.038560 2.090634 0.2385791 -0.0008185653
369.4 9.636000 1264.994 1501.000 1.037470 2.234063 0.1250165 -0.0009673349
369.7 9.948000 1321.507 1601.000 1.036580 2.378389 0.4192830 -0.001105054
370.0 10.25000 1376.911 1701.000 1.035790 2.054427 0.1988883 -0.001144123
370.3 10.54600 1431.368 1801.000 1.035170 1.797088 0.5452719 -0.001189528
370.6 10.83400 1484.817 1901.000 1.034510 1.277989 0.5771145 -0.001054116
370.8 11.11500 1537.217 2001.000 1.033780 0.7034936 0.4351378 -0.0007411925
371.1 11.39000 1588.690 2101.000 1.033100 0.1242383 0.3007145 -0.0004049287
371.4 11.65900 1639.277 2201.000 1.032420 -0.5079274 0.01848245 -0.00002915712
371.6 11.92300 1689.044 2301.000 1.031760 -1.076551 -0.1791085 0.0003303577
371.9 12.18300 1738.039 2401.000 1.031100 -1.442178 -0.02558281 0.0006570781
372.2 12.43700 1786.263 2501.000 1.030420 -1.977939 -0.4094057 0.0009382425
372.4 12.68700 1833.705 2601.000 1.029720 -2.293958 -0.6602864 0.001135694
372.7 12.93400 1880.406 2701.000 1.029010 -2.234783 -0.4593594 0.001200791
372.9 13.17700 1926.401 2801.000 1.028300 -2.015742 -0.2899259 0.001089643
373.1 13.41600 1971.737 2901.000 1.027590 -1.675432 -0.2369689 0.0007674797
373.4 13.65200 2016.449 3001.000 1.026900 -1.047471 0.04512735 0.0001747043
373.6 13.88400 2060.565 3101.000 1.026220 -0.3414646 0.06056405 -0.0007074011
373.8 14.11200 2103.691 3200.000 1.025560 0.8454735 0.7313232 -0.001919562
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962651005">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-14.4874213836 39.3081761006</math:coefficients>
     <math:minRange>0.394</math:minRange>
     <math:maxRange>2.938</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265490">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>3796.34156506 -3358.81485796 988.767172899 -93.1385720864</math:coefficients>
     <math:minRange>2.938</math:minRange>
     <math:maxRange>3.808</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265984">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>50.5277163838 -47.0659536507 24.4643330465 -0.484635888145 0.00767458428684</math:coefficients>
     <math:minRange>3.808</math:minRange>
     <math:maxRange>14.112</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265771">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-3.61209037143 23.3494931496 19.8398467787 -1.14197156607 0.0261457159892</math:coefficients>
     <math:minRange>0.394</math:minRange>
     <math:maxRange>14.112</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265933">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.112</math:minRange>
     <math:maxRange>14.112</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265310">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.112</math:minRange>
     <math:maxRange>14.112</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265643">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.996930393082 0.00779088050314</math:coefficients>
     <math:minRange>0.394</math:minRange>
     <math:maxRange>2.938</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265517">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>2.5296397954 -1.38676023522 0.416626406712 -0.04068445669</math:coefficients>
     <math:minRange>2.938</math:minRange>
     <math:maxRange>3.808</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265297">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.822839954754 0.116892743113 -0.0205544862387 0.00148321836651 -3.84216329714E-5</math:coefficients>
     <math:minRange>3.808</math:minRange>
     <math:maxRange>14.112</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.0160</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775348226"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777159250">
   <gml:description>Gelesen aus: PROF0046.1000.txt</gml:description>
   <gml:name>46.1000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930586.027 776894.371 358.505</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState12225222962651124">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation12225222962651357">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296265726">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296265387">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296265392">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296265502">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296265473">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296265411">
     <gml:description>Übernommen aus Datei: PROF0046.1000.txt</gml:description>
     <gml:name>0046.1000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296265587">
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
     <om:result><![CDATA[359.1 0.5920000 7.506000 1.000000 1.000000 -1.065814E-14 -3.552714E-15 0E-7
362.5 3.963000 186.8730 101.0000 1.026680 0E-7 0E-7 0E-7
363.6 5.065000 310.0320 201.0000 1.048720 0.1221585 -0.02260709 -0.00005721369
364.4 5.892000 408.8790 301.0000 1.038310 -1.181673 0.3410022 -0.0001759461
365.1 6.599000 495.3680 401.0000 1.031140 -0.8785391 0.09298150 0.0009220404
365.7 7.229000 574.2700 501.0000 1.027270 0.07125265 -0.2786093 0.001141414
366.3 7.807000 649.0280 601.0000 1.025890 0.5500686 -0.05617583 0.0002942917
366.8 8.342000 720.6210 701.0000 1.025770 0.1915954 0.1518119 -0.0009233036
367.3 8.841000 788.3850 801.0000 1.024860 0.1729686 -0.005795228 -0.0007998084
367.8 9.312000 853.4150 901.0000 1.024220 0.1732683 -0.1837190 -0.0006153217
368.3 9.761000 916.1900 1001.000 1.023720 0.3113903 -0.03761100 -0.0003809656
368.7 10.19000 977.0450 1101.000 1.023460 0.3499741 0.1381171 -0.0002892942
369.1 10.60000 1036.012 1201.000 1.022980 0.2431146 -0.07463441 0.00005705555
369.5 10.99600 1093.585 1301.000 1.022820 0.08827697 0.01154517 0.00007537259
369.9 11.37800 1149.673 1401.000 1.022520 -0.1271642 -0.03682900 0.0001994092
370.3 11.74800 1204.285 1501.000 1.022140 -0.2100011 -0.08323679 0.0003527887
370.6 12.10800 1257.649 1601.000 1.021810 -0.1691164 0.09330495 0.0003963753
371.0 12.45800 1309.832 1701.000 1.021460 -0.1376056 0.2111404 0.0003981471
371.3 12.79800 1360.925 1801.000 1.021070 -0.2665714 0.0006182616 0.0003810603
371.6 13.13100 1411.100 1901.000 1.020790 -0.3300461 0.1163928 0.0001974125
372.0 13.45600 1460.247 2001.000 1.020390 -0.4139922 0.06200936 0.00008686672
372.3 13.77400 1508.346 2101.000 1.019900 -0.3890973 -0.04357030 0.00002842215
372.6 14.08600 1555.499 2201.000 1.019380 -0.2430517 -0.04832572 -0.00002778401
372.9 14.39200 1601.779 2301.000 1.018850 -0.08244480 -0.09926173 -0.00008870693
373.2 14.69300 1647.253 2401.000 1.018320 0.1467857 -0.002814070 -0.0001522810
373.5 14.98800 1691.969 2501.000 1.017800 0.2166508 -0.2200006 -0.0002118081
373.8 15.27900 1735.984 2601.000 1.017290 0.3478319 -0.1902765 -0.0002559151
374.1 15.56600 1779.329 2701.000 1.016800 0.4850016 -0.009708894 -0.0002786733
374.4 15.84900 1822.043 2801.000 1.016330 0.5664561 0.2283411 -0.0002637322
374.6 16.12700 1864.166 2901.000 1.015880 0.3804669 0.07165480 -0.0001932981
374.9 16.40200 1905.730 3001.000 1.015460 0.1773220 0.1507670 -0.00006354815
375.2 16.67300 1946.754 3101.000 1.015050 -0.2309225 0.02377841 0.0001634902
375.4 16.93800 1986.869 3200.000 1.014670 -0.7894665 -0.1420400 0.0004839501
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962651295">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-16.5615544349 29.6647878968</math:coefficients>
     <math:minRange>0.592</math:minRange>
     <math:maxRange>3.963</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962651105">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>3546.87375356 -2300.42382812 495.243151981 -33.8569696894</math:coefficients>
     <math:minRange>3.963</math:minRange>
     <math:maxRange>5.065</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229626598">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-25.8800974923 -20.7527220357 12.7599902297 0.0580828411111 -0.00444410317315</math:coefficients>
     <math:minRange>5.065</math:minRange>
     <math:maxRange>16.938</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265449">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-23.9936333432 53.2088401068</math:coefficients>
     <math:minRange>0.592</math:minRange>
     <math:maxRange>3.963</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265810">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>4431.83030917 -2910.17253037 644.384026559 -45.5047447795</math:coefficients>
     <math:minRange>3.963</math:minRange>
     <math:maxRange>5.065</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265736">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>-183.72721896 79.0204078626 3.7177914987 0.00161473337513 -0.00295357595337</math:coefficients>
     <math:minRange>5.065</math:minRange>
     <math:maxRange>16.938</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229626516">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.995314577277 0.00791456541086</math:coefficients>
     <math:minRange>0.592</math:minRange>
     <math:maxRange>3.963</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265817">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>4.26742257605 -2.25557809086 0.517282898431 -0.0389779995342</math:coefficients>
     <math:minRange>3.963</math:minRange>
     <math:maxRange>5.065</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296265469">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.26517994448 -0.0815659610382 0.0102046957063 -5.57923300409E-4 1.11172140577E-5</math:coefficients>
     <math:minRange>5.065</math:minRange>
     <math:maxRange>16.938</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.1000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775364885"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation121268077715918">
   <gml:description>Gelesen aus: PROF0046.2000.txt</gml:description>
   <gml:name>46.2000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930630.518 776986.725 358.194</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296156432">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296156164">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296156584">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296156942">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296156663">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296156546">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296156718">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296156286">
     <gml:description>Übernommen aus Datei: PROF0046.2000.txt</gml:description>
     <gml:name>0046.2000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296156457">
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
     <om:result><![CDATA[358.8 0.5960000 8.959000 1.000000 1.000000 -1.065814E-14 -1.065814E-14 0E-7
362.1 3.891000 191.1410 101.0000 1.042070 0E-7 0E-7 0E-7
363.0 4.844000 300.7620 201.0000 1.071250 0.01222991 -0.07130986 0.00006740697
363.8 5.650000 404.1020 301.0000 1.057720 -0.2798043 0.7440826 -0.001588611
364.5 6.331000 493.7830 401.0000 1.046760 -0.3865434 0.3987587 -0.0002183895
365.1 6.936000 574.9800 501.0000 1.038920 0.08159860 -0.07623236 0.001159096
365.7 7.488000 650.4940 601.0000 1.033820 0.8744347 -0.3427055 0.001765025
366.2 8.001000 723.0350 701.0000 1.031820 0.8260319 -0.09494260 0.0005836171
366.7 8.480000 792.8870 801.0000 1.030940 0.04931376 0.02177465 -0.0007925706
367.1 8.931000 859.6380 901.0000 1.029680 -0.4397890 -0.01742632 -0.001140824
367.6 9.360000 923.8810 1001.000 1.028430 -0.5616020 0.1490206 -0.001045138
368.0 9.768000 985.8310 1101.000 1.027150 -0.5592111 -0.03052910 -0.0005930493
368.4 10.15900 1046.054 1201.000 1.026250 -0.5459483 -0.2352652 -0.0002949798
368.7 10.53600 1104.713 1301.000 1.025510 -0.3491942 -0.2135108 -0.000002486859
369.1 10.89900 1161.923 1401.000 1.024830 -0.1967469 -0.4094558 0.0003340413
369.4 11.25100 1217.918 1501.000 1.024330 0.05145855 -0.3798502 0.0005538752
369.8 11.59300 1272.856 1601.000 1.024070 0.3116549 -0.1513766 0.0005681025
370.1 11.92500 1326.803 1701.000 1.023890 0.4382680 -0.009355542 0.0005156601
370.4 12.24800 1379.737 1801.000 1.023670 0.5424616 0.08621135 0.0005004699
370.8 12.56400 1431.887 1901.000 1.023630 0.6592456 0.5395656 0.0002900950
371.1 12.87200 1483.276 2001.000 1.023670 0.5406652 0.8366474 -0.00002209647
371.4 13.17200 1533.781 2101.000 1.023560 0.2517820 0.7766403 -0.0002103668
371.7 13.46500 1583.416 2201.000 1.023320 -0.1070970 0.5099603 -0.0002976215
371.9 13.75200 1632.095 2301.000 1.022970 -0.3293697 0.2218050 -0.0003052336
372.2 14.03300 1679.863 2401.000 1.022540 -0.5038563 -0.2277053 -0.0002620210
372.5 14.31000 1726.761 2501.000 1.022050 -0.3728122 -0.2457024 -0.0001886082
372.8 14.58100 1772.871 2601.000 1.021530 -0.3955735 -0.6664035 -0.0001087606
373.0 14.84900 1818.243 2701.000 1.021000 -0.1475158 -0.4873629 -0.00004337473
373.3 15.11200 1862.915 2801.000 1.020470 -0.04048858 -0.5439797 0.000005583024
373.6 15.37100 1906.924 2901.000 1.019930 0.02699971 -0.5511219 0.00005162680
373.8 15.62700 1950.317 3001.000 1.019400 0.1478148 -0.1972894 0.00007847136
374.1 15.88000 1993.124 3101.000 1.018890 0.2626113 0.4533908 0.00008213114
374.3 16.12600 2034.959 3200.000 1.018390 0.05337221 0.7128367 0.00008708200
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156227">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-17.0880121396 30.3490136571</math:coefficients>
     <math:minRange>0.596</math:minRange>
     <math:maxRange>3.891</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156388">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>6487.77819269 -4411.70764677 994.293569215 -72.5575032636</math:coefficients>
     <math:minRange>3.891</math:minRange>
     <math:maxRange>4.844</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156790">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-1.04377917598 -31.6585241512 15.4500046358 -0.0592412426627 -8.43311643701E-4</math:coefficients>
     <math:minRange>4.844</math:minRange>
     <math:maxRange>16.126</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156479">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-23.9941022762 55.2904400607</math:coefficients>
     <math:minRange>0.596</math:minRange>
     <math:maxRange>3.891</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156463">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>4864.93071312 -3270.96681776 740.9674841 -53.7201867361</math:coefficients>
     <math:minRange>3.891</math:minRange>
     <math:maxRange>4.844</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229615655">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>-280.884846609 120.048240647 -1.4642698864 0.356009173726 -0.0108267271261</math:coefficients>
     <math:minRange>4.844</math:minRange>
     <math:maxRange>16.126</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229615645">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.992390373293 0.0127678300455</math:coefficients>
     <math:minRange>0.596</math:minRange>
     <math:maxRange>3.891</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156881">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>6.95968742037 -4.22757964136 0.997134183633 -0.0774852137352</math:coefficients>
     <math:minRange>3.891</math:minRange>
     <math:maxRange>4.844</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156714">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.30640128987 -0.0843233890261 0.00944905298824 -4.66030041804E-4 8.41377270776E-6</math:coefficients>
     <math:minRange>4.844</math:minRange>
     <math:maxRange>16.126</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.2000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775364797"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777159342">
   <gml:description>Gelesen aus: PROF0046.3000.txt</gml:description>
   <gml:name>46.3000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930698.622 777066.027 359.02</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296250993">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296250886">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition12225222962501275">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296250131">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296250520">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296250173">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222962501138">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296250462">
     <gml:description>Übernommen aus Datei: PROF0046.3000.txt</gml:description>
     <gml:name>0046.3000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition12225222962501081">
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
     <om:result><![CDATA[359.5 0.4800000 8.536000 1.000000 1.000000 -2.483637 3.552714E-15 -7.327472E-15
362.2 3.177000 212.0510 101.0000 1.024730 9.847125 0E-7 2.087219E-14
363.0 4.023000 309.2850 201.0000 1.033300 1.885015 -0.05705779 -1.554312E-15
363.8 4.814000 405.1500 301.0000 1.029110 -3.152452 0.5495029 -0.00009141543
364.5 5.496000 489.8710 401.0000 1.026000 -4.567975 0.3255710 0.0001628153
365.1 6.106000 567.2640 501.0000 1.023550 -3.996758 -0.2479702 0.001058697
365.7 6.669000 640.0260 601.0000 1.022580 -2.227583 0.01046298 0.001245530
366.2 7.190000 710.1210 701.0000 1.023300 -1.381086 0.01324561 0.0002255098
366.7 7.679000 778.0260 801.0000 1.024550 -1.152206 0.1537262 -0.001031732
367.2 8.140000 842.9330 901.0000 1.024820 -0.5855403 0.09373204 -0.001137849
367.6 8.578000 905.4560 1001.000 1.024910 0.1102289 -0.01029842 -0.0009739034
368.0 8.996000 965.9960 1101.000 1.024750 0.7388044 -0.2161844 -0.0005247700
368.4 9.397000 1024.852 1201.000 1.024640 1.264795 -0.4170289 -0.0001273810
368.8 9.784000 1082.242 1301.000 1.024600 1.780250 -0.3571506 0.0001735995
369.2 10.15700 1138.253 1401.000 1.024490 2.090195 -0.4627987 0.0005000866
369.5 10.51900 1193.076 1501.000 1.024430 2.369639 -0.2983032 0.0007220540
369.9 10.87000 1246.931 1601.000 1.024580 2.328948 -0.1773898 0.0006723764
370.2 11.21200 1299.815 1701.000 1.024750 2.220636 0.1934078 0.0005380369
370.6 11.54500 1351.801 1801.000 1.024920 1.925112 0.5713403 0.0003380901
370.9 11.86900 1402.967 1901.000 1.025110 1.324021 0.7221852 0.00005411406
371.2 12.18500 1453.211 2001.000 1.025060 0.6416068 0.7403829 -0.00005090390
371.5 12.49300 1502.702 2101.000 1.025010 -0.3193701 0.4297587 -0.0002124296
371.8 12.79500 1551.242 2201.000 1.024870 -1.068291 0.2690030 -0.0003366737
372.1 13.09100 1598.791 2301.000 1.024530 -1.582730 0.1152088 -0.0003080743
372.4 13.38100 1645.448 2401.000 1.024110 -1.976883 -0.1703368 -0.0002403072
372.7 13.66600 1691.299 2501.000 1.023660 -2.189463 -0.3688638 -0.0001779177
373.0 13.94600 1736.395 2601.000 1.023180 -2.281115 -0.5925412 -0.0001139998
373.2 14.22200 1780.774 2701.000 1.022680 -2.136448 -0.5851681 -0.00005294295
373.5 14.49400 1824.485 2801.000 1.022180 -1.809727 -0.4354921 -0.000007499400
373.8 14.76200 1867.558 2901.000 1.021670 -1.335194 -0.2301391 0.00003982156
374.0 15.02600 1910.038 3001.000 1.021180 -0.7612217 -0.05360750 0.00006665862
374.3 15.28700 1951.953 3101.000 1.020690 0.04553851 0.3992748 0.00009896705
374.6 15.54100 1992.925 3200.000 1.020220 0.8044710 0.4929331 0.0001313514
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250199">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-16.7975528365 37.078235076</math:coefficients>
     <math:minRange>0.48</math:minRange>
     <math:maxRange>3.177</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250251">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>5618.79019928 -4671.12028227 1288.88398692 -114.972649611</math:coefficients>
     <math:minRange>3.177</math:minRange>
     <math:maxRange>4.023</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250196">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-58.9148697798 12.5197684848 12.8325774904 0.0405459456401 -0.00320074657087</math:coefficients>
     <math:minRange>4.023</math:minRange>
     <math:maxRange>15.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501004">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-19.00794454 47.3621081012 10.3127196042 -0.452064073849 0.00827575830208</math:coefficients>
     <math:minRange>0.48</math:minRange>
     <math:maxRange>15.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250115">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>15.541</math:minRange>
     <math:maxRange>15.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501332">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>15.541</math:minRange>
     <math:maxRange>15.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501014">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.996012111455 0.00817796381406 2.71119420358E-4</math:coefficients>
     <math:minRange>0.48</math:minRange>
     <math:maxRange>4.023</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501061">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-1.32192268306 1.57407098128 -0.348541677973 0.0255522779473</math:coefficients>
     <math:minRange>4.023</math:minRange>
     <math:maxRange>4.814</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250393">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.11391638065 -0.0354965889115 0.00497687942514 -2.92255525793E-4 6.05213575E-6</math:coefficients>
     <math:minRange>4.814</math:minRange>
     <math:maxRange>15.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.3000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775381238"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777159696">
   <gml:description>Gelesen aus: PROF0046.4000.txt</gml:description>
   <gml:name>46.4000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930792.961 777121.784 359.094</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296140184">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296140884">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296140652">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296140840">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296140839">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval122252229614090">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296140843">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296140331">
     <gml:description>Übernommen aus Datei: PROF0046.4000.txt</gml:description>
     <gml:name>0046.4000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296140220">
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
     <om:result><![CDATA[359.5 0.4510000 8.633000 1.000000 1.000000 -2.136292 -3.552714E-15 -2.220446E-16
362.3 3.253000 217.1990 101.0000 1.026240 8.303148 0E-7 1.554312E-15
363.1 4.056000 309.0140 201.0000 1.031460 3.144192 -0.1016304 -2.220446E-16
363.9 4.838000 406.7740 301.0000 1.032680 -2.459981 1.084041 -0.00004194094
364.6 5.502000 492.4730 401.0000 1.028880 -4.745353 0.3823933 -0.00008859473
365.2 6.099000 570.3940 501.0000 1.025790 -4.177039 0.04706327 0.0007321655
365.7 6.646000 643.4070 601.0000 1.024280 -2.746551 -0.09037653 0.0009312945
366.2 7.154000 713.8740 701.0000 1.024910 -2.182630 -0.1055456 -0.0004134259
366.7 7.630000 781.0590 801.0000 1.024870 -1.355745 -0.1565986 -0.0007169474
367.2 8.079000 845.4410 901.0000 1.024470 -0.4462431 -0.3837934 -0.0004358808
367.6 8.506000 907.5350 1001.000 1.024190 0.4573748 -0.6063452 -0.0001479410
368.0 8.916000 967.9500 1101.000 1.024290 1.267446 -0.3618990 -0.0001783763
368.4 9.310000 1026.712 1201.000 1.024400 1.943915 0.02214607 -0.0002023064
368.8 9.688000 1083.783 1301.000 1.024020 2.391752 0.01905040 0.0002490060
369.1 10.05300 1139.674 1401.000 1.023970 2.451563 -0.03451401 0.0003354633
369.5 10.40800 1194.461 1501.000 1.024090 2.429225 0.3294326 0.0002038925
369.8 10.75200 1248.189 1601.000 1.024230 2.063153 0.5164165 -0.000003532723
370.2 11.08500 1300.578 1701.000 1.023950 1.580231 0.2157376 0.0001503081
370.5 11.41000 1351.941 1801.000 1.023710 1.093962 0.06410429 0.0002042934
370.8 11.72800 1402.342 1901.000 1.023510 0.6664495 0.1637253 0.0001595677
371.1 12.03900 1451.851 2001.000 1.023330 0.2033679 0.3280336 0.00003980103
371.4 12.34300 1500.513 2101.000 1.023110 -0.3602368 0.3770672 -0.00008954911
371.7 12.64000 1548.227 2201.000 1.022710 -0.9395987 0.1374219 -0.00008164601
372.0 12.93100 1595.072 2301.000 1.022300 -1.467433 -0.2121668 -0.0001001830
372.3 13.21800 1641.078 2401.000 1.021830 -1.663241 -0.1098166 -0.00009030107
372.6 13.49900 1686.301 2501.000 1.021340 -1.908028 -0.3721377 -0.00008030904
372.9 13.77600 1730.792 2601.000 1.020840 -1.936698 -0.4047908 -0.00007459505
373.1 14.04900 1774.598 2701.000 1.020330 -1.797478 -0.3021830 -0.00006396065
373.4 14.31800 1817.762 2801.000 1.019820 -1.533353 -0.1561244 -0.00004884850
373.7 14.58300 1860.314 2901.000 1.019320 -1.173274 -0.05583454 -0.00002947486
373.9 14.84400 1902.291 3001.000 1.018840 -0.7523505 -0.08795289 -0.000005951976
374.2 15.10200 1943.719 3101.000 1.018360 -0.1307716 0.05410730 0.00005002241
374.4 15.35500 1984.227 3200.000 1.017900 0.7395646 0.5123824 0.0001315377
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296140481">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-15.0956459672 35.6887937188</math:coefficients>
     <math:minRange>0.451</math:minRange>
     <math:maxRange>3.253</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296140213">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>7457.49557211 -6112.69595132 1661.65210189 -146.863222596</math:coefficients>
     <math:minRange>3.253</math:minRange>
     <math:maxRange>4.056</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296140439">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-45.3247660541 2.56834316697 14.3791740052 0.00300051622492 -0.00350289570556</math:coefficients>
     <math:minRange>4.056</math:minRange>
     <math:maxRange>15.355</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296140798">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-13.6138677192 39.1208163021 12.3970850971 -0.599251467356 0.0115926525574</math:coefficients>
     <math:minRange>0.451</math:minRange>
     <math:maxRange>15.355</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296140193">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>15.355</math:minRange>
     <math:maxRange>15.355</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296140347">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>15.355</math:minRange>
     <math:maxRange>15.355</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229614050">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.99461091183 0.0123075102492 -7.94484551124E-4</math:coefficients>
     <math:minRange>0.451</math:minRange>
     <math:maxRange>4.056</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296140566">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>1.45850526192 -0.328563386814 0.0826928604388 -0.00681570856251</math:coefficients>
     <math:minRange>4.056</math:minRange>
     <math:maxRange>4.838</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296140667">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.14327722809 -0.0458816493417 0.00643129448857 -3.86294559198E-4 8.30072299233E-6</math:coefficients>
     <math:minRange>4.838</math:minRange>
     <math:maxRange>15.355</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.4000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775397827"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation12126807771591234">
   <gml:description>Gelesen aus: PROF0046.5000.txt</gml:description>
   <gml:name>46.5000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930881.155 777155.892 359.172</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296140670">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296140180">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296140655">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter122252229614037">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296140350">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296140416">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296140105">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296140557">
     <gml:description>Übernommen aus Datei: PROF0046.5000.txt</gml:description>
     <gml:name>0046.5000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296140605">
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
     <om:result><![CDATA[359.6 0.4540000 8.659000 1.000000 1.000000 -1.484846 -5.329071E-15 3.330669E-15
362.6 3.427000 210.5260 101.0000 1.027450 7.071477 0E-7 -1.110223E-14
363.4 4.265000 298.4850 201.0000 1.032390 -0.1982063 0.02345551 1.110223E-15
364.3 5.114000 391.0060 301.0000 1.027100 -3.215953 -0.4028879 -0.0003036953
365.0 5.848000 472.9110 401.0000 1.023540 -2.148244 -0.4297881 0.002932465
365.7 6.507000 549.1480 501.0000 1.023250 0.04839245 0.3107816 0.003177870
366.3 7.105000 624.5220 601.0000 1.027610 -1.254711 0.7085904 -0.001087466
366.8 7.654000 695.0970 701.0000 1.028250 -1.636072 0.3833053 -0.001572817
367.3 8.167000 762.2660 801.0000 1.028580 -1.494466 -0.01231739 -0.001732210
367.8 8.653000 826.9540 901.0000 1.028130 -1.020844 0.08284191 -0.001121539
368.3 9.112000 889.0080 1001.000 1.027560 -0.3989415 -0.2909035 -0.0004188201
368.7 9.551000 949.2950 1101.000 1.027290 0.1994991 -0.4082987 -0.00005305506
369.1 9.972000 1007.883 1201.000 1.026980 0.7893140 -0.4293419 0.0003095047
369.5 10.37700 1064.937 1301.000 1.026640 1.330066 -0.4073279 0.0006556040
369.9 10.76800 1120.753 1401.000 1.026510 1.684293 -0.2976294 0.0007439016
370.3 11.14600 1175.378 1501.000 1.026430 1.845704 -0.2346608 0.0007344759
370.7 11.51300 1228.927 1601.000 1.026350 1.904946 -0.02566821 0.0006780481
371.0 11.87000 1281.479 1701.000 1.026310 1.860316 0.3151980 0.0005360548
371.4 12.21700 1333.177 1801.000 1.026360 1.510029 0.5229704 0.0002610435
371.7 12.55500 1384.035 1901.000 1.026410 0.9378018 0.6423238 -0.00005456165
372.1 12.88400 1433.957 2001.000 1.026350 0.1967541 0.4489773 -0.0002973330
372.4 13.20600 1482.917 2101.000 1.026120 -0.4238017 0.3541732 -0.0004056568
372.7 13.52100 1530.862 2201.000 1.025750 -0.9004023 0.1893058 -0.0004061375
373.0 13.82900 1577.878 2301.000 1.025300 -1.345039 -0.2088508 -0.0003551267
373.3 14.13200 1624.014 2401.000 1.024790 -1.525755 -0.3327535 -0.0002717295
373.6 14.43000 1669.344 2501.000 1.024250 -1.534144 -0.2991797 -0.0001826366
373.9 14.72300 1713.911 2601.000 1.023690 -1.428713 -0.2218360 -0.00009440477
374.2 15.01100 1757.770 2701.000 1.023120 -1.278080 -0.2113178 -0.00001350421
374.5 15.29400 1800.966 2801.000 1.022550 -1.139157 -0.3750792 0.00005363833
374.7 15.57400 1843.538 2901.000 1.021990 -0.7542102 -0.09588871 0.00009684715
375.0 15.85000 1885.516 3001.000 1.021450 -0.3145040 0.1899254 0.0001112801
375.3 16.12100 1926.935 3101.000 1.020920 -0.01625459 0.02740608 0.0001123318
375.6 16.38700 1967.412 3200.000 1.020410 0.5167189 0.3202862 0.00009349453
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296140195">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-14.2707702657 33.636057854</math:coefficients>
     <math:minRange>0.454</math:minRange>
     <math:maxRange>3.427</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156661">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>8171.99680935 -6381.59618804 1652.81465416 -139.447711139</math:coefficients>
     <math:minRange>3.427</math:minRange>
     <math:maxRange>4.265</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156508">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-42.896443802 8.39393073816 11.086153226 0.102280180944 -0.00445735050333</math:coefficients>
     <math:minRange>4.265</math:minRange>
     <math:maxRange>16.387</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156736">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-12.556720267 39.3424956917 9.22265118523 -0.339428635765 0.00489291228252</math:coefficients>
     <math:minRange>0.454</math:minRange>
     <math:maxRange>16.387</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156261">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.387</math:minRange>
     <math:maxRange>16.387</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156419">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>16.387</math:minRange>
     <math:maxRange>16.387</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156363">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.994445375121 0.0126325217037 -8.75914409363E-4</math:coefficients>
     <math:minRange>0.454</math:minRange>
     <math:maxRange>4.265</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156532">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-1.51937821254 1.63689504879 -0.347957305267 0.024488275744</math:coefficients>
     <math:minRange>4.265</math:minRange>
     <math:maxRange>5.114</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156372">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.04811139324 -0.00946159144207 0.0014349771866 -8.70717061481E-5 1.73699222247E-6</math:coefficients>
     <math:minRange>5.114</math:minRange>
     <math:maxRange>16.387</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.5000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775397603"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation12126807771597">
   <gml:description>Gelesen aus: PROF0046.6000.txt</gml:description>
   <gml:name>46.6000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3930981.771 777180.843 359.309</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState12225222962181113">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296218443">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition12225222962181184">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296218685">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions122252229621818">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296218370">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes12225222962181150">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296218278">
     <gml:description>Übernommen aus Datei: PROF0046.6000.txt</gml:description>
     <gml:name>0046.6000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296218838">
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
     <om:result><![CDATA[359.8 0.5280000 8.079000 1.000000 1.000000 1.421085E-14 -7.105427E-15 0E-7
363.2 3.874000 161.3670 101.0000 1.028250 0E-7 0E-7 0E-7
363.9 4.591000 297.9990 201.0000 1.045920 -0.02479793 -0.04890131 -0.0001854636
364.7 5.422000 391.8850 301.0000 1.036960 -0.3295976 0.2173625 0.001240867
365.4 6.136000 475.3480 401.0000 1.031640 0.5580963 0.2720860 0.002241009
366.1 6.774000 552.9390 501.0000 1.030290 1.296195 0.5251756 0.001023084
366.7 7.353000 627.6290 601.0000 1.031150 0.0005152769 0.3321617 -0.001364497
367.2 7.887000 697.8130 701.0000 1.030390 -0.6372887 -0.2160277 -0.001508781
367.7 8.389000 764.9190 801.0000 1.029600 -0.8213570 -0.2903973 -0.001251457
368.2 8.862000 829.1960 901.0000 1.028950 -0.7464053 -0.4759345 -0.0009130317
368.6 9.313000 891.4240 1001.000 1.028320 -0.5107712 -0.2313945 -0.0004765909
369.1 9.742000 951.4570 1101.000 1.027590 -0.1868677 -0.3234011 0.0001118495
369.5 10.15300 1009.801 1201.000 1.026960 0.09761480 -0.4839393 0.0006067702
369.9 10.55000 1066.753 1301.000 1.026560 0.4718327 -0.2537811 0.0008480133
370.2 10.93300 1122.502 1401.000 1.026420 0.6255215 -0.06564330 0.0007873461
370.6 11.30400 1177.068 1501.000 1.026300 0.7244183 0.2163111 0.0006533641
371.0 11.66400 1230.539 1601.000 1.026130 0.7370044 0.5298630 0.0005106408
371.3 12.01300 1283.047 1701.000 1.026010 0.4501853 0.5684998 0.0002590041
371.7 12.35300 1334.516 1801.000 1.025770 0.1682936 0.6347562 0.00006927529
372.0 12.68400 1384.967 1901.000 1.025480 -0.1916802 0.4911582 -0.0001233592
372.3 13.00700 1434.387 2001.000 1.025090 -0.5209323 0.2216722 -0.0002633084
372.6 13.32300 1482.687 2101.000 1.024490 -0.6273663 -0.05381838 -0.0002340740
372.9 13.63300 1530.048 2201.000 1.023880 -0.5828893 -0.1820767 -0.0002285382
373.2 13.93700 1576.509 2301.000 1.023230 -0.4659526 -0.3104855 -0.0002069914
373.5 14.23600 1622.137 2401.000 1.022560 -0.2264323 -0.2440317 -0.0001811749
373.8 14.52900 1666.989 2501.000 1.021900 -0.1079899 -0.4466664 -0.0001680663
374.1 14.81800 1711.118 2601.000 1.021240 0.1126811 -0.3538992 -0.0001508715
374.4 15.10300 1754.568 2701.000 1.020590 0.3630264 -0.06212838 -0.0001280959
374.7 15.38300 1797.378 2801.000 1.019960 0.4243243 -0.02519813 -0.00009584371
375.0 15.65900 1839.585 2901.000 1.019360 0.3883198 0.01515058 -0.00005448955
375.2 15.93100 1881.221 3001.000 1.018770 0.2001292 -0.02947165 0.00002784363
375.5 16.20000 1922.315 3101.000 1.018210 -0.03876047 0.1297954 0.0001414057
375.8 16.46300 1962.490 3200.000 1.017670 -0.4254838 0.2855130 0.0003124067
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962181207">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-14.7800358637 29.8864315601</math:coefficients>
     <math:minRange>0.528</math:minRange>
     <math:maxRange>3.874</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218612">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>20871.9014658 -14803.2958103 3482.66813407 -269.870088232</math:coefficients>
     <math:minRange>3.874</math:minRange>
     <math:maxRange>4.591</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218669">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-42.5491879145 -2.88388662113 11.8066376644 0.105214239382 -0.00516092338902</math:coefficients>
     <math:minRange>4.591</math:minRange>
     <math:maxRange>16.463</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218823">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-16.1099013748 45.8123132098</math:coefficients>
     <math:minRange>0.528</math:minRange>
     <math:maxRange>3.874</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218613">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>33259.3167162 -23721.2185673 5618.42831122 -438.981764748</math:coefficients>
     <math:minRange>3.874</math:minRange>
     <math:maxRange>4.591</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962181024">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>-123.542536897 73.726985811 3.93307937684 0.0187968581316 -0.00378471211003</math:coefficients>
     <math:minRange>4.591</math:minRange>
     <math:maxRange>16.463</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962181061">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.995542139869 0.00844291691572</math:coefficients>
     <math:minRange>0.528</math:minRange>
     <math:maxRange>3.874</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218895">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>8.2055279585 -5.21157456461 1.25365761224 -0.0997991920625</math:coefficients>
     <math:minRange>3.874</math:minRange>
     <math:maxRange>4.591</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218945">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.17601552615 -0.0524885580339 0.00688302868227 -3.92222506463E-4 8.04087727112E-6</math:coefficients>
     <math:minRange>4.591</math:minRange>
     <math:maxRange>16.463</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.6000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775414191"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777176684">
   <gml:description>Gelesen aus: PROF0046.7000.txt</gml:description>
   <gml:name>46.7000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3931071.328 777227.814 361.199</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState12225222962501124">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296250680">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition12225222962501268">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296250741">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296250247">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval122252229625014">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296250188">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296250335">
     <gml:description>Übernommen aus Datei: PROF0046.7000.txt</gml:description>
     <gml:name>0046.7000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296250505">
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
     <om:result><![CDATA[361.7 0.5200000 9.034000 1.000000 1.000000 -1.698518 -1.065814E-14 0E-7
364.1 2.941000 200.2410 101.0000 1.055930 6.204486 0E-7 0E-7
365.2 3.953000 313.8580 201.0000 1.030570 -0.2490499 -0.07264018 -0.0001766835
366.0 4.761000 407.8990 301.0000 1.023380 -0.7465133 0.4599649 0.003073808
366.7 5.456000 494.2750 401.0000 1.024410 -1.803323 0.7525544 -0.00006357714
367.3 6.072000 573.9700 501.0000 1.024090 -2.612284 0.2343263 -0.0008722313
367.8 6.636000 648.3930 601.0000 1.023830 -2.451577 -0.04045328 -0.001199452
368.4 7.159000 718.6090 701.0000 1.023050 -1.734795 -0.2607765 -0.0006952954
368.8 7.649000 785.6940 801.0000 1.022660 -1.000100 -0.4707664 -0.0004096552
369.3 8.113000 850.0500 901.0000 1.022180 -0.06369521 -0.4178414 0.00004974436
369.8 8.554000 912.0390 1001.000 1.021660 0.8543022 -0.3746589 0.0005767506
370.2 8.975000 972.1920 1101.000 1.021430 1.436247 -0.4250452 0.0008049191
370.6 9.380000 1030.780 1201.000 1.021530 1.825960 -0.2318516 0.0006709434
371.0 9.771000 1087.900 1301.000 1.021630 2.093550 0.2072344 0.0004901996
371.3 10.14700 1143.659 1401.000 1.021640 1.882996 0.1818438 0.0003453489
371.7 10.51300 1198.355 1501.000 1.021910 1.554283 0.6422162 -0.0001185567
372.1 10.86700 1251.833 1601.000 1.022000 0.9010593 0.7160475 -0.0004603489
372.4 11.21000 1303.899 1701.000 1.021720 0.2116032 0.3652241 -0.0004873261
372.7 11.54500 1354.631 1801.000 1.021260 -0.1856684 0.2008215 -0.0003875595
373.1 11.87200 1404.188 1901.000 1.020710 -0.4838627 0.006274127 -0.0002454984
373.4 12.19200 1452.669 2001.000 1.020130 -0.6595141 -0.1109754 -0.0001157398
373.7 12.50500 1500.154 2101.000 1.019540 -0.8157448 -0.3310085 -0.00001084377
374.0 12.81300 1546.733 2201.000 1.018950 -0.7587401 -0.1704524 0.00006392347
374.3 13.11500 1592.461 2301.000 1.018370 -0.7100255 -0.09391333 0.0001085166
374.6 13.41100 1637.399 2401.000 1.017800 -0.7431874 -0.2451679 0.0001318155
374.9 13.70300 1681.596 2501.000 1.017260 -0.6136198 -0.07097282 0.0001190984
375.2 13.99000 1725.095 2601.000 1.016740 -0.5237410 -0.02333725 0.00009139568
375.5 14.27200 1767.942 2701.000 1.016250 -0.5255024 -0.2158904 0.00004800258
375.8 14.55100 1810.178 2801.000 1.015770 -0.3602754 -0.03513333 0.00001455820
376.0 14.82600 1851.827 2901.000 1.015330 -0.2068798 0.07593701 -0.00002753117
376.3 15.09700 1892.930 3001.000 1.014910 -0.1080445 0.02844655 -0.00004880781
376.6 15.36500 1933.501 3101.000 1.014510 0.07361274 0.1120476 -0.00004131325
376.8 15.62700 1973.170 3200.000 1.014130 0.2520702 0.1164277 0.000008179552
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501027">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-20.4787277984 41.3052457662</math:coefficients>
     <math:minRange>0.52</math:minRange>
     <math:maxRange>2.941</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250261">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>1889.39036631 -1616.44091745 464.912940987 -41.5004382217</math:coefficients>
     <math:minRange>2.941</math:minRange>
     <math:maxRange>3.953</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501198">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-39.2812322177 7.93017678094 13.2708589151 0.0433521599582 -0.00487550167615</math:coefficients>
     <math:minRange>3.953</math:minRange>
     <math:maxRange>15.627</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250898">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-22.6066289579 52.5982154471 9.80478694243 -0.43168983667 0.00716213649274</math:coefficients>
     <math:minRange>0.52</math:minRange>
     <math:maxRange>15.627</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501070">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>15.627</math:minRange>
     <math:maxRange>15.627</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250618">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>15.627</math:minRange>
     <math:maxRange>15.627</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501067">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.987986947542 0.023102023957</math:coefficients>
     <math:minRange>0.52</math:minRange>
     <math:maxRange>2.941</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250745">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-1.67895511945 2.40688552935 -0.696063541587 0.065917721777</math:coefficients>
     <math:minRange>2.941</math:minRange>
     <math:maxRange>3.953</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250194">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.09159920573 -0.0283667005672 0.00422900938758 -2.69260256345E-4 6.04725244661E-6</math:coefficients>
     <math:minRange>3.953</math:minRange>
     <math:maxRange>15.627</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.7000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775446666"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777176308">
   <gml:description>Gelesen aus: PROF0046.8000.txt</gml:description>
   <gml:name>46.8000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3931129.447 777312.963 362.984</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296281837">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation12225222962811216">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition12225222962811172">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter12225222962811016">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions12225222962811049">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296281605">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes122252229628162">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation12225222962811330">
     <gml:description>Übernommen aus Datei: PROF0046.8000.txt</gml:description>
     <gml:name>0046.8000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296281626">
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
     <om:result><![CDATA[363.4 0.4550000 20.57400 1.000000 1.006040 -0.009517423 -1.093826 0E-7
365.2 2.247000 207.8560 101.0000 1.020940 -0.1392668 2.332373 0E-7
366.2 3.209000 319.4030 201.0000 1.015990 0.6856308 0.8217998 0.0001596006
367.0 3.980000 414.8880 301.0000 1.018800 0.4340324 0.1575445 -0.001145913
367.6 4.642000 501.0430 401.0000 1.019640 -0.6649856 -0.6527322 -0.001072291
368.2 5.237000 580.1080 501.0000 1.020000 -0.9423776 -0.9247410 -0.0008664471
368.8 5.782000 653.8570 601.0000 1.019530 -0.7614428 -1.063444 -0.00006192874
369.3 6.289000 723.6360 701.0000 1.019090 -0.3903708 -1.064742 0.0005513917
369.8 6.766000 790.2760 801.0000 1.018800 0.06774532 -0.9012168 0.0008976281
370.2 7.218000 854.3420 901.0000 1.018610 0.4749039 -0.6829640 0.001056161
370.6 7.650000 916.3380 1001.000 1.018750 0.8329303 -0.1902406 0.0008172148
371.0 8.064000 976.4790 1101.000 1.018860 1.054352 0.3353720 0.0005554807
371.4 8.461000 1034.949 1201.000 1.018900 0.9682353 0.5274669 0.0003223906
371.8 8.845000 1092.064 1301.000 1.019120 0.7406298 0.8984452 -0.0001251363
372.2 9.216000 1147.830 1401.000 1.019270 0.2793487 1.036943 -0.0005303978
372.6 9.576000 1202.211 1501.000 1.019270 -0.1542321 1.121683 -0.0008095003
372.9 9.925000 1255.149 1601.000 1.018920 -0.5634237 0.8370324 -0.0007583406
373.2 10.26500 1306.786 1701.000 1.018510 -0.8410778 0.4791752 -0.0006654692
373.6 10.59800 1357.211 1801.000 1.018020 -0.8167122 0.4166300 -0.0005105867
373.9 10.92300 1406.532 1901.000 1.017490 -0.7869157 0.1530065 -0.0003310520
374.2 11.24100 1454.840 2001.000 1.016940 -0.7236762 -0.1916702 -0.0001466457
374.5 11.55400 1502.213 2101.000 1.016390 -0.4287980 -0.1402729 0.00002103840
374.8 11.86000 1548.715 2201.000 1.015850 -0.2957277 -0.4749091 0.0001649330
375.1 12.16200 1594.427 2301.000 1.015320 0.02969898 -0.3465276 0.0002813571
375.4 12.45800 1639.385 2401.000 1.014820 0.1871334 -0.5321200 0.0003530022
375.7 12.75000 1683.633 2501.000 1.014330 0.4200325 -0.4667556 0.0003968106
376.0 13.03700 1727.234 2601.000 1.013870 0.4975812 -0.5900781 0.0003939421
376.3 13.32000 1770.205 2701.000 1.013430 0.5412181 -0.6473923 0.0003523461
376.6 13.60000 1812.589 2801.000 1.013020 0.6471249 -0.3602359 0.0002595582
376.9 13.87600 1854.411 2901.000 1.012630 0.6270683 -0.1587924 0.0001265625
377.1 14.14800 1895.712 3001.000 1.012250 0.4302899 -0.1197353 -0.00003733319
377.4 14.41700 1936.506 3101.000 1.011900 0.1848660 0.05773108 -0.0002549910
377.7 14.68000 1976.423 3200.000 1.011570 -0.1980372 0.1783992 -0.0005105897
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296281158">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-8.85505846937 11.1727495323 17.8981104302 -0.295537837702 0.00264609229432</math:coefficients>
     <math:minRange>0.455</math:minRange>
     <math:maxRange>14.68</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296281498">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.68</math:minRange>
     <math:maxRange>14.68</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962811105">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.68</math:minRange>
     <math:maxRange>14.68</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962811180">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-21.7627249445 90.5246731413 5.58606023468 -0.191498778355 0.00153089846597</math:coefficients>
     <math:minRange>0.455</math:minRange>
     <math:maxRange>14.68</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296281235">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.68</math:minRange>
     <math:maxRange>14.68</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962811029">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.68</math:minRange>
     <math:maxRange>14.68</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296281587">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.00225679687 0.00831473214286</math:coefficients>
     <math:minRange>0.455</math:minRange>
     <math:maxRange>2.247</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962811022">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.572413636998 0.503113457574 -0.185005808558 0.0222233299133</math:coefficients>
     <math:minRange>2.247</math:minRange>
     <math:maxRange>3.209</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296281586">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.00276608265 0.00646533800587 -8.5670673618E-4 4.78110472648E-5 -1.14659450337E-6</math:coefficients>
     <math:minRange>3.209</math:minRange>
     <math:maxRange>14.68</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.8000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775446106"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777176982">
   <gml:description>Gelesen aus: PROF0046.9000.txt</gml:description>
   <gml:name>46.9000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3931211.262 777372.837 362.748</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296187192">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296187453">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296187644">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter12225222961871112">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296203810">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval122252229620310">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296203912">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation12225222962031004">
     <gml:description>Übernommen aus Datei: PROF0046.9000.txt</gml:description>
     <gml:name>0046.9000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296203273">
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
     <om:result><![CDATA[363.3 0.5330000 17.20600 1.000000 1.000000 -1.053902 -3.552714E-15 -1.110223E-16
365.3 2.510000 207.4800 101.0000 1.032600 2.960414 0E-7 0E-7
366.2 3.479000 321.1240 201.0000 1.020500 0.8720638 -0.07241953 0.0001248687
367.0 4.252000 417.4880 301.0000 1.020750 -0.2171021 0.7062466 -0.0007112727
367.7 4.914000 504.3930 401.0000 1.020750 -1.688932 0.4280035 -0.001034361
368.3 5.508000 583.9210 501.0000 1.020400 -1.913217 0.2404299 -0.0008793206
368.8 6.051000 657.9650 601.0000 1.019350 -1.528278 -0.1215132 0.00004079186
369.3 6.556000 728.1240 701.0000 1.018940 -0.9983636 -0.3879880 0.0003500043
369.8 7.031000 795.0580 801.0000 1.018520 -0.2975095 -0.4715279 0.0006770018
370.2 7.481000 859.3210 901.0000 1.018200 0.4220114 -0.4476811 0.0008985434
370.7 7.910000 921.4260 1001.000 1.018090 0.9939916 -0.3149726 0.0008961039
371.1 8.321000 981.6540 1101.000 1.018120 1.400020 -0.08671202 0.0007342282
371.5 8.716000 1040.359 1201.000 1.018430 1.443808 0.1328730 0.0002696413
371.8 9.097000 1097.602 1301.000 1.018700 1.252810 0.3521530 -0.0001796650
372.2 9.465000 1153.428 1401.000 1.018770 0.8439258 0.4044540 -0.0004543591
372.6 9.822000 1207.936 1501.000 1.018840 0.3484143 0.4657537 -0.0007550837
372.9 10.16800 1261.004 1601.000 1.018620 -0.1698544 0.2121786 -0.0007908242
373.3 10.50600 1312.680 1701.000 1.018210 -0.3503445 0.2355195 -0.0006625738
373.6 10.83500 1363.126 1801.000 1.017720 -0.5461320 0.002205823 -0.0004780635
373.9 11.15700 1412.450 1901.000 1.017200 -0.5922887 -0.09784543 -0.0002874680
374.2 11.47200 1460.745 2001.000 1.016660 -0.6086519 -0.2511728 -0.00009907631
374.5 11.78100 1508.095 2101.000 1.016120 -0.5485109 -0.3099254 0.00006781789
374.8 12.08500 1554.572 2201.000 1.015600 -0.3491848 -0.09426469 0.0001937626
375.1 12.38300 1600.231 2301.000 1.015100 -0.2351017 -0.07199547 0.0002819769
375.4 12.67600 1645.136 2401.000 1.014610 -0.1300195 -0.04051970 0.0003431344
375.7 12.96400 1689.322 2501.000 1.014150 -0.08000533 -0.1193472 0.0003592676
376.0 13.24800 1732.835 2601.000 1.013710 0.01335712 -0.06865749 0.0003408368
376.3 13.52800 1775.716 2701.000 1.013290 0.1014723 0.01597236 0.0002898045
376.6 13.80400 1818.003 2801.000 1.012890 0.1402068 0.04211622 0.0002081911
376.8 14.07700 1859.725 2901.000 1.012520 0.2489107 0.2900134 0.00008622579
377.1 14.34500 1900.914 3001.000 1.012170 0.08551398 -0.06113708 -0.00006040439
377.4 14.61100 1941.588 3101.000 1.011830 0.08846207 0.02720961 -0.0002252297
377.6 14.87100 1981.380 3200.000 1.011520 0.01150207 -0.03051290 -0.0004185798
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203707">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-25.9600404654 50.5816894284</math:coefficients>
     <math:minRange>0.533</math:minRange>
     <math:maxRange>2.51</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203537">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>1164.1651036 -1141.62014514 383.244357599 -38.7128691387</math:coefficients>
     <math:minRange>2.51</math:minRange>
     <math:maxRange>3.479</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203466">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-35.3109461763 15.29475599 15.4289892588 -0.0776556408428 -0.0030436678538</math:coefficients>
     <math:minRange>3.479</math:minRange>
     <math:maxRange>14.871</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203898">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-27.1524236153 77.1969001752 7.77829110615 -0.340663946578 0.00533142399782</math:coefficients>
     <math:minRange>0.533</math:minRange>
     <math:maxRange>14.871</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962031110">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.871</math:minRange>
     <math:maxRange>14.871</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962031074">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.871</math:minRange>
     <math:maxRange>14.871</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203809">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.991211026808 0.0164896307537</math:coefficients>
     <math:minRange>0.533</math:minRange>
     <math:maxRange>2.51</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203471">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-0.136280961141 1.18506300907 -0.394241741447 0.0428839840504</math:coefficients>
     <math:minRange>2.51</math:minRange>
     <math:maxRange>3.479</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203643">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.02868217527 -0.00420754384418 7.05321397013E-4 -5.03338863368E-5 1.11524109657E-6</math:coefficients>
     <math:minRange>3.479</math:minRange>
     <math:maxRange>14.871</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>46.9000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775463827"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777176703">
   <gml:description>Gelesen aus: PROF0047.0000.txt</gml:description>
   <gml:name>47.0000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3931275.948 777450.32 362.535</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState12225222962501253">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296250283">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296250657">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296250910">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296250723">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296250306">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296250336">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296250868">
     <gml:description>Übernommen aus Datei: PROF0047.0000.txt</gml:description>
     <gml:name>0047.0000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition12225222962501297">
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
     <om:result><![CDATA[363.1 0.5740000 14.98800 1.000000 1.000000 -1.423240 3.552714E-15 0E-7
365.2 2.618000 206.0500 101.0000 1.044130 4.179198 0E-7 0E-7
366.1 3.586000 320.8330 201.0000 1.026920 1.216197 -0.07913676 0.0001597860
366.9 4.355000 418.6230 301.0000 1.026280 -0.7667381 0.7450509 -0.001293452
367.5 5.013000 506.2460 401.0000 1.024740 -2.156113 0.5586380 -0.001068902
368.1 5.601000 586.4740 501.0000 1.023370 -2.402902 0.1820015 -0.0005846304
368.7 6.139000 661.3240 601.0000 1.022000 -1.933398 -0.1590465 0.0001599794
369.2 6.639000 732.2470 701.0000 1.021310 -1.228290 -0.3510647 0.0003881260
369.6 7.108000 799.8880 801.0000 1.020580 -0.4135255 -0.5254259 0.0007602400
370.1 7.552000 864.8940 901.0000 1.020080 0.3936234 -0.5652738 0.0009662276
370.5 7.976000 927.8640 1001.000 1.020150 1.083691 -0.2277501 0.0006385155
370.9 8.381000 988.8840 1101.000 1.020100 1.526900 -0.001798481 0.0004494273
371.3 8.769000 1048.114 1201.000 1.019840 1.710840 -0.02674561 0.0004757502
371.7 9.144000 1106.063 1301.000 1.020030 1.618908 0.2194097 0.00004664569
372.0 9.507000 1162.761 1401.000 1.020330 1.281541 0.5929412 -0.0005044718
372.4 9.857000 1218.000 1501.000 1.020290 0.6717162 0.4273147 -0.0007307197
372.7 10.19700 1271.873 1601.000 1.020060 0.09911850 0.2331675 -0.0007866859
373.1 10.52900 1324.388 1701.000 1.019680 -0.1818820 0.3297413 -0.0007156304
373.4 10.85200 1375.616 1801.000 1.019190 -0.4426291 0.1711580 -0.0005572950
373.7 11.16700 1425.678 1901.000 1.018640 -0.6831207 -0.1674030 -0.0003625317
374.0 11.47600 1474.675 2001.000 1.018050 -0.7179237 -0.2452601 -0.0001531844
374.3 11.77900 1522.696 2101.000 1.017460 -0.6608568 -0.2265019 0.00003140369
374.6 12.07600 1569.806 2201.000 1.016880 -0.5981909 -0.2692023 0.0001823503
374.9 12.36800 1616.078 2301.000 1.016310 -0.4620225 -0.1796468 0.0002995540
375.2 12.65500 1661.567 2401.000 1.015760 -0.3224328 -0.08642695 0.0003745208
375.5 12.93700 1706.320 2501.000 1.015230 -0.2391478 -0.1136523 0.0004090253
375.8 13.21500 1750.387 2601.000 1.014720 -0.1138212 -0.01821381 0.0004031568
376.0 13.48900 1793.800 2701.000 1.014250 0.01264889 0.1003423 0.0003388048
376.3 13.75900 1836.609 2801.000 1.013790 0.08293590 0.1456348 0.0002480198
376.6 14.02500 1878.836 2901.000 1.013360 0.06914314 0.02444115 0.0001129816
376.8 14.28800 1920.508 3001.000 1.012950 0.09815400 0.02925125 -0.00005627112
377.1 14.54800 1961.661 3101.000 1.012560 0.1300485 0.08658091 -0.0002575941
377.3 14.80200 2001.911 3200.000 1.012200 0.07144629 -0.04916693 -0.0004916482
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250997">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-27.0821917808 48.9236790607</math:coefficients>
     <math:minRange>0.574</math:minRange>
     <math:maxRange>2.618</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250298">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>1343.55561424 -1273.1679734 410.065602454 -40.1235769947</math:coefficients>
     <math:minRange>2.618</math:minRange>
     <math:maxRange>3.586</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501283">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-38.8729194467 13.3478025006 14.9889461374 4.64050608358E-4 -0.00508955932492</math:coefficients>
     <math:minRange>3.586</math:minRange>
     <math:maxRange>14.802</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501203">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-30.2053377186 71.3368123337 8.78296782344 -0.378827726975 0.00584315820917</math:coefficients>
     <math:minRange>0.574</math:minRange>
     <math:maxRange>14.802</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501129">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.802</math:minRange>
     <math:maxRange>14.802</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229625088">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.802</math:minRange>
     <math:maxRange>14.802</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250772">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.987607328767 0.0215900195695</math:coefficients>
     <math:minRange>0.574</math:minRange>
     <math:maxRange>2.618</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296250848">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-0.696999670994 1.70089781265 -0.545531961734 0.0572468928532</math:coefficients>
     <math:minRange>2.618</math:minRange>
     <math:maxRange>3.586</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962501260">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.0498557204 -0.010498928646 0.00145089014684 -8.852428012E-5 1.80113588957E-6</math:coefficients>
     <math:minRange>3.586</math:minRange>
     <math:maxRange>14.802</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>47.0000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775479357"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777176240">
   <gml:description>Gelesen aus: PROF0047.1000.txt</gml:description>
   <gml:name>47.1000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3931330.728 777535.229 362.399</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState122252229615681">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296156294">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296156272">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296156340">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296156754">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296156946">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296156698">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296156961">
     <gml:description>Übernommen aus Datei: PROF0047.1000.txt</gml:description>
     <gml:name>0047.1000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296156548">
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
     <om:result><![CDATA[362.9 0.4970000 10.72100 1.000000 1.000000 -2.905553 1.065814E-14 0E-7
365.1 2.736000 211.1180 101.0000 1.073570 9.923988 0E-7 0E-7
366.0 3.634000 332.8810 201.0000 1.042390 2.387286 -0.09422886 0.00007094594
366.7 4.339000 435.5110 301.0000 1.034820 -2.235915 0.8258751 -0.0005045256
367.3 4.942000 526.2280 401.0000 1.029700 -4.409108 0.6248488 -0.0005315111
367.9 5.482000 608.7830 501.0000 1.025930 -4.626473 0.2797514 -0.0002163168
368.4 5.976000 685.5770 601.0000 1.023210 -4.005452 -0.2408273 0.0001187703
368.8 6.438000 758.2460 701.0000 1.021790 -2.772109 -0.1723903 -0.0001503332
369.3 6.871000 827.2510 801.0000 1.020310 -1.421080 -0.3415445 0.0001257570
369.7 7.281000 893.3410 901.0000 1.019010 -0.08775704 -0.5302203 0.0005511487
370.1 7.672000 957.1710 1001.000 1.018310 0.9795992 -0.6488399 0.0006011548
370.4 8.048000 1019.076 1101.000 1.018020 1.931580 -0.3565733 0.0003911251
370.8 8.410000 1079.228 1201.000 1.017830 2.632640 0.05944901 0.0001802800
371.2 8.758000 1137.788 1301.000 1.017630 2.821779 0.09568792 0.00004172895
371.5 9.095000 1194.876 1401.000 1.017380 2.811438 0.1851744 -0.00001420999
371.8 9.423000 1250.830 1501.000 1.017390 2.549634 0.5863967 -0.0003184173
372.1 9.741000 1305.586 1601.000 1.017360 1.891628 0.6888203 -0.0005834960
372.4 10.05000 1358.870 1701.000 1.017010 1.253141 0.5183767 -0.0005393968
372.8 10.35100 1410.860 1801.000 1.016520 0.6074077 0.1482421 -0.0003726902
373.0 10.64600 1461.708 1901.000 1.016030 0.1319507 0.03706598 -0.0002286578
373.3 10.93500 1511.503 2001.000 1.015510 -0.2681334 0.01276901 -0.00007848200
373.6 11.21800 1560.309 2101.000 1.014990 -0.6585360 -0.08959394 0.00004848516
373.9 11.49500 1608.210 2201.000 1.014480 -1.121536 -0.4280468 0.0001443939
374.2 11.76800 1655.268 2301.000 1.013980 -1.371591 -0.4148131 0.0002093189
374.4 12.03700 1701.543 2401.000 1.013500 -1.460732 -0.1574502 0.0002370656
374.7 12.30100 1747.076 2501.000 1.013040 -1.592123 -0.1417088 0.0002341132
375.0 12.56100 1791.913 2601.000 1.012590 -1.628777 -0.09772252 0.0002141903
375.2 12.81700 1836.099 2701.000 1.012170 -1.603060 -0.1217220 0.0001632355
375.5 13.07000 1879.662 2801.000 1.011770 -1.356033 0.09228908 0.00009577849
375.7 13.31900 1922.642 2901.000 1.011390 -1.085872 0.06656221 0.00002040875
376.0 13.56500 1965.070 3001.000 1.011040 -0.6342972 0.1235774 -0.00006765722
376.2 13.80800 2006.971 3101.000 1.010700 -0.009405072 0.1946339 -0.0001410776
376.4 14.04500 2047.956 3200.000 1.010380 0.6664339 -0.04423631 -0.0001977504
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156889">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-21.1974095578 44.662795891</math:coefficients>
     <math:minRange>0.497</math:minRange>
     <math:maxRange>2.736</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156598">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>2118.00385237 -1954.99230147 604.420605244 -58.2325925131</math:coefficients>
     <math:minRange>2.736</math:minRange>
     <math:maxRange>3.634</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156869">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>-31.8173884804 -5.81920374717 20.0474294375 -0.224945396212 -4.5927073428E-4</math:coefficients>
     <math:minRange>3.634</math:minRange>
     <math:maxRange>14.045</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156968">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-21.5336844772 51.2305048544 16.2269910997 -0.994274487094 0.0232403829039</math:coefficients>
     <math:minRange>0.497</math:minRange>
     <math:maxRange>14.045</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156255">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.045</math:minRange>
     <math:maxRange>14.045</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156649">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>14.045</math:minRange>
     <math:maxRange>14.045</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156925">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.983669365788 0.032858418937</math:coefficients>
     <math:minRange>0.497</math:minRange>
     <math:maxRange>2.736</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156381">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-2.57218280227 3.45582160955 -1.07710733576 0.110030657165</math:coefficients>
     <math:minRange>2.736</math:minRange>
     <math:maxRange>3.634</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296156436">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.14319303234 -0.0464993585055 0.00652074855652 -4.05698390932E-4 9.1945808562E-6</math:coefficients>
     <math:minRange>3.634</math:minRange>
     <math:maxRange>14.045</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>47.1000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775479843"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1212680777176456">
   <gml:description>Gelesen aus: PROF0047.2000.txt</gml:description>
   <gml:name>47.2000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3931373.91 777626.618 362.532</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296203894">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation1222522296203324">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296203699">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter1222522296203278">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296203294">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval1222522296203447">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296203587">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296203886">
     <gml:description>Übernommen aus Datei: PROF0047.2000.txt</gml:description>
     <gml:name>0047.2000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296203846">
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
     <om:result><![CDATA[362.9 0.4140000 11.79700 1.000000 1.000000 -2.318251 -1.065814E-14 0E-7
365.2 2.646000 212.6940 101.0000 1.078640 7.730457 0E-7 0E-7
366.1 3.538000 335.9050 201.0000 1.042530 1.982144 -0.04001632 0.00009825002
366.8 4.239000 440.7240 301.0000 1.037530 -1.286994 0.4600075 -0.001075077
367.4 4.833000 534.0100 401.0000 1.032670 -3.513810 0.1833076 -0.0005667963
367.9 5.363000 618.6070 501.0000 1.028750 -3.762668 0.003829756 0.00005816740
368.4 5.847000 697.1460 601.0000 1.025880 -3.227410 -0.2140791 0.0003404139
368.8 6.298000 771.1720 701.0000 1.023830 -2.167605 -0.05716381 0.0002950546
369.3 6.721000 841.4940 801.0000 1.022080 -1.093882 -0.008996947 0.0003218071
369.7 7.121000 908.8090 901.0000 1.020610 -0.2221276 -0.1100852 0.0003504297
370.0 7.502000 973.6650 1001.000 1.019570 0.3332589 -0.2983955 0.0001657688
370.4 7.869000 1036.197 1101.000 1.018680 1.125259 0.03641353 -0.000005061966
370.8 8.221000 1096.656 1201.000 1.017840 1.608664 0.04226085 -0.00008776010
371.1 8.561000 1155.320 1301.000 1.017060 1.937135 0.06529042 -0.0001232060
371.4 8.890000 1212.294 1401.000 1.016230 2.118447 -0.01272507 -0.00002166683
371.7 9.210000 1267.908 1501.000 1.015620 2.128680 0.07110594 -0.00007172976
372.1 9.521000 1322.265 1601.000 1.015140 1.838000 0.01966896 -0.0001947923
372.4 9.825000 1375.419 1701.000 1.014680 1.524356 0.2205406 -0.0002939308
372.7 10.12100 1427.276 1801.000 1.014080 1.100923 0.1152564 -0.0002148159
372.9 10.41100 1478.028 1901.000 1.013530 0.7230749 0.1733045 -0.0001571006
373.2 10.69400 1527.743 2001.000 1.012990 0.1543701 -0.1339635 -0.00008403927
373.5 10.97300 1576.499 2101.000 1.012470 -0.1526263 0.09649031 -0.00001452516
373.8 11.24600 1624.364 2201.000 1.011970 -0.6001249 0.01004577 0.00005152731
374.0 11.51400 1671.389 2301.000 1.011500 -1.049578 -0.1672785 0.00009997958
374.3 11.77800 1717.645 2401.000 1.011050 -1.379881 -0.1776617 0.0001373158
374.6 12.03800 1763.168 2501.000 1.010620 -1.606620 -0.1216830 0.0001620713
374.8 12.29400 1808.007 2601.000 1.010220 -1.756897 -0.09582771 0.0001631817
375.1 12.54700 1852.197 2701.000 1.009850 -1.667506 0.2065878 0.0001383746
375.3 12.79500 1895.780 2801.000 1.009490 -1.706857 -0.09633735 0.0001103695
375.6 13.04100 1938.779 2901.000 1.009160 -1.346137 0.1238386 0.00005433380
375.8 13.28300 1981.234 3001.000 1.008850 -0.9515374 -0.01420870 -0.00001650543
376.1 13.52200 2023.164 3101.000 1.008560 -0.3395978 -0.1697477 -0.0001034221
376.3 13.75700 2064.175 3200.000 1.008290 0.7306675 0.1703356 -0.0002043662
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203853">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-17.5483870968 44.8028673835</math:coefficients>
     <math:minRange>0.414</math:minRange>
     <math:maxRange>2.646</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962031139">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>2054.58042281 -1957.89483463 625.866595557 -62.340104902</math:coefficients>
     <math:minRange>2.646</math:minRange>
     <math:maxRange>3.538</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203646">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>8.99138458003 -27.0043984008 24.6795778427 -0.503624084701 0.00567237666008</math:coefficients>
     <math:minRange>3.538</math:minRange>
     <math:maxRange>13.757</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962031114">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-12.4068976043 45.0337206309 19.4455252006 -1.2986403723 0.0323511417797</math:coefficients>
     <math:minRange>0.414</math:minRange>
     <math:maxRange>13.757</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203656">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>13.757</math:minRange>
     <math:maxRange>13.757</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203587">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>13.757</math:minRange>
     <math:maxRange>13.757</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203799">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.985413548387 0.0352329749104</math:coefficients>
     <math:minRange>0.414</math:minRange>
     <math:maxRange>2.646</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203273">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-2.91694272125 3.92011495586 -1.26429468892 0.133582768003</math:coefficients>
     <math:minRange>2.646</math:minRange>
     <math:maxRange>3.538</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296203302">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.0981354783 -0.0231096591313 0.0025313314696 -1.31688361755E-4 2.55919459544E-6</math:coefficients>
     <math:minRange>3.538</math:minRange>
     <math:maxRange>13.757</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>47.2000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile121268077549688"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation12126807771761193">
   <gml:description>Gelesen aus: PROF0047.3000.txt</gml:description>
   <gml:name>47.3000</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3931412.2 777722.447 362.51</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:wspmTuhhCalculationMember>
    <tuhh:CalculationReibConstWspmTuhhSteadyState gml:id="CalculationReibConstWspmTuhhSteadyState1222522296218822">
     <gml:description>Eine Berechnung für den Import in das instationäre FE-Modell</gml:description>
     <gml:name>Polynomberechnung 1</gml:name>
     <wspm:calcCreationMember>
      <wspm:CalcCreation gml:id="CalcCreation122252229621823">
       <wspm:user>BCE</wspm:user>
       <wspm:date>2008-05-30T09:47:09.129+02:00</wspm:date>
      </wspm:CalcCreation>
     </wspm:calcCreationMember>
     <tuhh:subReachDefinitionMember>
      <tuhh:SubReachDefinition gml:id="SubReachDefinition1222522296218825">
       <tuhh:startStation>40</tuhh:startStation>
       <tuhh:endStation>50.0000</tuhh:endStation>
      </tuhh:SubReachDefinition>
     </tuhh:subReachDefinitionMember>
     <tuhh:waterlevelParameterMember>
      <tuhh:WaterlevelParameter gml:id="WaterlevelParameter12225222962181155">
       <tuhh:exeVersion>_2_1_4_0</tuhh:exeVersion>
       <tuhh:wspIteration>SIMPLE</tuhh:wspIteration>
       <tuhh:verzoegerungsverlust>BJOERNSEN</tuhh:verzoegerungsverlust>
       <tuhh:reibungsverlust>TRAPEZ_FORMULA</tuhh:reibungsverlust>
       <tuhh:specialOptionsMember>
        <tuhh:SpecialOptions gml:id="SpecialOptions1222522296218320">
         <tuhh:doCalcBridges>true</tuhh:doCalcBridges>
         <tuhh:doCalcBarrages>true</tuhh:doCalcBarrages>
         <tuhh:useExtremeRoughness>true</tuhh:useExtremeRoughness>
        </tuhh:SpecialOptions>
       </tuhh:specialOptionsMember>
      </tuhh:WaterlevelParameter>
     </tuhh:waterlevelParameterMember>
     <tuhh:bottomSlope>0.0001</tuhh:bottomSlope>
     <tuhh:runOffIntervalMember>
      <tuhh:RunOffInterval gml:id="RunOffInterval12225222962181111">
       <tuhh:minimalRunOff>1.0</tuhh:minimalRunOff>
       <tuhh:runOffStep>100.0</tuhh:runOffStep>
       <tuhh:maximalRunOff>3200.0</tuhh:maximalRunOff>
      </tuhh:RunOffInterval>
     </tuhh:runOffIntervalMember>
     <tuhh:calcPolynomesMember>
      <tuhh:CalcPolynomes gml:id="CalcPolynomes1222522296218257">
       <tuhh:degree>4</tuhh:degree>
       <tuhh:trippleIt>true</tuhh:trippleIt>
       <tuhh:trippleMode>slopeChange</tuhh:trippleMode>
       <tuhh:runoffSlope>2.0000</tuhh:runoffSlope>
       <tuhh:areaSlope>2</tuhh:areaSlope>
       <tuhh:alphaSlope>2.0000</tuhh:alphaSlope>
       <tuhh:weightSplinePoint>8</tuhh:weightSplinePoint>
       <tuhh:ignoreOutlier>true</tuhh:ignoreOutlier>
       <tuhh:alphaLimit>8</tuhh:alphaLimit>
      </tuhh:CalcPolynomes>
     </tuhh:calcPolynomesMember>
    </tuhh:CalculationReibConstWspmTuhhSteadyState>
   </wb1d2d:wspmTuhhCalculationMember>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1222522296218453">
     <gml:description>Übernommen aus Datei: PROF0047.3000.txt</gml:description>
     <gml:name>0047.3000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1222522296218558">
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
     <om:result><![CDATA[363.0 0.5300000 14.22200 1.000000 1.000000 -2.058004 3.552714E-15 0E-7
365.4 2.854000 198.5980 101.0000 1.062400 5.727959 0E-7 0E-7
366.4 3.844000 314.8440 201.0000 1.040060 2.076267 -0.07304264 0.0001040720
367.1 4.619000 413.9290 301.0000 1.037370 0.1648425 0.5474319 -0.0005332424
367.8 5.274000 503.3430 401.0000 1.034960 -2.245306 0.4862724 -0.001075739
368.4 5.856000 584.4490 501.0000 1.031700 -3.040573 0.2409764 -0.0004443999
368.9 6.388000 659.7120 601.0000 1.028730 -2.903082 0.09925905 0.0001808957
369.4 6.882000 730.7620 701.0000 1.026770 -2.574036 -0.01358111 0.00005368842
369.9 7.345000 798.0620 801.0000 1.024630 -2.025737 -0.3330373 0.0003426932
370.3 7.784000 862.2380 901.0000 1.022800 -1.205040 -0.5629208 0.0005291741
370.7 8.204000 923.8380 1001.000 1.021270 -0.1543234 -0.4547971 0.0005996259
371.1 8.606000 983.2490 1101.000 1.020010 0.7216572 -0.4845667 0.0005725847
371.5 8.994000 1040.793 1201.000 1.019030 1.581119 -0.2401886 0.0004155116
371.9 9.369000 1096.660 1301.000 1.018160 2.299779 0.06807522 0.0002851633
372.2 9.732000 1151.056 1401.000 1.017370 2.761819 0.2984505 0.0001975460
372.6 10.08400 1204.785 1501.000 1.017200 2.272301 0.3704256 -0.0004015310
372.9 10.42700 1257.050 1601.000 1.016810 1.903083 0.5543102 -0.0006879567
373.3 10.76000 1307.925 1701.000 1.016210 1.412191 0.2895433 -0.0006814222
373.6 11.08600 1357.655 1801.000 1.015630 1.000363 0.2431199 -0.0006286450
373.9 11.40500 1406.290 1901.000 1.015040 0.6160524 0.2275491 -0.0005103930
374.2 11.71700 1453.930 2001.000 1.014460 0.1630470 0.06511591 -0.0003574984
374.5 12.02300 1500.648 2101.000 1.013890 -0.2723819 -0.08331786 -0.0001819328
374.8 12.32400 1546.517 2201.000 1.013340 -0.6004702 -0.02433901 -0.000005255733
375.1 12.61900 1591.586 2301.000 1.012820 -1.007251 -0.2169588 0.0001536525
375.4 12.91000 1635.924 2401.000 1.012320 -1.242257 -0.09685038 0.0002924048
375.7 13.19600 1679.554 2501.000 1.011850 -1.461388 -0.1069472 0.0003924970
376.0 13.47700 1722.537 2601.000 1.011410 -1.706005 -0.3500678 0.0004445205
376.3 13.75500 1764.901 2701.000 1.010990 -1.678320 -0.1989357 0.0004463094
376.5 14.02900 1806.678 2801.000 1.010600 -1.539741 -0.08542245 0.0003800077
376.8 14.29900 1847.905 2901.000 1.010220 -1.304236 -0.08590328 0.0002567333
377.1 14.56600 1888.611 3001.000 1.009880 -0.8229465 0.1038776 0.00003572943
377.3 14.82900 1928.815 3101.000 1.009550 -0.2446802 0.04511377 -0.0002594796
377.6 15.08700 1968.141 3200.000 1.009240 0.6731621 0.2826544 -0.0006438178
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D12225222962181050">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-21.8055077453 43.0292598967</math:coefficients>
     <math:minRange>0.53</math:minRange>
     <math:maxRange>2.854</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218536">
     <gml:description>Spl_Q</gml:description>
     <gml:name>Spl_Q</gml:name>
     <math:coefficients>1787.79461159 -1571.90272594 465.203733919 -42.5787011568</math:coefficients>
     <math:minRange>2.854</math:minRange>
     <math:maxRange>3.844</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218140">
     <gml:description>Q2(h)</gml:description>
     <gml:name>Q2(h)</gml:name>
     <math:coefficients>19.6994609556 -28.9488826908 21.4785531442 -0.460889792516 0.00600591540299</math:coefficients>
     <math:minRange>3.844</math:minRange>
     <math:maxRange>15.087</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218446">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-12.9944513323 39.6377715245 15.2594968768 -0.924360079125 0.0209377110036</math:coefficients>
     <math:minRange>0.53</math:minRange>
     <math:maxRange>15.087</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218119">
     <gml:description>Spl_A</gml:description>
     <gml:name>Spl_A</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>15.087</math:minRange>
     <math:maxRange>15.087</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218331">
     <gml:description>A2(h)</gml:description>
     <gml:name>A2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>15.087</math:minRange>
     <math:maxRange>15.087</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218753">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>0.985769363167 0.0268502581756</math:coefficients>
     <math:minRange>0.53</math:minRange>
     <math:maxRange>2.854</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D122252229621842">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>-1.58215226545 2.38886513507 -0.70944037425 0.0690568984404</math:coefficients>
     <math:minRange>2.854</math:minRange>
     <math:maxRange>3.844</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1222522296218606">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>1.04492414355 0.0034436393146 -0.00176034392604 1.58227089568E-4 -4.45783310033E-6</math:coefficients>
     <math:minRange>3.844</math:minRange>
     <math:maxRange>15.087</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>47.3000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile1212680775512883"/>
   <wb1d2d:slope>0.00010</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition12127360097821176">
   <gml:description>Manuell erzeugt am: 06.06.2008 9:06</gml:description>
   <gml:name>Abfluss Hochwasser 1D</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3931412.2 777722.447</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:direction>250</op1d2d:direction>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource12127360097821736">
     <gml:name>Importierte Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition1212736009891379">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2008-06-03T22%3A00%3A00.625%2B02%3A00 250.000 null null
2008-06-04T01%3A15%3A00.000Z 255 null null
2008-06-04T01%3A30%3A00.000Z 257.99 null null
2008-06-04T02%3A15%3A00.000Z 258.0 null null
2008-06-04T02%3A45%3A00.000Z 261.28 null null
2008-06-04T03%3A15%3A00.000Z 261.28 null null
2008-06-04T03%3A30%3A00.000Z 262.93 null null
2008-06-04T04%3A00%3A00.000Z 262.93 null null
2008-06-04T04%3A15%3A00.000Z 264.59 null null
2008-06-04T04%3A30%3A00.000Z 264.59 null null
2008-06-04T04%3A45%3A00.000Z 266.26 null null
2008-06-04T08%3A45%3A00.000Z 266.29 null null
2008-06-04T09%3A00%3A00.000Z 269.65 null null
2008-06-04T11%3A15%3A00.000Z 269.66 null null
2008-06-04T11%3A30%3A00.000Z 271.36 null null
2008-06-04T12%3A15%3A00.000Z 271.36 null null
2008-06-04T12%3A30%3A00.000Z 273.06 null null
2008-06-04T13%3A15%3A00.000Z 273.07 null null
2008-06-04T13%3A30%3A00.000Z 274.78 null null
2008-06-04T14%3A00%3A00.000Z 274.78 null null
2008-06-04T14%3A15%3A00.000Z 278.23 null null
2008-06-04T14%3A30%3A00.000Z 279.96 null null
2008-06-04T14%3A45%3A00.000Z 283.46 null null
2008-06-04T15%3A00%3A00.000Z 283.46 null null
2008-06-04T15%3A45%3A00.000Z 294.15 null null
2008-06-04T16%3A00%3A00.000Z 295.96 null null
2008-06-04T16%3A15%3A00.000Z 299.61 null null
2008-06-04T16%3A30%3A00.000Z 301.45 null null
2008-06-04T16%3A45%3A00.000Z 301.45 null null
2008-06-04T17%3A00%3A00.000Z 303.3 null null
2008-06-04T17%3A15%3A00.000Z 303.3 null null
2008-06-04T17%3A30%3A00.000Z 305.15 null null
2008-06-04T18%3A30%3A00.000Z 305.16 null null
2008-06-04T18%3A45%3A00.000Z 310.77 null null
2008-06-04T21%3A45%3A00.000Z 310.8 null null
2008-06-04T21%3A49%3A00.000Z 312.68 null null
2008-06-04T22%3A00%3A00.000Z 310.8 null null
2008-06-04T22%3A15%3A00.000Z 312.69 null null
2008-06-04T23%3A00%3A00.000Z 312.69 null null
2008-06-04T23%3A01%3A00.000Z 312.69 null null
2008-06-04T23%3A15%3A00.000Z 316.49 null null
2008-06-04T23%3A30%3A00.000Z 318.41 null null
2008-06-05T00%3A30%3A00.000Z 318.42 null null
2008-06-05T00%3A45%3A00.000Z 320.34 null null
2008-06-05T01%3A30%3A00.000Z 320.34 null null
2008-06-05T01%3A45%3A00.000Z 322.28 null null
2008-06-05T02%3A00%3A00.000Z 322.28 null null
2008-06-05T02%3A15%3A00.000Z 324.22 null null
2008-06-05T02%3A45%3A00.000Z 324.22 null null
2008-06-05T03%3A15%3A00.000Z 328.12 null null
2008-06-05T03%3A45%3A00.000Z 328.13 null null
2008-06-05T04%3A00%3A00.000Z 330.09 null null
2008-06-05T04%3A30%3A00.000Z 330.1 null null
2008-06-05T04%3A45%3A00.000Z 332.07 null null
2008-06-05T05%3A00%3A00.000Z 332.07 null null
2008-06-05T05%3A15%3A00.000Z 334.05 null null
2008-06-05T05%3A45%3A00.000Z 334.06 null null
2008-06-05T06%3A00%3A00.000Z 338.04 null null
2008-06-05T06%3A30%3A00.000Z 338.04 null null
2008-06-05T06%3A45%3A00.000Z 340.05 null null
2008-06-05T07%3A15%3A00.000Z 340.05 null null
2008-06-05T07%3A30%3A00.000Z 342.07 null null
2008-06-05T07%3A45%3A00.000Z 342.07 null null
2008-06-05T08%3A30%3A00.000Z 348.16 null null
2008-06-05T08%3A45%3A00.000Z 348.16 null null
2008-06-05T09%3A15%3A00.000Z 352.26 null null
2008-06-05T09%3A30%3A00.000Z 358.47 null null
2008-06-05T10%3A15%3A00.000Z 364.83 null null
2008-06-05T10%3A30%3A00.000Z 364.86 null null
2008-06-05T10%3A45%3A00.000Z 369.12 null null
2008-06-05T11%3A00%3A00.000Z 369.15 null null
2008-06-05T11%3A15%3A00.000Z 371.3 null null
2008-06-05T11%3A45%3A00.000Z 371.36 null null
2008-06-05T12%3A00%3A00.000Z 373.53 null null
2008-06-05T12%3A30%3A00.000Z 373.59 null null
2008-06-05T12%3A45%3A00.000Z 375.76 null null
2008-06-05T13%3A00%3A00.000Z 375.79 null null
2008-06-05T13%3A15%3A00.000Z 380.12 null null
2008-06-05T13%3A30%3A00.000Z 380.15 null null
2008-06-05T13%3A45%3A00.000Z 382.34 null null
2008-06-05T14%3A00%3A00.000Z 382.37 null null
2008-06-05T14%3A15%3A00.000Z 384.57 null null
2008-06-05T14%3A30%3A00.000Z 384.6 null null
2008-06-05T14%3A45%3A00.000Z 386.81 null null
2008-06-05T15%3A00%3A00.000Z 386.84 null null
2008-06-05T15%3A15%3A00.000Z 389.05 null null
2008-06-05T15%3A30%3A00.000Z 389.08 null null
2008-06-05T15%3A45%3A00.000Z 391.3 null null
2008-06-05T16%3A00%3A00.000Z 391.33 null null
2008-06-05T16%3A30%3A00.000Z 395.8 null null
2008-06-05T16%3A45%3A00.000Z 395.83 null null
2008-06-05T17%3A00%3A00.000Z 398.07 null null
2008-06-05T17%3A45%3A00.000Z 398.16 null null
2008-06-05T18%3A00%3A00.000Z 400.41 null null
2008-06-05T19%3A00%3A00.000Z 400.54 null null
2008-06-05T19%3A45%3A00.000Z 407.32 null null
2008-06-05T20%3A30%3A00.000Z 407.42 null null
2008-06-05T20%3A45%3A00.000Z 409.69 null null
2008-06-05T21%3A00%3A00.000Z 409.72 null null
2008-06-05T21%3A15%3A00.000Z 412.0 null null
2008-06-05T21%3A45%3A00.000Z 412.07 null null
2008-06-05T22%3A00%3A00.000Z 414.35 null null
2008-06-05T22%3A30%3A00.000Z 414.42 null null
2008-06-05T22%3A39%3A00.000Z 418.96 null null
2008-06-05T22%3A45%3A00.000Z 414.45 null null
2008-06-05T23%3A00%3A00.000Z 419.01 null null
2008-06-05T23%3A30%3A00.000Z 419.07 null null
2008-06-05T23%3A45%3A00.000Z 421.38 null null
2008-06-06T00%3A15%3A00.000Z 421.44 null null
2008-06-06T00%3A30%3A00.000Z 423.75 null null
2008-06-06T01%3A00%3A00.000Z 423.82 null null
2008-06-06T01%3A15%3A00.000Z 426.13 null null
2008-06-06T01%3A45%3A00.000Z 426.2 null null
2008-06-06T02%3A00%3A00.000Z 430.82 null null
2008-06-06T02%3A45%3A00.000Z 430.91 null null
2008-06-06T03%3A00%3A00.000Z 433.25 null null
2008-06-06T03%3A30%3A00.000Z 433.31 null null
2008-06-06T03%3A45%3A00.000Z 435.65 null null
2008-06-06T04%3A15%3A00.000Z 435.71 null null
2008-06-06T04%3A30%3A00.000Z 438.06 null null
2008-06-06T04%3A45%3A00.000Z 438.09 null null
2008-06-06T05%3A31%3A00.000Z 440.68 null null
2008-06-06T06%3A05%3A00.000Z 440.75 null null
2008-06-06T07%3A00%3A00.000Z 442.91 null null
2008-06-06T07%3A40%3A00.000Z 443.12 null null
2008-06-06T08%3A32%3A00.000Z 450.21 null null
2008-06-06T09%3A30%3A00.000Z 452.53 null null
2008-06-06T10%3A31%3A00.000Z 450.2 null null
2008-06-06T10%3A55%3A00.000Z 445.5 null null
2008-06-06T11%3A45%3A00.000Z 440.8 null null
2008-06-06T12%3A15%3A00.000Z 440.79 null null
2008-06-06T12%3A30%3A00.000Z 438.46 null null
2008-06-06T12%3A45%3A00.000Z 438.46 null null
2008-06-06T13%3A00%3A00.000Z 433.83 null null
2008-06-06T14%3A15%3A00.000Z 433.8 null null
2008-06-06T14%3A30%3A00.000Z 431.49 null null
2008-06-06T16%3A45%3A00.000Z 431.43 null null
2008-06-06T17%3A15%3A00.000Z 426.83 null null
2008-06-06T17%3A30%3A00.000Z 422.26 null null
2008-06-06T17%3A45%3A00.000Z 419.98 null null
2008-06-06T18%3A00%3A00.000Z 419.97 null null
2008-06-06T18%3A15%3A00.000Z 417.7 null null
2008-06-06T18%3A30%3A00.000Z 417.69 null null
2008-06-06T18%3A45%3A00.000Z 415.42 null null
2008-06-06T19%3A30%3A00.000Z 415.4 null null
2008-06-06T19%3A45%3A00.000Z 410.89 null null
2008-06-06T20%3A30%3A00.000Z 410.87 null null
2008-06-06T20%3A45%3A00.000Z 408.62 null null
2008-06-06T21%3A45%3A00.000Z 408.59 null null
2008-06-06T21%3A50%3A00.000Z 406.35 null null
2008-06-06T23%3A30%3A00.000Z 406.31 null null
2008-06-06T23%3A45%3A00.000Z 404.07 null null
2008-06-07T01%3A00%3A00.000Z 404.04 null null
2008-06-07T01%3A15%3A00.000Z 399.59 null null
2008-06-07T01%3A45%3A00.000Z 399.58 null null
2008-06-07T02%3A00%3A00.000Z 397.36 null null
2008-06-07T03%3A00%3A00.000Z 397.33 null null
2008-06-07T03%3A15%3A00.000Z 395.12 null null
2008-06-07T04%3A15%3A00.000Z 395.1 null null
2008-06-07T04%3A30%3A00.000Z 392.89 null null
2008-06-07T05%3A15%3A00.000Z 392.87 null null
2008-06-07T05%3A45%3A00.000Z 388.48 null null
2008-06-07T06%3A15%3A00.000Z 388.47 null null
2008-06-07T06%3A30%3A00.000Z 386.29 null null
2008-06-07T07%3A15%3A00.000Z 386.27 null null
2008-06-07T07%3A30%3A00.000Z 384.09 null null
2008-06-07T08%3A30%3A00.000Z 384.06 null null
2008-06-07T08%3A45%3A00.000Z 381.89 null null
2008-06-07T09%3A00%3A00.000Z 381.89 null null
2008-06-07T11%3A00%3A00.000Z 364.8 null null
2008-06-07T11%3A15%3A00.000Z 364.79 null null
2008-06-07T11%3A45%3A00.000Z 360.59 null null
2008-06-07T12%3A00%3A00.000Z 360.59 null null
2008-06-07T12%3A15%3A00.000Z 356.43 null null
2008-06-07T12%3A30%3A00.000Z 354.35 null null
2008-06-07T12%3A45%3A00.000Z 354.35 null null
2008-06-07T13%3A00%3A00.000Z 352.28 null null
2008-06-07T13%3A30%3A00.000Z 352.27 null null
2008-06-07T13%3A45%3A00.000Z 350.21 null null
2008-06-07T14%3A15%3A00.000Z 350.2 null null
2008-06-07T14%3A30%3A00.000Z 346.12 null null
2008-06-07T15%3A15%3A00.000Z 346.1 null null
2008-06-07T15%3A30%3A00.000Z 344.07 null null
2008-06-07T16%3A00%3A00.000Z 344.06 null null
2008-06-07T16%3A15%3A00.000Z 342.03 null null
2008-06-07T17%3A00%3A00.000Z 342.01 null null
2008-06-07T17%3A15%3A00.000Z 340.0 null null
2008-06-07T18%3A30%3A00.000Z 339.97 null null
2008-06-07T19%3A00%3A00.000Z 335.96 null null
2008-06-07T19%3A45%3A00.000Z 335.94 null null
2008-06-07T20%3A00%3A00.000Z 333.95 null null
2008-06-07T20%3A45%3A00.000Z 333.93 null null
2008-06-07T21%3A00%3A00.000Z 331.95 null null
2008-06-07T22%3A00%3A00.000Z 331.92 null null
2008-06-07T22%3A01%3A00.000Z 329.95 null null
2008-06-07T23%3A00%3A00.000Z 329.93 null null
2008-06-07T23%3A15%3A00.000Z 329.92 null null
2008-06-07T23%3A30%3A00.000Z 326.0 null null
2008-06-08T00%3A00%3A00.000Z 325.99 null null
2008-06-08T00%3A15%3A00.000Z 324.04 null null
2008-06-08T01%3A15%3A00.000Z 324.02 null null
2008-06-08T01%3A30%3A00.000Z 322.08 null null
2008-06-08T02%3A00%3A00.000Z 322.06 null null
2008-06-08T02%3A15%3A00.000Z 320.13 null null
2008-06-08T02%3A30%3A00.000Z 320.13 null null
2008-06-08T03%3A00%3A00.000Z 316.28 null null
2008-06-08T03%3A30%3A00.000Z 316.27 null null
2008-06-08T03%3A45%3A00.000Z 314.36 null null
2008-06-08T04%3A15%3A00.000Z 314.35 null null
2008-06-08T04%3A30%3A00.000Z 312.45 null null
2008-06-08T05%3A15%3A00.000Z 312.43 null null
2008-06-08T05%3A30%3A00.000Z 310.54 null null
2008-06-08T06%3A30%3A00.000Z 310.52 null null
2008-06-08T06%3A45%3A00.000Z 306.77 null null
2008-06-08T07%3A15%3A00.000Z 306.76 null null
2008-06-08T07%3A30%3A00.000Z 304.89 null null
2008-06-08T08%3A30%3A00.000Z 304.87 null null
2008-06-08T09%3A00%3A00.000Z 301.16 null null
2008-06-08T10%3A00%3A00.000Z 301.18 null null
2008-06-08T10%3A30%3A00.000Z 297.54 null null
2008-06-08T11%3A00%3A00.000Z 297.57 null null
2008-06-08T11%3A15%3A00.000Z 295.76 null null
2008-06-08T11%3A30%3A00.000Z 295.77 null null
2008-06-08T11%3A45%3A00.000Z 297.6 null null
2008-06-08T12%3A00%3A00.000Z 297.62 null null
2008-06-08T12%3A15%3A00.000Z 295.81 null null
2008-06-08T13%3A00%3A00.000Z 295.85 null null
2008-06-08T13%3A15%3A00.000Z 294.05 null null
2008-06-08T14%3A15%3A00.000Z 294.1 null null
2008-06-08T14%3A30%3A00.000Z 290.52 null null
2008-06-08T15%3A15%3A00.000Z 290.55 null null
2008-06-08T15%3A30%3A00.000Z 288.78 null null
2008-06-08T16%3A15%3A00.000Z 288.82 null null
2008-06-08T16%3A30%3A00.000Z 287.05 null null
2008-06-08T17%3A30%3A00.000Z 287.1 null null
2008-06-08T17%3A45%3A00.000Z 285.34 null null
2008-06-08T18%3A30%3A00.000Z 285.38 null null
2008-06-08T18%3A45%3A00.000Z 281.88 null null
2008-06-08T19%3A30%3A00.000Z 281.91 null null
2008-06-08T19%3A45%3A00.000Z 280.18 null null
2008-06-08T20%3A15%3A00.000Z 280.21 null null
2008-06-08T20%3A30%3A00.000Z 278.48 null null
2008-06-08T21%3A30%3A00.000Z 278.53 null null
2008-06-08T21%3A45%3A00.000Z 276.81 null null
2008-06-08T22%3A15%3A00.000Z 276.84 null null
2008-06-08T22%3A20%3A00.000Z 273.41 null null
2008-06-08T23%3A15%3A00.000Z 273.46 null null
2008-06-08T23%3A30%3A00.000Z 271.76 null null
2008-06-09T00%3A15%3A00.000Z 271.8 null null
2008-06-09T00%3A30%3A00.000Z 270.12 null null
2008-06-09T01%3A00%3A00.000Z 270.14 null null
2008-06-09T01%3A15%3A00.000Z 268.47 null null
2008-06-09T02%3A00%3A00.000Z 268.5 null null
2008-06-09T02%3A15%3A00.000Z 265.16 null null
2008-06-09T03%3A00%3A00.000Z 265.2 null null
2008-06-09T03%3A15%3A00.000Z 263.55 null null
2008-06-09T04%3A00%3A00.000Z 263.58 null null
2008-06-09T04%3A30%3A00.000Z 260.31 null null
2008-06-09T05%3A00%3A00.000Z 260.33 null null
2008-06-09T05%3A30%3A00.000Z 257.09 null null
2008-06-09T06%3A00%3A00.000Z 257.11 null null
2008-06-09T06%3A15%3A00.000Z 255.5 null null
2008-06-09T06%3A45%3A00.000Z 255.52 null null
2008-06-09T07%3A00%3A00.000Z 253.92 null null
2008-06-09T08%3A00%3A00.000Z 253.97 null null
2008-06-09T08%3A30%3A00.000Z 250.79 null null
2008-06-09T09%3A15%3A00.000Z 250.76 null null
2008-06-09T09%3A30%3A00.000Z 249.15 null null
2008-06-09T10%3A15%3A00.000Z 249.11 null null
2008-06-09T10%3A30%3A00.000Z 247.52 null null
2008-06-09T11%3A00%3A00.000Z 247.49 null null
2008-06-09T11%3A15%3A00.000Z 244.34 null null
2008-06-09T11%3A30%3A00.000Z 245.89 null null
2008-06-09T11%3A45%3A00.000Z 244.31 null null
2008-06-09T12%3A30%3A00.000Z 244.27 null null
2008-06-09T12%3A45%3A00.000Z 242.7 null null
2008-06-09T13%3A15%3A00.000Z 242.67 null null
2008-06-09T13%3A30%3A00.000Z 239.58 null null
2008-06-09T14%3A15%3A00.000Z 239.54 null null
2008-06-09T14%3A30%3A00.000Z 237.99 null null
2008-06-09T15%3A30%3A00.000Z 237.94 null null
2008-06-09T15%3A45%3A00.000Z 236.41 null null
2008-06-09T16%3A30%3A00.000Z 236.36 null null
2008-06-09T16%3A45%3A00.000Z 234.84 null null
2008-06-09T17%3A15%3A00.000Z 234.81 null null
2008-06-09T17%3A45%3A00.000Z 231.79 null null
2008-06-09T18%3A15%3A00.000Z 231.76 null null
2008-06-09T18%3A30%3A00.000Z 230.26 null null
2008-06-09T19%3A00%3A00.000Z 230.23 null null
2008-06-09T19%3A15%3A00.000Z 228.74 null null
2008-06-09T20%3A00%3A00.000Z 228.7 null null
2008-06-09T20%3A15%3A00.000Z 227.22 null null
2008-06-09T21%3A00%3A00.000Z 227.18 null null
2008-06-09T21%3A30%3A00.000Z 224.25 null null
2008-06-09T22%3A00%3A00.000Z 224.22 null null
2008-06-09T22%3A14%3A00.000Z 222.77 null null
2008-06-09T23%3A00%3A00.000Z 222.73 null null
]]></om:result>
     <ns2:dataSourceURI/>
    </ns2:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine1D121268079519494</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit1D12125830336860</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12144639393734308</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12144639393734308</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D121455082661013417</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D121455342596917318</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12300482827813667</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>250.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition1214399762355391">
   <gml:description>Manuell erzeugt am: 25.06.2008 15:16</gml:description>
   <gml:name>Wasserstand Teichlösung 2D</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929045.7501006303 773764.8059923581</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:direction/>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource1214399762355420">
     <gml:name>Wasserstand - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition1214399762495882">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2008-06-25T00%3A00%3A00.000%2B02%3A00 367 null null
2008-06-25T06%3A00%3A00.127%2B02%3A00 366.85 null null
2008-06-25T12%3A00%3A00.658%2B02%3A00 366.72 null null
2008-06-25T18%3A00%3A00.799%2B02%3A00 366.6 null null
2008-06-26T00%3A00%3A00.299%2B02%3A00 366.5 null null
2008-06-26T06%3A00%3A00.314%2B02%3A00 366.50 null null
]]></om:result>
     <ns2:dataSourceURI/>
    </ns2:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D12143995593866821</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D121439979923417593</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>367.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition1214472319537349">
   <gml:description>Manuell erzeugt am: 26.06.2008 11:25</gml:description>
   <gml:name>W/Q - Beziehung</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929088.2594008152 773754.3590080483</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:direction>260</op1d2d:direction>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource1214472319537536">
     <gml:name>W/Q - Beziehung</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition1214472319552768">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[363.58 10 null null
364.88 92 null null
365.85 162 null null
366.03 185 null null
366.28 215 null null
366.73 296 null null
366.98 360 null null
367.28 454 null null
368.08 742 null null
400 742 null null
]]></om:result>
     <ns2:dataSourceURI/>
    </ns2:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D12143995593866821</op1d2d:parentModelElement>
   <op1d2d:stationaryCondition>364.7</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition1214479265441757">
   <gml:description>Manuell erzeugt am: 26.06.2008 13:21</gml:description>
   <gml:name>Wasserstand Hochwasser 2D</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929130.7565019033 773743.8734519278</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:direction/>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource1214479265441730">
     <gml:name>Importierte Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition121447926567574">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2008-06-03T22%3A00%3A00.000Z 366.57 null null
2008-06-03T22%3A04%3A00.000Z 366.58 null null
2008-06-03T22%3A08%3A00.000Z 366.57 null null
2008-06-03T22%3A40%3A00.000Z 366.57 null null
2008-06-03T22%3A44%3A00.000Z 366.56 null null
2008-06-03T22%3A48%3A00.000Z 366.57 null null
2008-06-03T22%3A52%3A00.000Z 366.56 null null
2008-06-03T22%3A56%3A00.000Z 366.57 null null
2008-06-03T23%3A00%3A00.000Z 366.56 null null
2008-06-03T23%3A04%3A00.000Z 366.56 null null
2008-06-03T23%3A08%3A00.000Z 366.57 null null
2008-06-03T23%3A12%3A00.000Z 366.56 null null
2008-06-03T23%3A32%3A00.000Z 366.56 null null
2008-06-03T23%3A36%3A00.000Z 366.55 null null
2008-06-03T23%3A40%3A00.000Z 366.56 null null
2008-06-03T23%3A44%3A00.000Z 366.55 null null
2008-06-03T23%3A56%3A00.000Z 366.55 null null
2008-06-04T00%3A00%3A00.000Z 366.56 null null
2008-06-04T00%3A16%3A00.000Z 366.56 null null
2008-06-04T00%3A20%3A00.000Z 366.54 null null
2008-06-04T00%3A24%3A00.000Z 366.54 null null
2008-06-04T00%3A28%3A00.000Z 366.56 null null
2008-06-04T00%3A32%3A00.000Z 366.55 null null
2008-06-04T00%3A36%3A00.000Z 366.55 null null
2008-06-04T00%3A40%3A00.000Z 366.56 null null
2008-06-04T00%3A44%3A00.000Z 366.55 null null
2008-06-04T01%3A00%3A00.000Z 366.55 null null
2008-06-04T01%3A04%3A00.000Z 366.56 null null
2008-06-04T01%3A08%3A00.000Z 366.55 null null
2008-06-04T01%3A16%3A00.000Z 366.55 null null
2008-06-04T01%3A20%3A00.000Z 366.54 null null
2008-06-04T01%3A24%3A00.000Z 366.55 null null
2008-06-04T02%3A24%3A00.000Z 366.55 null null
2008-06-04T02%3A28%3A00.000Z 366.54 null null
2008-06-04T02%3A36%3A00.000Z 366.56 null null
2008-06-04T02%3A44%3A00.000Z 366.54 null null
2008-06-04T02%3A52%3A00.000Z 366.56 null null
2008-06-04T02%3A56%3A00.000Z 366.54 null null
2008-06-04T03%3A00%3A00.000Z 366.55 null null
2008-06-04T04%3A08%3A00.000Z 366.55 null null
2008-06-04T04%3A12%3A00.000Z 366.56 null null
2008-06-04T04%3A16%3A00.000Z 366.56 null null
2008-06-04T04%3A20%3A00.000Z 366.55 null null
2008-06-04T04%3A24%3A00.000Z 366.55 null null
2008-06-04T04%3A28%3A00.000Z 366.56 null null
2008-06-04T05%3A08%3A00.000Z 366.56 null null
2008-06-04T05%3A12%3A00.000Z 366.55 null null
2008-06-04T05%3A16%3A00.000Z 366.56 null null
2008-06-04T05%3A20%3A00.000Z 366.56 null null
2008-06-04T05%3A24%3A00.000Z 366.57 null null
2008-06-04T05%3A28%3A00.000Z 366.56 null null
2008-06-04T05%3A36%3A00.000Z 366.56 null null
2008-06-04T05%3A40%3A00.000Z 366.57 null null
2008-06-04T05%3A44%3A00.000Z 366.56 null null
2008-06-04T05%3A48%3A00.000Z 366.56 null null
2008-06-04T05%3A52%3A00.000Z 366.57 null null
2008-06-04T06%3A12%3A00.000Z 366.57 null null
2008-06-04T06%3A16%3A00.000Z 366.56 null null
2008-06-04T06%3A20%3A00.000Z 366.57 null null
2008-06-04T06%3A24%3A00.000Z 366.57 null null
2008-06-04T06%3A28%3A00.000Z 366.56 null null
2008-06-04T06%3A32%3A00.000Z 366.57 null null
2008-06-04T06%3A40%3A00.000Z 366.57 null null
2008-06-04T06%3A44%3A00.000Z 366.56 null null
2008-06-04T06%3A48%3A00.000Z 366.57 null null
2008-06-04T07%3A16%3A00.000Z 366.57 null null
2008-06-04T07%3A20%3A00.000Z 366.58 null null
2008-06-04T07%3A24%3A00.000Z 366.57 null null
2008-06-04T07%3A28%3A00.000Z 366.58 null null
2008-06-04T07%3A36%3A00.000Z 366.58 null null
2008-06-04T07%3A40%3A00.000Z 366.57 null null
2008-06-04T07%3A44%3A00.000Z 366.57 null null
2008-06-04T07%3A48%3A00.000Z 366.58 null null
2008-06-04T07%3A52%3A00.000Z 366.57 null null
2008-06-04T07%3A56%3A00.000Z 366.58 null null
2008-06-04T08%3A56%3A00.000Z 366.58 null null
2008-06-04T09%3A00%3A00.000Z 366.59 null null
2008-06-04T09%3A04%3A00.000Z 366.59 null null
2008-06-04T09%3A08%3A00.000Z 366.58 null null
2008-06-04T09%3A24%3A00.000Z 366.58 null null
2008-06-04T09%3A28%3A00.000Z 366.59 null null
2008-06-04T09%3A32%3A00.000Z 366.58 null null
2008-06-04T09%3A36%3A00.000Z 366.58 null null
2008-06-04T09%3A40%3A00.000Z 366.59 null null
2008-06-04T09%3A48%3A00.000Z 366.59 null null
2008-06-04T09%3A52%3A00.000Z 366.58 null null
2008-06-04T10%3A04%3A00.000Z 366.58 null null
2008-06-04T10%3A08%3A00.000Z 366.59 null null
2008-06-04T10%3A16%3A00.000Z 366.59 null null
2008-06-04T10%3A20%3A00.000Z 366.6 null null
2008-06-04T10%3A24%3A00.000Z 366.59 null null
2008-06-04T10%3A36%3A00.000Z 366.59 null null
2008-06-04T10%3A40%3A00.000Z 366.6 null null
2008-06-04T10%3A44%3A00.000Z 366.59 null null
2008-06-04T10%3A48%3A00.000Z 366.59 null null
2008-06-04T10%3A52%3A00.000Z 366.6 null null
2008-06-04T10%3A56%3A00.000Z 366.59 null null
2008-06-04T11%3A00%3A00.000Z 366.6 null null
2008-06-04T11%3A04%3A00.000Z 366.59 null null
2008-06-04T11%3A12%3A00.000Z 366.59 null null
2008-06-04T11%3A16%3A00.000Z 366.6 null null
2008-06-04T11%3A20%3A00.000Z 366.59 null null
2008-06-04T11%3A24%3A00.000Z 366.6 null null
2008-06-04T11%3A52%3A00.000Z 366.6 null null
2008-06-04T11%3A56%3A00.000Z 366.61 null null
2008-06-04T12%3A00%3A00.000Z 366.6 null null
2008-06-04T12%3A12%3A00.000Z 366.6 null null
2008-06-04T12%3A16%3A00.000Z 366.61 null null
2008-06-04T12%3A20%3A00.000Z 366.6 null null
2008-06-04T12%3A32%3A00.000Z 366.6 null null
2008-06-04T12%3A36%3A00.000Z 366.61 null null
2008-06-04T12%3A48%3A00.000Z 366.61 null null
2008-06-04T12%3A52%3A00.000Z 366.6 null null
2008-06-04T12%3A56%3A00.000Z 366.61 null null
2008-06-04T13%3A12%3A00.000Z 366.61 null null
2008-06-04T13%3A16%3A00.000Z 366.6 null null
2008-06-04T13%3A20%3A00.000Z 366.61 null null
2008-06-04T13%3A24%3A00.000Z 366.6 null null
2008-06-04T13%3A28%3A00.000Z 366.61 null null
2008-06-04T14%3A24%3A00.000Z 366.61 null null
2008-06-04T14%3A28%3A00.000Z 366.62 null null
2008-06-04T14%3A32%3A00.000Z 366.61 null null
2008-06-04T14%3A36%3A00.000Z 366.62 null null
2008-06-04T14%3A48%3A00.000Z 366.62 null null
2008-06-04T14%3A52%3A00.000Z 366.63 null null
2008-06-04T14%3A56%3A00.000Z 366.63 null null
2008-06-04T15%3A00%3A00.000Z 366.62 null null
2008-06-04T15%3A04%3A00.000Z 366.64 null null
2008-06-04T15%3A08%3A00.000Z 366.63 null null
2008-06-04T15%3A16%3A00.000Z 366.63 null null
2008-06-04T15%3A20%3A00.000Z 366.64 null null
2008-06-04T15%3A36%3A00.000Z 366.64 null null
2008-06-04T15%3A40%3A00.000Z 366.65 null null
2008-06-04T15%3A52%3A00.000Z 366.65 null null
2008-06-04T15%3A56%3A00.000Z 366.66 null null
2008-06-04T16%3A12%3A00.000Z 366.66 null null
2008-06-04T16%3A16%3A00.000Z 366.67 null null
2008-06-04T16%3A32%3A00.000Z 366.67 null null
2008-06-04T16%3A36%3A00.000Z 366.68 null null
2008-06-04T16%3A40%3A00.000Z 366.68 null null
2008-06-04T16%3A44%3A00.000Z 366.69 null null
2008-06-04T16%3A48%3A00.000Z 366.69 null null
2008-06-04T16%3A52%3A00.000Z 366.68 null null
2008-06-04T16%3A56%3A00.000Z 366.69 null null
2008-06-04T17%3A00%3A00.000Z 366.69 null null
2008-06-04T17%3A04%3A00.000Z 366.7 null null
2008-06-04T17%3A24%3A00.000Z 366.7 null null
2008-06-04T17%3A28%3A00.000Z 366.69 null null
2008-06-04T17%3A32%3A00.000Z 366.7 null null
2008-06-04T17%3A36%3A00.000Z 366.7 null null
2008-06-04T17%3A40%3A00.000Z 366.71 null null
2008-06-04T17%3A56%3A00.000Z 366.71 null null
2008-06-04T18%3A00%3A00.000Z 366.72 null null
2008-06-04T18%3A04%3A00.000Z 366.71 null null
2008-06-04T18%3A08%3A00.000Z 366.72 null null
2008-06-04T18%3A32%3A00.000Z 366.72 null null
2008-06-04T18%3A36%3A00.000Z 366.73 null null
2008-06-04T18%3A44%3A00.000Z 366.73 null null
2008-06-04T18%3A48%3A00.000Z 366.74 null null
2008-06-04T18%3A52%3A00.000Z 366.73 null null
2008-06-04T19%3A00%3A00.000Z 366.73 null null
2008-06-04T19%3A04%3A00.000Z 366.74 null null
2008-06-04T19%3A08%3A00.000Z 366.73 null null
2008-06-04T19%3A12%3A00.000Z 366.74 null null
2008-06-04T19%3A40%3A00.000Z 366.74 null null
2008-06-04T19%3A44%3A00.000Z 366.75 null null
2008-06-04T19%3A48%3A00.000Z 366.74 null null
2008-06-04T19%3A56%3A00.000Z 366.74 null null
2008-06-04T20%3A00%3A00.000Z 366.75 null null
2008-06-04T20%3A12%3A00.000Z 366.75 null null
2008-06-04T20%3A16%3A00.000Z 366.76 null null
2008-06-04T20%3A40%3A00.000Z 366.76 null null
2008-06-04T20%3A44%3A00.000Z 366.75 null null
2008-06-04T20%3A48%3A00.000Z 366.77 null null
2008-06-04T20%3A52%3A00.000Z 366.76 null null
2008-06-04T20%3A56%3A00.000Z 366.77 null null
2008-06-04T21%3A00%3A00.000Z 366.77 null null
2008-06-04T21%3A04%3A00.000Z 366.76 null null
2008-06-04T21%3A08%3A00.000Z 366.76 null null
2008-06-04T21%3A12%3A00.000Z 366.77 null null
2008-06-04T21%3A28%3A00.000Z 366.77 null null
2008-06-04T21%3A32%3A00.000Z 366.79 null null
2008-06-04T21%3A40%3A00.000Z 366.77 null null
2008-06-04T21%3A44%3A00.000Z 366.78 null null
2008-06-04T22%3A04%3A00.000Z 366.78 null null
2008-06-04T22%3A08%3A00.000Z 366.79 null null
2008-06-04T22%3A12%3A00.000Z 366.78 null null
2008-06-04T22%3A16%3A00.000Z 366.79 null null
2008-06-04T22%3A20%3A00.000Z 366.78 null null
2008-06-04T22%3A36%3A00.000Z 366.78 null null
2008-06-04T22%3A40%3A00.000Z 366.79 null null
2008-06-04T22%3A44%3A00.000Z 366.78 null null
2008-06-04T22%3A48%3A00.000Z 366.79 null null
2008-06-04T22%3A52%3A00.000Z 366.78 null null
2008-06-04T22%3A56%3A00.000Z 366.79 null null
2008-06-04T23%3A12%3A00.000Z 366.79 null null
2008-06-04T23%3A16%3A00.000Z 366.78 null null
2008-06-04T23%3A20%3A00.000Z 366.8 null null
2008-06-04T23%3A24%3A00.000Z 366.8 null null
2008-06-04T23%3A28%3A00.000Z 366.79 null null
2008-06-04T23%3A32%3A00.000Z 366.79 null null
2008-06-04T23%3A36%3A00.000Z 366.8 null null
2008-06-04T23%3A40%3A00.000Z 366.79 null null
2008-06-04T23%3A44%3A00.000Z 366.8 null null
2008-06-04T23%3A52%3A00.000Z 366.8 null null
2008-06-04T23%3A56%3A00.000Z 366.79 null null
2008-06-05T00%3A00%3A00.000Z 366.79 null null
2008-06-05T00%3A04%3A00.000Z 366.81 null null
2008-06-05T00%3A20%3A00.000Z 366.81 null null
2008-06-05T00%3A24%3A00.000Z 366.8 null null
2008-06-05T00%3A28%3A00.000Z 366.81 null null
2008-06-05T00%3A40%3A00.000Z 366.81 null null
2008-06-05T00%3A44%3A00.000Z 366.8 null null
2008-06-05T00%3A52%3A00.000Z 366.82 null null
2008-06-05T00%3A56%3A00.000Z 366.81 null null
2008-06-05T01%3A00%3A00.000Z 366.81 null null
2008-06-05T01%3A04%3A00.000Z 366.82 null null
2008-06-05T01%3A44%3A00.000Z 366.82 null null
2008-06-05T01%3A48%3A00.000Z 366.83 null null
2008-06-05T01%3A52%3A00.000Z 366.82 null null
2008-06-05T02%3A00%3A00.000Z 366.82 null null
2008-06-05T02%3A04%3A00.000Z 366.83 null null
2008-06-05T02%3A08%3A00.000Z 366.82 null null
2008-06-05T02%3A12%3A00.000Z 366.83 null null
2008-06-05T02%3A16%3A00.000Z 366.83 null null
2008-06-05T02%3A20%3A00.000Z 366.82 null null
2008-06-05T02%3A24%3A00.000Z 366.83 null null
2008-06-05T02%3A28%3A00.000Z 366.83 null null
2008-06-05T02%3A32%3A00.000Z 366.82 null null
2008-06-05T02%3A36%3A00.000Z 366.83 null null
2008-06-05T02%3A52%3A00.000Z 366.83 null null
2008-06-05T02%3A56%3A00.000Z 366.84 null null
2008-06-05T03%3A00%3A00.000Z 366.83 null null
2008-06-05T03%3A20%3A00.000Z 366.83 null null
2008-06-05T03%3A24%3A00.000Z 366.84 null null
2008-06-05T04%3A00%3A00.000Z 366.84 null null
2008-06-05T04%3A04%3A00.000Z 366.85 null null
2008-06-05T04%3A08%3A00.000Z 366.84 null null
2008-06-05T04%3A12%3A00.000Z 366.85 null null
2008-06-05T04%3A16%3A00.000Z 366.84 null null
2008-06-05T04%3A20%3A00.000Z 366.84 null null
2008-06-05T04%3A24%3A00.000Z 366.85 null null
2008-06-05T04%3A28%3A00.000Z 366.84 null null
2008-06-05T04%3A32%3A00.000Z 366.84 null null
2008-06-05T04%3A36%3A00.000Z 366.85 null null
2008-06-05T04%3A40%3A00.000Z 366.84 null null
2008-06-05T04%3A44%3A00.000Z 366.84 null null
2008-06-05T04%3A48%3A00.000Z 366.85 null null
2008-06-05T05%3A32%3A00.000Z 366.85 null null
2008-06-05T05%3A36%3A00.000Z 366.86 null null
2008-06-05T05%3A40%3A00.000Z 366.85 null null
2008-06-05T05%3A44%3A00.000Z 366.85 null null
2008-06-05T05%3A48%3A00.000Z 366.86 null null
2008-06-05T05%3A52%3A00.000Z 366.85 null null
2008-06-05T05%3A56%3A00.000Z 366.86 null null
2008-06-05T06%3A20%3A00.000Z 366.86 null null
2008-06-05T06%3A24%3A00.000Z 366.87 null null
2008-06-05T06%3A28%3A00.000Z 366.86 null null
2008-06-05T06%3A32%3A00.000Z 366.86 null null
2008-06-05T06%3A36%3A00.000Z 366.87 null null
2008-06-05T07%3A00%3A00.000Z 366.87 null null
2008-06-05T07%3A04%3A00.000Z 366.86 null null
2008-06-05T07%3A12%3A00.000Z 366.88 null null
2008-06-05T07%3A20%3A00.000Z 366.86 null null
2008-06-05T07%3A24%3A00.000Z 366.87 null null
2008-06-05T07%3A28%3A00.000Z 366.87 null null
2008-06-05T07%3A32%3A00.000Z 366.88 null null
2008-06-05T07%3A36%3A00.000Z 366.87 null null
2008-06-05T07%3A44%3A00.000Z 366.87 null null
2008-06-05T07%3A48%3A00.000Z 366.89 null null
2008-06-05T07%3A52%3A00.000Z 366.88 null null
2008-06-05T08%3A12%3A00.000Z 366.88 null null
2008-06-05T08%3A16%3A00.000Z 366.89 null null
2008-06-05T08%3A20%3A00.000Z 366.88 null null
2008-06-05T08%3A32%3A00.000Z 366.88 null null
2008-06-05T08%3A36%3A00.000Z 366.89 null null
2008-06-05T08%3A44%3A00.000Z 366.89 null null
2008-06-05T08%3A48%3A00.000Z 366.9 null null
2008-06-05T08%3A52%3A00.000Z 366.89 null null
2008-06-05T09%3A00%3A00.000Z 366.89 null null
2008-06-05T09%3A04%3A00.000Z 366.9 null null
2008-06-05T09%3A08%3A00.000Z 366.89 null null
2008-06-05T09%3A12%3A00.000Z 366.89 null null
2008-06-05T09%3A16%3A00.000Z 366.9 null null
2008-06-05T09%3A28%3A00.000Z 366.9 null null
2008-06-05T09%3A32%3A00.000Z 366.91 null null
2008-06-05T09%3A56%3A00.000Z 366.91 null null
2008-06-05T10%3A00%3A00.000Z 366.92 null null
2008-06-05T10%3A04%3A00.000Z 366.91 null null
2008-06-05T10%3A08%3A00.000Z 366.92 null null
2008-06-05T10%3A32%3A00.000Z 366.92 null null
2008-06-05T10%3A36%3A00.000Z 366.93 null null
2008-06-05T10%3A40%3A00.000Z 366.93 null null
2008-06-05T10%3A44%3A00.000Z 366.94 null null
2008-06-05T10%3A48%3A00.000Z 366.93 null null
2008-06-05T10%3A52%3A00.000Z 366.93 null null
2008-06-05T10%3A56%3A00.000Z 366.94 null null
2008-06-05T11%3A00%3A00.000Z 366.93 null null
2008-06-05T11%3A04%3A00.000Z 366.94 null null
2008-06-05T11%3A08%3A00.000Z 366.94 null null
2008-06-05T11%3A12%3A00.000Z 366.93 null null
2008-06-05T11%3A20%3A00.000Z 366.95 null null
2008-06-05T11%3A24%3A00.000Z 366.95 null null
2008-06-05T11%3A28%3A00.000Z 366.94 null null
2008-06-05T11%3A32%3A00.000Z 366.95 null null
2008-06-05T11%3A36%3A00.000Z 366.95 null null
2008-06-05T11%3A40%3A00.000Z 366.94 null null
2008-06-05T11%3A44%3A00.000Z 366.96 null null
2008-06-05T11%3A48%3A00.000Z 366.95 null null
2008-06-05T11%3A52%3A00.000Z 366.95 null null
2008-06-05T11%3A56%3A00.000Z 366.96 null null
2008-06-05T12%3A00%3A00.000Z 366.95 null null
2008-06-05T12%3A04%3A00.000Z 366.96 null null
2008-06-05T12%3A12%3A00.000Z 366.96 null null
2008-06-05T12%3A16%3A00.000Z 366.97 null null
2008-06-05T12%3A36%3A00.000Z 366.97 null null
2008-06-05T12%3A40%3A00.000Z 366.98 null null
2008-06-05T12%3A44%3A00.000Z 366.97 null null
2008-06-05T12%3A48%3A00.000Z 366.97 null null
2008-06-05T12%3A52%3A00.000Z 366.98 null null
2008-06-05T13%3A12%3A00.000Z 366.98 null null
2008-06-05T13%3A16%3A00.000Z 366.99 null null
2008-06-05T13%3A24%3A00.000Z 366.99 null null
2008-06-05T13%3A28%3A00.000Z 367.0 null null
2008-06-05T13%3A32%3A00.000Z 366.99 null null
2008-06-05T13%3A40%3A00.000Z 366.99 null null
2008-06-05T13%3A44%3A00.000Z 367.0 null null
2008-06-05T14%3A12%3A00.000Z 367.0 null null
2008-06-05T14%3A16%3A00.000Z 367.01 null null
2008-06-05T14%3A20%3A00.000Z 367.0 null null
2008-06-05T14%3A24%3A00.000Z 367.01 null null
2008-06-05T14%3A32%3A00.000Z 367.01 null null
2008-06-05T14%3A36%3A00.000Z 367.02 null null
2008-06-05T15%3A04%3A00.000Z 367.02 null null
2008-06-05T15%3A08%3A00.000Z 367.03 null null
2008-06-05T15%3A12%3A00.000Z 367.03 null null
2008-06-05T15%3A16%3A00.000Z 367.02 null null
2008-06-05T15%3A20%3A00.000Z 367.03 null null
2008-06-05T15%3A28%3A00.000Z 367.03 null null
2008-06-05T15%3A32%3A00.000Z 367.02 null null
2008-06-05T15%3A40%3A00.000Z 367.04 null null
2008-06-05T15%3A44%3A00.000Z 367.04 null null
2008-06-05T15%3A48%3A00.000Z 367.03 null null
2008-06-05T15%3A52%3A00.000Z 367.04 null null
2008-06-05T16%3A12%3A00.000Z 367.04 null null
2008-06-05T16%3A16%3A00.000Z 367.05 null null
2008-06-05T16%3A20%3A00.000Z 367.04 null null
2008-06-05T16%3A24%3A00.000Z 367.05 null null
2008-06-05T17%3A00%3A00.000Z 367.05 null null
2008-06-05T17%3A04%3A00.000Z 367.06 null null
2008-06-05T17%3A32%3A00.000Z 367.06 null null
2008-06-05T17%3A36%3A00.000Z 367.07 null null
2008-06-05T17%3A40%3A00.000Z 367.06 null null
2008-06-05T17%3A44%3A00.000Z 367.07 null null
2008-06-05T17%3A48%3A00.000Z 367.06 null null
2008-06-05T17%3A56%3A00.000Z 367.08 null null
2008-06-05T18%3A00%3A00.000Z 367.07 null null
2008-06-05T18%3A04%3A00.000Z 367.08 null null
2008-06-05T18%3A08%3A00.000Z 367.07 null null
2008-06-05T18%3A16%3A00.000Z 367.07 null null
2008-06-05T18%3A20%3A00.000Z 367.08 null null
2008-06-05T18%3A24%3A00.000Z 367.07 null null
2008-06-05T18%3A32%3A00.000Z 367.09 null null
2008-06-05T18%3A36%3A00.000Z 367.08 null null
2008-06-05T18%3A40%3A00.000Z 367.09 null null
2008-06-05T18%3A44%3A00.000Z 367.08 null null
2008-06-05T18%3A48%3A00.000Z 367.08 null null
2008-06-05T18%3A52%3A00.000Z 367.07 null null
2008-06-05T19%3A00%3A00.000Z 367.07 null null
2008-06-05T19%3A04%3A00.000Z 367.08 null null
2008-06-05T19%3A08%3A00.000Z 367.07 null null
2008-06-05T19%3A12%3A00.000Z 367.08 null null
2008-06-05T19%3A16%3A00.000Z 367.08 null null
2008-06-05T19%3A20%3A00.000Z 367.07 null null
2008-06-05T19%3A24%3A00.000Z 367.07 null null
2008-06-05T19%3A28%3A00.000Z 367.08 null null
2008-06-05T19%3A32%3A00.000Z 367.08 null null
2008-06-05T19%3A36%3A00.000Z 367.09 null null
2008-06-05T19%3A40%3A00.000Z 367.08 null null
2008-06-05T19%3A48%3A00.000Z 367.08 null null
2008-06-05T19%3A52%3A00.000Z 367.09 null null
2008-06-05T19%3A56%3A00.000Z 367.08 null null
2008-06-05T20%3A16%3A00.000Z 367.08 null null
2008-06-05T20%3A20%3A00.000Z 367.09 null null
2008-06-05T20%3A28%3A00.000Z 367.09 null null
2008-06-05T20%3A32%3A00.000Z 367.08 null null
2008-06-05T20%3A36%3A00.000Z 367.09 null null
2008-06-05T20%3A40%3A00.000Z 367.09 null null
2008-06-05T20%3A44%3A00.000Z 367.08 null null
2008-06-05T20%3A48%3A00.000Z 367.09 null null
2008-06-05T20%3A52%3A00.000Z 367.08 null null
2008-06-05T20%3A56%3A00.000Z 367.09 null null
2008-06-05T21%3A00%3A00.000Z 367.09 null null
2008-06-05T21%3A04%3A00.000Z 367.1 null null
2008-06-05T21%3A32%3A00.000Z 367.1 null null
2008-06-05T21%3A36%3A00.000Z 367.11 null null
2008-06-05T21%3A40%3A00.000Z 367.11 null null
2008-06-05T21%3A44%3A00.000Z 367.1 null null
2008-06-05T21%3A48%3A00.000Z 367.1 null null
2008-06-05T21%3A52%3A00.000Z 367.11 null null
2008-06-05T21%3A56%3A00.000Z 367.11 null null
2008-06-05T22%3A00%3A00.000Z 367.12 null null
2008-06-05T22%3A04%3A00.000Z 367.11 null null
2008-06-05T22%3A12%3A00.000Z 367.11 null null
2008-06-05T22%3A16%3A00.000Z 367.12 null null
2008-06-05T22%3A20%3A00.000Z 367.11 null null
2008-06-05T22%3A24%3A00.000Z 367.12 null null
2008-06-05T22%3A28%3A00.000Z 367.12 null null
2008-06-05T22%3A32%3A00.000Z 367.11 null null
2008-06-05T22%3A36%3A00.000Z 367.12 null null
2008-06-05T22%3A40%3A00.000Z 367.12 null null
2008-06-05T22%3A44%3A00.000Z 367.13 null null
2008-06-05T23%3A16%3A00.000Z 367.13 null null
2008-06-05T23%3A20%3A00.000Z 367.14 null null
2008-06-05T23%3A24%3A00.000Z 367.13 null null
2008-06-05T23%3A32%3A00.000Z 367.13 null null
2008-06-05T23%3A36%3A00.000Z 367.14 null null
2008-06-05T23%3A44%3A00.000Z 367.14 null null
2008-06-05T23%3A48%3A00.000Z 367.13 null null
2008-06-05T23%3A52%3A00.000Z 367.13 null null
2008-06-05T23%3A56%3A00.000Z 367.14 null null
2008-06-06T00%3A16%3A00.000Z 367.14 null null
2008-06-06T00%3A20%3A00.000Z 367.13 null null
2008-06-06T00%3A28%3A00.000Z 367.15 null null
2008-06-06T00%3A32%3A00.000Z 367.15 null null
2008-06-06T00%3A36%3A00.000Z 367.14 null null
2008-06-06T00%3A40%3A00.000Z 367.15 null null
2008-06-06T00%3A44%3A00.000Z 367.15 null null
2008-06-06T00%3A48%3A00.000Z 367.14 null null
2008-06-06T00%3A52%3A00.000Z 367.14 null null
2008-06-06T00%3A56%3A00.000Z 367.15 null null
2008-06-06T01%3A12%3A00.000Z 367.15 null null
2008-06-06T01%3A16%3A00.000Z 367.16 null null
2008-06-06T01%3A20%3A00.000Z 367.16 null null
2008-06-06T01%3A24%3A00.000Z 367.15 null null
2008-06-06T01%3A28%3A00.000Z 367.16 null null
2008-06-06T01%3A32%3A00.000Z 367.16 null null
2008-06-06T01%3A36%3A00.000Z 367.15 null null
2008-06-06T01%3A40%3A00.000Z 367.16 null null
2008-06-06T02%3A08%3A00.000Z 367.16 null null
2008-06-06T02%3A12%3A00.000Z 367.15 null null
2008-06-06T02%3A16%3A00.000Z 367.17 null null
2008-06-06T02%3A20%3A00.000Z 367.16 null null
2008-06-06T02%3A36%3A00.000Z 367.16 null null
2008-06-06T02%3A40%3A00.000Z 367.17 null null
2008-06-06T02%3A44%3A00.000Z 367.16 null null
2008-06-06T02%3A48%3A00.000Z 367.16 null null
2008-06-06T02%3A52%3A00.000Z 367.17 null null
2008-06-06T03%3A44%3A00.000Z 367.17 null null
2008-06-06T03%3A48%3A00.000Z 367.18 null null
2008-06-06T04%3A12%3A00.000Z 367.18 null null
2008-06-06T04%3A16%3A00.000Z 367.19 null null
2008-06-06T04%3A20%3A00.000Z 367.18 null null
2008-06-06T04%3A24%3A00.000Z 367.18 null null
2008-06-06T04%3A28%3A00.000Z 367.19 null null
2008-06-06T04%3A32%3A00.000Z 367.19 null null
2008-06-06T04%3A36%3A00.000Z 367.18 null null
2008-06-06T04%3A40%3A00.000Z 367.19 null null
2008-06-06T04%3A44%3A00.000Z 367.18 null null
2008-06-06T04%3A48%3A00.000Z 367.19 null null
2008-06-06T04%3A52%3A00.000Z 367.18 null null
2008-06-06T04%3A56%3A00.000Z 367.19 null null
2008-06-06T05%3A04%3A00.000Z 367.19 null null
2008-06-06T05%3A08%3A00.000Z 367.2 null null
2008-06-06T05%3A12%3A00.000Z 367.19 null null
2008-06-06T05%3A36%3A00.000Z 367.19 null null
2008-06-06T05%3A40%3A00.000Z 367.2 null null
2008-06-06T05%3A44%3A00.000Z 367.19 null null
2008-06-06T05%3A48%3A00.000Z 367.2 null null
2008-06-06T05%3A56%3A00.000Z 367.2 null null
2008-06-06T06%3A00%3A00.000Z 367.19 null null
2008-06-06T06%3A08%3A00.000Z 367.21 null null
2008-06-06T06%3A12%3A00.000Z 367.2 null null
2008-06-06T06%3A16%3A00.000Z 367.2 null null
2008-06-06T06%3A20%3A00.000Z 367.21 null null
2008-06-06T06%3A24%3A00.000Z 367.2 null null
2008-06-06T06%3A32%3A00.000Z 367.2 null null
2008-06-06T06%3A36%3A00.000Z 367.21 null null
2008-06-06T06%3A40%3A00.000Z 367.2 null null
2008-06-06T06%3A44%3A00.000Z 367.2 null null
2008-06-06T06%3A48%3A00.000Z 367.21 null null
2008-06-06T07%3A12%3A00.000Z 367.21 null null
2008-06-06T07%3A16%3A00.000Z 367.22 null null
2008-06-06T07%3A20%3A00.000Z 367.21 null null
2008-06-06T07%3A24%3A00.000Z 367.22 null null
2008-06-06T07%3A32%3A00.000Z 367.22 null null
2008-06-06T07%3A36%3A00.000Z 367.21 null null
2008-06-06T07%3A40%3A00.000Z 367.22 null null
2008-06-06T07%3A52%3A00.000Z 367.22 null null
2008-06-06T07%3A56%3A00.000Z 367.21 null null
2008-06-06T08%3A04%3A00.000Z 367.23 null null
2008-06-06T08%3A08%3A00.000Z 367.23 null null
2008-06-06T08%3A12%3A00.000Z 367.22 null null
2008-06-06T08%3A16%3A00.000Z 367.23 null null
2008-06-06T08%3A28%3A00.000Z 367.23 null null
2008-06-06T08%3A32%3A00.000Z 367.22 null null
2008-06-06T08%3A36%3A00.000Z 367.23 null null
2008-06-06T08%3A40%3A00.000Z 367.23 null null
2008-06-06T08%3A44%3A00.000Z 367.22 null null
2008-06-06T08%3A48%3A00.000Z 367.23 null null
2008-06-06T09%3A24%3A00.000Z 367.23 null null
2008-06-06T09%3A28%3A00.000Z 367.24 null null
2008-06-06T09%3A32%3A00.000Z 367.23 null null
2008-06-06T09%3A44%3A00.000Z 367.23 null null
2008-06-06T09%3A48%3A00.000Z 367.24 null null
2008-06-06T10%3A16%3A00.000Z 367.24 null null
2008-06-06T10%3A20%3A00.000Z 367.25 null null
2008-06-06T10%3A24%3A00.000Z 367.24 null null
2008-06-06T10%3A28%3A00.000Z 367.25 null null
2008-06-06T10%3A32%3A00.000Z 367.24 null null
2008-06-06T10%3A36%3A00.000Z 367.25 null null
2008-06-06T10%3A40%3A00.000Z 367.24 null null
2008-06-06T10%3A44%3A00.000Z 367.25 null null
2008-06-06T10%3A56%3A00.000Z 367.25 null null
2008-06-06T11%3A00%3A00.000Z 367.26 null null
2008-06-06T11%3A04%3A00.000Z 367.24 null null
2008-06-06T11%3A08%3A00.000Z 367.25 null null
2008-06-06T11%3A12%3A00.000Z 367.24 null null
2008-06-06T11%3A16%3A00.000Z 367.25 null null
2008-06-06T11%3A20%3A00.000Z 367.24 null null
2008-06-06T11%3A24%3A00.000Z 367.25 null null
2008-06-06T11%3A28%3A00.000Z 367.25 null null
2008-06-06T11%3A32%3A00.000Z 367.24 null null
2008-06-06T11%3A36%3A00.000Z 367.25 null null
2008-06-06T11%3A40%3A00.000Z 367.25 null null
2008-06-06T11%3A44%3A00.000Z 367.24 null null
2008-06-06T11%3A48%3A00.000Z 367.25 null null
2008-06-06T11%3A52%3A00.000Z 367.24 null null
2008-06-06T11%3A56%3A00.000Z 367.25 null null
2008-06-06T13%3A04%3A00.000Z 367.25 null null
2008-06-06T13%3A08%3A00.000Z 367.24 null null
2008-06-06T13%3A12%3A00.000Z 367.24 null null
2008-06-06T13%3A16%3A00.000Z 367.25 null null
2008-06-06T13%3A20%3A00.000Z 367.25 null null
2008-06-06T13%3A24%3A00.000Z 367.24 null null
2008-06-06T13%3A28%3A00.000Z 367.25 null null
2008-06-06T13%3A36%3A00.000Z 367.25 null null
2008-06-06T13%3A40%3A00.000Z 367.23 null null
2008-06-06T13%3A44%3A00.000Z 367.25 null null
2008-06-06T13%3A48%3A00.000Z 367.24 null null
2008-06-06T13%3A52%3A00.000Z 367.24 null null
2008-06-06T13%3A56%3A00.000Z 367.25 null null
2008-06-06T14%3A00%3A00.000Z 367.24 null null
2008-06-06T14%3A20%3A00.000Z 367.24 null null
2008-06-06T14%3A24%3A00.000Z 367.23 null null
2008-06-06T14%3A28%3A00.000Z 367.24 null null
2008-06-06T14%3A32%3A00.000Z 367.24 null null
2008-06-06T14%3A36%3A00.000Z 367.25 null null
2008-06-06T14%3A40%3A00.000Z 367.24 null null
2008-06-06T14%3A52%3A00.000Z 367.24 null null
2008-06-06T14%3A56%3A00.000Z 367.23 null null
2008-06-06T15%3A00%3A00.000Z 367.24 null null
2008-06-06T15%3A04%3A00.000Z 367.24 null null
2008-06-06T15%3A08%3A00.000Z 367.23 null null
2008-06-06T15%3A28%3A00.000Z 367.23 null null
2008-06-06T15%3A32%3A00.000Z 367.24 null null
2008-06-06T15%3A36%3A00.000Z 367.24 null null
2008-06-06T15%3A40%3A00.000Z 367.23 null null
2008-06-06T15%3A44%3A00.000Z 367.21 null null
2008-06-06T15%3A48%3A00.000Z 367.23 null null
2008-06-06T15%3A52%3A00.000Z 367.22 null null
2008-06-06T15%3A56%3A00.000Z 367.22 null null
2008-06-06T16%3A00%3A00.000Z 367.23 null null
2008-06-06T16%3A04%3A00.000Z 367.23 null null
2008-06-06T16%3A08%3A00.000Z 367.22 null null
2008-06-06T16%3A12%3A00.000Z 367.23 null null
2008-06-06T16%3A32%3A00.000Z 367.23 null null
2008-06-06T16%3A36%3A00.000Z 367.22 null null
2008-06-06T16%3A40%3A00.000Z 367.23 null null
2008-06-06T16%3A56%3A00.000Z 367.23 null null
2008-06-06T17%3A04%3A00.000Z 367.21 null null
2008-06-06T17%3A12%3A00.000Z 367.23 null null
2008-06-06T17%3A16%3A00.000Z 367.23 null null
2008-06-06T17%3A20%3A00.000Z 367.22 null null
2008-06-06T17%3A32%3A00.000Z 367.22 null null
2008-06-06T17%3A36%3A00.000Z 367.23 null null
2008-06-06T17%3A40%3A00.000Z 367.22 null null
2008-06-06T17%3A48%3A00.000Z 367.22 null null
2008-06-06T17%3A52%3A00.000Z 367.21 null null
2008-06-06T18%3A00%3A00.000Z 367.21 null null
2008-06-06T18%3A04%3A00.000Z 367.22 null null
2008-06-06T18%3A08%3A00.000Z 367.22 null null
2008-06-06T18%3A16%3A00.000Z 367.2 null null
2008-06-06T18%3A20%3A00.000Z 367.22 null null
2008-06-06T18%3A24%3A00.000Z 367.2 null null
2008-06-06T18%3A28%3A00.000Z 367.21 null null
2008-06-06T18%3A40%3A00.000Z 367.21 null null
2008-06-06T18%3A44%3A00.000Z 367.2 null null
2008-06-06T18%3A48%3A00.000Z 367.21 null null
2008-06-06T18%3A52%3A00.000Z 367.2 null null
2008-06-06T19%3A12%3A00.000Z 367.2 null null
2008-06-06T19%3A16%3A00.000Z 367.21 null null
2008-06-06T19%3A24%3A00.000Z 367.19 null null
2008-06-06T19%3A28%3A00.000Z 367.19 null null
2008-06-06T19%3A36%3A00.000Z 367.21 null null
2008-06-06T19%3A44%3A00.000Z 367.19 null null
2008-06-06T19%3A52%3A00.000Z 367.19 null null
2008-06-06T19%3A56%3A00.000Z 367.2 null null
2008-06-06T20%3A00%3A00.000Z 367.19 null null
2008-06-06T20%3A16%3A00.000Z 367.19 null null
2008-06-06T20%3A20%3A00.000Z 367.18 null null
2008-06-06T20%3A24%3A00.000Z 367.19 null null
2008-06-06T20%3A28%3A00.000Z 367.18 null null
2008-06-06T20%3A32%3A00.000Z 367.18 null null
2008-06-06T20%3A36%3A00.000Z 367.19 null null
2008-06-06T20%3A40%3A00.000Z 367.18 null null
2008-06-06T21%3A00%3A00.000Z 367.18 null null
2008-06-06T21%3A04%3A00.000Z 367.19 null null
2008-06-06T21%3A08%3A00.000Z 367.18 null null
2008-06-06T21%3A24%3A00.000Z 367.18 null null
2008-06-06T21%3A28%3A00.000Z 367.17 null null
2008-06-06T21%3A44%3A00.000Z 367.17 null null
2008-06-06T21%3A48%3A00.000Z 367.16 null null
2008-06-06T21%3A52%3A00.000Z 367.17 null null
2008-06-06T22%3A00%3A00.000Z 367.17 null null
2008-06-06T22%3A04%3A00.000Z 367.18 null null
2008-06-06T22%3A12%3A00.000Z 367.16 null null
2008-06-06T22%3A36%3A00.000Z 367.16 null null
2008-06-06T22%3A40%3A00.000Z 367.17 null null
2008-06-06T22%3A44%3A00.000Z 367.17 null null
2008-06-06T22%3A48%3A00.000Z 367.15 null null
2008-06-06T22%3A52%3A00.000Z 367.16 null null
2008-06-06T23%3A36%3A00.000Z 367.16 null null
2008-06-06T23%3A40%3A00.000Z 367.15 null null
2008-06-06T23%3A44%3A00.000Z 367.16 null null
2008-06-06T23%3A48%3A00.000Z 367.15 null null
2008-06-07T00%3A08%3A00.000Z 367.15 null null
2008-06-07T00%3A12%3A00.000Z 367.14 null null
2008-06-07T00%3A16%3A00.000Z 367.15 null null
2008-06-07T00%3A20%3A00.000Z 367.15 null null
2008-06-07T00%3A24%3A00.000Z 367.14 null null
2008-06-07T00%3A28%3A00.000Z 367.15 null null
2008-06-07T00%3A32%3A00.000Z 367.14 null null
2008-06-07T00%3A44%3A00.000Z 367.14 null null
2008-06-07T00%3A48%3A00.000Z 367.13 null null
2008-06-07T00%3A52%3A00.000Z 367.14 null null
2008-06-07T01%3A04%3A00.000Z 367.14 null null
2008-06-07T01%3A08%3A00.000Z 367.13 null null
2008-06-07T01%3A12%3A00.000Z 367.13 null null
2008-06-07T01%3A16%3A00.000Z 367.14 null null
2008-06-07T01%3A20%3A00.000Z 367.14 null null
2008-06-07T01%3A24%3A00.000Z 367.13 null null
2008-06-07T01%3A36%3A00.000Z 367.13 null null
2008-06-07T01%3A40%3A00.000Z 367.12 null null
2008-06-07T01%3A48%3A00.000Z 367.14 null null
2008-06-07T01%3A56%3A00.000Z 367.12 null null
2008-06-07T02%3A00%3A00.000Z 367.13 null null
2008-06-07T02%3A04%3A00.000Z 367.13 null null
2008-06-07T02%3A08%3A00.000Z 367.12 null null
2008-06-07T02%3A20%3A00.000Z 367.12 null null
2008-06-07T02%3A24%3A00.000Z 367.13 null null
2008-06-07T02%3A32%3A00.000Z 367.11 null null
2008-06-07T02%3A40%3A00.000Z 367.13 null null
2008-06-07T02%3A44%3A00.000Z 367.12 null null
2008-06-07T02%3A52%3A00.000Z 367.12 null null
2008-06-07T02%3A56%3A00.000Z 367.11 null null
2008-06-07T03%3A00%3A00.000Z 367.12 null null
2008-06-07T03%3A04%3A00.000Z 367.12 null null
2008-06-07T03%3A08%3A00.000Z 367.11 null null
2008-06-07T03%3A12%3A00.000Z 367.12 null null
2008-06-07T03%3A16%3A00.000Z 367.11 null null
2008-06-07T03%3A24%3A00.000Z 367.11 null null
2008-06-07T03%3A28%3A00.000Z 367.1 null null
2008-06-07T03%3A32%3A00.000Z 367.11 null null
2008-06-07T03%3A44%3A00.000Z 367.11 null null
2008-06-07T03%3A48%3A00.000Z 367.1 null null
2008-06-07T03%3A52%3A00.000Z 367.11 null null
2008-06-07T03%3A56%3A00.000Z 367.1 null null
2008-06-07T04%3A00%3A00.000Z 367.11 null null
2008-06-07T04%3A04%3A00.000Z 367.1 null null
2008-06-07T04%3A12%3A00.000Z 367.1 null null
2008-06-07T04%3A16%3A00.000Z 367.11 null null
2008-06-07T04%3A20%3A00.000Z 367.11 null null
2008-06-07T04%3A24%3A00.000Z 367.1 null null
2008-06-07T04%3A32%3A00.000Z 367.1 null null
2008-06-07T04%3A36%3A00.000Z 367.09 null null
2008-06-07T04%3A44%3A00.000Z 367.11 null null
2008-06-07T04%3A48%3A00.000Z 367.1 null null
2008-06-07T04%3A52%3A00.000Z 367.1 null null
2008-06-07T04%3A56%3A00.000Z 367.09 null null
2008-06-07T05%3A00%3A00.000Z 367.09 null null
2008-06-07T05%3A04%3A00.000Z 367.11 null null
2008-06-07T05%3A08%3A00.000Z 367.1 null null
2008-06-07T05%3A16%3A00.000Z 367.1 null null
2008-06-07T05%3A20%3A00.000Z 367.08 null null
2008-06-07T05%3A28%3A00.000Z 367.1 null null
2008-06-07T05%3A32%3A00.000Z 367.09 null null
2008-06-07T05%3A36%3A00.000Z 367.1 null null
2008-06-07T05%3A40%3A00.000Z 367.09 null null
2008-06-07T06%3A08%3A00.000Z 367.09 null null
2008-06-07T06%3A12%3A00.000Z 367.07 null null
2008-06-07T06%3A16%3A00.000Z 367.09 null null
2008-06-07T06%3A24%3A00.000Z 367.07 null null
2008-06-07T06%3A28%3A00.000Z 367.09 null null
2008-06-07T06%3A32%3A00.000Z 367.09 null null
2008-06-07T06%3A36%3A00.000Z 367.08 null null
2008-06-07T06%3A40%3A00.000Z 367.08 null null
2008-06-07T06%3A44%3A00.000Z 367.07 null null
2008-06-07T06%3A48%3A00.000Z 367.08 null null
2008-06-07T07%3A12%3A00.000Z 367.08 null null
2008-06-07T07%3A16%3A00.000Z 367.09 null null
2008-06-07T07%3A20%3A00.000Z 367.08 null null
2008-06-07T07%3A28%3A00.000Z 367.08 null null
2008-06-07T07%3A32%3A00.000Z 367.07 null null
2008-06-07T07%3A36%3A00.000Z 367.07 null null
2008-06-07T07%3A40%3A00.000Z 367.08 null null
2008-06-07T07%3A44%3A00.000Z 367.07 null null
2008-06-07T07%3A56%3A00.000Z 367.07 null null
2008-06-07T08%3A00%3A00.000Z 367.06 null null
2008-06-07T08%3A04%3A00.000Z 367.06 null null
2008-06-07T08%3A08%3A00.000Z 367.07 null null
2008-06-07T08%3A12%3A00.000Z 367.07 null null
2008-06-07T08%3A16%3A00.000Z 367.06 null null
2008-06-07T08%3A20%3A00.000Z 367.07 null null
2008-06-07T08%3A40%3A00.000Z 367.07 null null
2008-06-07T08%3A44%3A00.000Z 367.06 null null
2008-06-07T08%3A48%3A00.000Z 367.07 null null
2008-06-07T08%3A52%3A00.000Z 367.07 null null
2008-06-07T08%3A56%3A00.000Z 367.06 null null
2008-06-07T09%3A00%3A00.000Z 367.07 null null
2008-06-07T09%3A08%3A00.000Z 367.07 null null
2008-06-07T09%3A12%3A00.000Z 367.06 null null
2008-06-07T09%3A20%3A00.000Z 367.06 null null
2008-06-07T09%3A24%3A00.000Z 367.05 null null
2008-06-07T09%3A28%3A00.000Z 367.06 null null
2008-06-07T09%3A32%3A00.000Z 367.06 null null
2008-06-07T09%3A36%3A00.000Z 367.05 null null
2008-06-07T09%3A40%3A00.000Z 367.06 null null
2008-06-07T09%3A44%3A00.000Z 367.05 null null
2008-06-07T09%3A48%3A00.000Z 367.05 null null
2008-06-07T09%3A52%3A00.000Z 367.06 null null
2008-06-07T09%3A56%3A00.000Z 367.05 null null
2008-06-07T10%3A24%3A00.000Z 367.05 null null
2008-06-07T10%3A28%3A00.000Z 367.04 null null
2008-06-07T10%3A32%3A00.000Z 367.05 null null
2008-06-07T10%3A36%3A00.000Z 367.04 null null
2008-06-07T10%3A44%3A00.000Z 367.04 null null
2008-06-07T10%3A48%3A00.000Z 367.03 null null
2008-06-07T10%3A52%3A00.000Z 367.03 null null
2008-06-07T10%3A56%3A00.000Z 367.04 null null
2008-06-07T11%3A00%3A00.000Z 367.04 null null
2008-06-07T11%3A04%3A00.000Z 367.03 null null
2008-06-07T11%3A08%3A00.000Z 367.03 null null
2008-06-07T11%3A12%3A00.000Z 367.04 null null
2008-06-07T11%3A16%3A00.000Z 367.03 null null
2008-06-07T11%3A20%3A00.000Z 367.04 null null
2008-06-07T11%3A24%3A00.000Z 367.03 null null
2008-06-07T11%3A56%3A00.000Z 367.03 null null
2008-06-07T12%3A00%3A00.000Z 367.02 null null
2008-06-07T12%3A20%3A00.000Z 367.02 null null
2008-06-07T12%3A24%3A00.000Z 367.0 null null
2008-06-07T12%3A28%3A00.000Z 367.02 null null
2008-06-07T12%3A32%3A00.000Z 367.01 null null
2008-06-07T12%3A48%3A00.000Z 367.01 null null
2008-06-07T12%3A52%3A00.000Z 367.0 null null
2008-06-07T12%3A56%3A00.000Z 367.01 null null
2008-06-07T13%3A00%3A00.000Z 367.01 null null
2008-06-07T13%3A04%3A00.000Z 366.99 null null
2008-06-07T13%3A08%3A00.000Z 367.01 null null
2008-06-07T13%3A12%3A00.000Z 367.01 null null
2008-06-07T13%3A20%3A00.000Z 366.99 null null
2008-06-07T13%3A24%3A00.000Z 366.99 null null
2008-06-07T13%3A28%3A00.000Z 367.0 null null
2008-06-07T13%3A32%3A00.000Z 366.99 null null
2008-06-07T13%3A36%3A00.000Z 367.0 null null
2008-06-07T13%3A40%3A00.000Z 367.0 null null
2008-06-07T13%3A44%3A00.000Z 366.99 null null
2008-06-07T13%3A56%3A00.000Z 366.99 null null
2008-06-07T14%3A00%3A00.000Z 366.98 null null
2008-06-07T14%3A04%3A00.000Z 366.98 null null
2008-06-07T14%3A08%3A00.000Z 366.99 null null
2008-06-07T14%3A12%3A00.000Z 366.98 null null
2008-06-07T14%3A16%3A00.000Z 366.98 null null
2008-06-07T14%3A20%3A00.000Z 366.99 null null
2008-06-07T14%3A24%3A00.000Z 366.98 null null
2008-06-07T14%3A28%3A00.000Z 366.98 null null
2008-06-07T14%3A32%3A00.000Z 366.97 null null
2008-06-07T14%3A36%3A00.000Z 366.97 null null
2008-06-07T14%3A44%3A00.000Z 366.99 null null
2008-06-07T14%3A48%3A00.000Z 366.96 null null
2008-06-07T14%3A52%3A00.000Z 366.97 null null
2008-06-07T14%3A56%3A00.000Z 366.97 null null
2008-06-07T15%3A00%3A00.000Z 366.98 null null
2008-06-07T15%3A04%3A00.000Z 366.97 null null
2008-06-07T15%3A12%3A00.000Z 366.97 null null
2008-06-07T15%3A16%3A00.000Z 366.96 null null
2008-06-07T15%3A20%3A00.000Z 366.96 null null
2008-06-07T15%3A24%3A00.000Z 366.95 null null
2008-06-07T15%3A28%3A00.000Z 366.97 null null
2008-06-07T15%3A32%3A00.000Z 366.96 null null
2008-06-07T15%3A40%3A00.000Z 366.96 null null
2008-06-07T15%3A44%3A00.000Z 366.97 null null
2008-06-07T15%3A48%3A00.000Z 366.95 null null
2008-06-07T16%3A12%3A00.000Z 366.95 null null
2008-06-07T16%3A16%3A00.000Z 366.96 null null
2008-06-07T16%3A20%3A00.000Z 366.94 null null
2008-06-07T16%3A24%3A00.000Z 366.95 null null
2008-06-07T16%3A32%3A00.000Z 366.95 null null
2008-06-07T16%3A36%3A00.000Z 366.94 null null
2008-06-07T16%3A40%3A00.000Z 366.94 null null
2008-06-07T16%3A44%3A00.000Z 366.93 null null
2008-06-07T16%3A48%3A00.000Z 366.94 null null
2008-06-07T17%3A04%3A00.000Z 366.94 null null
2008-06-07T17%3A08%3A00.000Z 366.93 null null
2008-06-07T17%3A12%3A00.000Z 366.95 null null
2008-06-07T17%3A16%3A00.000Z 366.93 null null
2008-06-07T17%3A20%3A00.000Z 366.94 null null
2008-06-07T17%3A24%3A00.000Z 366.94 null null
2008-06-07T17%3A28%3A00.000Z 366.95 null null
2008-06-07T17%3A32%3A00.000Z 366.93 null null
2008-06-07T17%3A36%3A00.000Z 366.93 null null
2008-06-07T17%3A40%3A00.000Z 366.94 null null
2008-06-07T17%3A44%3A00.000Z 366.93 null null
2008-06-07T17%3A48%3A00.000Z 366.94 null null
2008-06-07T17%3A52%3A00.000Z 366.93 null null
2008-06-07T17%3A56%3A00.000Z 366.93 null null
2008-06-07T18%3A00%3A00.000Z 366.92 null null
2008-06-07T18%3A04%3A00.000Z 366.93 null null
2008-06-07T18%3A08%3A00.000Z 366.92 null null
2008-06-07T18%3A12%3A00.000Z 366.93 null null
2008-06-07T18%3A20%3A00.000Z 366.93 null null
2008-06-07T18%3A24%3A00.000Z 366.92 null null
2008-06-07T18%3A32%3A00.000Z 366.92 null null
2008-06-07T18%3A36%3A00.000Z 366.93 null null
2008-06-07T18%3A40%3A00.000Z 366.92 null null
2008-06-07T18%3A52%3A00.000Z 366.92 null null
2008-06-07T18%3A56%3A00.000Z 366.91 null null
2008-06-07T19%3A00%3A00.000Z 366.92 null null
2008-06-07T19%3A08%3A00.000Z 366.92 null null
2008-06-07T19%3A12%3A00.000Z 366.91 null null
2008-06-07T19%3A28%3A00.000Z 366.91 null null
2008-06-07T19%3A32%3A00.000Z 366.92 null null
2008-06-07T19%3A36%3A00.000Z 366.91 null null
2008-06-07T19%3A44%3A00.000Z 366.91 null null
2008-06-07T19%3A48%3A00.000Z 366.9 null null
2008-06-07T19%3A52%3A00.000Z 366.9 null null
2008-06-07T19%3A56%3A00.000Z 366.91 null null
2008-06-07T20%3A16%3A00.000Z 366.91 null null
2008-06-07T20%3A20%3A00.000Z 366.9 null null
2008-06-07T20%3A24%3A00.000Z 366.91 null null
2008-06-07T20%3A28%3A00.000Z 366.9 null null
2008-06-07T20%3A32%3A00.000Z 366.9 null null
2008-06-07T20%3A36%3A00.000Z 366.91 null null
2008-06-07T20%3A40%3A00.000Z 366.9 null null
2008-06-07T20%3A44%3A00.000Z 366.9 null null
2008-06-07T20%3A48%3A00.000Z 366.91 null null
2008-06-07T20%3A56%3A00.000Z 366.89 null null
2008-06-07T21%3A00%3A00.000Z 366.9 null null
2008-06-07T21%3A08%3A00.000Z 366.9 null null
2008-06-07T21%3A12%3A00.000Z 366.89 null null
2008-06-07T21%3A16%3A00.000Z 366.9 null null
2008-06-07T21%3A32%3A00.000Z 366.9 null null
2008-06-07T21%3A36%3A00.000Z 366.89 null null
2008-06-07T21%3A40%3A00.000Z 366.9 null null
2008-06-07T21%3A44%3A00.000Z 366.89 null null
2008-06-07T21%3A56%3A00.000Z 366.89 null null
2008-06-07T22%3A00%3A00.000Z 366.88 null null
2008-06-07T22%3A04%3A00.000Z 366.89 null null
2008-06-07T22%3A08%3A00.000Z 366.89 null null
2008-06-07T22%3A12%3A00.000Z 366.88 null null
2008-06-07T22%3A16%3A00.000Z 366.89 null null
2008-06-07T22%3A20%3A00.000Z 366.88 null null
2008-06-07T22%3A24%3A00.000Z 366.89 null null
2008-06-07T22%3A28%3A00.000Z 366.89 null null
2008-06-07T22%3A32%3A00.000Z 366.88 null null
2008-06-07T22%3A36%3A00.000Z 366.89 null null
2008-06-07T22%3A40%3A00.000Z 366.89 null null
2008-06-07T22%3A44%3A00.000Z 366.87 null null
2008-06-07T22%3A48%3A00.000Z 366.89 null null
2008-06-07T22%3A52%3A00.000Z 366.88 null null
2008-06-07T22%3A56%3A00.000Z 366.88 null null
2008-06-07T23%3A00%3A00.000Z 366.87 null null
2008-06-07T23%3A04%3A00.000Z 366.87 null null
2008-06-07T23%3A08%3A00.000Z 366.88 null null
2008-06-07T23%3A24%3A00.000Z 366.88 null null
2008-06-07T23%3A28%3A00.000Z 366.87 null null
2008-06-07T23%3A32%3A00.000Z 366.88 null null
2008-06-07T23%3A40%3A00.000Z 366.88 null null
2008-06-07T23%3A44%3A00.000Z 366.87 null null
2008-06-07T23%3A48%3A00.000Z 366.88 null null
2008-06-07T23%3A52%3A00.000Z 366.87 null null
2008-06-07T23%3A56%3A00.000Z 366.87 null null
2008-06-08T00%3A00%3A00.000Z 366.86 null null
2008-06-08T00%3A04%3A00.000Z 366.87 null null
2008-06-08T00%3A08%3A00.000Z 366.86 null null
2008-06-08T00%3A12%3A00.000Z 366.86 null null
2008-06-08T00%3A16%3A00.000Z 366.87 null null
2008-06-08T00%3A20%3A00.000Z 366.86 null null
2008-06-08T00%3A24%3A00.000Z 366.87 null null
2008-06-08T00%3A28%3A00.000Z 366.87 null null
2008-06-08T00%3A32%3A00.000Z 366.86 null null
2008-06-08T00%3A36%3A00.000Z 366.87 null null
2008-06-08T00%3A40%3A00.000Z 366.86 null null
2008-06-08T01%3A04%3A00.000Z 366.86 null null
2008-06-08T01%3A08%3A00.000Z 366.85 null null
2008-06-08T01%3A12%3A00.000Z 366.86 null null
2008-06-08T01%3A24%3A00.000Z 366.86 null null
2008-06-08T01%3A28%3A00.000Z 366.85 null null
2008-06-08T01%3A36%3A00.000Z 366.85 null null
2008-06-08T01%3A40%3A00.000Z 366.86 null null
2008-06-08T01%3A44%3A00.000Z 366.85 null null
2008-06-08T02%3A04%3A00.000Z 366.85 null null
2008-06-08T02%3A08%3A00.000Z 366.86 null null
2008-06-08T02%3A12%3A00.000Z 366.85 null null
2008-06-08T02%3A36%3A00.000Z 366.85 null null
2008-06-08T02%3A40%3A00.000Z 366.84 null null
2008-06-08T02%3A44%3A00.000Z 366.85 null null
2008-06-08T02%3A48%3A00.000Z 366.84 null null
2008-06-08T02%3A52%3A00.000Z 366.84 null null
2008-06-08T02%3A56%3A00.000Z 366.85 null null
2008-06-08T03%3A00%3A00.000Z 366.84 null null
2008-06-08T03%3A16%3A00.000Z 366.84 null null
2008-06-08T03%3A20%3A00.000Z 366.83 null null
2008-06-08T03%3A24%3A00.000Z 366.84 null null
2008-06-08T03%3A56%3A00.000Z 366.84 null null
2008-06-08T04%3A04%3A00.000Z 366.82 null null
2008-06-08T04%3A08%3A00.000Z 366.83 null null
2008-06-08T04%3A20%3A00.000Z 366.83 null null
2008-06-08T04%3A24%3A00.000Z 366.82 null null
2008-06-08T04%3A28%3A00.000Z 366.83 null null
2008-06-08T04%3A32%3A00.000Z 366.83 null null
2008-06-08T04%3A36%3A00.000Z 366.82 null null
2008-06-08T04%3A40%3A00.000Z 366.83 null null
2008-06-08T04%3A48%3A00.000Z 366.83 null null
2008-06-08T04%3A52%3A00.000Z 366.82 null null
2008-06-08T04%3A56%3A00.000Z 366.82 null null
2008-06-08T05%3A00%3A00.000Z 366.83 null null
2008-06-08T05%3A04%3A00.000Z 366.81 null null
2008-06-08T05%3A08%3A00.000Z 366.82 null null
2008-06-08T05%3A12%3A00.000Z 366.81 null null
2008-06-08T05%3A16%3A00.000Z 366.81 null null
2008-06-08T05%3A20%3A00.000Z 366.82 null null
2008-06-08T05%3A36%3A00.000Z 366.82 null null
2008-06-08T05%3A40%3A00.000Z 366.81 null null
2008-06-08T05%3A48%3A00.000Z 366.81 null null
2008-06-08T05%3A52%3A00.000Z 366.8 null null
2008-06-08T05%3A56%3A00.000Z 366.81 null null
2008-06-08T06%3A00%3A00.000Z 366.81 null null
2008-06-08T06%3A04%3A00.000Z 366.82 null null
2008-06-08T06%3A08%3A00.000Z 366.81 null null
2008-06-08T06%3A24%3A00.000Z 366.81 null null
2008-06-08T06%3A28%3A00.000Z 366.8 null null
2008-06-08T07%3A00%3A00.000Z 366.8 null null
2008-06-08T07%3A04%3A00.000Z 366.79 null null
2008-06-08T07%3A08%3A00.000Z 366.8 null null
2008-06-08T07%3A12%3A00.000Z 366.8 null null
2008-06-08T07%3A16%3A00.000Z 366.79 null null
2008-06-08T07%3A24%3A00.000Z 366.79 null null
2008-06-08T07%3A28%3A00.000Z 366.8 null null
2008-06-08T07%3A32%3A00.000Z 366.79 null null
2008-06-08T07%3A36%3A00.000Z 366.8 null null
2008-06-08T07%3A40%3A00.000Z 366.79 null null
2008-06-08T07%3A48%3A00.000Z 366.79 null null
2008-06-08T07%3A52%3A00.000Z 366.78 null null
2008-06-08T07%3A56%3A00.000Z 366.79 null null
2008-06-08T08%3A04%3A00.000Z 366.79 null null
2008-06-08T08%3A08%3A00.000Z 366.78 null null
2008-06-08T08%3A12%3A00.000Z 366.79 null null
2008-06-08T08%3A24%3A00.000Z 366.79 null null
2008-06-08T08%3A32%3A00.000Z 366.77 null null
2008-06-08T08%3A36%3A00.000Z 366.79 null null
2008-06-08T08%3A40%3A00.000Z 366.78 null null
2008-06-08T08%3A44%3A00.000Z 366.79 null null
2008-06-08T08%3A48%3A00.000Z 366.78 null null
2008-06-08T08%3A56%3A00.000Z 366.78 null null
2008-06-08T09%3A00%3A00.000Z 366.77 null null
2008-06-08T09%3A04%3A00.000Z 366.78 null null
2008-06-08T09%3A08%3A00.000Z 366.77 null null
2008-06-08T09%3A16%3A00.000Z 366.77 null null
2008-06-08T09%3A20%3A00.000Z 366.78 null null
2008-06-08T09%3A32%3A00.000Z 366.78 null null
2008-06-08T09%3A36%3A00.000Z 366.77 null null
2008-06-08T09%3A48%3A00.000Z 366.77 null null
2008-06-08T09%3A52%3A00.000Z 366.76 null null
2008-06-08T09%3A56%3A00.000Z 366.76 null null
2008-06-08T10%3A00%3A00.000Z 366.77 null null
2008-06-08T10%3A04%3A00.000Z 366.77 null null
2008-06-08T10%3A08%3A00.000Z 366.76 null null
2008-06-08T10%3A12%3A00.000Z 366.76 null null
2008-06-08T10%3A16%3A00.000Z 366.77 null null
2008-06-08T10%3A20%3A00.000Z 366.76 null null
2008-06-08T10%3A36%3A00.000Z 366.76 null null
2008-06-08T10%3A40%3A00.000Z 366.77 null null
2008-06-08T10%3A44%3A00.000Z 366.76 null null
2008-06-08T10%3A48%3A00.000Z 366.76 null null
2008-06-08T10%3A52%3A00.000Z 366.77 null null
2008-06-08T10%3A56%3A00.000Z 366.76 null null
2008-06-08T11%3A00%3A00.000Z 366.76 null null
2008-06-08T11%3A04%3A00.000Z 366.75 null null
2008-06-08T11%3A08%3A00.000Z 366.76 null null
2008-06-08T11%3A16%3A00.000Z 366.76 null null
2008-06-08T11%3A20%3A00.000Z 366.75 null null
2008-06-08T11%3A24%3A00.000Z 366.76 null null
2008-06-08T11%3A40%3A00.000Z 366.76 null null
2008-06-08T11%3A44%3A00.000Z 366.75 null null
2008-06-08T11%3A48%3A00.000Z 366.75 null null
2008-06-08T11%3A52%3A00.000Z 366.76 null null
2008-06-08T11%3A56%3A00.000Z 366.76 null null
2008-06-08T12%3A00%3A00.000Z 366.75 null null
2008-06-08T12%3A12%3A00.000Z 366.75 null null
2008-06-08T12%3A16%3A00.000Z 366.74 null null
2008-06-08T12%3A20%3A00.000Z 366.74 null null
2008-06-08T12%3A24%3A00.000Z 366.73 null null
2008-06-08T12%3A28%3A00.000Z 366.75 null null
2008-06-08T12%3A32%3A00.000Z 366.74 null null
2008-06-08T12%3A36%3A00.000Z 366.75 null null
2008-06-08T12%3A40%3A00.000Z 366.74 null null
2008-06-08T12%3A44%3A00.000Z 366.75 null null
2008-06-08T12%3A48%3A00.000Z 366.74 null null
2008-06-08T13%3A08%3A00.000Z 366.74 null null
2008-06-08T13%3A12%3A00.000Z 366.73 null null
2008-06-08T13%3A16%3A00.000Z 366.74 null null
2008-06-08T13%3A20%3A00.000Z 366.74 null null
2008-06-08T13%3A24%3A00.000Z 366.73 null null
2008-06-08T13%3A28%3A00.000Z 366.74 null null
2008-06-08T13%3A32%3A00.000Z 366.73 null null
2008-06-08T13%3A36%3A00.000Z 366.74 null null
2008-06-08T13%3A40%3A00.000Z 366.73 null null
2008-06-08T13%3A44%3A00.000Z 366.74 null null
2008-06-08T13%3A48%3A00.000Z 366.74 null null
2008-06-08T13%3A52%3A00.000Z 366.73 null null
2008-06-08T13%3A56%3A00.000Z 366.73 null null
2008-06-08T14%3A00%3A00.000Z 366.72 null null
2008-06-08T14%3A04%3A00.000Z 366.73 null null
2008-06-08T14%3A12%3A00.000Z 366.73 null null
2008-06-08T14%3A16%3A00.000Z 366.72 null null
2008-06-08T14%3A20%3A00.000Z 366.73 null null
2008-06-08T14%3A24%3A00.000Z 366.72 null null
2008-06-08T14%3A28%3A00.000Z 366.73 null null
2008-06-08T14%3A40%3A00.000Z 366.73 null null
2008-06-08T14%3A44%3A00.000Z 366.72 null null
2008-06-08T14%3A48%3A00.000Z 366.72 null null
2008-06-08T14%3A52%3A00.000Z 366.71 null null
2008-06-08T14%3A56%3A00.000Z 366.72 null null
2008-06-08T15%3A00%3A00.000Z 366.71 null null
2008-06-08T15%3A04%3A00.000Z 366.72 null null
2008-06-08T15%3A12%3A00.000Z 366.72 null null
2008-06-08T15%3A16%3A00.000Z 366.71 null null
2008-06-08T15%3A20%3A00.000Z 366.72 null null
2008-06-08T15%3A28%3A00.000Z 366.72 null null
2008-06-08T15%3A32%3A00.000Z 366.71 null null
2008-06-08T16%3A12%3A00.000Z 366.71 null null
2008-06-08T16%3A16%3A00.000Z 366.7 null null
2008-06-08T16%3A20%3A00.000Z 366.7 null null
2008-06-08T16%3A24%3A00.000Z 366.71 null null
2008-06-08T16%3A28%3A00.000Z 366.7 null null
2008-06-08T17%3A00%3A00.000Z 366.7 null null
2008-06-08T17%3A04%3A00.000Z 366.69 null null
2008-06-08T17%3A08%3A00.000Z 366.7 null null
2008-06-08T17%3A16%3A00.000Z 366.7 null null
2008-06-08T17%3A20%3A00.000Z 366.69 null null
2008-06-08T17%3A28%3A00.000Z 366.69 null null
2008-06-08T17%3A32%3A00.000Z 366.7 null null
2008-06-08T17%3A36%3A00.000Z 366.69 null null
2008-06-08T18%3A08%3A00.000Z 366.69 null null
2008-06-08T18%3A12%3A00.000Z 366.7 null null
2008-06-08T18%3A16%3A00.000Z 366.69 null null
2008-06-08T18%3A20%3A00.000Z 366.69 null null
2008-06-08T18%3A24%3A00.000Z 366.68 null null
2008-06-08T18%3A28%3A00.000Z 366.68 null null
2008-06-08T18%3A32%3A00.000Z 366.69 null null
2008-06-08T18%3A36%3A00.000Z 366.69 null null
2008-06-08T18%3A40%3A00.000Z 366.68 null null
2008-06-08T19%3A00%3A00.000Z 366.68 null null
2008-06-08T19%3A04%3A00.000Z 366.69 null null
2008-06-08T19%3A08%3A00.000Z 366.68 null null
2008-06-08T19%3A16%3A00.000Z 366.68 null null
2008-06-08T19%3A20%3A00.000Z 366.69 null null
2008-06-08T19%3A28%3A00.000Z 366.67 null null
2008-06-08T19%3A32%3A00.000Z 366.68 null null
2008-06-08T19%3A36%3A00.000Z 366.67 null null
2008-06-08T19%3A44%3A00.000Z 366.67 null null
2008-06-08T19%3A48%3A00.000Z 366.68 null null
2008-06-08T19%3A52%3A00.000Z 366.67 null null
2008-06-08T19%3A56%3A00.000Z 366.68 null null
2008-06-08T20%3A00%3A00.000Z 366.67 null null
2008-06-08T20%3A24%3A00.000Z 366.67 null null
2008-06-08T20%3A28%3A00.000Z 366.65 null null
2008-06-08T20%3A32%3A00.000Z 366.67 null null
2008-06-08T20%3A36%3A00.000Z 366.66 null null
2008-06-08T21%3A00%3A00.000Z 366.66 null null
2008-06-08T21%3A04%3A00.000Z 366.65 null null
2008-06-08T21%3A08%3A00.000Z 366.65 null null
2008-06-08T21%3A12%3A00.000Z 366.66 null null
2008-06-08T21%3A20%3A00.000Z 366.66 null null
2008-06-08T21%3A24%3A00.000Z 366.65 null null
2008-06-08T21%3A56%3A00.000Z 366.65 null null
2008-06-08T22%3A00%3A00.000Z 366.64 null null
2008-06-08T22%3A04%3A00.000Z 366.64 null null
2008-06-08T22%3A08%3A00.000Z 366.65 null null
2008-06-08T22%3A16%3A00.000Z 366.65 null null
2008-06-08T22%3A20%3A00.000Z 366.64 null null
2008-06-08T22%3A24%3A00.000Z 366.65 null null
2008-06-08T22%3A28%3A00.000Z 366.64 null null
2008-06-08T22%3A52%3A00.000Z 366.64 null null
2008-06-08T22%3A56%3A00.000Z 366.63 null null
2008-06-08T23%3A00%3A00.000Z 366.63 null null
2008-06-08T23%3A04%3A00.000Z 366.62 null null
2008-06-08T23%3A08%3A00.000Z 366.64 null null
2008-06-08T23%3A12%3A00.000Z 366.63 null null
2008-06-08T23%3A20%3A00.000Z 366.63 null null
2008-06-08T23%3A24%3A00.000Z 366.64 null null
2008-06-08T23%3A32%3A00.000Z 366.62 null null
2008-06-08T23%3A36%3A00.000Z 366.63 null null
2008-06-08T23%3A40%3A00.000Z 366.63 null null
2008-06-08T23%3A44%3A00.000Z 366.62 null null
2008-06-08T23%3A48%3A00.000Z 366.63 null null
2008-06-08T23%3A52%3A00.000Z 366.62 null null
2008-06-09T00%3A20%3A00.000Z 366.62 null null
2008-06-09T00%3A24%3A00.000Z 366.61 null null
2008-06-09T00%3A28%3A00.000Z 366.61 null null
2008-06-09T00%3A32%3A00.000Z 366.62 null null
2008-06-09T00%3A36%3A00.000Z 366.62 null null
2008-06-09T00%3A40%3A00.000Z 366.61 null null
2008-06-09T00%3A44%3A00.000Z 366.61 null null
2008-06-09T00%3A48%3A00.000Z 366.62 null null
2008-06-09T00%3A56%3A00.000Z 366.6 null null
2008-06-09T01%3A00%3A00.000Z 366.61 null null
2008-06-09T01%3A16%3A00.000Z 366.61 null null
2008-06-09T01%3A20%3A00.000Z 366.6 null null
2008-06-09T01%3A36%3A00.000Z 366.6 null null
2008-06-09T01%3A40%3A00.000Z 366.61 null null
2008-06-09T01%3A44%3A00.000Z 366.59 null null
2008-06-09T01%3A48%3A00.000Z 366.6 null null
2008-06-09T02%3A16%3A00.000Z 366.6 null null
2008-06-09T02%3A24%3A00.000Z 366.58 null null
2008-06-09T02%3A32%3A00.000Z 366.6 null null
2008-06-09T02%3A36%3A00.000Z 366.59 null null
2008-06-09T03%3A08%3A00.000Z 366.59 null null
2008-06-09T03%3A12%3A00.000Z 366.58 null null
2008-06-09T03%3A16%3A00.000Z 366.58 null null
2008-06-09T03%3A20%3A00.000Z 366.59 null null
2008-06-09T03%3A24%3A00.000Z 366.58 null null
2008-06-09T03%3A36%3A00.000Z 366.58 null null
2008-06-09T03%3A40%3A00.000Z 366.57 null null
2008-06-09T03%3A44%3A00.000Z 366.57 null null
2008-06-09T03%3A48%3A00.000Z 366.58 null null
2008-06-09T03%3A56%3A00.000Z 366.58 null null
2008-06-09T04%3A00%3A00.000Z 366.57 null null
2008-06-09T04%3A08%3A00.000Z 366.57 null null
2008-06-09T04%3A12%3A00.000Z 366.56 null null
2008-06-09T04%3A20%3A00.000Z 366.56 null null
2008-06-09T04%3A24%3A00.000Z 366.57 null null
2008-06-09T04%3A28%3A00.000Z 366.57 null null
2008-06-09T04%3A32%3A00.000Z 366.55 null null
2008-06-09T04%3A36%3A00.000Z 366.56 null null
2008-06-09T05%3A12%3A00.000Z 366.56 null null
2008-06-09T05%3A16%3A00.000Z 366.55 null null
2008-06-09T05%3A20%3A00.000Z 366.56 null null
2008-06-09T05%3A24%3A00.000Z 366.55 null null
2008-06-09T05%3A28%3A00.000Z 366.56 null null
2008-06-09T05%3A32%3A00.000Z 366.56 null null
2008-06-09T05%3A36%3A00.000Z 366.55 null null
2008-06-09T05%3A44%3A00.000Z 366.55 null null
2008-06-09T05%3A48%3A00.000Z 366.54 null null
2008-06-09T05%3A52%3A00.000Z 366.55 null null
2008-06-09T05%3A56%3A00.000Z 366.54 null null
2008-06-09T06%3A00%3A00.000Z 366.54 null null
2008-06-09T06%3A04%3A00.000Z 366.55 null null
2008-06-09T06%3A12%3A00.000Z 366.55 null null
2008-06-09T06%3A16%3A00.000Z 366.54 null null
2008-06-09T06%3A28%3A00.000Z 366.54 null null
2008-06-09T06%3A32%3A00.000Z 366.53 null null
2008-06-09T06%3A36%3A00.000Z 366.54 null null
2008-06-09T06%3A44%3A00.000Z 366.54 null null
2008-06-09T06%3A48%3A00.000Z 366.53 null null
2008-06-09T07%3A04%3A00.000Z 366.53 null null
2008-06-09T07%3A08%3A00.000Z 366.52 null null
2008-06-09T07%3A12%3A00.000Z 366.52 null null
2008-06-09T07%3A16%3A00.000Z 366.53 null null
2008-06-09T07%3A20%3A00.000Z 366.52 null null
2008-06-09T07%3A32%3A00.000Z 366.52 null null
2008-06-09T07%3A36%3A00.000Z 366.53 null null
2008-06-09T07%3A40%3A00.000Z 366.53 null null
2008-06-09T07%3A44%3A00.000Z 366.52 null null
2008-06-09T07%3A52%3A00.000Z 366.52 null null
2008-06-09T07%3A56%3A00.000Z 366.51 null null
2008-06-09T08%3A00%3A00.000Z 366.52 null null
2008-06-09T08%3A04%3A00.000Z 366.51 null null
2008-06-09T08%3A16%3A00.000Z 366.51 null null
2008-06-09T08%3A20%3A00.000Z 366.52 null null
2008-06-09T08%3A24%3A00.000Z 366.51 null null
2008-06-09T08%3A36%3A00.000Z 366.51 null null
2008-06-09T08%3A40%3A00.000Z 366.5 null null
2008-06-09T08%3A44%3A00.000Z 366.5 null null
2008-06-09T08%3A48%3A00.000Z 366.51 null null
2008-06-09T08%3A52%3A00.000Z 366.51 null null
2008-06-09T08%3A56%3A00.000Z 366.5 null null
2008-06-09T09%3A20%3A00.000Z 366.5 null null
2008-06-09T09%3A24%3A00.000Z 366.49 null null
2008-06-09T09%3A28%3A00.000Z 366.5 null null
2008-06-09T09%3A36%3A00.000Z 366.5 null null
2008-06-09T09%3A40%3A00.000Z 366.49 null null
2008-06-09T09%3A44%3A00.000Z 366.49 null null
2008-06-09T09%3A48%3A00.000Z 366.48 null null
2008-06-09T09%3A52%3A00.000Z 366.49 null null
2008-06-09T09%3A56%3A00.000Z 366.48 null null
2008-06-09T10%3A00%3A00.000Z 366.49 null null
2008-06-09T10%3A04%3A00.000Z 366.49 null null
2008-06-09T10%3A08%3A00.000Z 366.48 null null
2008-06-09T10%3A12%3A00.000Z 366.49 null null
2008-06-09T10%3A16%3A00.000Z 366.48 null null
2008-06-09T10%3A28%3A00.000Z 366.48 null null
2008-06-09T10%3A32%3A00.000Z 366.49 null null
2008-06-09T10%3A36%3A00.000Z 366.48 null null
2008-06-09T10%3A40%3A00.000Z 366.48 null null
2008-06-09T10%3A44%3A00.000Z 366.47 null null
2008-06-09T10%3A48%3A00.000Z 366.48 null null
2008-06-09T11%3A00%3A00.000Z 366.48 null null
2008-06-09T11%3A08%3A00.000Z 366.46 null null
2008-06-09T11%3A12%3A00.000Z 366.47 null null
2008-06-09T11%3A16%3A00.000Z 366.47 null null
2008-06-09T11%3A20%3A00.000Z 366.48 null null
2008-06-09T11%3A24%3A00.000Z 366.47 null null
2008-06-09T11%3A28%3A00.000Z 366.48 null null
2008-06-09T11%3A32%3A00.000Z 366.46 null null
2008-06-09T11%3A36%3A00.000Z 366.47 null null
2008-06-09T11%3A40%3A00.000Z 366.46 null null
2008-06-09T11%3A52%3A00.000Z 366.46 null null
2008-06-09T11%3A56%3A00.000Z 366.45 null null
2008-06-09T12%3A00%3A00.000Z 366.46 null null
2008-06-09T12%3A16%3A00.000Z 366.46 null null
2008-06-09T12%3A20%3A00.000Z 366.45 null null
2008-06-09T12%3A24%3A00.000Z 366.45 null null
2008-06-09T12%3A28%3A00.000Z 366.46 null null
2008-06-09T12%3A32%3A00.000Z 366.45 null null
2008-06-09T12%3A48%3A00.000Z 366.45 null null
2008-06-09T12%3A52%3A00.000Z 366.44 null null
2008-06-09T13%3A00%3A00.000Z 366.44 null null
2008-06-09T13%3A04%3A00.000Z 366.45 null null
2008-06-09T13%3A12%3A00.000Z 366.45 null null
2008-06-09T13%3A16%3A00.000Z 366.44 null null
2008-06-09T13%3A40%3A00.000Z 366.44 null null
2008-06-09T13%3A44%3A00.000Z 366.43 null null
2008-06-09T13%3A48%3A00.000Z 366.43 null null
2008-06-09T13%3A52%3A00.000Z 366.44 null null
2008-06-09T13%3A56%3A00.000Z 366.43 null null
2008-06-09T14%3A08%3A00.000Z 366.43 null null
2008-06-09T14%3A12%3A00.000Z 366.42 null null
2008-06-09T14%3A20%3A00.000Z 366.42 null null
2008-06-09T14%3A24%3A00.000Z 366.43 null null
2008-06-09T14%3A28%3A00.000Z 366.42 null null
2008-06-09T15%3A04%3A00.000Z 366.42 null null
2008-06-09T15%3A08%3A00.000Z 366.41 null null
2008-06-09T15%3A28%3A00.000Z 366.41 null null
2008-06-09T15%3A32%3A00.000Z 366.4 null null
2008-06-09T15%3A44%3A00.000Z 366.4 null null
2008-06-09T15%3A48%3A00.000Z 366.41 null null
2008-06-09T15%3A52%3A00.000Z 366.41 null null
2008-06-09T16%3A00%3A00.000Z 366.39 null null
2008-06-09T16%3A04%3A00.000Z 366.4 null null
2008-06-09T16%3A24%3A00.000Z 366.4 null null
2008-06-09T16%3A28%3A00.000Z 366.39 null null
2008-06-09T16%3A40%3A00.000Z 366.39 null null
2008-06-09T16%3A44%3A00.000Z 366.4 null null
2008-06-09T16%3A48%3A00.000Z 366.39 null null
2008-06-09T17%3A00%3A00.000Z 366.39 null null
2008-06-09T17%3A04%3A00.000Z 366.4 null null
2008-06-09T17%3A08%3A00.000Z 366.38 null null
2008-06-09T17%3A12%3A00.000Z 366.39 null null
2008-06-09T17%3A16%3A00.000Z 366.38 null null
2008-06-09T17%3A24%3A00.000Z 366.38 null null
2008-06-09T17%3A28%3A00.000Z 366.37 null null
2008-06-09T17%3A32%3A00.000Z 366.38 null null
2008-06-09T17%3A40%3A00.000Z 366.38 null null
2008-06-09T17%3A44%3A00.000Z 366.37 null null
2008-06-09T17%3A48%3A00.000Z 366.37 null null
2008-06-09T17%3A52%3A00.000Z 366.38 null null
2008-06-09T17%3A56%3A00.000Z 366.37 null null
2008-06-09T18%3A04%3A00.000Z 366.37 null null
2008-06-09T18%3A08%3A00.000Z 366.38 null null
2008-06-09T18%3A12%3A00.000Z 366.38 null null
2008-06-09T18%3A20%3A00.000Z 366.36 null null
2008-06-09T18%3A24%3A00.000Z 366.37 null null
2008-06-09T18%3A28%3A00.000Z 366.36 null null
2008-06-09T18%3A32%3A00.000Z 366.36 null null
2008-06-09T18%3A36%3A00.000Z 366.37 null null
2008-06-09T18%3A40%3A00.000Z 366.36 null null
2008-06-09T19%3A08%3A00.000Z 366.36 null null
2008-06-09T19%3A12%3A00.000Z 366.35 null null
2008-06-09T19%3A24%3A00.000Z 366.35 null null
2008-06-09T19%3A28%3A00.000Z 366.36 null null
2008-06-09T19%3A32%3A00.000Z 366.35 null null
2008-06-09T20%3A00%3A00.000Z 366.35 null null
2008-06-09T20%3A04%3A00.000Z 366.34 null null
2008-06-09T20%3A08%3A00.000Z 366.34 null null
2008-06-09T20%3A12%3A00.000Z 366.35 null null
2008-06-09T20%3A16%3A00.000Z 366.34 null null
2008-06-09T20%3A56%3A00.000Z 366.34 null null
2008-06-09T21%3A00%3A00.000Z 366.32 null null
2008-06-09T21%3A04%3A00.000Z 366.33 null null
2008-06-09T21%3A40%3A00.000Z 366.33 null null
2008-06-09T21%3A44%3A00.000Z 366.32 null null
2008-06-09T21%3A48%3A00.000Z 366.33 null null
2008-06-09T21%3A52%3A00.000Z 366.32 null null
2008-06-09T22%3A20%3A00.000Z 366.32 null null
2008-06-09T22%3A24%3A00.000Z 366.31 null null
2008-06-09T22%3A28%3A00.000Z 366.32 null null
2008-06-09T22%3A32%3A00.000Z 366.31 null null
2008-06-09T22%3A44%3A00.000Z 366.31 null null
2008-06-09T22%3A48%3A00.000Z 366.32 null null
2008-06-09T22%3A52%3A00.000Z 366.31 null null
2008-06-09T22%3A56%3A00.000Z 366.31 null null
2008-06-09T23%3A00%3A00.000Z 366.3 null null
2008-06-09T23%3A32%3A00.000Z 366.3 null null
2008-06-09T23%3A36%3A00.000Z 366.29 null null
2008-06-09T23%3A40%3A00.000Z 366.3 null null
2008-06-09T23%3A52%3A00.000Z 366.3 null null
2008-06-09T23%3A56%3A00.000Z 366.29 null null
2008-06-10T00%3A12%3A00.000Z 366.29 null null
2008-06-10T00%3A16%3A00.000Z 366.28 null null
2008-06-10T00%3A20%3A00.000Z 366.29 null null
2008-06-10T00%3A40%3A00.000Z 366.29 null null
2008-06-10T00%3A44%3A00.000Z 366.28 null null
2008-06-10T00%3A48%3A00.000Z 366.28 null null
2008-06-10T00%3A52%3A00.000Z 366.29 null null
2008-06-10T00%3A56%3A00.000Z 366.28 null null
2008-06-10T01%3A00%3A00.000Z 366.28 null null
2008-06-10T01%3A04%3A00.000Z 366.29 null null
2008-06-10T01%3A08%3A00.000Z 366.27 null null
2008-06-10T01%3A12%3A00.000Z 366.28 null null
2008-06-10T01%3A16%3A00.000Z 366.28 null null
2008-06-10T01%3A20%3A00.000Z 366.27 null null
2008-06-10T01%3A24%3A00.000Z 366.27 null null
2008-06-10T01%3A28%3A00.000Z 366.28 null null
2008-06-10T01%3A32%3A00.000Z 366.27 null null
2008-06-10T01%3A36%3A00.000Z 366.27 null null
2008-06-10T01%3A40%3A00.000Z 366.28 null null
2008-06-10T01%3A44%3A00.000Z 366.27 null null
2008-06-10T02%3A08%3A00.000Z 366.27 null null
2008-06-10T02%3A12%3A00.000Z 366.26 null null
2008-06-10T02%3A28%3A00.000Z 366.26 null null
2008-06-10T02%3A32%3A00.000Z 366.27 null null
2008-06-10T02%3A36%3A00.000Z 366.25 null null
2008-06-10T02%3A40%3A00.000Z 366.26 null null
2008-06-10T02%3A44%3A00.000Z 366.25 null null
2008-06-10T02%3A48%3A00.000Z 366.25 null null
2008-06-10T02%3A52%3A00.000Z 366.26 null null
2008-06-10T02%3A56%3A00.000Z 366.26 null null
2008-06-10T03%3A00%3A00.000Z 366.25 null null
2008-06-10T03%3A20%3A00.000Z 366.25 null null
2008-06-10T03%3A24%3A00.000Z 366.24 null null
2008-06-10T03%3A28%3A00.000Z 366.25 null null
2008-06-10T03%3A32%3A00.000Z 366.24 null null
2008-06-10T03%3A36%3A00.000Z 366.24 null null
2008-06-10T03%3A40%3A00.000Z 366.25 null null
2008-06-10T03%3A44%3A00.000Z 366.25 null null
2008-06-10T03%3A48%3A00.000Z 366.24 null null
2008-06-10T03%3A52%3A00.000Z 366.24 null null
2008-06-10T03%3A56%3A00.000Z 366.25 null null
2008-06-10T04%3A00%3A00.000Z 366.23 null null
2008-06-10T04%3A04%3A00.000Z 366.24 null null
2008-06-10T04%3A08%3A00.000Z 366.24 null null
2008-06-10T04%3A12%3A00.000Z 366.23 null null
2008-06-10T04%3A16%3A00.000Z 366.24 null null
2008-06-10T04%3A20%3A00.000Z 366.23 null null
2008-06-10T04%3A24%3A00.000Z 366.24 null null
2008-06-10T04%3A28%3A00.000Z 366.23 null null
2008-06-10T04%3A52%3A00.000Z 366.23 null null
2008-06-10T04%3A56%3A00.000Z 366.22 null null
2008-06-10T05%3A04%3A00.000Z 366.22 null null
2008-06-10T05%3A08%3A00.000Z 366.23 null null
2008-06-10T05%3A16%3A00.000Z 366.23 null null
2008-06-10T05%3A20%3A00.000Z 366.22 null null
2008-06-10T05%3A48%3A00.000Z 366.22 null null
2008-06-10T05%3A52%3A00.000Z 366.21 null null
2008-06-10T05%3A56%3A00.000Z 366.22 null null
2008-06-10T06%3A00%3A00.000Z 366.22 null null
2008-06-10T06%3A04%3A00.000Z 366.21 null null
2008-06-10T06%3A08%3A00.000Z 366.22 null null
2008-06-10T06%3A12%3A00.000Z 366.21 null null
2008-06-10T06%3A24%3A00.000Z 366.21 null null
2008-06-10T06%3A28%3A00.000Z 366.2 null null
2008-06-10T06%3A32%3A00.000Z 366.21 null null
2008-06-10T06%3A36%3A00.000Z 366.21 null null
2008-06-10T06%3A40%3A00.000Z 366.2 null null
2008-06-10T06%3A44%3A00.000Z 366.2 null null
2008-06-10T06%3A48%3A00.000Z 366.21 null null
2008-06-10T06%3A52%3A00.000Z 366.2 null null
2008-06-10T07%3A16%3A00.000Z 366.2 null null
2008-06-10T07%3A20%3A00.000Z 366.19 null null
2008-06-10T07%3A44%3A00.000Z 366.19 null null
2008-06-10T07%3A48%3A00.000Z 366.2 null null
2008-06-10T07%3A52%3A00.000Z 366.19 null null
2008-06-10T08%3A00%3A00.000Z 366.19 null null
2008-06-10T08%3A04%3A00.000Z 366.18 null null
2008-06-10T08%3A08%3A00.000Z 366.19 null null
2008-06-10T08%3A12%3A00.000Z 366.19 null null
2008-06-10T08%3A16%3A00.000Z 366.18 null null
2008-06-10T08%3A20%3A00.000Z 366.18 null null
2008-06-10T08%3A24%3A00.000Z 366.19 null null
2008-06-10T08%3A32%3A00.000Z 366.17 null null
2008-06-10T08%3A36%3A00.000Z 366.18 null null
2008-06-10T08%3A40%3A00.000Z 366.18 null null
2008-06-10T08%3A44%3A00.000Z 366.17 null null
2008-06-10T08%3A48%3A00.000Z 366.18 null null
2008-06-10T08%3A52%3A00.000Z 366.17 null null
2008-06-10T09%3A20%3A00.000Z 366.17 null null
2008-06-10T09%3A24%3A00.000Z 366.18 null null
2008-06-10T09%3A28%3A00.000Z 366.17 null null
2008-06-10T09%3A32%3A00.000Z 366.17 null null
2008-06-10T09%3A36%3A00.000Z 366.16 null null
2008-06-10T09%3A40%3A00.000Z 366.17 null null
2008-06-10T09%3A44%3A00.000Z 366.17 null null
2008-06-10T09%3A48%3A00.000Z 366.16 null null
2008-06-10T10%3A00%3A00.000Z 366.16 null null
2008-06-10T10%3A04%3A00.000Z 366.15 null null
2008-06-10T10%3A08%3A00.000Z 366.16 null null
2008-06-10T10%3A24%3A00.000Z 366.16 null null
2008-06-10T10%3A28%3A00.000Z 366.15 null null
2008-06-10T10%3A32%3A00.000Z 366.16 null null
2008-06-10T10%3A36%3A00.000Z 366.15 null null
2008-06-10T10%3A48%3A00.000Z 366.15 null null
2008-06-10T10%3A52%3A00.000Z 366.14 null null
2008-06-10T10%3A56%3A00.000Z 366.15 null null
2008-06-10T11%3A04%3A00.000Z 366.15 null null
2008-06-10T11%3A08%3A00.000Z 366.14 null null
2008-06-10T11%3A32%3A00.000Z 366.14 null null
2008-06-10T11%3A36%3A00.000Z 366.13 null null
2008-06-10T11%3A40%3A00.000Z 366.14 null null
2008-06-10T11%3A52%3A00.000Z 366.14 null null
2008-06-10T11%3A56%3A00.000Z 366.13 null null
2008-06-10T12%3A08%3A00.000Z 366.13 null null
2008-06-10T12%3A12%3A00.000Z 366.14 null null
2008-06-10T12%3A16%3A00.000Z 366.13 null null
2008-06-10T12%3A24%3A00.000Z 366.13 null null
2008-06-10T12%3A28%3A00.000Z 366.12 null null
2008-06-10T12%3A32%3A00.000Z 366.13 null null
2008-06-10T12%3A36%3A00.000Z 366.12 null null
2008-06-10T12%3A44%3A00.000Z 366.12 null null
2008-06-10T12%3A48%3A00.000Z 366.13 null null
2008-06-10T12%3A52%3A00.000Z 366.12 null null
2008-06-10T13%3A04%3A00.000Z 366.12 null null
2008-06-10T13%3A08%3A00.000Z 366.11 null null
2008-06-10T13%3A40%3A00.000Z 366.11 null null
2008-06-10T13%3A44%3A00.000Z 366.1 null null
2008-06-10T13%3A56%3A00.000Z 366.1 null null
2008-06-10T14%3A00%3A00.000Z 366.09 null null
2008-06-10T14%3A04%3A00.000Z 366.1 null null
2008-06-10T14%3A16%3A00.000Z 366.1 null null
2008-06-10T14%3A20%3A00.000Z 366.09 null null
2008-06-10T14%3A24%3A00.000Z 366.1 null null
2008-06-10T14%3A28%3A00.000Z 366.1 null null
2008-06-10T14%3A32%3A00.000Z 366.09 null null
2008-06-10T15%3A08%3A00.000Z 366.09 null null
2008-06-10T15%3A12%3A00.000Z 366.08 null null
2008-06-10T15%3A16%3A00.000Z 366.08 null null
2008-06-10T15%3A20%3A00.000Z 366.07 null null
2008-06-10T15%3A24%3A00.000Z 366.07 null null
2008-06-10T15%3A28%3A00.000Z 366.08 null null
2008-06-10T15%3A32%3A00.000Z 366.07 null null
2008-06-10T15%3A44%3A00.000Z 366.07 null null
2008-06-10T15%3A48%3A00.000Z 366.06 null null
2008-06-10T15%3A52%3A00.000Z 366.07 null null
2008-06-10T15%3A56%3A00.000Z 366.06 null null
2008-06-10T16%3A00%3A00.000Z 366.06 null null
2008-06-10T16%3A04%3A00.000Z 366.07 null null
2008-06-10T16%3A08%3A00.000Z 366.06 null null
2008-06-10T16%3A16%3A00.000Z 366.06 null null
2008-06-10T16%3A20%3A00.000Z 366.05 null null
2008-06-10T16%3A24%3A00.000Z 366.06 null null
2008-06-10T16%3A28%3A00.000Z 366.06 null null
2008-06-10T16%3A32%3A00.000Z 366.05 null null
2008-06-10T17%3A00%3A00.000Z 366.05 null null
2008-06-10T17%3A04%3A00.000Z 366.04 null null
2008-06-10T17%3A08%3A00.000Z 366.04 null null
2008-06-10T17%3A12%3A00.000Z 366.05 null null
2008-06-10T17%3A16%3A00.000Z 366.04 null null
2008-06-10T17%3A20%3A00.000Z 366.04 null null
2008-06-10T17%3A24%3A00.000Z 366.03 null null
2008-06-10T17%3A40%3A00.000Z 366.03 null null
2008-06-10T17%3A44%3A00.000Z 366.02 null null
2008-06-10T17%3A48%3A00.000Z 366.02 null null
2008-06-10T17%3A52%3A00.000Z 366.03 null null
2008-06-10T18%3A04%3A00.000Z 366.03 null null
2008-06-10T18%3A08%3A00.000Z 366.01 null null
2008-06-10T18%3A12%3A00.000Z 366.02 null null
2008-06-10T18%3A16%3A00.000Z 366.01 null null
2008-06-10T18%3A20%3A00.000Z 366.02 null null
2008-06-10T18%3A24%3A00.000Z 366.01 null null
2008-06-10T18%3A48%3A00.000Z 366.01 null null
2008-06-10T18%3A52%3A00.000Z 366.0 null null
2008-06-10T18%3A56%3A00.000Z 366.01 null null
2008-06-10T19%3A00%3A00.000Z 366.0 null null
2008-06-10T19%3A24%3A00.000Z 366.0 null null
2008-06-10T19%3A28%3A00.000Z 365.99 null null
2008-06-10T19%3A36%3A00.000Z 365.99 null null
2008-06-10T19%3A40%3A00.000Z 365.98 null null
2008-06-10T19%3A44%3A00.000Z 365.99 null null
2008-06-10T19%3A48%3A00.000Z 365.99 null null
2008-06-10T19%3A52%3A00.000Z 365.98 null null
2008-06-10T19%3A56%3A00.000Z 365.99 null null
2008-06-10T20%3A00%3A00.000Z 365.98 null null
2008-06-10T20%3A24%3A00.000Z 365.98 null null
2008-06-10T20%3A28%3A00.000Z 365.97 null null
2008-06-10T20%3A44%3A00.000Z 365.97 null null
2008-06-10T20%3A48%3A00.000Z 365.96 null null
2008-06-10T20%3A52%3A00.000Z 365.97 null null
2008-06-10T21%3A00%3A00.000Z 365.95 null null
2008-06-10T21%3A04%3A00.000Z 365.95 null null
2008-06-10T21%3A08%3A00.000Z 365.96 null null
2008-06-10T21%3A16%3A00.000Z 365.96 null null
2008-06-10T21%3A20%3A00.000Z 365.95 null null
2008-06-10T21%3A28%3A00.000Z 365.95 null null
2008-06-10T21%3A32%3A00.000Z 365.94 null null
2008-06-10T21%3A36%3A00.000Z 365.94 null null
2008-06-10T21%3A40%3A00.000Z 365.95 null null
2008-06-10T21%3A44%3A00.000Z 365.94 null null
2008-06-10T21%3A52%3A00.000Z 365.94 null null
2008-06-10T21%3A56%3A00.000Z 365.95 null null
]]></om:result>
     <ns2:dataSourceURI/>
    </ns2:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D12143995593866821</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D121447405594010843</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D121455082661013417</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D121455342596917318</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12294293459017619</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D122960321527117317</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12144639393734308</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12300482827813667</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D123142936348411130</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12314971785275075</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D123149718683712977</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12326300610525933</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>366.55</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition1228824707389906">
   <gml:description>Manuell erzeugt am: 09.12.2008 13:11</gml:description>
   <gml:name>Wasserstand Hochwasser 1D</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929971.777 775460.254</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:direction/>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource1228824707389193">
     <gml:name>Wasserstand - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition1228824707420907">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2008-12-09T00%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T01%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T02%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T03%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T04%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T05%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T06%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T07%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T08%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T09%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T10%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T11%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T12%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T13%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T14%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T15%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T16%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T17%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T18%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T19%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T20%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T21%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T22%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-09T23%3A00%3A00.000%2B01%3A00 367.0500 null null
2008-12-10T00%3A00%3A00.000%2B01%3A00 367.0500 null null
]]></om:result>
     <ns2:dataSourceURI/>
    </ns2:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine1D121439960812112374</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit1D12125830336860</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>367.1</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition1231505809018736">
   <gml:description>Manuell erzeugt am: 09.01.2009 13:56</gml:description>
   <gml:name>Abfluss Teichlösung 2D</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929967.2990107248 775458.2210388153</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:direction>251</op1d2d:direction>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource1231505809018173">
     <gml:name>Abfluss - Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition1231505809097475">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2008-06-25T00%3A00%3A00.000%2B02%3A00 50.0000 null null
2008-06-25T06%3A00%3A00.000%2B02%3A00 90.0000 null null
2008-06-25T12%3A00%3A00.000%2B02%3A00 130.0000 null null
2008-06-25T18%3A00%3A00.000%2B02%3A00 170.0000 null null
2008-06-26T00%3A00%3A00.000%2B02%3A00 230 null null
2008-06-26T06%3A00%3A00.000%2B02%3A00 250.0000 null null
]]></om:result>
     <ns2:dataSourceURI/>
    </ns2:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D12315057770971165</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit2D121439979923417593</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12294293459017619</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D122960321527117317</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D123142936348411130</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12314971785275075</op1d2d:parentCalculationUnit>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D123149718683712977</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>250.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <op1d2d:BoundaryCondition gml:id="BoundaryCondition1232630197583201">
   <gml:description>Manuell erzeugt am: 22.01.2009 14:16</gml:description>
   <gml:name>Abfluss Hochwasser 2D</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns4="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns1="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3929998.623668724 775446.5197622114</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <op1d2d:absolute/>
   <op1d2d:direction>251</op1d2d:direction>
   <op1d2d:observation>
    <ns2:ObservationWithSource gml:id="ObservationWithSource1232630197583218">
     <gml:name>Importierte Zeitreihe</gml:name>
     <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
     <om:resultDefinition>
      <sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition1232630197599565">
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
       <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
       <sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
      </sweExt:SortedRecordDefinition>
     </om:resultDefinition>
     <om:result><![CDATA[2008-06-03T22%3A00%3A00.958%2B02%3A00 250 null null
2008-06-04T01%3A15%3A00.000Z 256.37 null null
2008-06-04T01%3A30%3A00.000Z 257.99 null null
2008-06-04T02%3A15%3A00.000Z 258.0 null null
2008-06-04T02%3A45%3A00.000Z 261.28 null null
2008-06-04T03%3A15%3A00.000Z 261.28 null null
2008-06-04T03%3A30%3A00.000Z 262.93 null null
2008-06-04T04%3A00%3A00.000Z 262.93 null null
2008-06-04T04%3A15%3A00.000Z 264.59 null null
2008-06-04T04%3A30%3A00.000Z 264.59 null null
2008-06-04T04%3A45%3A00.000Z 266.26 null null
2008-06-04T08%3A45%3A00.000Z 266.29 null null
2008-06-04T09%3A00%3A00.000Z 269.65 null null
2008-06-04T11%3A15%3A00.000Z 269.66 null null
2008-06-04T11%3A30%3A00.000Z 271.36 null null
2008-06-04T12%3A15%3A00.000Z 271.36 null null
2008-06-04T12%3A30%3A00.000Z 273.06 null null
2008-06-04T13%3A15%3A00.000Z 273.07 null null
2008-06-04T13%3A30%3A00.000Z 274.78 null null
2008-06-04T14%3A00%3A00.000Z 274.78 null null
2008-06-04T14%3A15%3A00.000Z 278.23 null null
2008-06-04T14%3A30%3A00.000Z 279.96 null null
2008-06-04T14%3A45%3A00.000Z 283.46 null null
2008-06-04T15%3A00%3A00.000Z 283.46 null null
2008-06-04T15%3A45%3A00.000Z 294.15 null null
2008-06-04T16%3A00%3A00.000Z 295.96 null null
2008-06-04T16%3A15%3A00.000Z 299.61 null null
2008-06-04T16%3A30%3A00.000Z 301.45 null null
2008-06-04T16%3A45%3A00.000Z 301.45 null null
2008-06-04T17%3A00%3A00.000Z 303.3 null null
2008-06-04T17%3A15%3A00.000Z 303.3 null null
2008-06-04T17%3A30%3A00.000Z 305.15 null null
2008-06-04T18%3A30%3A00.000Z 305.16 null null
2008-06-04T18%3A45%3A00.000Z 310.77 null null
2008-06-04T21%3A45%3A00.000Z 310.8 null null
2008-06-04T21%3A49%3A00.000Z 312.68 null null
2008-06-04T22%3A00%3A00.000Z 310.8 null null
2008-06-04T22%3A15%3A00.000Z 312.69 null null
2008-06-04T23%3A00%3A00.000Z 312.69 null null
2008-06-04T23%3A01%3A00.000Z 312.69 null null
2008-06-04T23%3A15%3A00.000Z 316.49 null null
2008-06-04T23%3A30%3A00.000Z 318.41 null null
2008-06-05T00%3A30%3A00.000Z 318.42 null null
2008-06-05T00%3A45%3A00.000Z 320.34 null null
2008-06-05T01%3A30%3A00.000Z 320.34 null null
2008-06-05T01%3A45%3A00.000Z 322.28 null null
2008-06-05T02%3A00%3A00.000Z 322.28 null null
2008-06-05T02%3A15%3A00.000Z 324.22 null null
2008-06-05T02%3A45%3A00.000Z 324.22 null null
2008-06-05T03%3A15%3A00.000Z 328.12 null null
2008-06-05T03%3A45%3A00.000Z 328.13 null null
2008-06-05T04%3A00%3A00.000Z 330.09 null null
2008-06-05T04%3A30%3A00.000Z 330.1 null null
2008-06-05T04%3A45%3A00.000Z 332.07 null null
2008-06-05T05%3A00%3A00.000Z 332.07 null null
2008-06-05T05%3A15%3A00.000Z 334.05 null null
2008-06-05T05%3A45%3A00.000Z 334.06 null null
2008-06-05T06%3A00%3A00.000Z 338.04 null null
2008-06-05T06%3A30%3A00.000Z 338.04 null null
2008-06-05T06%3A45%3A00.000Z 340.05 null null
2008-06-05T07%3A15%3A00.000Z 340.05 null null
2008-06-05T07%3A30%3A00.000Z 342.07 null null
2008-06-05T07%3A45%3A00.000Z 342.07 null null
2008-06-05T08%3A30%3A00.000Z 348.16 null null
2008-06-05T08%3A45%3A00.000Z 348.16 null null
2008-06-05T09%3A15%3A00.000Z 352.26 null null
2008-06-05T09%3A30%3A00.000Z 358.47 null null
2008-06-05T10%3A15%3A00.000Z 364.83 null null
2008-06-05T10%3A30%3A00.000Z 364.86 null null
2008-06-05T10%3A45%3A00.000Z 369.12 null null
2008-06-05T11%3A00%3A00.000Z 369.15 null null
2008-06-05T11%3A15%3A00.000Z 371.3 null null
2008-06-05T11%3A45%3A00.000Z 371.36 null null
2008-06-05T12%3A00%3A00.000Z 373.53 null null
2008-06-05T12%3A30%3A00.000Z 373.59 null null
2008-06-05T12%3A45%3A00.000Z 375.76 null null
2008-06-05T13%3A00%3A00.000Z 375.79 null null
2008-06-05T13%3A15%3A00.000Z 380.12 null null
2008-06-05T13%3A30%3A00.000Z 380.15 null null
2008-06-05T13%3A45%3A00.000Z 382.34 null null
2008-06-05T14%3A00%3A00.000Z 382.37 null null
2008-06-05T14%3A15%3A00.000Z 384.57 null null
2008-06-05T14%3A30%3A00.000Z 384.6 null null
2008-06-05T14%3A45%3A00.000Z 386.81 null null
2008-06-05T15%3A00%3A00.000Z 386.84 null null
2008-06-05T15%3A15%3A00.000Z 389.05 null null
2008-06-05T15%3A30%3A00.000Z 389.08 null null
2008-06-05T15%3A45%3A00.000Z 391.3 null null
2008-06-05T16%3A00%3A00.000Z 391.33 null null
2008-06-05T16%3A30%3A00.000Z 395.8 null null
2008-06-05T16%3A45%3A00.000Z 395.83 null null
2008-06-05T17%3A00%3A00.000Z 398.07 null null
2008-06-05T17%3A45%3A00.000Z 398.16 null null
2008-06-05T18%3A00%3A00.000Z 400.41 null null
2008-06-05T19%3A00%3A00.000Z 400.54 null null
2008-06-05T19%3A45%3A00.000Z 407.32 null null
2008-06-05T20%3A30%3A00.000Z 407.42 null null
2008-06-05T20%3A45%3A00.000Z 409.69 null null
2008-06-05T21%3A00%3A00.000Z 409.72 null null
2008-06-05T21%3A15%3A00.000Z 412.0 null null
2008-06-05T21%3A45%3A00.000Z 412.07 null null
2008-06-05T22%3A00%3A00.000Z 414.35 null null
2008-06-05T22%3A30%3A00.000Z 414.42 null null
2008-06-05T22%3A39%3A00.000Z 418.96 null null
2008-06-05T22%3A45%3A00.000Z 414.45 null null
2008-06-05T23%3A00%3A00.000Z 419.01 null null
2008-06-05T23%3A30%3A00.000Z 419.07 null null
2008-06-05T23%3A45%3A00.000Z 421.38 null null
2008-06-06T00%3A15%3A00.000Z 421.44 null null
2008-06-06T00%3A30%3A00.000Z 423.75 null null
2008-06-06T01%3A00%3A00.000Z 423.82 null null
2008-06-06T01%3A15%3A00.000Z 426.13 null null
2008-06-06T01%3A45%3A00.000Z 426.2 null null
2008-06-06T02%3A00%3A00.000Z 430.82 null null
2008-06-06T02%3A45%3A00.000Z 430.91 null null
2008-06-06T03%3A00%3A00.000Z 433.25 null null
2008-06-06T03%3A30%3A00.000Z 433.31 null null
2008-06-06T03%3A45%3A00.000Z 435.65 null null
2008-06-06T04%3A15%3A00.000Z 435.71 null null
2008-06-06T04%3A30%3A00.000Z 438.06 null null
2008-06-06T04%3A45%3A00.000Z 438.09 null null
2008-06-06T05%3A31%3A00.000Z 440.68 null null
2008-06-06T06%3A05%3A00.000Z 440.75 null null
2008-06-06T07%3A00%3A00.000Z 442.91 null null
2008-06-06T07%3A40%3A00.000Z 443.12 null null
2008-06-06T08%3A32%3A00.000Z 450.21 null null
2008-06-06T09%3A30%3A00.000Z 452.53 null null
2008-06-06T10%3A31%3A00.000Z 450.2 null null
2008-06-06T10%3A55%3A00.000Z 445.5 null null
2008-06-06T11%3A45%3A00.000Z 440.8 null null
2008-06-06T12%3A15%3A00.000Z 440.79 null null
2008-06-06T12%3A30%3A00.000Z 438.46 null null
2008-06-06T12%3A45%3A00.000Z 438.46 null null
2008-06-06T13%3A00%3A00.000Z 433.83 null null
2008-06-06T14%3A15%3A00.000Z 433.8 null null
2008-06-06T14%3A30%3A00.000Z 431.49 null null
2008-06-06T16%3A45%3A00.000Z 431.43 null null
2008-06-06T17%3A15%3A00.000Z 426.83 null null
2008-06-06T17%3A30%3A00.000Z 422.26 null null
2008-06-06T17%3A45%3A00.000Z 419.98 null null
2008-06-06T18%3A00%3A00.000Z 419.97 null null
2008-06-06T18%3A15%3A00.000Z 417.7 null null
2008-06-06T18%3A30%3A00.000Z 417.69 null null
2008-06-06T18%3A45%3A00.000Z 415.42 null null
2008-06-06T19%3A30%3A00.000Z 415.4 null null
2008-06-06T19%3A45%3A00.000Z 410.89 null null
2008-06-06T20%3A30%3A00.000Z 410.87 null null
2008-06-06T20%3A45%3A00.000Z 408.62 null null
2008-06-06T21%3A45%3A00.000Z 408.59 null null
2008-06-06T21%3A50%3A00.000Z 406.35 null null
2008-06-06T23%3A30%3A00.000Z 406.31 null null
2008-06-06T23%3A45%3A00.000Z 404.07 null null
2008-06-07T01%3A00%3A00.000Z 404.04 null null
2008-06-07T01%3A15%3A00.000Z 399.59 null null
2008-06-07T01%3A45%3A00.000Z 399.58 null null
2008-06-07T02%3A00%3A00.000Z 397.36 null null
2008-06-07T03%3A00%3A00.000Z 397.33 null null
2008-06-07T03%3A15%3A00.000Z 395.12 null null
2008-06-07T04%3A15%3A00.000Z 395.1 null null
2008-06-07T04%3A30%3A00.000Z 392.89 null null
2008-06-07T05%3A15%3A00.000Z 392.87 null null
2008-06-07T05%3A45%3A00.000Z 388.48 null null
2008-06-07T06%3A15%3A00.000Z 388.47 null null
2008-06-07T06%3A30%3A00.000Z 386.29 null null
2008-06-07T07%3A15%3A00.000Z 386.27 null null
2008-06-07T07%3A30%3A00.000Z 384.09 null null
2008-06-07T08%3A30%3A00.000Z 384.06 null null
2008-06-07T08%3A45%3A00.000Z 381.89 null null
2008-06-07T09%3A00%3A00.000Z 381.89 null null
2008-06-07T11%3A00%3A00.000Z 364.8 null null
2008-06-07T11%3A15%3A00.000Z 364.79 null null
2008-06-07T11%3A45%3A00.000Z 360.59 null null
2008-06-07T12%3A00%3A00.000Z 360.59 null null
2008-06-07T12%3A15%3A00.000Z 356.43 null null
2008-06-07T12%3A30%3A00.000Z 354.35 null null
2008-06-07T12%3A45%3A00.000Z 354.35 null null
2008-06-07T13%3A00%3A00.000Z 352.28 null null
2008-06-07T13%3A30%3A00.000Z 352.27 null null
2008-06-07T13%3A45%3A00.000Z 350.21 null null
2008-06-07T14%3A15%3A00.000Z 350.2 null null
2008-06-07T14%3A30%3A00.000Z 346.12 null null
2008-06-07T15%3A15%3A00.000Z 346.1 null null
2008-06-07T15%3A30%3A00.000Z 344.07 null null
2008-06-07T16%3A00%3A00.000Z 344.06 null null
2008-06-07T16%3A15%3A00.000Z 342.03 null null
2008-06-07T17%3A00%3A00.000Z 342.01 null null
2008-06-07T17%3A15%3A00.000Z 340.0 null null
2008-06-07T18%3A30%3A00.000Z 339.97 null null
2008-06-07T19%3A00%3A00.000Z 335.96 null null
2008-06-07T19%3A45%3A00.000Z 335.94 null null
2008-06-07T20%3A00%3A00.000Z 333.95 null null
2008-06-07T20%3A45%3A00.000Z 333.93 null null
2008-06-07T21%3A00%3A00.000Z 331.95 null null
2008-06-07T22%3A00%3A00.000Z 331.92 null null
2008-06-07T22%3A01%3A00.000Z 329.95 null null
2008-06-07T23%3A00%3A00.000Z 329.93 null null
2008-06-07T23%3A15%3A00.000Z 329.92 null null
2008-06-07T23%3A30%3A00.000Z 326.0 null null
2008-06-08T00%3A00%3A00.000Z 325.99 null null
2008-06-08T00%3A15%3A00.000Z 324.04 null null
2008-06-08T01%3A15%3A00.000Z 324.02 null null
2008-06-08T01%3A30%3A00.000Z 322.08 null null
2008-06-08T02%3A00%3A00.000Z 322.06 null null
2008-06-08T02%3A15%3A00.000Z 320.13 null null
2008-06-08T02%3A30%3A00.000Z 320.13 null null
2008-06-08T03%3A00%3A00.000Z 316.28 null null
2008-06-08T03%3A30%3A00.000Z 316.27 null null
2008-06-08T03%3A45%3A00.000Z 314.36 null null
2008-06-08T04%3A15%3A00.000Z 314.35 null null
2008-06-08T04%3A30%3A00.000Z 312.45 null null
2008-06-08T05%3A15%3A00.000Z 312.43 null null
2008-06-08T05%3A30%3A00.000Z 310.54 null null
2008-06-08T06%3A30%3A00.000Z 310.52 null null
2008-06-08T06%3A45%3A00.000Z 306.77 null null
2008-06-08T07%3A15%3A00.000Z 306.76 null null
2008-06-08T07%3A30%3A00.000Z 304.89 null null
2008-06-08T08%3A30%3A00.000Z 304.87 null null
2008-06-08T09%3A00%3A00.000Z 301.16 null null
2008-06-08T10%3A00%3A00.000Z 301.18 null null
2008-06-08T10%3A30%3A00.000Z 297.54 null null
2008-06-08T11%3A00%3A00.000Z 297.57 null null
2008-06-08T11%3A15%3A00.000Z 295.76 null null
2008-06-08T11%3A30%3A00.000Z 295.77 null null
2008-06-08T11%3A45%3A00.000Z 297.6 null null
2008-06-08T12%3A00%3A00.000Z 297.62 null null
2008-06-08T12%3A15%3A00.000Z 295.81 null null
2008-06-08T13%3A00%3A00.000Z 295.85 null null
2008-06-08T13%3A15%3A00.000Z 294.05 null null
2008-06-08T14%3A15%3A00.000Z 294.1 null null
2008-06-08T14%3A30%3A00.000Z 290.52 null null
2008-06-08T15%3A15%3A00.000Z 290.55 null null
2008-06-08T15%3A30%3A00.000Z 288.78 null null
2008-06-08T16%3A15%3A00.000Z 288.82 null null
2008-06-08T16%3A30%3A00.000Z 287.05 null null
2008-06-08T17%3A30%3A00.000Z 287.1 null null
2008-06-08T17%3A45%3A00.000Z 285.34 null null
2008-06-08T18%3A30%3A00.000Z 285.38 null null
2008-06-08T18%3A45%3A00.000Z 281.88 null null
2008-06-08T19%3A30%3A00.000Z 281.91 null null
2008-06-08T19%3A45%3A00.000Z 280.18 null null
2008-06-08T20%3A15%3A00.000Z 280.21 null null
2008-06-08T20%3A30%3A00.000Z 278.48 null null
2008-06-08T21%3A30%3A00.000Z 278.53 null null
2008-06-08T21%3A45%3A00.000Z 276.81 null null
2008-06-08T22%3A15%3A00.000Z 276.84 null null
2008-06-08T22%3A20%3A00.000Z 273.41 null null
2008-06-08T23%3A15%3A00.000Z 273.46 null null
2008-06-08T23%3A30%3A00.000Z 271.76 null null
2008-06-09T00%3A15%3A00.000Z 271.8 null null
2008-06-09T00%3A30%3A00.000Z 270.12 null null
2008-06-09T01%3A00%3A00.000Z 270.14 null null
2008-06-09T01%3A15%3A00.000Z 268.47 null null
2008-06-09T02%3A00%3A00.000Z 268.5 null null
2008-06-09T02%3A15%3A00.000Z 265.16 null null
2008-06-09T03%3A00%3A00.000Z 265.2 null null
2008-06-09T03%3A15%3A00.000Z 263.55 null null
2008-06-09T04%3A00%3A00.000Z 263.58 null null
2008-06-09T04%3A30%3A00.000Z 260.31 null null
2008-06-09T05%3A00%3A00.000Z 260.33 null null
2008-06-09T05%3A30%3A00.000Z 257.09 null null
2008-06-09T06%3A00%3A00.000Z 257.11 null null
2008-06-09T06%3A15%3A00.000Z 255.5 null null
2008-06-09T06%3A45%3A00.000Z 255.52 null null
2008-06-09T07%3A00%3A00.000Z 253.92 null null
2008-06-09T08%3A00%3A00.000Z 253.97 null null
2008-06-09T08%3A30%3A00.000Z 250.79 null null
2008-06-09T09%3A15%3A00.000Z 250.76 null null
2008-06-09T09%3A30%3A00.000Z 249.15 null null
2008-06-09T10%3A15%3A00.000Z 249.11 null null
2008-06-09T10%3A30%3A00.000Z 247.52 null null
2008-06-09T11%3A00%3A00.000Z 247.49 null null
2008-06-09T11%3A15%3A00.000Z 244.34 null null
2008-06-09T11%3A30%3A00.000Z 245.89 null null
2008-06-09T11%3A45%3A00.000Z 244.31 null null
2008-06-09T12%3A30%3A00.000Z 244.27 null null
2008-06-09T12%3A45%3A00.000Z 242.7 null null
2008-06-09T13%3A15%3A00.000Z 242.67 null null
2008-06-09T13%3A30%3A00.000Z 239.58 null null
2008-06-09T14%3A15%3A00.000Z 239.54 null null
2008-06-09T14%3A30%3A00.000Z 237.99 null null
2008-06-09T15%3A30%3A00.000Z 237.94 null null
2008-06-09T15%3A45%3A00.000Z 236.41 null null
2008-06-09T16%3A30%3A00.000Z 236.36 null null
2008-06-09T16%3A45%3A00.000Z 234.84 null null
2008-06-09T17%3A15%3A00.000Z 234.81 null null
2008-06-09T17%3A45%3A00.000Z 231.79 null null
2008-06-09T18%3A15%3A00.000Z 231.76 null null
2008-06-09T18%3A30%3A00.000Z 230.26 null null
2008-06-09T19%3A00%3A00.000Z 230.23 null null
2008-06-09T19%3A15%3A00.000Z 228.74 null null
2008-06-09T20%3A00%3A00.000Z 228.7 null null
2008-06-09T20%3A15%3A00.000Z 227.22 null null
2008-06-09T21%3A00%3A00.000Z 227.18 null null
2008-06-09T21%3A30%3A00.000Z 224.25 null null
2008-06-09T22%3A00%3A00.000Z 224.22 null null
2008-06-09T22%3A14%3A00.000Z 222.77 null null
2008-06-09T23%3A00%3A00.000Z 222.73 null null
]]></om:result>
     <ns2:dataSourceURI/>
    </ns2:ObservationWithSource>
   </op1d2d:observation>
   <op1d2d:bcType>LINE1D2D</op1d2d:bcType>
   <op1d2d:parentModelElement>ContinuityLine2D12315057770971165</op1d2d:parentModelElement>
   <op1d2d:parentCalculationUnit>CalculationUnit1D2D12326300610525933</op1d2d:parentCalculationUnit>
   <op1d2d:stationaryCondition>250.0</op1d2d:stationaryCondition>
  </op1d2d:BoundaryCondition>
 </simBase:flowRelationshipMember>
</simBase:FlowRelationshipModel>
