<?xml version="1.0" encoding="WINDOWS-1252"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:swe="http://www.opengis.net/swe" xmlns:math="org.kalypso.gml.common.math" xmlns:om="http://www.opengis.net/om" gml:id="root">
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559517121">
   <gml:name>Gelesen aus: PROF0058.8530.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552876.7786 5987232.8608 2.75</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559517149">
     <gml:description>Übernommen aus Datei: PROF0058.8530.txt</gml:description>
     <gml:name>0058.8530</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559517167">
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
     <om:result>3.089 0.3391210 1.472186 1.000000 0.9999998 0.007841056 0.06848547 1.800000E-7
3.216 0.4662640 2.349944 2.000000 0.9999998 -0.02075648 -0.09413956 1.800000E-7
3.311 0.5614450 3.109214 3.000000 0.9999999 -0.003512365 -0.09473790 1.200000E-7
3.390 0.6400030 3.801787 4.000000 0.9999999 0.01284661 -0.03414655 1.200000E-7
3.458 0.7079730 4.449103 5.000000 1.000000 0.01711984 0.05121512 -1.200000E-7
3.518 0.7677950 5.055093 6.000000 1.000000 0.007471583 0.1315112 -1.200000E-7
3.566 0.8158410 5.556281 7.000000 0.9999999 -0.002497448 0.08136135 1.200000E-7
3.611 0.8610380 6.030731 8.000000 0.9999999 -0.006859638 0.03966414 6.000000E-8
3.654 0.9040970 6.485421 9.000000 0.9999998 -0.007856432 0.009194098 2.400000E-7
3.695 0.9453500 6.923492 10.00000 0.9999999 -0.006932828 -0.01193376 6.000000E-8
3.735 0.9850550 7.347393 11.00000 0.9999998 -0.005063906 -0.02525814 2.400000E-7
3.773 1.023389 7.758760 12.00000 1.000000 -0.002888678 -0.03281583 -1.200000E-7
3.811 1.060532 8.159331 13.00000 1.000000 -0.0008347844 -0.03542684 0E-7
3.847 1.096589 8.550056 14.00000 1.000000 0.0008543388 -0.03500850 -1.200000E-7
3.882 1.131675 8.932011 15.00000 1.000000 0.002072693 -0.03253288 0E-7
3.916 1.165875 9.306004 16.00000 0.9999999 0.002775327 -0.02924485 1.200000E-7
3.949 1.199276 9.672838 17.00000 1.000000 0.003037746 -0.02586046 -1.200000E-7
3.982 1.231940 10.03311 18.00000 1.000000 0.002942051 -0.02344594 -1.200000E-7
4.014 1.263937 10.38748 19.00000 0.9999999 0.002653793 -0.02249410 1.200000E-7
4.045 1.295277 10.73597 20.00000 0.9999999 0.002355785 -0.02511715 6.000000E-8
4.077 1.327018 11.09075 21.00000 0.9999999 0.001838655 0.0008460984 6.000000E-8
4.108 1.358302 11.44293 22.00000 1.000000 0.0006966361 0.02630612 0E-7
4.139 1.388983 11.79076 23.00000 0.9999999 -0.0007441929 0.04445857 6.000000E-8
4.169 1.419092 12.13444 24.00000 0.9999999 -0.002127014 0.05435530 6.000000E-8
4.199 1.448678 12.47441 25.00000 1.000000 -0.003089743 0.05582321 0E-7
4.228 1.477750 12.81063 26.00000 1.000000 -0.003263674 0.04748585 0E-7
4.256 1.506341 13.14341 27.00000 1.000000 -0.002297847 0.02890517 0E-7
4.284 1.534473 13.47290 28.00000 0.9999998 0.0001540330 -0.0006146386 1.800000E-7
4.310 1.560000 13.77361 29.00000 0.9999998 0.004064879 -0.1168346 1.800000E-7
4.310 1.560000 13.77361 30.00000 1.000000 -13.77361 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559518719">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.61235918998 -7.38581255713 16.3846499566 2.5863277102 -1.84043120305</math:coefficients>
     <math:minRange>0.3391</math:minRange>
     <math:maxRange>1.56</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559518726">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.883265594852 -3.10738604612 17.460268208 -9.90020534675 2.16734775895</math:coefficients>
     <math:minRange>0.3391</math:minRange>
     <math:maxRange>1.56</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559518711">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3391</math:minRange>
     <math:maxRange>1.56</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559518759">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.56</math:minRange>
     <math:maxRange>1.56</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559518766">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.56</math:minRange>
     <math:maxRange>1.56</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>58.8530</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559518755">
   <gml:name>Gelesen aus: PROF0058.9000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552880.6369 5987277.4016 2.72</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559518716">
     <gml:description>Übernommen aus Datei: PROF0058.9000.txt</gml:description>
     <gml:name>0058.9000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559518733">
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
     <om:result>3.032 0.3119700 1.681917 1.000000 0.9999999 0.004019089 0.02542919 1.200000E-7
3.131 0.4108930 2.559325 2.000000 0.9999997 -0.008221632 -0.02639381 3.000000E-7
3.209 0.4888610 3.296899 3.000000 0.9999999 -0.003601982 -0.03484757 1.200000E-7
3.276 0.5555090 3.959576 4.000000 0.9999998 0.003039407 -0.02081812 2.400000E-7
3.335 0.6147110 4.573083 5.000000 1.000000 0.006906935 0.005544758 0E-7
3.389 0.6685190 5.150993 6.000000 1.000000 0.006129423 0.03854770 0E-7
3.437 0.7169690 5.686902 7.000000 1.000000 0.001226289 0.04843030 -1.200000E-7
3.481 0.7607710 6.178676 8.000000 1.000000 -0.002339129 0.02501744 -1.200000E-7
3.522 0.8023490 6.649821 9.000000 1.000000 -0.003690511 0.006654029 0E-7
3.562 0.8421030 7.104228 10.00000 1.000000 -0.003688225 -0.006103569 -1.200000E-7
3.600 0.8802790 7.544228 11.00000 0.9999999 -0.002946661 -0.01426379 1.200000E-7
3.637 0.9170730 7.971657 12.00000 1.000000 -0.001867312 -0.01865285 -1.200000E-7
3.673 0.9526490 8.388072 13.00000 1.000000 -0.0007290491 -0.01982349 0E-7
3.707 0.9871290 8.794600 14.00000 1.000000 0.0002873785 -0.01862584 -1.200000E-7
3.741 1.020619 9.192220 15.00000 0.9999999 0.001080819 -0.01569891 6.000000E-8
3.773 1.053208 9.581778 16.00000 1.000000 0.001575375 -0.01163512 0E-7
3.805 1.084981 9.964068 17.00000 0.9999999 0.001774517 -0.006684776 1.200000E-7
3.836 1.115995 10.33959 18.00000 0.9999999 0.001694536 -0.001599491 6.000000E-8
3.866 1.146313 10.70895 19.00000 0.9999998 0.001382287 0.003369567 1.800000E-7
3.896 1.176004 11.07285 20.00000 1.000000 0.0008724892 0.008468602 -1.200000E-7
3.925 1.205069 11.43115 21.00000 1.000000 0.0002794366 0.01185805 -1.200000E-7
3.954 1.233566 11.78445 22.00000 1.000000 -0.0003469935 0.01381055 -1.200000E-7
3.982 1.261533 12.13310 23.00000 0.9999998 -0.0008798802 0.01410085 1.800000E-7
4.009 1.289000 12.47737 24.00000 1.000000 -0.001225004 0.01237029 0E-7
4.036 1.315994 12.81750 25.00000 1.000000 -0.001280532 0.008279266 -1.200000E-7
4.063 1.342541 13.15373 26.00000 1.000000 -0.0009212536 0.001565695 0E-7
4.089 1.368663 13.48626 27.00000 0.9999998 -0.00003457083 -0.008079738 1.800000E-7
4.114 1.394399 13.81549 28.00000 0.9999999 0.001504755 -0.02021924 1.200000E-7
4.110 1.390002 13.75913 29.00000 1.000000 -13.75913 -29.00000 -1.000000
4.140 1.420003 14.14464 30.00000 1.000000 -14.14464 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559520360">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.395022041801 -3.86496755488 19.1283848363 -0.634101921775 -0.661026888375</math:coefficients>
     <math:minRange>0.312</math:minRange>
     <math:maxRange>1.3944</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559520358">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.245797428235 3.69299549207 9.53050958486 -5.25394778449 1.22395265218</math:coefficients>
     <math:minRange>0.312</math:minRange>
     <math:maxRange>1.3944</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559520322">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.312</math:minRange>
     <math:maxRange>1.3944</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559520317">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3944</math:minRange>
     <math:maxRange>1.3944</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559520359">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3944</math:minRange>
     <math:maxRange>1.3944</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>58.9000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559520332">
   <gml:name>Gelesen aus: PROF0058.9510.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552869.1786 5987324.5788 2.55</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559520341">
     <gml:description>Übernommen aus Datei: PROF0058.9510.txt</gml:description>
     <gml:name>0058.9510</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559520359">
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
     <om:result>2.918 0.3680420 1.545824 1.000000 1.000000 -0.002611287 0.02300237 0E-7
3.032 0.4815330 2.391146 2.000000 1.000000 0.0002228899 -0.03984104 0E-7
3.120 0.5698060 3.111219 3.000000 0.9999999 0.005721249 -0.02856694 6.000000E-8
3.195 0.6445000 3.763310 4.000000 1.000000 0.006180146 0.008321588 -2.400000E-7
3.260 0.7096240 4.363681 5.000000 1.000000 0.0001623738 0.04151794 -1.200000E-7
3.316 0.7655990 4.896191 6.000000 0.9999998 -0.004515344 0.02212935 1.800000E-7
3.368 0.8176860 5.401493 7.000000 1.000000 -0.005800877 0.009167788 0E-7
3.417 0.8666410 5.885017 8.000000 1.000000 -0.005196535 0.0007339806 -1.200000E-7
3.463 0.9129960 6.350526 9.000000 1.000000 -0.003619367 -0.004501861 -1.200000E-7
3.507 0.9571390 6.800780 10.00000 1.000000 -0.001703547 -0.007582320 -1.200000E-7
3.549 0.9993860 7.238019 11.00000 0.9999999 0.0001987424 -0.008800785 1.200000E-7
3.590 1.039953 7.663727 12.00000 0.9999999 0.001805049 -0.009090986 1.200000E-7
3.629 1.079040 8.079301 13.00000 1.000000 0.003003119 -0.008532805 -1.200000E-7
3.667 1.116799 8.485801 14.00000 1.000000 0.003700598 -0.007429758 0E-7
3.703 1.153360 8.884125 15.00000 1.000000 0.003875617 -0.005903249 -1.200000E-7
3.739 1.188830 9.275006 16.00000 0.9999999 0.003534566 -0.004089218 6.000000E-8
3.773 1.223301 9.659065 17.00000 1.000000 0.002720091 -0.002052869 0E-7
3.807 1.256867 10.03702 18.00000 1.000000 0.001474143 0.0006241751 0E-7
3.840 1.289580 10.40912 19.00000 0.9999998 -0.0001074337 0.003453259 1.800000E-7
3.871 1.320927 10.76872 20.00000 1.000000 -0.001466854 -0.01177646 -1.200000E-7
3.902 1.351597 11.12292 21.00000 1.000000 -0.002079491 -0.02561528 -1.200000E-7
3.932 1.382152 11.47822 22.00000 0.9999999 -0.002009113 -0.02074208 6.000000E-8
3.963 1.412847 11.83833 23.00000 0.9999999 -0.001896938 0.01140819 1.200000E-7
3.993 1.442918 12.19440 24.00000 1.000000 -0.001841952 0.04478444 0E-7
4.022 1.472397 12.54663 25.00000 1.000000 -0.001725335 0.07934075 0E-7
4.051 1.501327 12.89532 26.00000 1.000000 -0.001416791 0.1155489 -2.400000E-7
4.080 1.529725 13.24053 27.00000 1.000000 -0.0008151271 0.1530668 0E-7
4.108 1.557635 13.58263 28.00000 1.000000 0.0001895853 0.1926341 -1.200000E-7
4.110 1.560002 13.61177 29.00000 1.000000 0.0003055112 -0.7182881 0E-7
4.160 1.610000 14.23202 30.00000 0.9999999 0.003712310 0.1970802 1.200000E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559520311">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.23142784513 -8.20903602178 22.8600715126 -6.16382115249 1.28739236955</math:coefficients>
     <math:minRange>0.368</math:minRange>
     <math:maxRange>1.61</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559520344">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.311894047254 2.68734395731 7.5464390355 -3.39719250192 0.71994828804</math:coefficients>
     <math:minRange>0.368</math:minRange>
     <math:maxRange>1.61</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559520310">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.368</math:minRange>
     <math:maxRange>1.61</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559520347">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.61</math:minRange>
     <math:maxRange>1.61</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559521852">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.61</math:minRange>
     <math:maxRange>1.61</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>58.9510</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559521829">
   <gml:name>Gelesen aus: PROF0059.0000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552853.1587 5987371.3982 2.85</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559521817">
     <gml:description>Übernommen aus Datei: PROF0059.0000.txt</gml:description>
     <gml:name>0059.0000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559521827">
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
     <om:result>3.150 0.3004030 1.709789 1.000000 1.000000 -0.007321120 0.06793597 -1.200000E-7
3.255 0.4048810 2.744553 2.000000 1.000000 0.01563780 -0.06381250 0E-7
3.326 0.4761370 3.543785 3.000000 1.000000 0.002104900 -0.06692124 0E-7
3.384 0.5338680 4.215714 4.000000 0.9999999 -0.004142271 -0.06203032 6.000000E-8
3.436 0.5857660 4.831687 5.000000 0.9999999 -0.004150978 -0.02940762 6.000000E-8
3.483 0.6334340 5.407417 6.000000 0.9999999 -0.002494293 0.01384355 6.000000E-8
3.528 0.6778280 5.952176 7.000000 1.000000 -0.001596739 0.05853637 0E-7
3.569 0.7191440 6.466447 8.000000 1.000000 -0.002641115 0.08856395 -2.400000E-7
3.607 0.7569590 6.940101 9.000000 1.000000 -0.002988893 0.07669530 0E-7
3.643 0.7931930 7.395226 10.00000 0.9999998 -0.002112217 0.06206328 1.800000E-7
3.678 0.8280620 7.834397 11.00000 0.9999999 -0.0006929566 0.04465491 6.000000E-8
3.712 0.8617410 8.259670 12.00000 1.000000 0.0008424221 0.02505515 -1.200000E-7
3.744 0.8943650 8.672657 13.00000 1.000000 0.002187403 0.003974250 0E-7
3.776 0.9260650 9.074903 14.00000 1.000000 0.003186474 -0.01701265 -1.200000E-7
3.807 0.9569110 9.467250 15.00000 0.9999999 0.003712336 -0.03746163 1.200000E-7
3.837 0.9869930 9.850743 16.00000 1.000000 0.003766052 -0.05574722 -1.200000E-7
3.866 1.016376 10.22615 17.00000 1.000000 0.003365422 -0.07057321 -1.200000E-7
3.895 1.045118 10.59416 18.00000 1.000000 0.002571215 -0.08050629 -1.200000E-7
3.923 1.073267 10.95535 19.00000 0.9999999 0.001458174 -0.08415187 1.200000E-7
3.951 1.100867 11.31023 20.00000 1.000000 0.0001592214 -0.08000641 0E-7
3.978 1.127964 11.65935 21.00000 1.000000 -0.001196093 -0.06626802 0E-7
4.005 1.154581 12.00296 22.00000 0.9999999 -0.002454017 -0.04181241 6.000000E-8
4.031 1.180753 12.34148 23.00000 0.9999999 -0.003440247 -0.004921944 6.000000E-8
4.057 1.206507 12.67522 24.00000 1.000000 -0.003986334 0.04594850 0E-7
4.082 1.231869 13.00451 25.00000 1.000000 -0.003917958 0.1124199 0E-7
4.107 1.256856 13.32953 26.00000 1.000000 -0.003037014 0.1958295 -1.200000E-7
4.132 1.281564 13.65151 27.00000 1.000000 -0.001141802 0.3011682 -1.200000E-7
4.157 1.306881 13.98255 28.00000 0.9999999 0.001605826 0.4711570 6.000000E-8
4.160 1.310000 14.02343 29.00000 0.9999998 0.001993009 -0.3819633 1.800000E-7
4.180 1.330000 14.28618 30.00000 1.000000 0.004723799 -0.4252492 -2.400000E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955952345">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>4.40381538828 -30.3521547588 77.8161532723 -50.1578409361 14.6671123141</math:coefficients>
     <math:minRange>0.3004</math:minRange>
     <math:maxRange>1.33</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559523489">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.336292927569 3.37916168775 13.9343097769 -9.34104884049 2.3843381425</math:coefficients>
     <math:minRange>0.3004</math:minRange>
     <math:maxRange>1.33</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559523473">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3004</math:minRange>
     <math:maxRange>1.33</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955952349">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.33</math:minRange>
     <math:maxRange>1.33</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559523412">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.33</math:minRange>
     <math:maxRange>1.33</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.0000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559523427">
   <gml:name>Gelesen aus: PROF0059.0510.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552846.8788 5987422.5508 2.68</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559523440">
     <gml:description>Übernommen aus Datei: PROF0059.0510.txt</gml:description>
     <gml:name>0059.0510</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559523494">
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
     <om:result>3.125 0.4447140 1.510830 1.000000 1.000000 0.01137220 0.01071210 0E-7
3.250 0.5697210 2.434335 2.000000 0.9999999 -0.01656246 0.02215041 1.200000E-7
3.338 0.6580170 3.178864 3.000000 1.000000 -0.01590663 -0.03070379 -1.200000E-7
3.413 0.7325610 3.854874 4.000000 0.9999997 -0.003645161 -0.05320733 3.000000E-7
3.478 0.7981090 4.485193 5.000000 1.000000 0.008834429 -0.04718687 -1.200000E-7
3.537 0.8571670 5.081860 6.000000 0.9999999 0.01695937 -0.01776111 1.200000E-7
3.591 0.9112550 5.652236 7.000000 1.000000 0.01879265 0.02973571 0E-7
3.641 0.9613760 6.201189 8.000000 0.9999999 0.01360485 0.09037743 1.200000E-7
3.686 1.006024 6.705924 9.000000 1.000000 0.002818767 0.1076949 0E-7
3.725 1.045232 7.153922 10.00000 0.9999999 -0.004859340 0.05657969 1.200000E-7
3.763 1.082933 7.586891 11.00000 1.000000 -0.009277358 0.01691606 0E-7
3.799 1.119329 8.006898 12.00000 1.000000 -0.01125564 -0.01262968 0E-7
3.835 1.154558 8.415349 13.00000 1.000000 -0.01143313 -0.03408308 -1.200000E-7
3.869 1.188746 8.813533 14.00000 0.9999999 -0.01028354 -0.04904561 6.000000E-8
3.902 1.221997 9.202496 15.00000 1.000000 -0.008147769 -0.05910595 0E-7
3.934 1.254396 9.583089 16.00000 0.9999999 -0.005281286 -0.06589432 1.200000E-7
3.966 1.286019 9.956103 17.00000 1.000000 -0.001895919 -0.07088273 -1.200000E-7
3.998 1.317641 10.33083 18.00000 1.000000 0.001758276 -0.05240631 0E-7
4.029 1.349099 10.70634 19.00000 0.9999999 0.004554843 -0.01780268 1.200000E-7
4.060 1.379857 11.07624 20.00000 0.9999999 0.006334927 0.01246013 1.200000E-7
4.090 1.409962 11.44092 21.00000 0.9999999 0.007128024 0.03675044 6.000000E-8
4.119 1.439469 11.80089 22.00000 0.9999999 0.006995365 0.05396652 6.000000E-8
4.148 1.468386 12.15610 23.00000 1.000000 0.006031362 0.06160107 0E-7
4.177 1.496771 12.50711 24.00000 0.9999999 0.004317594 0.05907002 6.000000E-8
4.205 1.524639 12.85399 25.00000 0.9999999 0.001958945 0.04446472 6.000000E-8
4.232 1.552026 13.19705 26.00000 0.9999999 -0.0009117158 0.01676852 6.000000E-8
4.259 1.578954 13.53647 27.00000 1.000000 -0.004208560 -0.02538960 0E-7
4.285 1.605448 13.87246 28.00000 1.000000 -0.007793103 -0.08314867 0E-7
4.280 1.600003 13.80324 29.00000 1.000000 -13.80324 -29.00000 -1.000000
4.340 1.660000 14.57062 30.00000 0.9999999 -14.57062 -30.00000 -0.9999999
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559525064">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-1.57522142377 8.33509103132 -14.8467292193 23.5151656075 -6.46182398957</math:coefficients>
     <math:minRange>0.4447</math:minRange>
     <math:maxRange>1.6054</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559525023">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.641915188036 -3.47356097787 15.0399404355 -6.79178916365 1.22510908153</math:coefficients>
     <math:minRange>0.4447</math:minRange>
     <math:maxRange>1.6054</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955952504">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4447</math:minRange>
     <math:maxRange>1.6054</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559525089">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6054</math:minRange>
     <math:maxRange>1.6054</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559525082">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6054</math:minRange>
     <math:maxRange>1.6054</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.0510</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559525041">
   <gml:name>Gelesen aus: PROF0059.1000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552839.4772 5987474.3447 2.46</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559525024">
     <gml:description>Übernommen aus Datei: PROF0059.1000.txt</gml:description>
     <gml:name>0059.1000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559525058">
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
     <om:result>2.870 0.4095090 1.378917 1.000000 0.9999999 0.004052544 0.02052049 1.200000E-7
3.005 0.5451400 2.174739 2.000000 0.9999999 -0.008085528 -0.02029912 6.000000E-8
3.108 0.6475070 2.839823 3.000000 1.000000 -0.004052270 -0.03236770 0E-7
3.194 0.7340130 3.443559 4.000000 0.9999998 0.002457383 -0.02043214 1.800000E-7
3.270 0.8101340 4.006417 5.000000 0.9999999 0.006588071 0.003655030 1.200000E-7
3.339 0.8787570 4.539175 6.000000 0.9999999 0.006602102 0.03339671 6.000000E-8
3.401 0.9409280 5.042455 7.000000 0.9999999 0.002130371 0.05281457 6.000000E-8
3.456 0.9961100 5.500752 8.000000 0.9999998 -0.001588591 0.02586842 1.800000E-7
3.508 1.048388 5.942577 9.000000 1.000000 -0.003180137 0.006301556 0E-7
3.558 1.098194 6.370443 10.00000 0.9999998 -0.003471962 -0.007184664 2.400000E-7
3.606 1.145871 6.786345 11.00000 1.000000 -0.003005303 -0.01547118 -1.200000E-7
3.652 1.191670 7.191702 12.00000 1.000000 -0.002170116 -0.01973948 -1.200000E-7
3.696 1.235807 7.587744 13.00000 0.9999999 -0.001203919 -0.02072950 1.200000E-7
3.738 1.278452 7.975448 14.00000 1.000000 -0.0002960099 -0.01926192 0E-7
3.780 1.319749 8.355609 15.00000 1.000000 0.0004680312 -0.01601588 -1.200000E-7
3.820 1.359820 8.728934 16.00000 0.9999999 0.001012372 -0.01159038 6.000000E-8
3.859 1.398785 9.096141 17.00000 0.9999998 0.001332900 -0.006111422 2.400000E-7
3.897 1.436711 9.457531 18.00000 0.9999999 0.001422583 -0.0006488323 6.000000E-8
3.934 1.473679 9.813572 19.00000 1.000000 0.001300240 0.004415115 0E-7
3.970 1.509761 10.16466 20.00000 1.000000 0.001023370 0.008762992 -1.200000E-7
4.005 1.545034 10.51131 21.00000 0.9999998 0.0006322416 0.01249507 1.800000E-7
4.040 1.579527 10.85357 22.00000 0.9999999 0.0001900628 0.01456993 6.000000E-8
4.073 1.613289 11.19171 23.00000 1.000000 -0.0002374372 0.01468447 0E-7
4.106 1.646369 11.52603 24.00000 1.000000 -0.0005802479 0.01271190 -1.200000E-7
4.139 1.678806 11.85675 25.00000 0.9999999 -0.0007555799 0.008380875 1.200000E-7
4.171 1.710633 12.18404 26.00000 1.000000 -0.0007029123 0.001365913 0E-7
4.202 1.741884 12.50808 27.00000 1.000000 -0.0003241799 -0.008511601 -2.400000E-7
4.233 1.772585 12.82902 28.00000 0.9999999 0.0004419210 -0.02157923 6.000000E-8
4.230 1.770002 12.80192 29.00000 0.9999999 -12.80192 -29.00000 -0.9999999
4.260 1.800003 13.11809 30.00000 1.000000 -13.11809 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559525025">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.497816187439 -2.94584314321 9.97553231129 0.990567085473 -0.421205470931</math:coefficients>
     <math:minRange>0.4095</math:minRange>
     <math:maxRange>1.7726</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955952506">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.0295283706198 1.2652281171 6.22381694431 -2.34582689255 0.417917976615</math:coefficients>
     <math:minRange>0.4095</math:minRange>
     <math:maxRange>1.7726</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559525011">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4095</math:minRange>
     <math:maxRange>1.7726</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559525086">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7726</math:minRange>
     <math:maxRange>1.7726</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559525062">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7726</math:minRange>
     <math:maxRange>1.7726</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.1000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559525016">
   <gml:name>Gelesen aus: PROF0059.1500.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552849.6867 5987524.1193 2.78</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation11783955952502">
     <gml:description>Übernommen aus Datei: PROF0059.1500.txt</gml:description>
     <gml:name>0059.1500</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595250108">
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
     <om:result>3.226 0.4458160 1.628580 1.000000 1.000000 -0.01816181 0.04762988 -1.200000E-7
3.353 0.5726090 2.661610 2.000000 1.000000 0.03611255 -0.08000475 -1.200000E-7
3.440 0.6595700 3.509903 3.000000 1.000000 0.01349703 -0.003367223 0E-7
3.503 0.7229710 4.159938 4.000000 1.000000 -0.005357405 -0.01909295 -1.200000E-7
3.560 0.7800420 4.751898 5.000000 0.9999998 -0.01344819 -0.01047315 2.400000E-7
3.612 0.8321140 5.295999 6.000000 0.9999998 -0.01497830 -0.001416943 1.800000E-7
3.661 0.8808880 5.808297 7.000000 1.000000 -0.01266876 0.01193944 0E-7
3.707 0.9270190 6.295192 8.000000 0.9999998 -0.008620697 0.02496048 1.800000E-7
3.751 0.9709660 6.761192 9.000000 1.000000 -0.004140219 0.03494003 -1.200000E-7
3.793 1.013069 7.209590 10.00000 0.9999999 0.00002726239 0.04045986 6.000000E-8
3.834 1.053584 7.642876 11.00000 0.9999999 0.003445074 0.04083004 6.000000E-8
3.873 1.092709 8.062995 12.00000 1.000000 0.005876336 0.03585337 0E-7
3.911 1.130608 8.471516 13.00000 0.9999998 0.007291196 0.02587508 1.800000E-7
3.947 1.167410 8.869712 14.00000 0.9999999 0.007721230 0.01138543 1.200000E-7
3.983 1.203226 9.258629 15.00000 1.000000 0.007336694 -0.006799796 0E-7
4.018 1.238142 9.639123 16.00000 0.9999999 0.006307057 -0.02792008 6.000000E-8
4.052 1.272252 10.01210 17.00000 1.000000 0.004905655 -0.05048278 0E-7
4.086 1.305604 10.37800 18.00000 1.000000 0.003401902 -0.07390253 0E-7
4.119 1.338570 10.74089 19.00000 0.9999999 0.002049549 -0.08750451 1.200000E-7
4.152 1.371733 11.10809 20.00000 1.000000 0.0002976717 -0.07295870 0E-7
4.184 1.404194 11.46989 21.00000 1.000000 -0.001707424 -0.05703166 -1.200000E-7
4.216 1.436303 11.83023 22.00000 1.000000 -0.003633666 -0.02847251 -2.400000E-7
4.248 1.468036 12.18933 23.00000 1.000000 -0.005599409 0.01269555 -1.200000E-7
4.279 1.499146 12.54436 24.00000 1.000000 -0.007138418 0.05886705 0E-7
4.310 1.529639 12.89521 25.00000 1.000000 -0.007752360 0.1104685 0E-7
4.340 1.559560 13.24224 26.00000 1.000000 -0.006950325 0.1693262 -1.200000E-7
4.369 1.588939 13.58563 27.00000 0.9999999 -0.004235050 0.2368482 6.000000E-8
4.398 1.617805 13.92559 28.00000 0.9999999 0.0008639309 0.3144805 1.200000E-7
4.400 1.620003 13.95158 29.00000 1.000000 0.001365423 -0.6022405 0E-7
4.440 1.660000 14.42708 30.00000 1.000000 0.01389347 -0.05489159 -2.400000E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559526556">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>8.41992057671 -41.9369858824 71.4658348869 -35.9470261498 7.72278528818</math:coefficients>
     <math:minRange>0.4458</math:minRange>
     <math:maxRange>1.66</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559526511">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.102947763419 -1.7341765139 16.3609593973 -9.58768851061 2.13283511082</math:coefficients>
     <math:minRange>0.4458</math:minRange>
     <math:maxRange>1.66</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559526583">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4458</math:minRange>
     <math:maxRange>1.66</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559526573">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.66</math:minRange>
     <math:maxRange>1.66</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955952658">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.66</math:minRange>
     <math:maxRange>1.66</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.1500</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559526559">
   <gml:name>Gelesen aus: PROF0059.2000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552889.8488 5987558.463 2.52</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559526580">
     <gml:description>Übernommen aus Datei: PROF0059.2000.txt</gml:description>
     <gml:name>0059.2000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559526520">
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
     <om:result>2.965 0.4453890 1.348351 1.000000 1.000000 -0.001385284 0.1290971 -1.200000E-7
3.113 0.5931080 2.193690 2.000000 0.9999998 0.004265316 -0.1480003 1.800000E-7
3.221 0.7010680 2.927788 3.000000 1.000000 0.003453905 -0.09374161 -2.400000E-7
3.301 0.7814830 3.527501 4.000000 1.000000 -0.005993202 -0.07271362 -2.400000E-7
3.372 0.8515680 4.065086 5.000000 1.000000 -0.005934480 -0.04371045 -1.200000E-7
3.436 0.9159580 4.570478 6.000000 1.000000 -0.002543881 0.0006003561 -1.200000E-7
3.496 0.9759320 5.051121 7.000000 1.000000 0.0007736801 0.04776237 -1.200000E-7
3.552 1.032099 5.509840 8.000000 1.000000 0.002346443 0.08574283 -1.200000E-7
3.605 1.084685 5.944918 9.000000 0.9999999 0.003289102 0.1020115 1.200000E-7
3.655 1.135054 6.365886 10.00000 0.9999999 0.003882244 0.1113671 1.200000E-7
3.703 1.183472 6.774467 11.00000 1.000000 0.003777863 0.1113640 -1.200000E-7
3.750 1.230216 7.172550 12.00000 1.000000 0.002921169 0.1017800 0E-7
3.795 1.275473 7.561368 13.00000 1.000000 0.001415676 0.08200908 -1.200000E-7
3.839 1.319379 7.941780 14.00000 1.000000 -0.0005100357 0.05142555 -1.200000E-7
3.882 1.362068 8.314672 15.00000 1.000000 -0.002533674 0.01018824 -1.200000E-7
3.924 1.403649 8.680745 16.00000 1.000000 -0.004258759 -0.04144049 -1.200000E-7
3.964 1.444278 9.041164 17.00000 1.000000 -0.005252368 -0.1015737 0E-7
4.004 1.483917 9.395402 18.00000 1.000000 -0.005045054 -0.1724007 -1.200000E-7
4.043 1.522683 9.744323 19.00000 1.000000 -0.003145997 -0.2522070 -1.200000E-7
4.085 1.564907 10.12868 20.00000 1.000000 0.00006635327 -0.2370131 -2.400000E-7
4.127 1.606863 10.51779 21.00000 0.9999999 0.002591369 -0.2147174 6.000000E-8
4.169 1.649311 10.91897 22.00000 0.9999998 0.005305866 -0.1662560 2.400000E-7
4.217 1.697146 11.38453 23.00000 0.9999998 0.005601726 0.03319893 1.800000E-7
4.262 1.742475 11.84067 24.00000 0.9999999 0.002999979 0.1885040 1.200000E-7
4.306 1.785638 12.28853 25.00000 1.000000 -0.0004580609 0.3068280 -1.200000E-7
4.347 1.826900 12.72903 26.00000 0.9999999 -0.003173721 0.3939047 1.200000E-7
4.386 1.866478 13.16287 27.00000 1.000000 -0.003776598 0.4545689 0E-7
4.425 1.904548 13.59066 28.00000 1.000000 -0.001177650 0.4928306 -2.400000E-7
4.430 1.909999 13.65276 29.00000 1.000000 -0.0004774944 -0.3569748 -1.200000E-7
4.450 1.930000 13.88239 30.00000 1.000000 0.002975567 -0.8024342 -2.400000E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559528133">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>6.33531958251 -28.0475272382 44.0973051003 -17.8671846633 2.96825132546</math:coefficients>
     <math:minRange>0.4454</math:minRange>
     <math:maxRange>1.93</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559528124">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.680176079785 -3.16730204876 13.3289504466 -7.07129687234 1.47785315527</math:coefficients>
     <math:minRange>0.4454</math:minRange>
     <math:maxRange>1.93</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559528119">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4454</math:minRange>
     <math:maxRange>1.93</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559528173">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.93</math:minRange>
     <math:maxRange>1.93</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559528181">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.93</math:minRange>
     <math:maxRange>1.93</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.2000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559528144">
   <gml:name>Gelesen aus: PROF0059.2560.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552936.5794 5987592.6081 2.31</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559528171">
     <gml:description>Übernommen aus Datei: PROF0059.2560.txt</gml:description>
     <gml:name>0059.2560</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition11783955952814">
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
     <om:result>2.713 0.4031450 1.306218 1.000000 1.000000 0.009137113 0.001845573 0E-7
2.868 0.5583650 2.056219 2.000000 1.000000 -0.01322816 0.01544846 0E-7
2.989 0.6789110 2.702616 3.000000 1.000000 -0.01162295 -0.01929248 0E-7
3.094 0.7837260 3.312962 4.000000 0.9999999 -0.0001794155 -0.02344784 1.200000E-7
3.186 0.8759920 3.897008 5.000000 0.9999999 0.007440315 -0.01023939 1.200000E-7
3.268 0.9576490 4.451262 6.000000 0.9999998 0.01088853 0.002314463 2.400000E-7
3.341 1.031345 4.981588 7.000000 1.000000 0.01087780 0.01493278 0E-7
3.409 1.098913 5.492929 8.000000 0.9999999 0.007835464 0.02896395 6.000000E-8
3.471 1.161427 5.987412 9.000000 0.9999999 0.002076916 0.04207197 1.200000E-7
3.528 1.217595 6.446600 10.00000 1.000000 -0.003179768 0.01486397 -2.400000E-7
3.581 1.271028 6.894199 11.00000 1.000000 -0.006198337 -0.004164137 0E-7
3.632 1.322062 7.331490 12.00000 0.9999999 -0.007502680 -0.01636659 6.000000E-8
3.681 1.371029 7.760074 13.00000 0.9999999 -0.007545003 -0.02177727 6.000000E-8
3.728 1.418050 8.179925 14.00000 1.000000 -0.006682576 -0.02366431 -1.200000E-7
3.773 1.463367 8.592246 15.00000 1.000000 -0.005203135 -0.02202108 0E-7
3.817 1.507203 8.998279 16.00000 0.9999999 -0.003363255 -0.01633893 6.000000E-8
3.860 1.549575 9.397458 17.00000 0.9999999 -0.001367943 -0.01024884 6.000000E-8
3.901 1.590651 9.790730 18.00000 0.9999999 0.0005939111 -0.003461250 6.000000E-8
3.941 1.630538 10.17856 19.00000 1.000000 0.002361954 0.003361898 0E-7
3.979 1.669318 10.56123 20.00000 0.9999999 0.003816891 0.009305206 6.000000E-8
4.017 1.707083 10.93919 21.00000 1.000000 0.004829197 0.01403946 -1.200000E-7
4.054 1.743895 11.31266 22.00000 1.000000 0.005307840 0.01673523 -1.200000E-7
4.090 1.779831 11.68205 23.00000 1.000000 0.005143441 0.01722095 0E-7
4.125 1.814925 12.04737 24.00000 1.000000 0.004265460 0.01434908 0E-7
4.159 1.849235 12.40891 25.00000 0.9999998 0.002603503 0.007823401 2.400000E-7
4.193 1.882808 12.76685 26.00000 0.9999999 0.00009824796 -0.002786084 6.000000E-8
4.226 1.915683 13.12138 27.00000 0.9999999 -0.003317868 -0.01799079 1.200000E-7
4.259 1.948770 13.48227 28.00000 0.9999999 -0.007885489 -0.01147739 1.200000E-7
4.250 1.940002 13.38619 29.00000 1.000000 -13.38619 -29.00000 -1.000000
4.290 1.980003 13.82797 30.00000 1.000000 -13.82797 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559529683">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-1.0755001325 5.29606663404 -2.65665468284 6.18146628591 -1.1728656385</math:coefficients>
     <math:minRange>0.4031</math:minRange>
     <math:maxRange>1.9488</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595296117">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.0138626004617 2.30746513917 2.40953782909 0.147284723772 -0.0866154820629</math:coefficients>
     <math:minRange>0.4031</math:minRange>
     <math:maxRange>1.9488</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595296121">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4031</math:minRange>
     <math:maxRange>1.9488</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595296128">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9488</math:minRange>
     <math:maxRange>1.9488</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595296112">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9488</math:minRange>
     <math:maxRange>1.9488</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.2560</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559529698">
   <gml:name>Gelesen aus: PROF0059.3000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552969.4562 5987619.3316 3.01</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595296107">
     <gml:description>Übernommen aus Datei: PROF0059.3000.txt</gml:description>
     <gml:name>0059.3000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559529687">
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
     <om:result>3.341 0.3312470 1.571363 1.000000 1.000000 0.005659993 0.01679390 -1.200000E-7
3.452 0.4418510 2.424984 2.000000 0.9999999 -0.01064813 -0.01701611 1.200000E-7
3.538 0.5280060 3.150902 3.000000 0.9999999 -0.004869750 -0.02575523 1.200000E-7
3.611 0.6009910 3.807628 4.000000 0.9999999 0.002896016 -0.01508641 6.000000E-8
3.675 0.6653460 4.418498 5.000000 0.9999999 0.006538644 0.008773681 6.000000E-8
3.732 0.7222180 4.980845 6.000000 1.000000 0.006362596 0.01769477 0E-7
3.785 0.7746960 5.515593 7.000000 0.9999998 0.004181770 0.03500326 2.400000E-7
3.832 0.8224320 6.014469 8.000000 0.9999998 -0.00008522965 0.03160112 1.800000E-7
3.876 0.8663690 6.479771 9.000000 0.9999999 -0.002494001 0.007895979 1.200000E-7
3.918 0.9083510 6.929079 10.00000 1.000000 -0.003305701 -0.007474880 -2.400000E-7
3.959 0.9486400 7.364599 11.00000 1.000000 -0.003192920 -0.01613089 0E-7
3.997 0.9874590 7.788237 12.00000 1.000000 -0.002600934 -0.01916790 -1.200000E-7
4.035 1.024961 8.201243 13.00000 1.000000 -0.001830233 -0.01833835 0E-7
4.071 1.061284 8.604765 14.00000 1.000000 -0.001060761 -0.01494566 -1.200000E-7
4.107 1.096550 8.999851 15.00000 1.000000 -0.0004064211 -0.01001280 0E-7
4.141 1.130847 9.387196 16.00000 1.000000 0.0001014263 -0.004947591 -1.200000E-7
4.174 1.164261 9.767517 17.00000 1.000000 0.0004721983 -0.0007511375 -1.200000E-7
4.207 1.196892 10.14175 18.00000 1.000000 0.0007262517 0.002432745 0E-7
4.239 1.228740 10.50969 19.00000 1.000000 0.0009471467 0.001709443 -1.200000E-7
4.270 1.259891 10.87213 20.00000 1.000000 0.001216857 -0.002894952 0E-7
4.301 1.291111 11.23826 21.00000 1.000000 0.001316423 0.01122748 -1.200000E-7
4.332 1.321670 11.59973 22.00000 0.9999999 0.001072714 0.01856054 6.000000E-8
4.362 1.351605 11.95679 23.00000 1.000000 0.0006562526 0.01807821 -1.200000E-7
4.391 1.380965 12.30986 24.00000 1.000000 0.0002350853 0.009346820 -1.200000E-7
4.420 1.409755 12.65882 25.00000 1.000000 0.000002430927 -0.009389707 -2.400000E-7
4.449 1.438925 13.01538 26.00000 0.9999999 -0.00009307605 -0.007189634 6.000000E-8
4.478 1.468025 13.37485 27.00000 1.000000 -0.0005719452 -0.0007858403 -1.200000E-7
4.507 1.496551 13.73091 28.00000 1.000000 -0.001226699 -0.009230875 0E-7
4.500 1.490003 13.64885 29.00000 1.000000 -13.64885 -29.00000 -1.000000
4.530 1.520004 14.02639 30.00000 1.000000 -14.02639 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559531299">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.144443441589 0.447517885417 6.82292296341 8.30749571545 -3.12203167717</math:coefficients>
     <math:minRange>0.3312</math:minRange>
     <math:maxRange>1.4966</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559531217">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.0538009658185 1.71167970592 10.3835598129 -5.4399601888 1.21450561625</math:coefficients>
     <math:minRange>0.3312</math:minRange>
     <math:maxRange>1.4966</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595312113">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3312</math:minRange>
     <math:maxRange>1.4966</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595312137">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4966</math:minRange>
     <math:maxRange>1.4966</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559531237">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4966</math:minRange>
     <math:maxRange>1.4966</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.3000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559531267">
   <gml:name>Gelesen aus: PROF0059.3650.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552986.6852 5987678.0098 2.77</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559531257">
     <gml:description>Übernommen aus Datei: PROF0059.3650.txt</gml:description>
     <gml:name>0059.3650</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595312135">
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
     <om:result>3.148 0.3775160 1.499908 1.000000 0.9999999 -0.008669149 0.06723859 6.000000E-8
3.271 0.5010380 2.388544 2.000000 1.000000 0.01465131 -0.09576344 0E-7
3.364 0.5937600 3.156631 3.000000 1.000000 0.01083825 -0.03195155 -1.200000E-7
3.434 0.6640930 3.786980 4.000000 0.9999998 -0.003394217 -0.01968533 1.800000E-7
3.496 0.7255230 4.351278 5.000000 0.9999998 -0.008207060 -0.01084052 2.400000E-7
3.552 0.7819280 4.880057 6.000000 0.9999999 -0.008278262 0.006202762 1.200000E-7
3.604 0.8344510 5.381603 7.000000 0.9999999 -0.006304773 0.02371788 6.000000E-8
3.654 0.8838470 5.861347 8.000000 1.000000 -0.003832226 0.03753507 0E-7
3.701 0.9306220 6.322611 9.000000 1.000000 -0.001539862 0.04494665 -1.200000E-7
3.745 0.9752550 6.768391 10.00000 0.9999999 0.0007173826 0.04695483 6.000000E-8
3.788 1.018068 7.201092 11.00000 0.9999999 0.002633011 0.04409526 1.200000E-7
3.829 1.059280 7.622348 12.00000 1.000000 0.003975345 0.03650401 -1.200000E-7
3.869 1.099070 8.033466 13.00000 0.9999999 0.004672041 0.02474053 1.200000E-7
3.908 1.137586 8.435546 14.00000 1.000000 0.004716744 0.009634679 0E-7
3.945 1.174951 8.829483 15.00000 0.9999999 0.004182730 -0.007829172 6.000000E-8
3.981 1.211269 9.216033 16.00000 1.000000 0.003186079 -0.02655150 0E-7
4.017 1.246629 9.595840 17.00000 1.000000 0.001876386 -0.04535391 -1.200000E-7
4.051 1.281145 9.969886 18.00000 1.000000 0.0004015096 -0.06193609 0E-7
4.085 1.314809 10.33783 19.00000 1.000000 -0.001036651 -0.07717583 -1.200000E-7
4.118 1.347713 10.70046 20.00000 1.000000 -0.002244619 -0.08887579 -1.200000E-7
4.150 1.379910 11.05816 21.00000 1.000000 -0.003017431 -0.09573577 -1.200000E-7
4.181 1.411445 11.41124 22.00000 1.000000 -0.003136878 -0.09652782 -1.200000E-7
4.213 1.443238 11.77011 23.00000 1.000000 -0.002518053 -0.06098243 -1.200000E-7
4.246 1.475632 12.14006 24.00000 0.9999998 -0.002107265 0.02506753 2.400000E-7
4.277 1.507300 12.50619 25.00000 0.9999999 -0.001885779 0.1189137 1.200000E-7
4.308 1.538301 12.86887 26.00000 0.9999999 -0.001482008 0.2225050 1.200000E-7
4.339 1.568648 13.22801 27.00000 0.9999999 -0.0005797546 0.3362517 6.000000E-8
4.368 1.598386 13.58388 28.00000 0.9999999 0.001141949 0.4617645 6.000000E-8
4.380 1.610000 13.72391 29.00000 1.000000 0.002120777 -0.08905226 -1.200000E-7
4.390 1.620000 13.84497 30.00000 0.9999998 0.003120465 -0.6978112 2.400000E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559531219">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>4.584934144 -25.4674928872 52.0033538073 -26.8223443868 6.32059460071</math:coefficients>
     <math:minRange>0.3775</math:minRange>
     <math:maxRange>1.62</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595312109">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.139011918534 1.19118083688 10.1456277867 -5.38986881621 1.21182024139</math:coefficients>
     <math:minRange>0.3775</math:minRange>
     <math:maxRange>1.62</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595312142">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3775</math:minRange>
     <math:maxRange>1.62</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559531241">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.62</math:minRange>
     <math:maxRange>1.62</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559531276">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.62</math:minRange>
     <math:maxRange>1.62</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.3650</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559531239">
   <gml:name>Gelesen aus: PROF0059.4000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552983.6592 5987726.6338 2.68</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559531223">
     <gml:description>Übernommen aus Datei: PROF0059.4000.txt</gml:description>
     <gml:name>0059.4000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595312152">
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
     <om:result>3.170 0.4899550 1.301107 1.000000 1.000000 0.006134988 0.06706351 0E-7
3.332 0.6518590 2.121403 2.000000 1.000000 -0.01649254 -0.1004877 -1.200000E-7
3.450 0.7704100 2.834297 3.000000 0.9999999 -0.001262432 -0.08306310 6.000000E-8
3.547 0.8670580 3.485693 4.000000 0.9999998 0.01017865 -0.01446432 2.400000E-7
3.630 0.9500360 4.095270 5.000000 1.000000 0.009695318 0.06508957 0E-7
3.699 1.018578 4.629263 6.000000 0.9999999 0.002158942 0.06429190 6.000000E-8
3.760 1.080423 5.123272 7.000000 0.9999999 -0.001175521 0.04558690 6.000000E-8
3.819 1.138542 5.597462 8.000000 1.000000 -0.002206742 0.03381296 0E-7
3.874 1.193572 6.055326 9.000000 1.000000 -0.002250384 0.02608924 0E-7
3.926 1.245977 6.499378 10.00000 0.9999999 -0.002031836 0.02009984 6.000000E-8
3.976 1.296109 6.931496 11.00000 1.000000 -0.001896880 0.01396293 0E-7
4.024 1.344297 7.353621 12.00000 0.9999999 -0.001965544 0.007256835 1.200000E-7
4.071 1.390657 7.765972 13.00000 1.000000 -0.002167262 -0.003462918 -1.200000E-7
4.115 1.435422 8.169960 14.00000 0.9999999 -0.002362233 -0.01820851 1.200000E-7
4.159 1.478762 8.566522 15.00000 1.000000 -0.002302494 -0.03742972 -1.200000E-7
4.201 1.520786 8.956163 16.00000 1.000000 -0.001726784 -0.06227479 0E-7
4.242 1.561611 9.339508 17.00000 0.9999999 -0.0003194908 -0.09318241 1.200000E-7
4.281 1.601331 9.717046 18.00000 0.9999998 0.002237817 -0.1306788 1.800000E-7
4.324 1.644360 10.13279 19.00000 1.000000 0.005187486 -0.06719429 0E-7
4.367 1.686984 10.55490 20.00000 1.000000 0.005556888 0.006489916 0E-7
4.408 1.728038 10.97124 21.00000 1.000000 0.004075347 0.05857527 -1.200000E-7
4.448 1.767667 11.38223 22.00000 1.000000 0.001626825 0.08987440 0E-7
4.486 1.805988 11.78817 23.00000 0.9999999 -0.001025846 0.1009478 1.200000E-7
4.523 1.843108 12.18935 24.00000 1.000000 -0.003188755 0.09247678 0E-7
4.559 1.879139 12.58628 25.00000 0.9999999 -0.004256677 0.06560588 1.200000E-7
4.594 1.914131 12.97883 26.00000 1.000000 -0.003675468 0.02007091 -1.200000E-7
4.628 1.948171 13.36739 27.00000 0.9999998 -0.0009354754 -0.04317561 1.800000E-7
4.661 1.981321 13.75213 28.00000 1.000000 0.004390100 -0.1236725 -1.200000E-7
4.660 1.980000 13.73667 29.00000 0.9999999 -13.73667 -29.00000 -0.9999999
4.690 2.010001 14.09003 30.00000 0.9999998 -14.09003 -30.00000 -0.9999998
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955953285">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>2.94535479972 -11.1757299216 15.8659032863 -1.74529247616 -0.1061122412</math:coefficients>
     <math:minRange>0.49</math:minRange>
     <math:maxRange>1.9813</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955953288">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>1.16310771012 -4.60470928115 12.5185188886 -5.67426121212 1.08417406364</math:coefficients>
     <math:minRange>0.49</math:minRange>
     <math:maxRange>1.9813</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595328153">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.49</math:minRange>
     <math:maxRange>1.9813</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595328104">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9813</math:minRange>
     <math:maxRange>1.9813</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595328137">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9813</math:minRange>
     <math:maxRange>1.9813</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.4000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559532867">
   <gml:name>Gelesen aus: PROF0059.4480.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552986.9676 5987774.2405 2.8</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559532854">
     <gml:description>Übernommen aus Datei: PROF0059.4480.txt</gml:description>
     <gml:name>0059.4480</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559532899">
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
     <om:result>3.158 0.3581840 1.415171 1.000000 1.000000 -0.006999764 -0.006916108 -1.200000E-7
3.293 0.4931990 2.262980 2.000000 1.000000 0.01348153 -0.0009150515 0E-7
3.391 0.5911000 2.971010 3.000000 1.000000 0.004328364 0.02378147 0E-7
3.469 0.6687690 3.567978 4.000000 1.000000 -0.002056113 0.003993586 -2.400000E-7
3.538 0.7379930 4.120636 5.000000 0.9999998 -0.004172102 -0.002431572 1.800000E-7
3.601 0.8010720 4.641135 6.000000 1.000000 -0.004921408 -0.001618272 -1.200000E-7
3.659 0.8589630 5.132611 7.000000 0.9999999 -0.005435276 -0.005065564 1.200000E-7
3.713 0.9128660 5.599895 8.000000 0.9999999 -0.004433234 -0.01143071 1.200000E-7
3.764 0.9638830 6.050436 9.000000 1.000000 -0.002507444 -0.01365359 -1.200000E-7
3.812 1.012449 6.486788 10.00000 0.9999999 -0.0002652709 -0.01278388 1.200000E-7
3.859 1.058893 6.910895 11.00000 1.000000 0.001850279 -0.009676438 -1.200000E-7
3.903 1.103477 7.324284 12.00000 1.000000 0.003561623 -0.004906326 0E-7
3.946 1.146412 7.728176 13.00000 1.000000 0.004690026 0.001106104 -1.200000E-7
3.988 1.187866 8.123543 14.00000 1.000000 0.005100720 0.007930699 0E-7
4.028 1.227997 8.511334 15.00000 1.000000 0.004742168 0.01569080 -1.200000E-7
4.067 1.266908 8.892097 16.00000 0.9999999 0.003554679 0.02376598 6.000000E-8
4.105 1.304595 9.265322 17.00000 0.9999998 0.001569022 0.02911673 2.400000E-7
4.141 1.340544 9.624612 18.00000 0.9999999 -0.0003486586 0.01465832 6.000000E-8
4.176 1.375668 9.978221 19.00000 1.000000 -0.001751391 0.002628727 0E-7
4.210 1.410037 10.32670 20.00000 0.9999998 -0.002712305 -0.006645485 1.800000E-7
4.244 1.443701 10.67040 21.00000 1.000000 -0.003249581 -0.01318581 0E-7
4.277 1.476717 11.00975 22.00000 1.000000 -0.003390833 -0.01661828 0E-7
4.309 1.509108 11.34487 23.00000 0.9999999 -0.003139376 -0.01743836 6.000000E-8
4.341 1.540917 11.67607 24.00000 1.000000 -0.002487514 -0.01541185 -1.200000E-7
4.372 1.572176 12.00359 25.00000 1.000000 -0.001432585 -0.01052226 -1.200000E-7
4.403 1.602913 12.32760 26.00000 0.9999999 0.00005057636 -0.002780008 6.000000E-8
4.433 1.633156 12.64830 27.00000 1.000000 0.001985637 0.007895497 0E-7
4.463 1.662926 12.96584 28.00000 0.9999998 0.004388225 0.02143166 2.400000E-7
4.460 1.660001 12.93456 29.00000 1.000000 -12.93456 -29.00000 -1.000000
4.490 1.690002 13.25623 30.00000 1.000000 -13.25623 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595343100">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.888679877954 -5.44963956011 17.249098619 -3.65677699236 0.694593109077</math:coefficients>
     <math:minRange>0.3582</math:minRange>
     <math:maxRange>1.6629</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595343105">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.203486247544 2.79707452042 5.42337579922 -1.99884002768 0.355266121052</math:coefficients>
     <math:minRange>0.3582</math:minRange>
     <math:maxRange>1.6629</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559534328">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3582</math:minRange>
     <math:maxRange>1.6629</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595343129">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6629</math:minRange>
     <math:maxRange>1.6629</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595343102">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6629</math:minRange>
     <math:maxRange>1.6629</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.4480</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559534385">
   <gml:name>Gelesen aus: PROF0059.5000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553012.0314 5987820.7902 2.85</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559534391">
     <gml:description>Übernommen aus Datei: PROF0059.5000.txt</gml:description>
     <gml:name>0059.5000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559534352">
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
     <om:result>3.119 0.2688850 1.595086 1.000000 1.000000 -0.001862259 0.06277137 -1.200000E-7
3.224 0.3744190 2.405058 2.000000 0.9999999 0.001915817 -0.05287280 6.000000E-8
3.309 0.4586250 3.078787 3.000000 0.9999997 0.001968230 -0.06387894 3.000000E-7
3.381 0.5313240 3.680056 4.000000 1.000000 0.0009951476 -0.04146938 -2.400000E-7
3.446 0.5964470 4.234086 5.000000 0.9999999 -0.0001018973 -0.01093961 6.000000E-8
3.506 0.6560560 4.753976 6.000000 1.000000 -0.0009779596 0.01693309 -2.400000E-7
3.561 0.7114110 5.247697 7.000000 0.9999999 -0.001516650 0.03774235 1.200000E-7
3.613 0.7633300 5.720342 8.000000 0.9999998 -0.001702567 0.04972708 1.800000E-7
3.662 0.8124080 6.175642 9.000000 0.9999998 -0.001570944 0.05312641 1.800000E-7
3.709 0.8590690 6.616193 10.00000 1.000000 -0.001179734 0.04871405 -1.200000E-7
3.754 0.9036430 7.044030 11.00000 0.9999999 -0.0006093273 0.03791072 6.000000E-8
3.796 0.9463990 7.460826 12.00000 0.9999998 0.00006862890 0.02264200 2.400000E-7
3.838 0.9875330 7.867752 13.00000 1.000000 0.0007502801 0.004474339 0E-7
3.877 1.027222 8.265880 14.00000 0.9999998 0.001379537 -0.01453194 1.800000E-7
3.916 1.065603 8.656042 15.00000 0.9999999 0.001852388 -0.03257536 1.200000E-7
3.953 1.102807 9.039073 16.00000 0.9999999 0.002093411 -0.04745199 6.000000E-8
3.989 1.138924 9.415453 17.00000 1.000000 0.002037362 -0.05760628 0E-7
4.024 1.174045 9.785764 18.00000 0.9999999 0.001588388 -0.06114194 6.000000E-8
4.058 1.208248 10.15046 19.00000 0.9999998 0.0006981354 -0.05625349 1.800000E-7
4.091 1.241258 10.50618 20.00000 1.000000 -0.0006281627 -0.05183027 -1.200000E-7
4.123 1.272925 10.84998 21.00000 0.9999998 -0.001527437 -0.05407965 1.800000E-7
4.154 1.303965 11.18914 22.00000 0.9999999 -0.001878855 -0.04133924 6.000000E-8
4.184 1.334413 11.52393 23.00000 0.9999999 -0.001831564 -0.01223566 6.000000E-8
4.214 1.364307 11.85464 24.00000 0.9999998 -0.001483454 0.03479398 1.800000E-7
4.244 1.393673 12.18145 25.00000 0.9999999 -0.0009579746 0.1009718 6.000000E-8
4.273 1.422556 12.50476 26.00000 1.000000 -0.0003437287 0.1882942 0E-7
4.301 1.450954 12.82446 27.00000 0.9999999 0.0002541573 0.2970474 6.000000E-8
4.329 1.478929 13.14115 28.00000 0.9999999 0.0007476075 0.4300537 1.200000E-7
4.330 1.480000 13.15331 29.00000 0.9999999 0.0007591526 -0.5257344 6.000000E-8
4.360 1.510000 13.49493 30.00000 1.000000 0.001064272 -0.2612615 -1.200000E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595359139">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.2843217388 -9.25415265025 36.9077436075 -22.5111491356 6.88221108947</math:coefficients>
     <math:minRange>0.2689</math:minRange>
     <math:maxRange>1.51</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559535915">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.349201680769 6.93739736351 0.871670261757 0.791851344501 -0.258532801422</math:coefficients>
     <math:minRange>0.2689</math:minRange>
     <math:maxRange>1.51</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595359115">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.2689</math:minRange>
     <math:maxRange>1.51</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559535933">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.51</math:minRange>
     <math:maxRange>1.51</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955953593">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.51</math:minRange>
     <math:maxRange>1.51</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.5000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595359160">
   <gml:name>Gelesen aus: PROF0059.5510.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553038.0353 5987864.9639 2.67</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559535982">
     <gml:description>Übernommen aus Datei: PROF0059.5510.txt</gml:description>
     <gml:name>0059.5510</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595359165">
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
     <om:result>3.107 0.4367970 1.464802 1.000000 1.000000 -0.01270698 -0.002183679 -2.400000E-7
3.238 0.5682030 2.379438 2.000000 0.9999998 0.02463870 -0.01976622 1.800000E-7
3.332 0.6623750 3.156594 3.000000 1.000000 0.01100009 0.04402708 0E-7
3.403 0.7325530 3.777115 4.000000 1.000000 -0.004861255 0.01108122 -2.400000E-7
3.465 0.7953250 4.345889 5.000000 1.000000 -0.01095085 -0.007881465 -1.200000E-7
3.523 0.8530560 4.879735 6.000000 1.000000 -0.01148521 -0.01507055 0E-7
3.577 0.9068770 5.386689 7.000000 0.9999998 -0.009213342 -0.01561621 1.800000E-7
3.628 0.9575480 5.872145 8.000000 1.000000 -0.005755009 -0.01254928 -1.200000E-7
3.676 1.005604 6.339878 9.000000 1.000000 -0.002083749 -0.007807948 0E-7
3.721 1.051434 6.792589 10.00000 1.000000 0.001232645 -0.002753845 0E-7
3.765 1.095341 7.232385 11.00000 1.000000 0.003877173 0.001865162 -1.200000E-7
3.808 1.137559 7.660877 12.00000 1.000000 0.005692590 0.005525334 0E-7
3.848 1.178274 8.079335 13.00000 1.000000 0.006630442 0.007885475 -1.200000E-7
3.888 1.217646 8.488845 14.00000 0.9999999 0.006748844 0.008975404 6.000000E-8
3.926 1.255805 8.890311 15.00000 1.000000 0.006120693 0.008858388 -1.200000E-7
3.963 1.292897 9.284866 16.00000 1.000000 0.004873661 0.008680673 -2.400000E-7
3.999 1.328935 9.672274 17.00000 0.9999999 0.003176980 0.006480643 1.200000E-7
4.034 1.364043 10.05354 18.00000 1.000000 0.001200980 0.003677690 -1.200000E-7
4.068 1.398299 10.42923 19.00000 1.000000 -0.0008751348 0.0008063867 0E-7
4.102 1.431746 10.79954 20.00000 0.9999999 -0.002840449 -0.002344871 1.200000E-7
4.134 1.464446 11.16491 21.00000 0.9999998 -0.004502432 -0.005211697 2.400000E-7
4.166 1.496447 11.52567 22.00000 1.000000 -0.005658636 -0.007445564 0E-7
4.198 1.527792 11.88210 23.00000 1.000000 -0.006095378 -0.008685521 -1.200000E-7
4.229 1.558517 12.23443 24.00000 1.000000 -0.005624592 -0.008651618 -1.200000E-7
4.259 1.588670 12.58303 25.00000 0.9999999 -0.004041591 -0.006539178 1.200000E-7
4.288 1.618266 12.92793 26.00000 0.9999998 -0.001144411 -0.002526027 1.800000E-7
4.317 1.647339 13.26937 27.00000 0.9999999 0.003264849 0.003926498 6.000000E-8
4.346 1.675917 13.60753 28.00000 1.000000 0.009381382 0.01324374 -1.200000E-7
4.340 1.670000 13.53730 29.00000 0.9999999 -13.53730 -29.00000 -0.9999999
4.370 1.700001 13.89446 30.00000 1.000000 -13.89446 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595359118">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>3.23051977838 -17.1292637468 32.2156234521 -11.7899630877 2.34548923766</math:coefficients>
     <math:minRange>0.4368</math:minRange>
     <math:maxRange>1.6759</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559537552">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.228429788343 0.255984726392 10.2081742234 -5.0072216443 1.05394780535</math:coefficients>
     <math:minRange>0.4368</math:minRange>
     <math:maxRange>1.6759</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595375122">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4368</math:minRange>
     <math:maxRange>1.6759</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595375181">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6759</math:minRange>
     <math:maxRange>1.6759</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955953754">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6759</math:minRange>
     <math:maxRange>1.6759</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.5510</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595375163">
   <gml:name>Gelesen aus: PROF0059.6000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553060.4544 5987908.2036 2.77</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559537514">
     <gml:description>Übernommen aus Datei: PROF0059.6000.txt</gml:description>
     <gml:name>0059.6000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559537578">
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
     <om:result>3.298 0.5279610 1.347575 1.000000 1.000000 0.009094106 0.1390876 -1.200000E-7
3.448 0.6775370 2.219295 2.000000 1.000000 -0.01593686 -0.1671786 0E-7
3.555 0.7845820 2.975950 3.000000 1.000000 -0.007944561 -0.1352360 -1.200000E-7
3.636 0.8655890 3.615430 4.000000 1.000000 -0.004396222 -0.09422178 -2.400000E-7
3.705 0.9349300 4.191596 5.000000 1.000000 0.003942784 -0.04350938 0E-7
3.768 0.9977910 4.735903 6.000000 0.9999998 0.01132129 0.02455965 1.800000E-7
3.826 1.055657 5.255423 7.000000 0.9999999 0.01473463 0.09641377 1.200000E-7
3.880 1.109528 5.755000 8.000000 0.9999999 0.01297219 0.1641470 6.000000E-8
3.929 1.158637 6.223255 9.000000 1.000000 0.006588507 0.1913096 -1.200000E-7
3.973 1.203300 6.655094 10.00000 1.000000 0.0008697291 0.1664228 -1.200000E-7
4.016 1.246163 7.073741 11.00000 0.9999999 -0.003696114 0.1359863 6.000000E-8
4.057 1.287457 7.480956 12.00000 1.000000 -0.007319237 0.09882792 -1.200000E-7
4.097 1.327349 7.877975 13.00000 0.9999999 -0.01003118 0.05360601 6.000000E-8
4.136 1.365989 8.265931 14.00000 0.9999999 -0.01172759 -0.0004383295 1.200000E-7
4.173 1.403496 8.645713 15.00000 0.9999999 -0.01221831 -0.06403915 6.000000E-8
4.210 1.439984 9.018201 16.00000 1.000000 -0.01123947 -0.1374344 -2.400000E-7
4.246 1.475526 9.383902 17.00000 0.9999999 -0.008485671 -0.2214061 6.000000E-8
4.280 1.510202 9.743415 18.00000 0.9999999 -0.003607155 -0.3161651 6.000000E-8
4.314 1.544075 10.09721 19.00000 1.000000 0.003750971 -0.4220325 -1.200000E-7
4.347 1.577201 10.44569 20.00000 1.000000 0.01397019 -0.5392416 -1.200000E-7
4.405 1.634872 11.07387 21.00000 0.9999999 0.02509150 0.01448841 6.000000E-8
4.458 1.687626 11.68520 22.00000 0.9999999 0.01765926 0.4508248 1.200000E-7
4.502 1.731954 12.22541 23.00000 0.9999999 0.001365533 0.6660797 6.000000E-8
4.529 1.759461 12.56742 24.00000 1.000000 -0.006994433 0.4230110 0E-7
4.558 1.788291 12.92913 25.00000 1.000000 -0.01129621 0.2180249 0E-7
4.587 1.816653 13.28823 26.00000 1.000000 -0.01045590 0.001279367 -2.400000E-7
4.614 1.844488 13.64380 27.00000 0.9999999 -0.004106272 -0.2294147 6.000000E-8
4.642 1.871825 13.99603 28.00000 0.9999999 0.008094506 -0.4737513 6.000000E-8
4.640 1.870002 13.97245 29.00000 1.000000 -13.97245 -29.00000 -1.000000
4.670 1.900002 14.36224 30.00000 1.000000 -14.36224 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559539053">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>7.86475938602 -29.3053979074 36.1239299636 -9.40220344592 0.78287904241</math:coefficients>
     <math:minRange>0.528</math:minRange>
     <math:maxRange>1.8718</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595390116">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>2.2484537063 -10.3331457262 21.4490602813 -10.7540341472 2.1566045828</math:coefficients>
     <math:minRange>0.528</math:minRange>
     <math:maxRange>1.8718</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559539089">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.528</math:minRange>
     <math:maxRange>1.8718</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595390125">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.8718</math:minRange>
     <math:maxRange>1.8718</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595390114">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.8718</math:minRange>
     <math:maxRange>1.8718</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.6000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595390137">
   <gml:name>Gelesen aus: PROF0059.6520.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553086.5974 5987951.9282 2.94</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595390151">
     <gml:description>Übernommen aus Datei: PROF0059.6520.txt</gml:description>
     <gml:name>0059.6520</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595390101">
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
     <om:result>3.420 0.4798920 1.450253 1.000000 1.000000 -0.01764521 0.1931338 -1.200000E-7
3.555 0.6153650 2.384642 2.000000 1.000000 0.02722211 -0.2629998 0E-7
3.652 0.7117820 3.184514 3.000000 0.9999999 0.01892209 -0.1561180 1.200000E-7
3.721 0.7808250 3.804660 4.000000 1.000000 -0.0009387759 -0.1010072 0E-7
3.782 0.8421610 4.362754 5.000000 1.000000 -0.008556801 -0.02663339 -1.200000E-7
3.839 0.8988700 4.884499 6.000000 1.000000 -0.01036089 0.05393909 0E-7
3.892 0.9520150 5.378463 7.000000 1.000000 -0.009592329 0.1240010 -2.400000E-7
3.942 1.002289 5.850219 8.000000 0.9999999 -0.007993632 0.1742891 1.200000E-7
3.990 1.050178 6.303635 9.000000 1.000000 -0.006417783 0.1999515 0E-7
4.036 1.096062 6.741767 10.00000 1.000000 -0.005202621 0.1993770 0E-7
4.080 1.140176 7.166411 11.00000 1.000000 -0.004349993 0.1714327 -1.200000E-7
4.123 1.182779 7.579689 12.00000 0.9999998 -0.003641536 0.1180048 1.800000E-7
4.164 1.224024 7.982761 13.00000 0.9999998 -0.002698494 0.04040798 1.800000E-7
4.204 1.264056 8.376775 14.00000 0.9999999 -0.001051464 -0.05894799 1.200000E-7
4.243 1.303003 8.762752 15.00000 1.000000 0.001847956 -0.1770855 0E-7
4.281 1.340943 9.141264 16.00000 1.000000 0.006585183 -0.3114488 -2.400000E-7
4.318 1.377970 9.513041 17.00000 1.000000 0.01381094 -0.4585840 0E-7
4.354 1.414213 9.879238 18.00000 0.9999999 0.02417596 -0.6137873 6.000000E-8
4.419 1.479399 10.56434 19.00000 1.000000 0.03321507 -0.06023432 -1.200000E-7
4.482 1.542366 11.28107 20.00000 0.9999999 0.01252591 0.4990648 6.000000E-8
4.516 1.575888 11.68087 21.00000 1.000000 -0.004274403 0.3607885 -1.200000E-7
4.549 1.609430 12.08705 22.00000 0.9999999 -0.01720043 0.2502929 1.200000E-7
4.582 1.642091 12.48851 23.00000 1.000000 -0.02497107 0.1470195 0E-7
4.614 1.673938 12.88557 24.00000 1.000000 -0.02684864 0.05490143 0E-7
4.645 1.705033 13.27862 25.00000 0.9999999 -0.02219247 -0.02228154 6.000000E-8
4.675 1.735404 13.66764 26.00000 1.000000 -0.01038989 -0.08181610 0E-7
4.705 1.765096 14.05285 27.00000 0.9999999 0.009125632 -0.1205752 6.000000E-8
4.734 1.794165 14.43464 28.00000 1.000000 0.03689559 -0.1350850 0E-7
4.730 1.790001 14.37967 29.00000 0.9999998 -14.37967 -29.00000 -0.9999998
4.760 1.820002 14.77788 30.00000 1.000000 -14.77788 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595406139">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>17.4219751682 -78.3013158367 121.014849473 -65.4744620123 13.4647697291</math:coefficients>
     <math:minRange>0.4799</math:minRange>
     <math:maxRange>1.7942</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955954061">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.5351008608 -4.69062106728 18.2813387576 -10.8061245556 2.50087930247</math:coefficients>
     <math:minRange>0.4799</math:minRange>
     <math:maxRange>1.7942</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595406137">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4799</math:minRange>
     <math:maxRange>1.7942</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595406198">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7942</math:minRange>
     <math:maxRange>1.7942</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955954069">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7942</math:minRange>
     <math:maxRange>1.7942</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.6520</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595406173">
   <gml:name>Gelesen aus: PROF0059.7000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553111.1638 5987993.6385 3.04</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559540668">
     <gml:description>Übernommen aus Datei: PROF0059.7000.txt</gml:description>
     <gml:name>0059.7000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559540656">
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
     <om:result>3.391 0.3512230 1.512824 1.000000 0.9999999 0.001036917 -0.1082878 6.000000E-8
3.514 0.4742830 2.427194 2.000000 1.000000 0.001384888 0.1226186 -1.200000E-7
3.601 0.5612240 3.171738 3.000000 0.9999999 -0.002336639 0.1164143 1.200000E-7
3.672 0.6324650 3.819244 4.000000 1.000000 -0.0009181703 0.06495993 0E-7
3.736 0.6957130 4.420189 5.000000 1.000000 -0.002258534 0.03262604 0E-7
3.789 0.7493210 4.943500 6.000000 0.9999999 -0.004302314 -0.04455822 6.000000E-8
3.839 0.7991520 5.434649 7.000000 1.000000 -0.002738269 -0.09802940 -1.200000E-7
3.886 0.8462160 5.902657 8.000000 0.9999999 0.0001663104 -0.1243946 6.000000E-8
3.931 0.8909940 6.351653 9.000000 0.9999999 0.003097962 -0.1269716 6.000000E-8
3.974 0.9338290 6.784573 10.00000 1.000000 0.005309462 -0.1100597 0E-7
4.015 0.9749920 7.203737 11.00000 0.9999999 0.006440062 -0.07806155 1.200000E-7
4.055 1.014688 7.610857 12.00000 1.000000 0.006406210 -0.03573721 0E-7
4.093 1.053084 8.007377 13.00000 1.000000 0.005243068 0.01218872 0E-7
4.130 1.090317 8.394424 14.00000 0.9999999 0.003167183 0.06103445 1.200000E-7
4.167 1.126510 8.773078 15.00000 1.000000 0.0004251715 0.1064541 0E-7
4.202 1.161736 9.143897 16.00000 0.9999999 -0.002639476 0.1432588 6.000000E-8
4.236 1.196110 9.507903 17.00000 1.000000 -0.005637453 0.1680079 -1.200000E-7
4.270 1.229672 9.865387 18.00000 0.9999999 -0.008179171 0.1754856 6.000000E-8
4.302 1.262493 10.21695 19.00000 1.000000 -0.009819556 0.1617793 0E-7
4.335 1.294625 10.56303 20.00000 1.000000 -0.01011596 0.1227481 -2.400000E-7
4.366 1.326117 10.90403 21.00000 1.000000 -0.008606889 0.05445884 -1.200000E-7
4.397 1.357009 11.24027 22.00000 1.000000 -0.004822870 -0.04702617 -1.200000E-7
4.427 1.387348 11.57218 23.00000 1.000000 0.001704560 -0.1852162 -1.200000E-7
4.457 1.417168 11.90004 24.00000 1.000000 0.01144820 -0.3638064 0E-7
4.486 1.446470 12.22377 25.00000 0.9999999 0.02487357 -0.5871795 6.000000E-8
4.582 1.542091 13.38888 26.00000 0.9999999 0.005762524 0.6630906 1.200000E-7
4.610 1.570164 13.75173 27.00000 1.000000 -0.004670549 0.2184817 0E-7
4.636 1.596465 14.09441 28.00000 1.000000 -0.009420235 -0.3142785 0E-7
4.630 1.590000 14.00994 29.00000 1.000000 -14.00994 -29.00000 -1.000000
4.660 1.620001 14.40332 30.00000 1.000000 -14.40332 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595406166">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-4.09594147733 23.3933528079 -41.5611587416 49.2139199967 -15.3767027973</math:coefficients>
     <math:minRange>0.3512</math:minRange>
     <math:maxRange>1.5965</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595406175">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.405592139029 -1.3934414004 16.2943771728 -10.425668918 2.5855816594</math:coefficients>
     <math:minRange>0.3512</math:minRange>
     <math:maxRange>1.5965</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595406205">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3512</math:minRange>
     <math:maxRange>1.5965</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559540684">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5965</math:minRange>
     <math:maxRange>1.5965</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595406177">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5965</math:minRange>
     <math:maxRange>1.5965</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.7000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595406102">
   <gml:name>Gelesen aus: PROF0059.8000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553158.1544 5988081.3086 2.72</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595406169">
     <gml:description>Übernommen aus Datei: PROF0059.8000.txt</gml:description>
     <gml:name>0059.8000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595406169">
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
     <om:result>3.051 0.3307610 1.356254 1.000000 0.9999999 0.007228364 -0.03270948 6.000000E-8
3.192 0.4722710 2.149572 2.000000 0.9999999 -0.009370388 0.05710882 6.000000E-8
3.300 0.5797250 2.834574 3.000000 1.000000 -0.008772722 0.03310869 -2.400000E-7
3.389 0.6691680 3.459147 4.000000 0.9999999 -0.003426075 -0.004272000 1.200000E-7
3.467 0.7470060 4.042893 5.000000 1.000000 0.002413405 -0.03084058 -1.200000E-7
3.537 0.8165760 4.596301 6.000000 1.000000 0.006881492 -0.04103511 -2.400000E-7
3.600 0.8798840 5.125872 7.000000 0.9999998 0.009065837 -0.03511682 1.800000E-7
3.658 0.9376360 5.630144 8.000000 0.9999999 0.008929131 -0.02645868 6.000000E-8
3.711 0.9912690 6.115229 9.000000 1.000000 0.007516595 -0.01033358 -1.200000E-7
3.762 1.041739 6.586378 10.00000 1.000000 0.004491579 0.01615385 0E-7
3.810 1.089505 7.045378 11.00000 1.000000 -0.0004311379 0.05010854 -1.200000E-7
3.853 1.132741 7.470416 12.00000 0.9999999 -0.005644383 0.03763880 6.000000E-8
3.894 1.173754 7.878831 13.00000 0.9999998 -0.008449335 0.01874703 1.800000E-7
3.933 1.213423 8.278581 14.00000 0.9999999 -0.009377395 0.007611446 6.000000E-8
3.972 1.251869 8.670426 15.00000 1.000000 -0.008889333 0.002107732 0E-7
4.009 1.289203 9.055117 16.00000 1.000000 -0.007398217 0.0004178472 0E-7
4.046 1.325519 9.433253 17.00000 1.000000 -0.005223481 0.0007680653 0E-7
4.081 1.360898 9.805378 18.00000 0.9999999 -0.002669363 0.001451456 1.200000E-7
4.115 1.395407 10.17190 19.00000 0.9999998 0.00002717193 0.0007282266 2.400000E-7
4.149 1.429125 10.53341 20.00000 1.000000 0.002647546 -0.002464732 -1.200000E-7
4.182 1.462083 10.89001 21.00000 1.000000 0.004984524 -0.01032629 -2.400000E-7
4.214 1.494337 11.24210 22.00000 1.000000 0.006874480 -0.02408237 -2.400000E-7
4.246 1.525932 11.58996 23.00000 1.000000 0.008183906 -0.04506561 0E-7
4.279 1.558777 11.95515 24.00000 0.9999999 0.008331693 -0.01571177 1.200000E-7
4.311 1.591195 12.31984 25.00000 0.9999999 0.006387270 0.009780176 1.200000E-7
4.343 1.623031 12.68210 26.00000 1.000000 0.002277333 0.02393320 0E-7
4.374 1.654150 13.04012 27.00000 0.9999999 -0.004022672 0.01990175 6.000000E-8
4.405 1.684663 13.39496 28.00000 1.000000 -0.01256583 -0.001148627 -1.200000E-7
4.400 1.680002 13.34052 29.00000 0.9999998 -13.34052 -29.00000 -0.9999998
4.430 1.710002 13.69248 30.00000 1.000000 -13.69248 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559542138">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-1.78948757677 11.0515814514 -13.7467907403 18.2491054754 -4.60204081511</math:coefficients>
     <math:minRange>0.3308</math:minRange>
     <math:maxRange>1.6847</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559542129">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.16156850005 2.24733851979 4.36589747967 -0.504347431214 -0.0676113558149</math:coefficients>
     <math:minRange>0.3308</math:minRange>
     <math:maxRange>1.6847</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559542151">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3308</math:minRange>
     <math:maxRange>1.6847</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595421146">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6847</math:minRange>
     <math:maxRange>1.6847</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595421158">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6847</math:minRange>
     <math:maxRange>1.6847</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.8000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595421121">
   <gml:name>Gelesen aus: PROF0059.9000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553209.8569 5988169.6769 2.96</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595421142">
     <gml:description>Übernommen aus Datei: PROF0059.9000.txt</gml:description>
     <gml:name>0059.9000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595421132">
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
     <om:result>3.271 0.3114300 1.549494 1.000000 1.000000 0.003636108 -0.006581986 0E-7
3.382 0.4217430 2.355921 2.000000 0.9999998 -0.003799398 0.01238467 1.800000E-7
3.469 0.5091520 3.032686 3.000000 0.9999999 -0.004240996 0.006252722 6.000000E-8
3.544 0.5841740 3.640170 4.000000 1.000000 -0.002284960 -0.002681575 -1.200000E-7
3.611 0.6510320 4.202277 5.000000 0.9999999 0.0002872096 -0.008790031 1.200000E-7
3.672 0.7119560 4.731516 6.000000 1.000000 0.002593324 -0.01061141 0E-7
3.728 0.7682910 5.235321 7.000000 1.000000 0.004164237 -0.008303283 -1.200000E-7
3.781 0.8209440 5.718742 8.000000 1.000000 0.004703334 -0.002276109 -1.200000E-7
3.831 0.8705370 6.185152 9.000000 1.000000 0.004035778 0.006468160 0E-7
3.878 0.9175450 6.637173 10.00000 0.9999999 0.002056453 0.01723184 6.000000E-8
3.922 0.9617270 7.070463 11.00000 1.000000 -0.0008863816 0.01535205 0E-7
3.964 1.003593 7.486544 12.00000 0.9999999 -0.002805617 0.003779227 6.000000E-8
4.004 1.043978 7.892649 13.00000 1.000000 -0.003705116 -0.003909530 0E-7
4.043 1.083061 8.290086 14.00000 1.000000 -0.003838253 -0.007765425 0E-7
4.081 1.120950 8.679541 15.00000 0.9999998 -0.003426962 -0.008948205 1.800000E-7
4.118 1.157756 9.061787 16.00000 1.000000 -0.002653406 -0.008030654 -1.200000E-7
4.154 1.193569 9.437426 17.00000 1.000000 -0.001664021 -0.005698319 -1.200000E-7
4.188 1.228478 9.807108 18.00000 1.000000 -0.0005947249 -0.002276994 0E-7
4.223 1.262539 10.17115 19.00000 0.9999998 0.0004562323 0.001284850 1.800000E-7
4.256 1.295806 10.52992 20.00000 0.9999999 0.001373809 0.004226058 1.200000E-7
4.288 1.328361 10.88405 21.00000 1.000000 0.002098712 0.006886169 0E-7
4.320 1.360221 11.23356 22.00000 0.9999999 0.002541077 0.007826594 6.000000E-8
4.351 1.391440 11.57883 23.00000 1.000000 0.002657411 0.006916012 -2.400000E-7
4.382 1.422054 11.92012 24.00000 1.000000 0.002379182 0.003622603 0E-7
4.412 1.452097 12.25764 25.00000 0.9999999 0.001664821 -0.002512235 6.000000E-8
4.442 1.481598 12.59158 26.00000 0.9999999 0.0004583296 -0.01198854 6.000000E-8
4.471 1.510592 12.92220 27.00000 0.9999999 -0.001256753 -0.02499005 1.200000E-7
4.501 1.540979 13.27154 28.00000 0.9999999 -0.003949428 0.02313340 6.000000E-8
4.500 1.540000 13.26023 29.00000 1.000000 -13.26023 -29.00000 -1.000000
4.530 1.570000 13.60862 30.00000 0.9999999 -13.60862 -30.00000 -0.9999999
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595437156">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.655222304969 2.85067666079 6.54186265276 4.63756895823 -1.45755059219</math:coefficients>
     <math:minRange>0.3114</math:minRange>
     <math:maxRange>1.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559543759">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.343097101987 5.17495898849 3.08161761996 -0.476628540252 0.0111050982626</math:coefficients>
     <math:minRange>0.3114</math:minRange>
     <math:maxRange>1.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595437168">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3114</math:minRange>
     <math:maxRange>1.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559543719">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.541</math:minRange>
     <math:maxRange>1.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559543717">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.541</math:minRange>
     <math:maxRange>1.541</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.9000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559543789">
   <gml:name>Gelesen aus: PROF0060.0000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553279.0441 5988241.8846 2.47</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595437178">
     <gml:description>Übernommen aus Datei: PROF0060.0000.txt</gml:description>
     <gml:name>0060.0000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559543789">
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
     <om:result>2.987 0.5168140 1.295128 1.000000 0.9999999 -0.0009260572 -0.007166097 1.200000E-7
3.134 0.6642540 2.029252 2.000000 0.9999997 0.001908383 0.01374925 3.000000E-7
3.246 0.7761940 2.631310 3.000000 1.000000 0.0002258542 0.003017232 0E-7
3.342 0.8723440 3.175463 4.000000 1.000000 -0.0004388083 -0.002476563 0E-7
3.428 0.9580450 3.681525 5.000000 0.9999999 -0.0006151458 -0.004585777 6.000000E-8
3.506 1.036119 4.159817 6.000000 0.9999999 -0.0005560291 -0.004710616 6.000000E-8
3.578 1.108290 4.616591 7.000000 1.000000 -0.0003946869 -0.003740640 -1.200000E-7
3.646 1.175703 5.055962 8.000000 0.9999998 -0.0002055553 -0.002298843 2.400000E-7
3.709 1.239170 5.480832 9.000000 1.000000 -0.00002541255 -0.0007668267 0E-7
3.769 1.299289 5.893320 10.00000 0.9999999 0.0001269983 0.0005682161 6.000000E-8
3.827 1.356532 6.295149 11.00000 0.9999999 0.0002371209 0.001772192 6.000000E-8
3.881 1.411247 6.687505 12.00000 1.000000 0.0003029462 0.002533304 0E-7
3.934 1.463730 7.071453 13.00000 1.000000 0.0003265050 0.002910030 0E-7
3.984 1.514218 7.447826 14.00000 1.000000 0.0003128664 0.002881836 0E-7
4.033 1.562909 7.817322 15.00000 0.9999999 0.0002696468 0.002471004 6.000000E-8
4.080 1.609988 8.180681 16.00000 1.000000 0.0001947635 0.002108082 0E-7
4.126 1.655574 8.538211 17.00000 0.9999999 0.0001140089 0.001363791 6.000000E-8
4.170 1.699797 8.890426 18.00000 0.9999999 0.00001342657 0.0004593484 6.000000E-8
4.213 1.742765 9.237698 19.00000 0.9999998 -0.00007971277 -0.0004974435 1.800000E-7
4.255 1.784569 9.580344 20.00000 1.000000 -0.0001597818 -0.001481710 -3.600000E-7
4.295 1.825309 9.918816 21.00000 1.000000 -0.0002283558 -0.001974082 -1.200000E-7
4.335 1.865034 10.25317 22.00000 1.000000 -0.0002733169 -0.002472497 0E-7
4.374 1.903818 10.58373 23.00000 0.9999999 -0.0002868008 -0.002676942 6.000000E-8
4.412 1.941721 10.91069 24.00000 1.000000 -0.0002530884 -0.002460161 -1.200000E-7
4.449 1.978793 11.23424 25.00000 1.000000 -0.0001733547 -0.001807098 0E-7
4.485 2.015082 11.55454 26.00000 1.000000 -0.00002795128 -0.0006264289 0E-7
4.521 2.050647 11.87192 27.00000 1.000000 0.0001692797 0.001609941 0E-7
4.556 2.085503 12.18628 28.00000 1.000000 0.0004422581 0.004297496 -2.400000E-7
4.550 2.080002 12.13645 29.00000 1.000000 -12.13645 -29.00000 -1.000000
4.580 2.110002 12.40919 30.00000 0.9999999 -12.40919 -30.00000 -0.9999999
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559545332">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.107190916561 -2.61162554694 8.86009453466 -1.07438275649 0.240712547581</math:coefficients>
     <math:minRange>0.5168</math:minRange>
     <math:maxRange>2.0855</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595453123">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.788684908608 3.250289184 1.55520774219 -0.0978502621933 0.0169357632715</math:coefficients>
     <math:minRange>0.5168</math:minRange>
     <math:maxRange>2.0855</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559545378">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5168</math:minRange>
     <math:maxRange>2.0855</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595453143">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>2.0855</math:minRange>
     <math:maxRange>2.0855</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595453217">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>2.0855</math:minRange>
     <math:maxRange>2.0855</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.0000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595453128">
   <gml:name>Gelesen aus: PROF0060.0560.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553277.0202 5988299.2696 3.1</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559545368">
     <gml:description>Übernommen aus Datei: PROF0060.0560.txt</gml:description>
     <gml:name>0060.0560</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559545360">
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
     <om:result>3.417 0.3171400 1.614031 1.000000 0.9999999 -0.002232151 0.05795292 6.000000E-8
3.522 0.4218240 2.454337 2.000000 1.000000 0.004180713 -0.04332887 -1.200000E-7
3.603 0.5027430 3.139739 3.000000 1.000000 0.0008457990 -0.05869964 -1.200000E-7
3.672 0.5722130 3.745404 4.000000 1.000000 -0.0007320539 -0.04416268 -1.200000E-7
3.735 0.6345540 4.302235 5.000000 1.000000 -0.001266560 -0.01805919 -1.200000E-7
3.792 0.6917080 4.823812 6.000000 1.000000 -0.001265189 0.008388972 0E-7
3.845 0.7448630 5.318410 7.000000 1.000000 -0.001005631 0.03014890 -1.200000E-7
3.895 0.7947890 5.791302 8.000000 1.000000 -0.0006318635 0.04478051 -1.200000E-7
3.942 0.8420460 6.246361 9.000000 1.000000 -0.0002435093 0.05186702 0E-7
3.987 0.8870310 6.686271 10.00000 1.000000 0.0001047381 0.05153993 -1.200000E-7
4.030 0.9300550 7.113140 11.00000 1.000000 0.0003876240 0.04468529 0E-7
4.071 0.9713700 7.528702 12.00000 1.000000 0.0005876944 0.03270952 -1.200000E-7
4.111 1.011160 7.934170 13.00000 1.000000 0.0006882134 0.01673925 -1.200000E-7
4.150 1.049590 8.330638 14.00000 0.9999999 0.0007211236 -0.001595822 6.000000E-8
4.187 1.086790 8.718997 15.00000 0.9999999 0.0006606447 -0.02080736 6.000000E-8
4.223 1.122871 9.099944 16.00000 1.000000 0.0005552826 -0.03934296 -1.200000E-7
4.258 1.157947 9.474337 17.00000 1.000000 0.0003919823 -0.05510077 -1.200000E-7
4.292 1.192076 9.842450 18.00000 1.000000 0.0001988416 -0.06718862 -1.200000E-7
4.325 1.225338 10.20485 19.00000 1.000000 -0.00001047646 -0.07383710 0E-7
4.358 1.257799 10.56197 20.00000 1.000000 -0.0002071130 -0.07344233 0E-7
4.390 1.289509 10.91414 21.00000 1.000000 -0.0003969286 -0.06470586 -1.200000E-7
4.421 1.320520 11.26170 22.00000 0.9999997 -0.0005398785 -0.04611977 3.000000E-7
4.451 1.350875 11.60492 23.00000 0.9999998 -0.0006232107 -0.01633701 2.400000E-7
4.481 1.380613 11.94407 24.00000 0.9999999 -0.0006268243 0.02595896 6.000000E-8
4.510 1.409768 12.27935 25.00000 1.000000 -0.0005426846 0.08199700 0E-7
4.538 1.438372 12.61097 26.00000 1.000000 -0.0003390449 0.1530063 0E-7
4.566 1.466466 12.93927 27.00000 1.000000 -0.00001981005 0.2406411 -1.200000E-7
4.594 1.494478 13.26916 28.00000 1.000000 0.0004533822 0.3624436 -1.200000E-7
4.610 1.510000 13.45334 29.00000 1.000000 0.0005164024 0.0009081884 -1.200000E-7
4.620 1.520003 13.57264 30.00000 1.000000 0.0003904874 -0.5810395 0E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595468221">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>2.00630897559 -14.3284014064 42.933800839 -24.8662034977 6.99197018967</math:coefficients>
     <math:minRange>0.3171</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559546850">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.687176221657 6.56991157682 2.27018808286 -0.439614359015 0.107288601802</math:coefficients>
     <math:minRange>0.3171</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595468234">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3171</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595468109">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.52</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559546846">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.52</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.0560</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559546841">
   <gml:name>Gelesen aus: PROF0060.1000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553254.2697 5988333.2626 3.14</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595468228">
     <gml:description>Übernommen aus Datei: PROF0060.1000.txt</gml:description>
     <gml:name>0060.1000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595468126">
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
     <om:result>3.511 0.3705020 1.531282 1.000000 1.000000 -0.007911141 0.02143163 -1.200000E-7
3.630 0.4897370 2.420026 2.000000 1.000000 0.009528532 -0.05619923 0E-7
3.720 0.5800340 3.186041 3.000000 0.9999998 0.01457740 -0.006867077 1.800000E-7
3.793 0.6533980 3.866834 4.000000 1.000000 0.001894068 0.05252908 0E-7
3.852 0.7116810 4.428539 5.000000 0.9999999 -0.007589903 0.01866009 1.200000E-7
3.905 0.7652760 4.952829 6.000000 1.000000 -0.01017920 -0.0004915077 -1.200000E-7
3.955 0.8152530 5.448439 7.000000 0.9999999 -0.009149245 -0.009657792 1.200000E-7
4.002 0.8623220 5.921138 8.000000 1.000000 -0.006419372 -0.01212272 0E-7
4.047 0.9069790 6.374928 9.000000 0.9999999 -0.003142322 -0.01038357 1.200000E-7
4.090 0.9495920 6.812763 10.00000 0.9999998 -1.914149E-7 -0.006286381 1.800000E-7
4.130 0.9904430 7.236916 11.00000 1.000000 0.002603656 -0.001238155 0E-7
4.170 1.029747 7.649099 12.00000 1.000000 0.004463042 0.003486063 0E-7
4.208 1.067683 8.050731 13.00000 1.000000 0.005524325 0.007016266 0E-7
4.244 1.104394 8.442956 14.00000 0.9999999 0.005801868 0.008556515 6.000000E-8
4.280 1.140000 8.826708 15.00000 0.9999999 0.005404042 0.007481230 6.000000E-8
4.315 1.174601 9.202782 16.00000 1.000000 0.004461377 0.003240894 0E-7
4.348 1.208282 9.571835 17.00000 0.9999998 0.003155205 -0.004632030 1.800000E-7
4.381 1.241114 9.934415 18.00000 1.000000 0.001681287 -0.01659839 0E-7
4.413 1.273312 10.29272 19.00000 1.000000 0.0002331241 -0.02830436 -1.200000E-7
4.446 1.305926 10.65879 20.00000 1.000000 -0.001368622 -0.008419286 -1.200000E-7
4.478 1.337810 11.01999 21.00000 1.000000 -0.003055863 0.006284364 0E-7
4.509 1.369012 11.37662 22.00000 0.9999999 -0.004504122 0.01555482 6.000000E-8
4.540 1.399573 11.72898 23.00000 1.000000 -0.005423605 0.01912205 0E-7
4.570 1.429529 12.07729 24.00000 1.000000 -0.005502008 0.01672879 0E-7
4.599 1.458915 12.42178 25.00000 1.000000 -0.004431840 0.008239200 0E-7
4.628 1.487765 12.76269 26.00000 1.000000 -0.001917894 -0.006376940 -2.400000E-7
4.656 1.516100 13.10013 27.00000 1.000000 0.002320190 -0.02747532 -1.200000E-7
4.686 1.545708 13.45560 28.00000 1.000000 0.008947209 0.006721750 0E-7
4.680 1.540003 13.38680 29.00000 0.9999999 -13.38680 -29.00000 -0.9999999
4.750 1.610000 14.25198 30.00000 1.000000 -14.25198 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559548437">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>2.03835059852 -11.6123362631 25.9283679469 -5.54033017272 0.42566422211</math:coefficients>
     <math:minRange>0.3705</math:minRange>
     <math:maxRange>1.5457</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595484137">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.0104614681165 0.407302808985 12.2918744377 -7.00086186134 1.63111617639</math:coefficients>
     <math:minRange>0.3705</math:minRange>
     <math:maxRange>1.5457</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595484136">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3705</math:minRange>
     <math:maxRange>1.5457</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559548418">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5457</math:minRange>
     <math:maxRange>1.5457</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595484155">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5457</math:minRange>
     <math:maxRange>1.5457</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.1000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559548486">
   <gml:name>Gelesen aus: PROF0060.1520.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553219.0821 5988371.4247 3.09</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595484233">
     <gml:description>Übernommen aus Datei: PROF0060.1520.txt</gml:description>
     <gml:name>0060.1520</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595484199">
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
     <om:result>3.387 0.2973010 1.598196 1.000000 1.000000 -0.0009081314 -0.008965914 -1.200000E-7
3.498 0.4082660 2.522341 2.000000 1.000000 0.001946173 0.02064442 0E-7
3.577 0.4873190 3.238286 3.000000 1.000000 -0.0001212641 -0.002457516 -1.200000E-7
3.645 0.5552000 3.879403 4.000000 1.000000 0.0003679412 -0.002442462 -2.400000E-7
3.705 0.6151090 4.465223 5.000000 1.000000 -0.0009110538 0.002405316 0E-7
3.758 0.6683630 4.997182 6.000000 1.000000 -0.001087652 -0.009771037 0E-7
3.808 0.7179630 5.500466 7.000000 1.000000 -0.0002455017 -0.01134762 -1.200000E-7
3.855 0.7646260 5.980838 8.000000 0.9999999 0.0003975601 -0.004737231 1.200000E-7
3.899 0.8088520 6.442286 9.000000 0.9999999 0.0001254357 0.007844601 1.200000E-7
3.940 0.8500570 6.876566 10.00000 1.000000 -0.0003779344 0.0009393785 0E-7
3.980 0.8896220 7.296371 11.00000 0.9999999 -0.0003191581 -0.001914585 1.200000E-7
4.018 0.9277720 7.703764 12.00000 1.000000 -0.00001948744 -0.001479296 0E-7
4.055 0.9646700 8.100221 13.00000 1.000000 0.0003103592 0.001057776 0E-7
4.090 1.000446 8.486911 14.00000 0.9999998 0.0005461819 0.004495889 1.800000E-7
4.125 1.035223 8.864963 15.00000 0.9999999 0.0006307094 0.008150694 6.000000E-8
4.159 1.069078 9.235030 16.00000 0.9999999 0.0005615713 0.01058378 6.000000E-8
4.192 1.102096 9.597887 17.00000 1.000000 0.0003616954 0.01103523 0E-7
4.224 1.134343 9.954125 18.00000 1.000000 0.00007885228 0.008564116 -2.400000E-7
4.256 1.165878 10.30427 19.00000 1.000000 -0.0002115039 0.002333644 0E-7
4.287 1.196752 10.64876 20.00000 1.000000 -0.0004079301 -0.008471041 -1.200000E-7
4.317 1.227009 10.98800 21.00000 0.9999999 -0.0004172684 -0.02465621 6.000000E-8
4.347 1.257086 11.32684 22.00000 1.000000 -0.0001438662 -0.03380055 0E-7
4.378 1.287894 11.67621 23.00000 1.000000 -0.00002144345 -0.006846534 -1.200000E-7
4.408 1.318127 12.02159 24.00000 0.9999999 -0.0001651019 0.01142544 1.200000E-7
4.438 1.347819 12.36321 25.00000 0.9999999 -0.0003442818 0.02025592 1.200000E-7
4.467 1.376996 12.70126 26.00000 1.000000 -0.0003844431 0.01876110 -2.400000E-7
4.496 1.405687 13.03594 27.00000 1.000000 -0.00005872716 0.006289918 -1.200000E-7
4.524 1.433915 13.36742 28.00000 0.9999999 0.0008182698 -0.01789723 6.000000E-8
4.520 1.430002 13.32134 29.00000 1.000000 -13.32134 -29.00000 -1.000000
4.550 1.460003 13.67571 30.00000 1.000000 -13.67571 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559548451">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.132551672587 -1.49205400221 13.7765146463 3.90415053726 -2.32936062379</math:coefficients>
     <math:minRange>0.2973</math:minRange>
     <math:maxRange>1.4339</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595484105">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.259317703642 4.24295807733 8.01043220664 -4.62607834085 1.11462631395</math:coefficients>
     <math:minRange>0.2973</math:minRange>
     <math:maxRange>1.4339</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595484228">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.2973</math:minRange>
     <math:maxRange>1.4339</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595484164">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4339</math:minRange>
     <math:maxRange>1.4339</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595484237">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4339</math:minRange>
     <math:maxRange>1.4339</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.1520</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559548428">
   <gml:name>Gelesen aus: PROF0060.2000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553186.7416 5988407.8925 3.13</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559548475">
     <gml:description>Übernommen aus Datei: PROF0060.2000.txt</gml:description>
     <gml:name>0060.2000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595484234">
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
     <om:result>3.417 0.2866400 1.665987 1.000000 1.000000 -0.00007356361 -0.01359549 -1.200000E-7
3.515 0.3849330 2.502155 2.000000 0.9999999 0.0005819004 0.01448158 1.200000E-7
3.593 0.4634290 3.194850 3.000000 1.000000 0.00008384311 0.01431177 0E-7
3.661 0.5312620 3.811297 4.000000 1.000000 -0.0004500429 0.007076069 0E-7
3.722 0.5920840 4.378087 5.000000 1.000000 -0.0007219384 -0.0009427497 -1.200000E-7
3.778 0.6478080 4.909048 6.000000 1.000000 -0.0007048646 -0.007512447 -1.200000E-7
3.830 0.6995980 5.412527 7.000000 1.000000 -0.0004353308 -0.01172863 -2.400000E-7
3.878 0.7482180 5.893959 8.000000 1.000000 0.000001695232 -0.01345695 0E-7
3.924 0.7942110 6.357206 9.000000 1.000000 0.0004978665 -0.01273702 -2.400000E-7
3.968 0.8379750 6.805057 10.00000 0.9999999 0.0009631840 -0.009803638 1.200000E-7
4.010 0.8798140 7.239646 11.00000 0.9999999 0.001290746 -0.004951656 6.000000E-8
4.050 0.9199630 7.662607 12.00000 1.000000 0.001372122 0.001405661 0E-7
4.089 0.9586330 8.075448 13.00000 1.000000 0.001140459 0.009447295 0E-7
4.126 0.9959600 8.479061 14.00000 1.000000 0.0004768104 0.01832785 0E-7
4.162 1.032036 8.873908 15.00000 0.9999998 -0.0006919308 0.02663712 1.800000E-7
4.196 1.066319 9.252694 16.00000 0.9999999 -0.001631030 0.01489779 6.000000E-8
4.230 1.099725 9.624568 17.00000 1.000000 -0.001942867 0.005852286 0E-7
4.262 1.132314 9.989961 18.00000 0.9999999 -0.001764417 -0.001028125 6.000000E-8
4.294 1.164151 10.34942 19.00000 0.9999999 -0.001254687 -0.005852505 6.000000E-8
4.325 1.195287 10.70337 20.00000 0.9999999 -0.0005675481 -0.008921093 1.200000E-7
4.356 1.225771 11.05218 21.00000 1.000000 0.0001910895 -0.01039325 0E-7
4.386 1.255658 11.39638 22.00000 1.000000 0.0008729331 -0.01003467 0E-7
4.415 1.284965 11.73601 23.00000 0.9999999 0.001380016 -0.008725038 6.000000E-8
4.444 1.313729 12.07138 24.00000 1.000000 0.001612457 -0.006554912 -1.200000E-7
4.472 1.342000 12.40298 25.00000 1.000000 0.001460060 -0.003014745 0E-7
4.500 1.369783 12.73075 26.00000 0.9999998 0.0008375764 0.0009254265 1.800000E-7
4.527 1.397111 13.05498 27.00000 1.000000 -0.0003428309 0.005375264 -2.400000E-7
4.554 1.424016 13.37599 28.00000 0.9999996 -0.002181707 0.01051481 3.600000E-7
4.550 1.420002 13.32799 29.00000 0.9999999 -13.32799 -29.00000 -0.9999999
4.580 1.450003 13.68772 30.00000 1.000000 -13.68772 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595500189">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.621175710951 2.08875233627 11.9838862029 1.16999031316 -0.491829383194</math:coefficients>
     <math:minRange>0.2866</math:minRange>
     <math:maxRange>1.424</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559551524">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.606023528309 7.55128477539 1.11107848367 0.773471586658 -0.306391648166</math:coefficients>
     <math:minRange>0.2866</math:minRange>
     <math:maxRange>1.424</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559551530">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.2866</math:minRange>
     <math:maxRange>1.424</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595515143">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.424</math:minRange>
     <math:maxRange>1.424</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595515260">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.424</math:minRange>
     <math:maxRange>1.424</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.2000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation11783955955150">
   <gml:name>Gelesen aus: PROF0060.3000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553115.4608 5988474.0839 3.12</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595515256">
     <gml:description>Übernommen aus Datei: PROF0060.3000.txt</gml:description>
     <gml:name>0060.3000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595515212">
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
     <om:result>3.629 0.5090980 1.382754 1.000000 0.9999999 0.008547461 -0.03861407 6.000000E-8
3.770 0.6499460 2.240281 2.000000 0.9999999 -0.01106313 0.07834199 6.000000E-8
3.873 0.7530910 2.979349 3.000000 1.000000 -0.01142502 0.03870398 -2.400000E-7
3.958 0.8375360 3.654164 4.000000 0.9999998 -0.005343470 -0.01414864 2.400000E-7
4.030 0.9102150 4.285217 5.000000 0.9999999 0.002242731 -0.05048603 6.000000E-8
4.095 0.9746520 4.883607 6.000000 0.9999999 0.008883663 -0.06420090 6.000000E-8
4.153 1.032923 5.456218 7.000000 1.000000 0.01322050 -0.05609086 0E-7
4.206 1.086373 6.007741 8.000000 0.9999999 0.01443779 -0.02902585 6.000000E-8
4.256 1.135922 6.541477 9.000000 1.000000 0.01201508 0.01343948 0E-7
4.302 1.182238 7.059930 10.00000 1.000000 0.005615622 0.06786927 0E-7
4.343 1.222864 7.528033 11.00000 1.000000 -0.001904078 0.05675333 0E-7
4.381 1.261166 7.976596 12.00000 1.000000 -0.007129581 0.04367022 -1.200000E-7
4.418 1.298026 8.414805 13.00000 1.000000 -0.01096567 0.04296869 -2.400000E-7
4.453 1.332547 8.830439 14.00000 1.000000 -0.01346249 0.02221264 0E-7
4.486 1.365547 9.230686 15.00000 1.000000 -0.01354243 -0.003316691 0E-7
4.518 1.397668 9.622886 16.00000 0.9999998 -0.01177870 -0.01972256 1.800000E-7
4.549 1.428983 10.00771 17.00000 1.000000 -0.008733784 -0.02853419 0E-7
4.580 1.459568 10.38594 18.00000 0.9999999 -0.004924587 -0.03081403 6.000000E-8
4.609 1.489464 10.75788 19.00000 0.9999999 -0.0007971307 -0.02845142 1.200000E-7
4.639 1.518725 11.12409 20.00000 0.9999999 0.003229504 -0.02265898 6.000000E-8
4.667 1.547396 11.48498 21.00000 0.9999999 0.006786783 -0.01471094 1.200000E-7
4.696 1.575528 11.84108 22.00000 1.000000 0.009543521 -0.005447762 0E-7
4.723 1.603132 12.19241 23.00000 0.9999999 0.01118237 0.003034105 6.000000E-8
4.750 1.630243 12.53931 24.00000 0.9999999 0.01142533 0.009643821 6.000000E-8
4.777 1.656892 12.88208 25.00000 1.000000 0.01000028 0.01329858 0E-7
4.803 1.683104 13.22096 26.00000 0.9999999 0.006664982 0.01282541 1.200000E-7
4.829 1.708902 13.55616 27.00000 1.000000 0.001197012 0.007093420 -2.400000E-7
4.854 1.734305 13.88786 28.00000 1.000000 -0.006625541 -0.005076050 0E-7
4.880 1.759999 14.22497 29.00000 1.000000 -0.01729701 0.001444023 -1.200000E-7
4.880 1.759999 14.22497 30.00000 0.9999999 -14.22497 -30.00000 -0.9999999
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595515244">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-6.05506274411 26.5250652638 -39.8642832439 32.6457650502 -6.89114088545</math:coefficients>
     <math:minRange>0.5091</math:minRange>
     <math:maxRange>1.76</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595515185">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-4.6169563058E-4 0.384550734004 4.14286446227 1.25130769451 -0.638183000444</math:coefficients>
     <math:minRange>0.5091</math:minRange>
     <math:maxRange>1.76</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559551548">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5091</math:minRange>
     <math:maxRange>1.76</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595515249">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.76</math:minRange>
     <math:maxRange>1.76</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595515124">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.76</math:minRange>
     <math:maxRange>1.76</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.3000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595515149">
   <gml:name>Gelesen aus: PROF0060.4000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553054.338 5988559.8399 3.22</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595515162">
     <gml:description>Übernommen aus Datei: PROF0060.4000.txt</gml:description>
     <gml:name>0060.4000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595515258">
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
     <om:result>3.733 0.5133370 1.374598 1.000000 0.9999999 0.0001882425 0.08418157 6.000000E-8
3.879 0.6586530 2.262998 2.000000 1.000000 -0.003420641 -0.1340436 0E-7
3.983 0.7626430 3.033985 3.000000 0.9999999 0.003505463 -0.08364847 1.200000E-7
4.064 0.8437970 3.712005 4.000000 1.000000 0.001799699 -0.01814073 0E-7
4.131 0.9113820 4.314566 5.000000 1.000000 0.002125300 0.01851746 0E-7
4.192 0.9722900 4.884517 6.000000 1.000000 0.002490854 0.06161593 -1.200000E-7
4.248 1.028085 5.429010 7.000000 1.000000 0.0003214117 0.1037986 -2.400000E-7
4.298 1.077704 5.929914 8.000000 0.9999999 -0.004015699 0.09832768 1.200000E-7
4.343 1.123118 6.396515 9.000000 1.000000 -0.005510978 0.06366189 0E-7
4.386 1.166389 6.847676 10.00000 1.000000 -0.004928081 0.03226886 0E-7
4.428 1.207807 7.285527 11.00000 1.000000 -0.003202601 0.004271722 0E-7
4.468 1.247603 7.711768 12.00000 0.9999999 -0.001002311 -0.01990800 1.200000E-7
4.506 1.285968 8.127814 13.00000 1.000000 0.001199496 -0.03957015 -1.200000E-7
4.543 1.323038 8.534623 14.00000 1.000000 0.003038615 -0.05448210 -1.200000E-7
4.579 1.358946 8.933155 15.00000 1.000000 0.004305203 -0.06378605 0E-7
4.614 1.393794 9.324163 16.00000 1.000000 0.004802317 -0.06688882 -1.200000E-7
4.648 1.427682 9.708383 17.00000 0.9999999 0.004445722 -0.06278579 1.200000E-7
4.681 1.460676 10.08625 18.00000 1.000000 0.003165606 -0.05109580 -2.400000E-7
4.712 1.492392 10.45287 19.00000 1.000000 0.001090324 -0.04566377 0E-7
4.743 1.522757 10.80590 20.00000 0.9999999 -0.0006512632 -0.05161167 1.200000E-7
4.773 1.552544 11.15386 21.00000 1.000000 -0.001881302 -0.04539039 -1.200000E-7
4.802 1.581759 11.49674 22.00000 0.9999999 -0.002677334 -0.02737095 6.000000E-8
4.830 1.610468 11.83521 23.00000 1.000000 -0.003055201 0.004264743 0E-7
4.859 1.638680 12.16929 24.00000 1.000000 -0.003034728 0.04945316 -1.200000E-7
4.886 1.666429 12.49934 25.00000 1.000000 -0.002636126 0.1091089 -1.200000E-7
4.914 1.693742 12.82558 26.00000 0.9999998 -0.001830179 0.1839894 2.400000E-7
4.941 1.720639 13.14820 27.00000 0.9999999 -0.0006143837 0.2746566 6.000000E-8
4.967 1.747142 13.46740 28.00000 1.000000 0.001051479 0.3818176 0E-7
4.980 1.759999 13.62271 29.00000 0.9999999 0.002039868 -0.06908809 6.000000E-8
4.990 1.769999 13.74373 30.00000 0.9999999 0.002891227 -0.6364596 1.200000E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559553134">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>7.74850233815 -32.4551302758 47.3410174284 -20.7100901253 4.64472480144</math:coefficients>
     <math:minRange>0.5133</math:minRange>
     <math:maxRange>1.77</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559553187">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>1.07793593492 -5.16330753385 13.6608382909 -5.24742351903 0.826066638152</math:coefficients>
     <math:minRange>0.5133</math:minRange>
     <math:maxRange>1.77</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595531121">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5133</math:minRange>
     <math:maxRange>1.77</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595531198">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.77</math:minRange>
     <math:maxRange>1.77</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595531109">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.77</math:minRange>
     <math:maxRange>1.77</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.4000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559553127">
   <gml:name>Gelesen aus: PROF0060.4510.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553054.7998 5988614.9783 3.54</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595531101">
     <gml:description>Übernommen aus Datei: PROF0060.4510.txt</gml:description>
     <gml:name>0060.4510</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595531147">
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
     <om:result>3.846 0.3057480 1.769416 1.000000 1.000000 -0.007960614 -0.01560910 -2.400000E-7
3.951 0.4105880 2.752561 2.000000 0.9999998 0.01566720 0.02647347 1.800000E-7
4.025 0.4846750 3.508902 3.000000 0.9999998 0.003783951 0.01319565 1.800000E-7
4.087 0.5474260 4.164567 4.000000 1.000000 -0.002943179 -0.002547947 -1.200000E-7
4.144 0.6037610 4.762287 5.000000 0.9999998 -0.005307966 -0.01041303 2.400000E-7
4.196 0.6555740 5.319534 6.000000 1.000000 -0.005342806 -0.01222894 -2.400000E-7
4.244 0.7039010 5.845769 7.000000 0.9999999 -0.004214663 -0.01068399 1.200000E-7
4.289 0.7494290 6.347253 8.000000 0.9999999 -0.002613342 -0.007421948 6.000000E-8
4.333 0.7926360 6.828303 9.000000 0.9999999 -0.0009395250 -0.003606996 6.000000E-8
4.374 0.8338770 7.292119 10.00000 1.000000 0.0005709060 0.00007585459 -1.200000E-7
4.413 0.8734240 7.741181 11.00000 0.9999999 0.001753698 0.003243023 6.000000E-8
4.451 0.9114870 8.177327 12.00000 0.9999998 0.002584431 0.005608181 1.800000E-7
4.488 0.9482340 8.602085 13.00000 0.9999999 0.003014470 0.007011647 6.000000E-8
4.524 0.9838030 9.016675 14.00000 1.000000 0.003066993 0.007416511 0E-7
4.558 1.018318 9.422220 15.00000 1.000000 0.002785269 0.007138160 0E-7
4.592 1.051865 9.819445 16.00000 1.000000 0.002231398 0.005979949 -1.200000E-7
4.625 1.084530 10.20912 17.00000 1.000000 0.001473381 0.004197944 0E-7
4.656 1.116383 10.59187 18.00000 0.9999999 0.0005806670 0.001949574 1.200000E-7
4.687 1.147494 10.96832 19.00000 1.000000 -0.0003590406 -0.0002824075 0E-7
4.718 1.177906 11.33882 20.00000 1.000000 -0.001247274 -0.002612457 -1.200000E-7
4.748 1.207670 11.70382 21.00000 1.000000 -0.002008715 -0.004684997 -1.200000E-7
4.777 1.236829 12.06370 22.00000 0.9999999 -0.002537498 -0.006215066 1.200000E-7
4.805 1.265421 12.41880 23.00000 1.000000 -0.002752607 -0.006923920 -1.200000E-7
4.833 1.293479 12.76940 24.00000 0.9999999 -0.002556060 -0.006566957 6.000000E-8
4.861 1.321032 13.11574 25.00000 0.9999999 -0.001857255 -0.004922790 6.000000E-8
4.888 1.348109 13.45807 26.00000 1.000000 -0.0005494192 -0.001664232 0E-7
4.915 1.374734 13.79660 27.00000 0.9999999 0.001450858 0.003445822 6.000000E-8
4.941 1.400929 14.13152 28.00000 0.9999999 0.004226743 0.01064897 1.200000E-7
4.940 1.400001 14.11963 29.00000 0.9999999 -14.11963 -29.00000 -0.9999999
4.960 1.420001 14.37653 30.00000 1.000000 -14.37653 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595546133">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.801413252175 -7.51638401756 29.6869576408 -11.0781037543 2.57913840225</math:coefficients>
     <math:minRange>0.3057</math:minRange>
     <math:maxRange>1.4009</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559554669">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.75130729034 6.91664511767 5.00420093894 -2.65391442766 0.69393928335</math:coefficients>
     <math:minRange>0.3057</math:minRange>
     <math:maxRange>1.4009</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595546120">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3057</math:minRange>
     <math:maxRange>1.4009</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595546109">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4009</math:minRange>
     <math:maxRange>1.4009</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595546203">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4009</math:minRange>
     <math:maxRange>1.4009</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.4510</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595546167">
   <gml:name>Gelesen aus: PROF0060.5210.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553087.1005 5988681.3983 3.35</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559554685">
     <gml:description>Übernommen aus Datei: PROF0060.5210.txt</gml:description>
     <gml:name>0060.5210</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595546179">
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
     <om:result>3.795 0.4452230 1.527937 1.000000 0.9999999 -0.00005458068 0.003708641 6.000000E-8
3.915 0.5648010 2.447322 2.000000 0.9999999 -0.002319595 -0.008955021 6.000000E-8
4.000 0.6504080 3.184746 3.000000 1.000000 0.001549934 -0.01836015 -1.200000E-7
4.073 0.7228240 3.852617 4.000000 1.000000 0.004681022 0.005904406 0E-7
4.136 0.7860870 4.469067 5.000000 1.000000 0.001561181 0.03980652 -2.400000E-7
4.190 0.8395710 5.007269 6.000000 0.9999999 -0.001529913 0.01391856 6.000000E-8
4.239 0.8893040 5.517171 7.000000 1.000000 -0.002206678 0.0006281449 0E-7
4.286 0.9360170 6.004403 8.000000 0.9999998 -0.002288135 -0.002941899 1.800000E-7
4.330 0.9798200 6.468323 9.000000 0.9999999 -0.002594458 -0.008686897 1.200000E-7
4.371 1.021170 6.910799 10.00000 1.000000 -0.002066846 -0.01827243 0E-7
4.411 1.060856 7.339289 11.00000 1.000000 -0.0009952568 -0.02169982 -1.200000E-7
4.449 1.099100 7.755749 12.00000 1.000000 0.0001983648 -0.02001083 -1.200000E-7
4.486 1.136052 8.161429 13.00000 1.000000 0.001256338 -0.01492994 -1.200000E-7
4.522 1.171851 8.557555 14.00000 1.000000 0.002000810 -0.007590094 0E-7
4.557 1.206610 8.945079 15.00000 0.9999999 0.002376416 0.0008899284 1.200000E-7
4.590 1.240422 9.324796 16.00000 1.000000 0.002365952 0.009405705 0E-7
4.623 1.273372 9.697437 17.00000 0.9999999 0.002007994 0.01710488 1.200000E-7
4.656 1.305533 10.06365 18.00000 1.000000 0.001352996 0.02318608 -1.200000E-7
4.687 1.336954 10.42380 19.00000 0.9999999 0.0005124841 0.02643277 6.000000E-8
4.718 1.367691 10.77840 20.00000 1.000000 -0.0004181229 0.02612726 0E-7
4.748 1.397793 11.12783 21.00000 1.000000 -0.001282429 0.02158037 0E-7
4.777 1.427298 11.47242 22.00000 1.000000 -0.001954732 0.01195466 0E-7
4.806 1.456244 11.81249 23.00000 0.9999999 -0.002276102 -0.003403735 1.200000E-7
4.835 1.484674 12.14843 24.00000 1.000000 -0.002090574 -0.02480791 -2.400000E-7
4.863 1.512603 12.48032 25.00000 1.000000 -0.001228259 -0.05339053 -1.200000E-7
4.891 1.540649 12.81562 26.00000 1.000000 0.0003523476 -0.06892229 -2.400000E-7
4.921 1.571031 13.18274 27.00000 0.9999999 0.001551676 0.006434175 6.000000E-8
4.951 1.600721 13.54636 28.00000 1.000000 0.001538168 0.06488946 -1.200000E-7
4.950 1.600002 13.53750 29.00000 0.9999999 -13.53750 -29.00000 -0.9999999
5.010 1.659999 14.28679 30.00000 1.000000 -14.28679 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595562106">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.03858223487 -5.72641663532 10.4647802439 6.03017655536 -2.33866098773</math:coefficients>
     <math:minRange>0.4452</math:minRange>
     <math:maxRange>1.6007</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595562158">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.0733866666327 -1.58478294455 13.6990670995 -6.92469781138 1.41834136387</math:coefficients>
     <math:minRange>0.4452</math:minRange>
     <math:maxRange>1.6007</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595562105">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4452</math:minRange>
     <math:maxRange>1.6007</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559556231">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6007</math:minRange>
     <math:maxRange>1.6007</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595562196">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6007</math:minRange>
     <math:maxRange>1.6007</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.5210</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595562235">
   <gml:name>Gelesen aus: PROF0060.5280.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553089.4394 5988686.5515 3.43</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595562110">
     <gml:description>Übernommen aus Datei: PROF0060.5280.txt</gml:description>
     <gml:name>0060.5280</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595562228">
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
     <om:result>3.658 0.2280310 1.725246 1.000000 1.000000 0.001861246 -0.004346858 0E-7
3.752 0.3216150 2.580056 2.000000 1.000000 -0.001681969 0.007923080 -1.200000E-7
3.827 0.3965100 3.284811 3.000000 1.000000 -0.002100223 0.004393612 -1.200000E-7
3.891 0.4613560 3.909846 4.000000 0.9999998 -0.001344912 -0.001113906 1.800000E-7
3.950 0.5196040 4.483020 5.000000 1.000000 -0.0001911417 -0.005138578 -2.400000E-7
4.003 0.5730610 5.018822 6.000000 0.9999998 0.0009524585 -0.006819641 1.800000E-7
4.053 0.6228200 5.525975 7.000000 1.000000 0.001830507 -0.006129152 -1.200000E-7
4.100 0.6696030 6.010186 8.000000 1.000000 0.002298492 -0.003369997 0E-7
4.144 0.7139160 6.475441 9.000000 0.9999999 0.002244753 0.0009950043 6.000000E-8
4.186 0.7561470 6.924816 10.00000 1.000000 0.001597706 0.006849636 -1.200000E-7
4.227 0.7965660 7.360390 11.00000 1.000000 0.0002874396 0.01339761 -2.400000E-7
4.265 0.8348990 7.777819 12.00000 0.9999999 -0.001076148 0.007051519 6.000000E-8
4.302 0.8718980 8.183939 13.00000 1.000000 -0.001820979 0.001659238 -1.200000E-7
4.338 0.9077420 8.580365 14.00000 1.000000 -0.002069015 -0.001981514 -1.200000E-7
4.373 0.9425460 8.968117 15.00000 0.9999999 -0.001966482 -0.004098990 6.000000E-8
4.406 0.9764020 9.347980 16.00000 1.000000 -0.001619071 -0.005057456 0E-7
4.439 1.009402 9.720762 17.00000 1.000000 -0.001104418 -0.004786120 0E-7
4.472 1.041602 10.08693 18.00000 0.9999998 -0.0005212847 -0.003944935 1.800000E-7
4.503 1.073067 10.44703 19.00000 1.000000 0.00007664456 -0.002637433 0E-7
4.534 1.103850 10.80152 20.00000 0.9999999 0.0006272905 -0.001087828 1.200000E-7
4.564 1.133996 11.15080 21.00000 0.9999999 0.001070087 0.0004337144 1.200000E-7
4.594 1.163545 11.49517 22.00000 1.000000 0.001364478 0.001672980 0E-7
4.623 1.192546 11.83512 23.00000 1.000000 0.001466301 0.002851822 0E-7
4.651 1.221019 12.17074 24.00000 0.9999999 0.001349071 0.003338616 6.000000E-8
4.679 1.248994 12.50231 25.00000 0.9999999 0.0009613356 0.002957173 1.200000E-7
4.706 1.276491 12.82997 26.00000 1.000000 0.0002861875 0.001278366 -1.200000E-7
4.734 1.303577 13.15443 27.00000 0.9999998 -0.0007160342 -0.0002970923 1.800000E-7
4.760 1.330218 13.47520 28.00000 1.000000 -0.002062319 -0.003992866 0E-7
4.760 1.330002 13.47260 29.00000 0.9999999 -13.47260 -29.00000 -0.9999999
4.780 1.350002 13.71446 30.00000 0.9999999 -13.71446 -30.00000 -0.9999999
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595578189">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.363386367903 2.46465383904 15.4636581576 -0.517984311877 -0.339354081375</math:coefficients>
     <math:minRange>0.228</math:minRange>
     <math:maxRange>1.3302</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595578171">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.204837865543 8.0095913973 2.09112230714 -0.268724836049 -0.0141163510042</math:coefficients>
     <math:minRange>0.228</math:minRange>
     <math:maxRange>1.3302</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595578240">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.228</math:minRange>
     <math:maxRange>1.3302</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595578219">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3302</math:minRange>
     <math:maxRange>1.3302</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559557891">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3302</math:minRange>
     <math:maxRange>1.3302</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.5280</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595578283">
   <gml:name>Gelesen aus: PROF0060.6000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553113.4613 5988757.3721 3.12</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595578207">
     <gml:description>Übernommen aus Datei: PROF0060.6000.txt</gml:description>
     <gml:name>0060.6000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595578219">
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
     <om:result>3.610 0.4903540 1.471983 1.000000 0.9999999 -0.003242985 0.004921532 1.200000E-7
3.749 0.6285150 2.418320 2.000000 1.000000 0.007950435 -0.02153901 0E-7
3.847 0.7273500 3.238689 3.000000 0.9999999 0.003943309 0.03316769 6.000000E-8
3.920 0.8002080 3.908357 4.000000 0.9999998 -0.006323761 0.003541545 1.800000E-7
3.984 0.8635520 4.516102 5.000000 1.000000 -0.006798223 -0.01980876 0E-7
4.041 0.9211480 5.088932 6.000000 1.000000 -0.003436261 -0.02487649 -1.200000E-7
4.094 0.9742960 5.634631 7.000000 1.000000 0.0005495712 -0.01708323 -1.200000E-7
4.144 1.023871 6.158443 8.000000 1.000000 0.003389787 -0.0001447448 -2.400000E-7
4.190 1.070495 6.664106 9.000000 1.000000 0.004090723 0.02324777 0E-7
4.234 1.114370 7.151461 10.00000 1.000000 0.002155832 0.04487076 -2.400000E-7
4.274 1.154259 7.601663 11.00000 1.000000 0.0001682986 0.02230573 0E-7
4.313 1.192630 8.039413 12.00000 0.9999998 -0.0006707324 0.005619371 1.800000E-7
4.350 1.229658 8.466210 13.00000 1.000000 -0.0008327480 -0.006041555 0E-7
4.386 1.265501 8.883414 14.00000 0.9999999 -0.0006159166 -0.01302914 6.000000E-8
4.420 1.300256 9.291779 15.00000 1.000000 -0.0002490932 -0.01664136 -1.200000E-7
4.454 1.334029 9.692217 16.00000 1.000000 0.0001159555 -0.01736403 -1.200000E-7
4.487 1.366901 10.08539 17.00000 0.9999999 0.0003908806 -0.01592284 1.200000E-7
4.519 1.398987 10.47241 18.00000 1.000000 0.0005230181 -0.01155633 0E-7
4.550 1.430271 10.85285 19.00000 0.9999999 0.0004989818 -0.007273937 6.000000E-8
4.581 1.460840 11.22755 20.00000 0.9999999 0.0003246019 -0.002607545 1.200000E-7
4.611 1.490746 11.59695 21.00000 1.000000 0.00003969854 0.002076620 -1.200000E-7
4.640 1.520033 11.96139 22.00000 0.9999999 -0.0002936949 0.006365600 6.000000E-8
4.669 1.548736 12.32117 23.00000 1.000000 -0.0006379071 0.009722525 -1.200000E-7
4.697 1.576891 12.67657 24.00000 1.000000 -0.0009055876 0.01180481 0E-7
4.725 1.604538 13.02798 25.00000 1.000000 -0.001027748 0.01256677 -1.200000E-7
4.752 1.631692 13.37543 26.00000 1.000000 -0.0008985840 0.01118638 0E-7
4.778 1.658383 13.71920 27.00000 1.000000 -0.0004453352 0.007491347 -2.400000E-7
4.805 1.684634 14.05948 28.00000 1.000000 0.0004315470 0.001155373 0E-7
4.830 1.709999 14.39031 29.00000 1.000000 0.001805937 -0.02615486 -1.200000E-7
4.830 1.709999 14.39031 30.00000 1.000000 -14.39031 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595578111">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.97420030139 -8.36808941233 11.6588313941 3.35234377774 -1.11631532344</math:coefficients>
     <math:minRange>0.4904</math:minRange>
     <math:maxRange>1.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595593164">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.718770912053 -4.19072624714 14.3944722123 -6.09440261415 1.07854171943</math:coefficients>
     <math:minRange>0.4904</math:minRange>
     <math:maxRange>1.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595593142">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4904</math:minRange>
     <math:maxRange>1.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595593131">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.71</math:minRange>
     <math:maxRange>1.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595593105">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.71</math:minRange>
     <math:maxRange>1.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.6000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation11783955955932">
   <gml:name>Gelesen aus: PROF0060.6310.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553128.8758 5988780.332 3.22</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595593302">
     <gml:description>Übernommen aus Datei: PROF0060.6310.txt</gml:description>
     <gml:name>0060.6310</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559559375">
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
     <om:result>3.563 0.3430980 1.569915 1.000000 1.000000 0.0004852617 0.1020627 -1.200000E-7
3.674 0.4539320 2.421649 2.000000 1.000000 -0.004090780 -0.1079278 -1.200000E-7
3.760 0.5401120 3.140848 3.000000 1.000000 0.0007646035 -0.1188133 -1.200000E-7
3.833 0.6133930 3.790759 4.000000 0.9999999 0.005235578 -0.06265033 1.200000E-7
3.898 0.6782010 4.394888 5.000000 1.000000 0.005858384 0.01428946 0E-7
3.957 0.7368670 4.965539 6.000000 1.000000 0.001380405 0.09256754 0E-7
4.007 0.7870900 5.467980 7.000000 1.000000 -0.003650465 0.08744672 0E-7
4.054 0.8341150 5.945150 8.000000 0.9999999 -0.005429623 0.07555454 6.000000E-8
4.099 0.8786930 6.403476 9.000000 1.000000 -0.005214614 0.06090021 -1.200000E-7
4.141 0.9212050 6.845979 10.00000 1.000000 -0.003853359 0.04383065 -2.400000E-7
4.182 0.9619250 7.274789 11.00000 1.000000 -0.001951539 0.02477772 -1.200000E-7
4.221 1.001076 7.691669 12.00000 0.9999998 0.00004069933 0.004713727 1.800000E-7
4.259 1.038835 8.097974 13.00000 0.9999999 0.001831365 -0.01524169 6.000000E-8
4.295 1.075365 8.495032 14.00000 1.000000 0.003188086 -0.03334497 -1.200000E-7
4.331 1.110767 8.883553 15.00000 0.9999999 0.003960565 -0.04877200 1.200000E-7
4.365 1.145150 9.264416 16.00000 0.9999999 0.004022364 -0.05994322 6.000000E-8
4.399 1.178601 9.638271 17.00000 0.9999999 0.003319807 -0.06544642 6.000000E-8
4.431 1.210951 10.00287 18.00000 1.000000 0.001879856 -0.07148905 -2.400000E-7
4.462 1.242114 10.35610 19.00000 0.9999998 0.0005496703 -0.08230752 1.800000E-7
4.493 1.272633 10.70368 20.00000 1.000000 -0.0005126745 -0.08161681 -1.200000E-7
4.523 1.302552 11.04600 21.00000 1.000000 -0.001353827 -0.06805296 0E-7
4.552 1.331920 11.38356 22.00000 0.9999999 -0.002036591 -0.03991276 1.200000E-7
4.581 1.360727 11.71610 23.00000 1.000000 -0.002540215 0.002521840 -1.200000E-7
4.609 1.389020 12.04387 24.00000 0.9999999 -0.002661499 0.06103444 1.200000E-7
4.637 1.416867 12.36753 25.00000 1.000000 -0.002352246 0.1384089 0E-7
4.664 1.444299 12.68738 26.00000 0.9999999 -0.001617024 0.2362225 1.200000E-7
4.691 1.471339 13.00363 27.00000 1.000000 -0.0004462113 0.3558318 0E-7
4.718 1.498003 13.31647 28.00000 1.000000 0.001129582 0.4983652 -1.200000E-7
4.720 1.500003 13.33997 29.00000 1.000000 0.001274882 -0.4143366 -1.200000E-7
4.740 1.520003 13.57530 30.00000 0.9999999 0.002789558 -0.5286726 6.000000E-8
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559560920">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>3.6523277701 -21.9996080036 51.5544002615 -29.2807188357 8.05089224604</math:coefficients>
     <math:minRange>0.3431</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595609313">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.256898391334 3.16452499503 7.28907250071 -3.07614417787 0.55958336564</math:coefficients>
     <math:minRange>0.3431</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595609131">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3431</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595609231">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.52</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559560983">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.52</math:minRange>
     <math:maxRange>1.52</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.6310</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595609292">
   <gml:name>Gelesen aus: PROF0060.7000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553194.98 5988821.9728 3.36</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation117839559560943">
     <gml:description>Übernommen aus Datei: PROF0060.7000.txt</gml:description>
     <gml:name>0060.7000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559560991">
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
     <om:result>3.639 0.2790220 1.800489 1.000000 1.000000 0.001383143 -0.01226916 0E-7
3.727 0.3668100 2.682997 2.000000 0.9999999 -0.0007035891 0.01569283 6.000000E-8
3.797 0.4371360 3.407744 3.000000 1.000000 -0.001492566 0.01332347 -1.200000E-7
3.858 0.4980910 4.048724 4.000000 0.9999999 -0.001469571 0.004196096 6.000000E-8
3.913 0.5529000 4.635224 5.000000 0.9999998 -0.0009366774 -0.004748061 2.400000E-7
3.963 0.6032500 5.182472 6.000000 1.000000 -0.0001227750 -0.01110043 0E-7
4.010 0.6501640 5.699678 7.000000 0.9999999 0.0007688998 -0.01402602 6.000000E-8
4.054 0.6943110 6.192807 8.000000 1.000000 0.001582895 -0.01357880 -2.400000E-7
4.096 0.7361700 6.666139 9.000000 1.000000 0.002173473 -0.009927121 -1.200000E-7
4.136 0.7760860 7.122719 10.00000 0.9999998 0.002419100 -0.003604223 2.400000E-7
4.174 0.8143260 7.564908 11.00000 1.000000 0.002205544 0.004896273 -1.200000E-7
4.211 0.8511030 7.994592 12.00000 1.000000 0.001430527 0.01515422 0E-7
4.247 0.8865800 8.413178 13.00000 1.000000 0.000009745629 0.02651125 0E-7
4.280 0.9204980 8.817038 14.00000 1.000000 -0.002026159 0.02657311 0E-7
4.313 0.9526300 9.201414 15.00000 1.000000 -0.003176873 0.003253188 0E-7
4.344 0.9842060 9.580382 16.00000 1.000000 -0.003258032 -0.009815294 0E-7
4.375 1.015333 9.955478 17.00000 0.9999999 -0.002764255 -0.01117847 6.000000E-8
4.406 1.045763 10.32368 18.00000 1.000000 -0.001915881 -0.01080552 -1.200000E-7
4.436 1.075547 10.68553 19.00000 0.9999999 -0.0008882171 -0.009199130 1.200000E-7
4.465 1.104733 11.04151 20.00000 1.000000 0.0001931657 -0.006733598 -1.200000E-7
4.493 1.133360 11.39201 21.00000 0.9999999 0.001184593 -0.003892850 6.000000E-8
4.521 1.161464 11.73740 22.00000 0.9999999 0.001978412 -0.001097484 6.000000E-8
4.549 1.189095 12.07822 23.00000 0.9999999 0.002454009 0.001897130 1.200000E-7
4.576 1.216256 12.41446 24.00000 1.000000 0.002494043 0.003763331 0E-7
4.603 1.242985 12.74651 25.00000 1.000000 0.002011154 0.004552195 -1.200000E-7
4.629 1.269307 13.07462 26.00000 0.9999999 0.0009131892 0.003948445 6.000000E-8
4.655 1.295240 13.39900 27.00000 0.9999999 -0.0009105837 0.001461077 6.000000E-8
4.681 1.320804 13.71983 28.00000 0.9999999 -0.003536711 -0.003246460 6.000000E-8
4.680 1.320001 13.70973 29.00000 1.000000 -13.70973 -29.00000 -1.000000
4.700 1.340001 13.96144 30.00000 1.000000 -13.96144 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595609222">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.73494592799 2.16981353841 13.6198984465 3.07601836399 -1.63704097265</math:coefficients>
     <math:minRange>0.279</math:minRange>
     <math:maxRange>1.3208</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559560998">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.834252671852 9.04262500936 1.31656866677 0.578766785221 -0.336258423874</math:coefficients>
     <math:minRange>0.279</math:minRange>
     <math:maxRange>1.3208</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595609132">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.279</math:minRange>
     <math:maxRange>1.3208</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595609304">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3208</math:minRange>
     <math:maxRange>1.3208</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595609182">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3208</math:minRange>
     <math:maxRange>1.3208</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.7000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595609247">
   <gml:name>Gelesen aus: PROF0060.8000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553297.0266 5988827.8428 3.33</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595609234">
     <gml:description>Übernommen aus Datei: PROF0060.8000.txt</gml:description>
     <gml:name>0060.8000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595609297">
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
     <om:result>3.794 0.4642970 1.479820 1.000000 1.000000 -0.006146173 0.06851968 -1.200000E-7
3.919 0.5888460 2.361854 2.000000 1.000000 0.01277422 -0.07364584 -1.200000E-7
4.010 0.6795700 3.084947 3.000000 1.000000 0.004059501 -0.05177824 0E-7
4.082 0.7515980 3.690485 4.000000 1.000000 -0.004127992 -0.04741065 -1.200000E-7
4.146 0.8162410 4.248923 5.000000 0.9999999 -0.006180959 -0.02238959 6.000000E-8
4.206 0.8755020 4.773321 6.000000 0.9999999 -0.005240979 0.007333534 6.000000E-8
4.261 0.9306100 5.271673 7.000000 1.000000 -0.003066305 0.03425281 0E-7
4.312 0.9823580 5.749018 8.000000 0.9999998 -0.0006749074 0.05447899 1.800000E-7
4.361 1.031318 6.209011 9.000000 1.000000 0.001299104 0.06666419 -1.200000E-7
4.408 1.077917 6.654383 10.00000 0.9999999 0.002462933 0.07086411 6.000000E-8
4.452 1.122466 7.087044 11.00000 0.9999998 0.002598038 0.06755196 2.400000E-7
4.495 1.164811 7.504260 12.00000 0.9999999 0.001865563 0.04831305 6.000000E-8
4.535 1.205412 7.908568 13.00000 1.000000 0.001370287 0.01991064 0E-7
4.575 1.244730 8.303965 14.00000 1.000000 0.001071712 -0.008989904 0E-7
4.613 1.282893 8.691363 15.00000 1.000000 0.0008756144 -0.03662346 0E-7
4.650 1.320012 9.071610 16.00000 1.000000 0.0006772567 -0.06108634 -1.200000E-7
4.686 1.356189 9.445456 17.00000 0.9999999 0.0004574589 -0.08024561 1.200000E-7
4.721 1.391448 9.812919 18.00000 1.000000 0.0001811824 -0.09377617 -1.200000E-7
4.756 1.425885 10.17475 19.00000 0.9999999 -0.0001306840 -0.09913829 6.000000E-8
4.790 1.459557 10.53136 20.00000 1.000000 -0.0004668101 -0.09463203 0E-7
4.823 1.492511 10.88308 21.00000 1.000000 -0.0008157285 -0.07868268 -1.200000E-7
4.855 1.524815 11.23044 22.00000 0.9999999 -0.001123058 -0.04889398 1.200000E-7
4.886 1.556482 11.57344 23.00000 1.000000 -0.001339489 -0.004482476 -2.400000E-7
4.918 1.587559 11.91244 24.00000 0.9999998 -0.001422028 0.05650219 1.800000E-7
4.948 1.618079 12.24768 25.00000 0.9999999 -0.001309266 0.1356491 6.000000E-8
4.978 1.648069 12.57932 26.00000 0.9999999 -0.0009535983 0.2344037 1.200000E-7
5.008 1.677558 12.90758 27.00000 0.9999999 -0.0002832645 0.3543411 6.000000E-8
5.037 1.706583 13.23276 28.00000 0.9999999 0.0007643013 0.4974249 1.200000E-7
5.040 1.709999 13.27117 29.00000 0.9999999 0.0009208875 -0.3652656 6.000000E-8
5.060 1.729999 13.49666 30.00000 1.000000 0.001903184 -0.5491691 0E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595625188">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>6.92608988039 -34.4771355662 60.0592388277 -31.2828796196 7.18874519138</math:coefficients>
     <math:minRange>0.4643</math:minRange>
     <math:maxRange>1.73</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955956254">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.748581892235 2.29566316889 6.44266544488 -2.53653894677 0.460720671066</math:coefficients>
     <math:minRange>0.4643</math:minRange>
     <math:maxRange>1.73</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595625169">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4643</math:minRange>
     <math:maxRange>1.73</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595625226">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.73</math:minRange>
     <math:maxRange>1.73</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559562570">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.73</math:minRange>
     <math:maxRange>1.73</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.8000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595625257">
   <gml:name>Gelesen aus: PROF0060.9000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553379.3268 5988867.2554 3.14</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595625172">
     <gml:description>Übernommen aus Datei: PROF0060.9000.txt</gml:description>
     <gml:name>0060.9000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595625194">
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
     <om:result>3.613 0.4731040 1.521465 1.000000 1.000000 0.002583651 0.09261385 -1.200000E-7
3.740 0.5998660 2.399552 2.000000 0.9999998 -0.007635712 -0.1154405 2.400000E-7
3.837 0.6965110 3.156251 3.000000 0.9999999 0.00005278150 -0.1092628 6.000000E-8
3.917 0.7771670 3.845542 4.000000 0.9999999 0.006231851 -0.03986995 1.200000E-7
3.987 0.8474870 4.489393 5.000000 1.000000 0.005758037 0.04549758 -1.200000E-7
4.048 0.9075540 5.069565 6.000000 1.000000 -0.0007668267 0.07695997 0E-7
4.101 0.9607720 5.596890 7.000000 1.000000 -0.003296001 0.06683837 -1.200000E-7
4.151 1.010687 6.101790 8.000000 0.9999999 -0.003204212 0.05788698 1.200000E-7
4.198 1.057857 6.588081 9.000000 0.9999999 -0.001943728 0.04893906 1.200000E-7
4.243 1.102708 7.058712 10.00000 1.000000 -0.0004802967 0.03993179 -1.200000E-7
4.286 1.145546 7.515747 11.00000 1.000000 0.0005095400 0.03079542 -2.400000E-7
4.327 1.186625 7.960906 12.00000 1.000000 0.0006019737 0.02209976 0E-7
4.365 1.225483 8.387751 13.00000 0.9999999 -0.00001436691 -0.002455188 1.200000E-7
4.403 1.262758 8.801028 14.00000 1.000000 -0.0001141863 -0.03142068 0E-7
4.439 1.298929 9.205528 15.00000 1.000000 0.0001219797 -0.05509060 0E-7
4.474 1.334114 9.602280 16.00000 1.000000 0.0004678070 -0.07209537 0E-7
4.508 1.368387 9.991842 17.00000 1.000000 0.0007750011 -0.08184126 0E-7
4.542 1.401819 10.37480 18.00000 1.000000 0.0009124761 -0.08350388 -1.200000E-7
4.574 1.434472 10.75165 19.00000 1.000000 0.0007945164 -0.07624983 -1.200000E-7
4.606 1.466298 11.12162 20.00000 1.000000 0.0003958831 -0.06256240 0E-7
4.637 1.497061 11.48141 21.00000 0.9999999 0.000006330396 -0.05142098 1.200000E-7
4.667 1.527252 11.83647 22.00000 0.9999999 -0.0002806584 -0.02780288 6.000000E-8
4.697 1.556859 12.18653 23.00000 0.9999999 -0.0004732067 0.007469556 1.200000E-7
4.726 1.585942 12.53219 24.00000 1.000000 -0.0005747899 0.05612322 0E-7
4.755 1.614530 12.87372 25.00000 1.000000 -0.0005893272 0.1189413 0E-7
4.783 1.642646 13.21130 26.00000 0.9999999 -0.0005033849 0.1965653 6.000000E-8
4.810 1.670346 13.54551 27.00000 0.9999999 -0.0002828075 0.2910256 1.200000E-7
4.838 1.697602 13.87594 28.00000 1.000000 0.00009053121 0.4011082 -1.200000E-7
4.850 1.709999 14.02676 29.00000 0.9999998 0.0003189988 -0.08271275 1.800000E-7
4.860 1.719999 14.14866 30.00000 0.9999999 0.0005381467 -0.6610669 6.000000E-8
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595640221">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>5.84544213112 -27.3288048946 45.2956732395 -20.8211716368 4.84953999843</math:coefficients>
     <math:minRange>0.4731</math:minRange>
     <math:maxRange>1.72</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559564096">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.213421945227 -1.40565129723 10.6166235637 -4.09644224787 0.661532620983</math:coefficients>
     <math:minRange>0.4731</math:minRange>
     <math:maxRange>1.72</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595640151">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4731</math:minRange>
     <math:maxRange>1.72</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11783955956407">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.72</math:minRange>
     <math:maxRange>1.72</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595640336">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.72</math:minRange>
     <math:maxRange>1.72</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.9000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation117839559564013">
   <gml:name>Gelesen aus: PROF0061.0000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553406.3291 5988962.2023 3.58</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595640314">
     <gml:description>Übernommen aus Datei: PROF0061.0000.txt</gml:description>
     <gml:name>0061.0000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595640178">
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
     <om:result>3.788 0.2082200 1.781730 1.000000 0.9999999 -0.03097222 0.1629523 6.000000E-8
3.882 0.3017500 2.713550 2.000000 1.000000 0.03215613 -0.1918867 0E-7
3.956 0.3755270 3.498325 3.000000 0.9999999 0.03670412 -0.1588034 1.200000E-7
4.016 0.4361230 4.173116 4.000000 0.9999999 0.01554944 -0.06314280 6.000000E-8
4.068 0.4880070 4.756109 5.000000 1.000000 -0.002708535 0.004200529 0E-7
4.116 0.5360130 5.296636 6.000000 1.000000 -0.01573666 0.06983728 0E-7
4.161 0.5810510 5.804712 7.000000 0.9999998 -0.02399926 0.1196805 2.400000E-7
4.204 0.6237050 6.286764 8.000000 0.9999998 -0.02785317 0.1465888 2.400000E-7
4.244 0.6644020 6.747485 9.000000 1.000000 -0.02757929 0.1481065 -1.200000E-7
4.283 0.7034400 7.190142 10.00000 1.000000 -0.02341230 0.1239105 0E-7
4.321 0.7410500 7.617284 11.00000 0.9999999 -0.01557494 0.07548640 6.000000E-8
4.357 0.7774290 8.031061 12.00000 0.9999999 -0.004230729 0.005861502 6.000000E-8
4.393 0.8127070 8.432901 13.00000 0.9999999 0.01043980 -0.08194637 1.200000E-7
4.427 0.8470070 8.824153 14.00000 1.000000 0.02828373 -0.1838699 -2.400000E-7
4.460 0.8804240 9.205863 15.00000 0.9999999 0.04914764 -0.2956491 1.200000E-7
4.493 0.9130590 9.579139 16.00000 1.000000 0.07291607 -0.4121510 0E-7
4.554 0.9739330 10.40624 17.00000 1.000000 -0.003109610 0.2934723 -1.200000E-7
4.584 1.004485 10.79342 18.00000 1.000000 -0.008142413 0.1868546 -1.200000E-7
4.614 1.034352 11.17401 19.00000 0.9999999 -0.01175131 0.09115190 1.200000E-7
4.644 1.063593 11.54858 20.00000 1.000000 -0.01399943 0.01171617 0E-7
4.672 1.092241 11.91745 21.00000 1.000000 -0.01497127 -0.04679449 -1.200000E-7
4.700 1.120338 12.28104 22.00000 1.000000 -0.01474601 -0.07937150 -1.200000E-7
4.728 1.147948 12.64009 23.00000 0.9999999 -0.01341300 -0.08009651 6.000000E-8
4.755 1.175043 12.99414 24.00000 1.000000 -0.01103681 -0.04636089 -1.200000E-7
4.782 1.201680 13.34383 25.00000 1.000000 -0.007695336 0.02751078 -1.200000E-7
4.808 1.227881 13.68938 26.00000 0.9999999 -0.003473431 0.1459368 6.000000E-8
4.834 1.253671 14.03105 27.00000 0.9999999 0.001565808 0.3134609 6.000000E-8
4.859 1.279068 14.36899 28.00000 1.000000 0.007346682 0.5343141 0E-7
4.860 1.280001 14.38143 29.00000 0.9999998 0.007579203 -0.4194058 1.800000E-7
4.880 1.300002 14.64864 30.00000 0.9999998 0.01271712 -0.4015635 1.800000E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595640345">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>3.98196986067 -32.6308664557 110.263627559 -95.9296354822 32.368743146</math:coefficients>
     <math:minRange>0.2082</math:minRange>
     <math:maxRange>1.3</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559564083">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.47592088472 10.8351770558 -1.04629580185 1.85780225157 -0.441801097816</math:coefficients>
     <math:minRange>0.2082</math:minRange>
     <math:maxRange>1.3</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559564011">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.2082</math:minRange>
     <math:maxRange>1.3</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595640109">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3</math:minRange>
     <math:maxRange>1.3</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595640131">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3</math:minRange>
     <math:maxRange>1.3</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>61.0000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595640120">
   <gml:name>Gelesen aus: PROF0061.1000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553451.2559 5989056.3129 3.45</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595640275">
     <gml:description>Übernommen aus Datei: PROF0061.1000.txt</gml:description>
     <gml:name>0061.1000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition117839559564015">
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
     <om:result>3.685 0.2346350 1.793473 1.000000 1.000000 -0.001568547 -0.008222103 0E-7
3.775 0.3247550 2.703345 2.000000 0.9999999 0.004258906 0.01386886 6.000000E-8
3.844 0.3943330 3.432983 3.000000 0.9999999 -0.0004369534 0.007287281 1.200000E-7
3.904 0.4542890 4.070524 4.000000 1.000000 -0.001986221 -0.002092433 -2.400000E-7
3.958 0.5083720 4.652310 5.000000 1.000000 -0.001869441 -0.006759934 -2.400000E-7
4.008 0.5581990 5.193937 6.000000 1.000000 -0.001024532 -0.007973684 -2.400000E-7
4.055 0.6047530 5.704871 7.000000 0.9999999 0.000001568931 -0.006666782 6.000000E-8
4.099 0.6486720 6.191196 8.000000 0.9999999 0.0008956176 -0.003854765 6.000000E-8
4.140 0.6904180 6.657335 9.000000 0.9999999 0.001447094 -0.00003215767 6.000000E-8
4.180 0.7303150 7.106385 10.00000 1.000000 0.001487988 0.004015235 0E-7
4.219 0.7686200 7.540759 11.00000 1.000000 0.0009568772 0.007900531 -1.200000E-7
4.255 0.8053550 7.960045 12.00000 0.9999999 0.00006744680 0.006359799 1.200000E-7
4.291 0.8408430 8.366839 13.00000 1.000000 -0.0004621976 0.003288656 0E-7
4.325 0.8753100 8.763515 14.00000 0.9999999 -0.0006978008 0.001248692 6.000000E-8
4.359 0.9088390 9.150880 15.00000 1.000000 -0.0007177896 -0.0004917139 0E-7
4.392 0.9415440 9.530158 16.00000 0.9999999 -0.0006185913 -0.001232944 1.200000E-7
4.423 0.9734780 9.901850 17.00000 1.000000 -0.0004461926 -0.001690906 -2.400000E-7
4.455 1.004708 10.26664 18.00000 1.000000 -0.0002405165 -0.001833063 0E-7
4.485 1.035287 10.62506 19.00000 1.000000 -0.00003940158 -0.001782555 -1.200000E-7
4.515 1.065262 10.97760 20.00000 1.000000 0.0001344033 -0.001620687 0E-7
4.545 1.094683 11.32477 21.00000 1.000000 0.0002615463 -0.001116941 -1.200000E-7
4.574 1.123575 11.66679 22.00000 0.9999999 0.0003339662 -0.0007142685 6.000000E-8
4.602 1.151975 12.00407 23.00000 1.000000 0.0003292088 -0.0003037443 -1.200000E-7
4.630 1.179912 12.33689 24.00000 1.000000 0.0002539647 0.00007892249 0E-7
4.657 1.207412 12.66549 25.00000 1.000000 0.0001186291 0.0004042784 0E-7
4.685 1.234503 12.99017 26.00000 0.9999998 -0.00008194396 0.0008191725 1.800000E-7
4.711 1.261201 13.31110 27.00000 1.000000 -0.0003570877 0.001117252 -2.400000E-7
4.412 0.9616430 9.763935 28.00000 1.000000 -9.763935 -28.00000 -1.000000
4.750 1.300001 13.79633 29.00000 1.000000 -13.79633 -29.00000 -1.000000
4.780 1.330001 14.17053 30.00000 1.000000 -14.17053 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559565644">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.109284542083 -0.54572309693 23.7438382869 -6.28980932406 1.04701569513</math:coefficients>
     <math:minRange>0.2346</math:minRange>
     <math:maxRange>1.2612</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559565619">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.443869457299 9.00174775032 2.47895705365 -1.04429266655 0.218749239328</math:coefficients>
     <math:minRange>0.2346</math:minRange>
     <math:maxRange>1.2612</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595656209">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.2346</math:minRange>
     <math:maxRange>1.2612</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559565625">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.2612</math:minRange>
     <math:maxRange>1.2612</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559565694">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.2612</math:minRange>
     <math:maxRange>1.2612</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>61.1000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595656132">
   <gml:name>Gelesen aus: PROF0061.1270.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553489.8333 5989054.3354 2.74</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595656139">
     <gml:description>Übernommen aus Datei: PROF0061.1270.txt</gml:description>
     <gml:name>0061.1270</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595656288">
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
     <om:result>3.250 0.5099950 1.478640 1.000000 0.9999998 0.0003130984 -0.06545977 1.800000E-7
3.386 0.6461310 2.360895 2.000000 1.000000 0.002903493 0.1085907 0E-7
3.484 0.7439840 3.069808 3.000000 0.9999999 -0.003947794 0.05398792 1.200000E-7
3.570 0.8296740 3.738037 4.000000 1.000000 -0.002234000 0.003807996 -1.200000E-7
3.646 0.9058200 4.380277 5.000000 1.000000 -0.001627616 -0.02775162 0E-7
3.713 0.9733390 4.988999 6.000000 0.9999998 -0.001030855 -0.05353852 1.800000E-7
3.774 1.034410 5.571348 7.000000 1.000000 0.0002364755 -0.06777438 -1.200000E-7
3.830 1.090431 6.132076 8.000000 1.000000 0.002026516 -0.06890085 -1.200000E-7
3.882 1.142367 6.674590 9.000000 0.9999998 0.003909861 -0.05707335 2.400000E-7
3.931 1.190915 7.201458 10.00000 1.000000 0.005340658 -0.03322351 -1.200000E-7
3.977 1.236599 7.714647 11.00000 1.000000 0.005806727 0.001434319 0E-7
4.020 1.279828 8.215807 12.00000 0.9999999 0.004786882 0.04569907 6.000000E-8
4.061 1.320913 8.706128 13.00000 0.9999998 0.001810830 0.09805302 1.800000E-7
4.100 1.360106 9.186588 14.00000 1.000000 -0.003530617 0.1570677 -1.200000E-7
4.133 1.393263 9.599968 15.00000 1.000000 -0.007723420 0.09538140 0E-7
4.165 1.425493 10.00501 16.00000 1.000000 -0.009150892 0.04489201 -1.200000E-7
4.197 1.456871 10.40237 17.00000 0.9999999 -0.008478912 0.004824728 6.000000E-8
4.227 1.487475 10.79281 18.00000 1.000000 -0.006343297 -0.02517135 -2.400000E-7
4.257 1.517350 11.17670 19.00000 1.000000 -0.003331757 -0.04620718 -1.200000E-7
4.287 1.546554 11.55461 20.00000 0.9999999 0.00003964894 -0.05875032 1.200000E-7
4.315 1.575133 11.92692 21.00000 1.000000 0.003305402 -0.06344218 0E-7
4.343 1.603124 12.29399 22.00000 1.000000 0.005985382 -0.06105245 0E-7
4.371 1.630575 12.65630 23.00000 1.000000 0.007675242 -0.05178017 0E-7
4.398 1.657507 13.01398 24.00000 0.9999998 0.008002266 -0.03665550 1.800000E-7
4.424 1.683952 13.36734 25.00000 1.000000 0.006577269 -0.01618239 -1.200000E-7
4.450 1.709937 13.71664 26.00000 1.000000 0.003059806 0.009070372 0E-7
4.475 1.735485 14.06205 27.00000 1.000000 -0.002865297 0.03848552 -1.200000E-7
4.501 1.760622 14.40386 28.00000 0.9999999 -0.01151510 0.07166881 6.000000E-8
4.500 1.760000 14.39537 29.00000 0.9999999 -14.39537 -29.00000 -0.9999999
4.520 1.780000 14.66866 30.00000 1.000000 -14.66866 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595671163">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-5.2490814657 20.0954283639 -24.8226419671 19.7835034992 -3.44316727918</math:coefficients>
     <math:minRange>0.51</math:minRange>
     <math:maxRange>1.7606</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595671125">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-1.66936742909 7.33954545316 -4.98838005309 6.06718663831 -1.51004315603</math:coefficients>
     <math:minRange>0.51</math:minRange>
     <math:maxRange>1.7606</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559567130">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.51</math:minRange>
     <math:maxRange>1.7606</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595671231">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7606</math:minRange>
     <math:maxRange>1.7606</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559567126">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7606</math:minRange>
     <math:maxRange>1.7606</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>61.1270</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation11783955956716">
   <gml:name>Gelesen aus: PROF0061.1390.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553501.9134 5989046.718 3.62</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595671329">
     <gml:description>Übernommen aus Datei: PROF0061.1390.txt</gml:description>
     <gml:name>0061.1390</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595671149">
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
     <om:result>4.063 0.4434510 1.649153 1.000000 0.9999999 -0.001750762 0.06404751 6.000000E-8
4.186 0.5663700 2.689917 2.000000 0.9999999 0.007420086 -0.03949210 6.000000E-8
4.265 0.6449000 3.448031 3.000000 1.000000 -0.004760906 -0.09036048 -1.200000E-7
4.332 0.7123980 4.125875 4.000000 1.000000 -0.004340475 -0.07650684 -1.200000E-7
4.393 0.7726770 4.751721 5.000000 1.000000 -0.0004766062 -0.03445238 -1.200000E-7
4.448 0.8277160 5.340047 6.000000 0.9999999 0.002728379 0.01852473 1.200000E-7
4.499 0.8786980 5.899389 7.000000 1.000000 0.003255712 0.07323898 0E-7
4.545 0.9253860 6.423378 8.000000 0.9999999 0.0006046508 0.1016267 6.000000E-8
4.588 0.9677830 6.904679 9.000000 0.9999999 -0.001074780 0.08463347 1.200000E-7
4.628 1.008302 7.368281 10.00000 0.9999999 -0.001443962 0.06619384 1.200000E-7
4.667 1.047192 7.816576 11.00000 0.9999999 -0.001137280 0.04604643 6.000000E-8
4.705 1.084683 8.251823 12.00000 1.000000 -0.0005567874 0.02520092 0E-7
4.741 1.120915 8.675332 13.00000 1.000000 0.00002933685 0.003618567 0E-7
4.776 1.156027 9.088452 14.00000 0.9999998 0.0004667957 -0.01785211 1.800000E-7
4.810 1.190128 9.492214 15.00000 0.9999999 0.0006941395 -0.03845090 1.200000E-7
4.843 1.223309 9.887489 16.00000 1.000000 0.0006938971 -0.05736289 -1.200000E-7
4.876 1.255658 10.27512 17.00000 1.000000 0.0005145329 -0.07339504 -1.200000E-7
4.907 1.287231 10.65564 18.00000 1.000000 0.0001986057 -0.08595313 0E-7
4.938 1.318095 11.02969 19.00000 1.000000 -0.0001390356 -0.09381110 -1.200000E-7
4.968 1.348294 11.39767 20.00000 0.9999998 -0.0003937917 -0.09622452 1.800000E-7
4.998 1.377875 11.76002 21.00000 0.9999998 -0.0004342451 -0.09215601 2.400000E-7
5.027 1.406881 12.11715 22.00000 0.9999998 -0.0001016856 -0.08052141 1.800000E-7
5.057 1.436616 12.48566 23.00000 0.9999999 0.0002727840 -0.01423112 1.200000E-7
5.086 1.465924 12.85198 24.00000 0.9999999 0.0001693903 0.06720984 1.200000E-7
5.115 1.494655 13.21409 25.00000 0.9999999 -0.0001632133 0.1582815 6.000000E-8
5.143 1.522848 13.57231 26.00000 1.000000 -0.0004875182 0.2602772 0E-7
5.171 1.550531 13.92683 27.00000 1.000000 -0.0005430479 0.3741735 0E-7
5.198 1.577766 14.27831 28.00000 1.000000 -0.00008991266 0.5024591 0E-7
5.200 1.580002 14.30728 29.00000 0.9999998 -0.00001209121 -0.4034285 1.800000E-7
5.220 1.600002 14.56726 30.00000 1.000000 0.0008577899 -0.5513338 -1.200000E-7
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595671268">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>6.85477117997 -34.8732819743 61.3634202542 -30.5988378503 7.11571411347</math:coefficients>
     <math:minRange>0.4435</math:minRange>
     <math:maxRange>1.6</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595671288">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.178675853328 -0.784111206356 13.975932891 -7.26358289144 1.52200145166</math:coefficients>
     <math:minRange>0.4435</math:minRange>
     <math:maxRange>1.6</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595671155">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4435</math:minRange>
     <math:maxRange>1.6</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595671310">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6</math:minRange>
     <math:maxRange>1.6</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595671175">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6</math:minRange>
     <math:maxRange>1.6</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>61.1390</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1178395595671191">
   <gml:name>Gelesen aus: PROF0061.2000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553560.5597 5989023.3662 4.21</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1178395595671208">
     <gml:description>Übernommen aus Datei: PROF0061.2000.txt</gml:description>
     <gml:name>0061.2000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1178395595671330">
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
     <om:result>4.534 0.3235660 1.860547 1.000000 0.9999998 0.001291310 -0.0007675631 2.400000E-7
4.626 0.4155210 2.778060 2.000000 1.000000 -0.001361086 0.002654613 -1.200000E-7
4.699 0.4887390 3.535979 3.000000 1.000000 -0.001481766 0.0001882531 -1.200000E-7
4.762 0.5518940 4.209218 4.000000 1.000000 -0.0007605089 -0.001800334 0E-7
4.818 0.6084420 4.827351 5.000000 0.9999999 0.0001498514 -0.002443247 6.000000E-8
4.870 0.6601950 5.405742 6.000000 1.000000 0.0009544860 -0.001896526 0E-7
4.918 0.7082460 5.953621 7.000000 1.000000 0.001459934 -0.0005966359 0E-7
4.963 0.7533260 6.477111 8.000000 1.000000 0.001589313 0.001192946 0E-7
5.006 0.7959390 6.980419 9.000000 1.000000 0.001257235 0.002981870 0E-7
5.046 0.8364620 7.466665 10.00000 0.9999999 0.0004378869 0.004484783 1.200000E-7
5.085 0.8750520 7.936209 11.00000 0.9999999 -0.0004686540 0.002014506 6.000000E-8
5.122 0.9121170 8.392605 12.00000 0.9999999 -0.0009720823 -0.00003104617 6.000000E-8
5.158 0.9478500 8.837609 13.00000 0.9999999 -0.001172486 -0.001299123 6.000000E-8
5.192 0.9823870 9.272398 14.00000 1.000000 -0.001160683 -0.002082004 -1.200000E-7
5.226 1.015863 9.698203 15.00000 0.9999999 -0.0009872567 -0.001976437 6.000000E-8
5.258 1.048353 10.11561 16.00000 0.9999999 -0.0007348617 -0.001792199 6.000000E-8
5.290 1.079949 10.52543 17.00000 0.9999998 -0.0004290725 -0.001392675 2.400000E-7
5.321 1.110722 10.92825 18.00000 1.000000 -0.0001029564 -0.0009046486 -1.200000E-7
5.351 1.140732 11.32462 19.00000 1.000000 0.0001948803 -0.0004965567 0E-7
5.380 1.170052 11.71520 20.00000 1.000000 0.0004599978 0.0003847944 0E-7
5.409 1.198708 12.10016 21.00000 1.000000 0.0006497779 0.0009289069 -1.200000E-7
5.437 1.226747 12.47988 22.00000 1.000000 0.0007546676 0.001218798 0E-7
5.464 1.254209 12.85473 23.00000 1.000000 0.0007620616 0.001270318 -1.200000E-7
5.491 1.281126 13.22496 24.00000 1.000000 0.0006513487 0.0009590596 0E-7
5.518 1.307549 13.59110 25.00000 0.9999999 0.0004309073 0.001015248 6.000000E-8
5.543 1.333479 13.95303 26.00000 0.9999999 0.00006963503 0.0003994096 6.000000E-8
5.569 1.358952 14.31111 27.00000 1.000000 -0.0004253871 -0.0005119775 0E-7
5.594 1.383992 14.66552 28.00000 1.000000 -0.001056493 -0.001702532 -1.200000E-7
5.590 1.380002 14.60889 29.00000 1.000000 -14.60889 -29.00000 -1.000000
5.620 1.410003 15.03625 30.00000 1.000000 -15.03625 -30.00000 -1.000000
</om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559568749">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.178724100535 -2.34984987429 19.3662395449 -2.78678229207 0.469366085384</math:coefficients>
     <math:minRange>0.3236</math:minRange>
     <math:maxRange>1.384</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D117839559568743">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-1.00956099061 7.99033857096 2.85108054352 -0.384116653065 0.0470657225242</math:coefficients>
     <math:minRange>0.3236</math:minRange>
     <math:maxRange>1.384</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595687142">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3236</math:minRange>
     <math:maxRange>1.384</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595687374">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.384</math:minRange>
     <math:maxRange>1.384</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1178395595687173">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.384</math:minRange>
     <math:maxRange>1.384</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>61.2000</wb1d2d:station>
   <wb1d2d:slope>0.00300</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
</simBase:FlowRelationshipModel>
