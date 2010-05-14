<?xml version="1.0" encoding="WINDOWS-1252"?>
<simBase:FlowRelationshipModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:swe="http://www.opengis.net/swe" xmlns:ns8="http://www.tu-harburg.de/wb/kalypso/schemata/observation" xmlns:math="org.kalypso.gml.common.math" xmlns:om="http://www.opengis.net/om" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952769553">
   <gml:name>Gelesen aus: PROF0058.8530.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552876.7786 5987232.8608 2.75</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952769538">
     <gml:description>Übernommen aus Datei: PROF0058.8530.txt</gml:description>
     <gml:name>0058.8530</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952769518">
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
     <om:result><![CDATA[3.186 0.4364550 2.130137 1.000000 1.000000 -0.001929754 0.01654743 0E-7
3.350 0.5995150 3.437403 2.000000 1.000000 0.002069790 -0.05729712 0E-7
3.470 0.7203710 4.571985 3.000000 1.000000 0.01060733 0.02030121 0E-7
3.563 0.8125160 5.521488 4.000000 1.000000 -0.005753025 0.03898030 0E-7
3.640 0.8897010 6.333106 5.000000 1.000000 -0.009106464 0.01046490 0E-7
3.711 0.9611070 7.091455 6.000000 1.000000 -0.005481057 -0.003271806 0E-7
3.778 1.028089 7.809345 7.000000 1.000000 -0.0004176271 -0.008020394 -1.200000E-7
3.841 1.091480 8.494576 8.000000 0.9999999 0.003469059 -0.008766754 6.000000E-8
3.902 1.151921 9.153209 9.000000 0.9999999 0.005242612 -0.008323208 1.200000E-7
3.960 1.209838 9.789166 10.00000 0.9999998 0.005003719 -0.009571166 1.800000E-7
4.016 1.265600 10.40594 11.00000 0.9999999 0.003460650 -0.01399518 6.000000E-8
4.070 1.320209 11.01443 12.00000 0.9999998 0.001512100 -0.009435535 1.800000E-7
4.124 1.374228 11.62318 13.00000 1.000000 -0.001803052 0.01052949 0E-7
4.177 1.426524 12.21963 14.00000 1.000000 -0.005073740 0.02056930 -1.200000E-7
4.227 1.477211 12.80439 15.00000 1.000000 -0.006125824 0.01874336 -1.200000E-7
4.276 1.526426 13.37845 16.00000 1.000000 -0.002788848 0.004242596 -1.200000E-7
4.324 1.574377 13.94370 17.00000 1.000000 0.007114132 -0.02169742 -1.200000E-7
4.270 1.520001 13.30315 18.00000 0.9999999 -13.30315 -18.00000 -0.9999999
4.310 1.560001 13.77362 19.00000 0.9999998 -13.77362 -19.00000 -0.9999998
4.350 1.600002 14.24820 20.00000 0.9999999 -14.24820 -20.00000 -0.9999999
4.460 1.710000 15.58857 21.00000 1.000000 -15.58857 -21.00000 -1.000000
4.460 1.710000 15.58857 22.00000 1.000000 -15.58857 -22.00000 -1.000000
4.614 1.864130 17.57663 23.00000 1.000000 -17.57663 -23.00000 -1.000000
4.530 1.780001 16.47556 24.00000 0.9999999 -16.47556 -24.00000 -0.9999999
4.570 1.820001 16.99430 25.00000 1.000000 -16.99430 -25.00000 -1.000000
4.610 1.860002 17.52171 26.00000 0.9999999 -17.52171 -26.00000 -0.9999999
4.710 1.960000 18.87803 27.00000 1.000000 -18.87803 -27.00000 -1.000000
4.710 1.960000 18.87803 28.00000 1.000000 -18.87803 -28.00000 -1.000000
4.720 1.970000 19.01664 29.00000 1.000000 -19.01664 -29.00000 -1.000000
4.760 2.010001 19.57651 30.00000 1.000000 -19.57651 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952771048">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>2.25957246495 -10.1542596709 18.670879219 -4.59016577618 0.380707565269</math:coefficients>
     <math:minRange>0.4365</math:minRange>
     <math:maxRange>1.5744</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952771023">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>1.2355057271 -4.61115627441 19.7266231372 -11.3441670651 2.49818461063</math:coefficients>
     <math:minRange>0.4365</math:minRange>
     <math:maxRange>1.5744</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952771027">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4365</math:minRange>
     <math:maxRange>1.5744</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952771040">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5744</math:minRange>
     <math:maxRange>1.5744</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952771019">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5744</math:minRange>
     <math:maxRange>1.5744</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>58.8530</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile11828595121635"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952771057">
   <gml:name>Gelesen aus: PROF0058.9000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552880.6369 5987277.4016 2.72</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952771055">
     <gml:description>Übernommen aus Datei: PROF0058.9000.txt</gml:description>
     <gml:name>0058.9000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952771068">
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
     <om:result><![CDATA[3.107 0.3871480 2.342753 1.000000 0.9999999 -0.0005377290 0.008090159 6.000000E-8
3.241 0.5209110 3.611873 2.000000 1.000000 -0.0007849034 -0.02336035 -1.200000E-7
3.346 0.6256530 4.689032 3.000000 0.9999999 0.005216587 -0.0004987320 1.200000E-7
3.434 0.7136710 5.650088 4.000000 1.000000 -0.00002877856 0.02481423 -1.200000E-7
3.508 0.7884560 6.491925 5.000000 1.000000 -0.004225803 0.007480596 0E-7
3.577 0.8572690 7.278601 6.000000 0.9999999 -0.003440927 -0.002667393 6.000000E-8
3.642 0.9215740 8.024166 7.000000 1.000000 -0.001059297 -0.006947627 0E-7
3.702 0.9822550 8.736961 8.000000 1.000000 0.001187232 -0.007368490 0E-7
3.760 1.039915 9.422564 9.000000 1.000000 0.002541597 -0.005702532 -2.400000E-7
3.815 1.095005 10.08518 10.00000 1.000000 0.002759543 -0.003172644 0E-7
3.868 1.147903 10.72838 11.00000 1.000000 0.001962692 -0.00005246863 0E-7
3.919 1.198872 11.35458 12.00000 1.000000 0.0004990446 0.002876691 0E-7
3.968 1.248088 11.96525 13.00000 0.9999999 -0.001146647 0.004274610 1.200000E-7
4.016 1.295748 12.56222 14.00000 0.9999998 -0.002368731 0.004109904 1.800000E-7
4.062 1.342055 13.14756 15.00000 1.000000 -0.002558385 0.003286692 0E-7
4.107 1.387059 13.72142 16.00000 0.9999999 -0.0009980868 0.000001626443 6.000000E-8
4.151 1.430900 14.28521 17.00000 1.000000 0.002982592 -0.005164276 0E-7
4.100 1.380002 13.63112 18.00000 0.9999999 -13.63112 -18.00000 -0.9999999
4.180 1.460000 14.66201 19.00000 1.000000 -14.66201 -19.00000 -1.000000
4.180 1.460000 14.66201 20.00000 0.9999998 -14.66201 -20.00000 -0.9999998
4.220 1.500001 15.18333 21.00000 1.000000 -15.18333 -21.00000 -1.000000
4.250 1.530002 15.57688 22.00000 1.000000 -15.57688 -22.00000 -1.000000
4.394 1.673559 17.49038 23.00000 1.000000 -17.49038 -23.00000 -1.000000
4.320 1.600002 16.50365 24.00000 1.000000 -16.50365 -24.00000 -1.000000
4.360 1.640003 17.03861 25.00000 0.9999998 -17.03861 -25.00000 -0.9999998
4.430 1.710000 17.98410 26.00000 1.000000 -17.98410 -26.00000 -1.000000
4.430 1.710000 17.98410 27.00000 1.000000 -17.98410 -27.00000 -1.000000
4.460 1.740001 18.39299 28.00000 1.000000 -18.39299 -28.00000 -1.000000
4.490 1.770001 18.80407 29.00000 1.000000 -18.80407 -29.00000 -1.000000
4.520 1.800002 19.21734 30.00000 1.000000 -19.21734 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952772657">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.701322827054 -4.6436223473 15.3808986232 -3.66120643595 0.518211561795</math:coefficients>
     <math:minRange>0.3871</math:minRange>
     <math:maxRange>1.4309</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952772656">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.137914332793 3.19627465176 10.3322875699 -5.79706804716 1.35522628157</math:coefficients>
     <math:minRange>0.3871</math:minRange>
     <math:maxRange>1.4309</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11828595277269">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3871</math:minRange>
     <math:maxRange>1.4309</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11828595277265">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4309</math:minRange>
     <math:maxRange>1.4309</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952772669">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4309</math:minRange>
     <math:maxRange>1.4309</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>58.9000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile11828595123044"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952772668">
   <gml:name>Gelesen aus: PROF0058.9510.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552869.1786 5987324.5788 2.55</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952772649">
     <gml:description>Übernommen aus Datei: PROF0058.9510.txt</gml:description>
     <gml:name>0058.9510</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952772631">
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
     <om:result><![CDATA[3.004 0.4544350 2.181082 1.000000 0.9999999 -0.002584818 -0.0007457348 6.000000E-8
3.156 0.6058140 3.420682 2.000000 1.000000 0.007507824 -0.003740989 -1.200000E-7
3.271 0.7209690 4.470728 3.000000 1.000000 -0.0002778845 0.01565765 0E-7
3.364 0.8140710 5.366122 4.000000 0.9999998 -0.005296675 -0.003193924 1.800000E-7
3.448 0.8975260 6.194344 5.000000 1.000000 -0.004112393 -0.009369229 -2.400000E-7
3.524 0.9739360 6.973876 6.000000 1.000000 -0.001117485 -0.008258347 0E-7
3.595 1.044906 7.716088 7.000000 1.000000 0.001635804 -0.003200993 0E-7
3.661 1.111461 8.428038 8.000000 1.000000 0.003237810 0.002814185 0E-7
3.724 1.174375 9.115185 9.000000 0.9999999 0.003405772 0.008313950 1.200000E-7
3.784 1.234174 9.781070 10.00000 1.000000 0.002165498 0.01145098 -1.200000E-7
3.841 1.291271 10.42846 11.00000 1.000000 -0.0002218492 0.01091589 -1.200000E-7
3.895 1.345059 11.04722 12.00000 1.000000 -0.001881685 -0.01158437 -1.200000E-7
3.948 1.398101 11.66491 13.00000 1.000000 -0.001679465 -0.01464280 0E-7
4.000 1.450299 12.28231 14.00000 1.000000 -0.001528582 -0.002676513 0E-7
4.051 1.500796 12.88890 15.00000 1.000000 -0.001212424 0.004444081 -1.200000E-7
4.100 1.549687 13.48492 16.00000 1.000000 -0.0001765529 0.004950704 -1.200000E-7
4.147 1.597130 14.07150 17.00000 1.000000 0.002137106 -0.001134543 -1.200000E-7
4.090 1.540001 13.36615 18.00000 1.000000 -13.36615 -18.00000 -1.000000
4.140 1.590002 13.98286 19.00000 0.9999998 -13.98286 -19.00000 -0.9999998
4.180 1.630003 14.48268 20.00000 1.000000 -14.48268 -20.00000 -1.000000
4.260 1.710000 15.49953 21.00000 0.9999999 -15.49953 -21.00000 -0.9999999
4.310 1.760000 16.14675 22.00000 1.000000 -16.14675 -22.00000 -1.000000
4.407 1.856929 17.42703 23.00000 1.000000 -17.42703 -23.00000 -1.000000
4.330 1.780000 16.40816 24.00000 1.000000 -16.40816 -24.00000 -1.000000
4.370 1.820001 16.93530 25.00000 0.9999999 -16.93530 -25.00000 -0.9999999
4.400 1.850002 17.33442 26.00000 1.000000 -17.33442 -26.00000 -1.000000
4.510 1.960000 18.82782 27.00000 0.9999999 -18.82782 -27.00000 -0.9999999
4.510 1.960000 18.82782 28.00000 0.9999999 -18.82782 -28.00000 -0.9999999
4.560 2.009999 19.52703 29.00000 1.000000 -19.52703 -29.00000 -1.000000
4.560 2.009999 19.52703 30.00000 0.9999999 -19.52703 -30.00000 -0.9999999
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952774173">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.564293666114 -3.55756102243 10.4047143363 -1.01352403721 -0.045344533046</math:coefficients>
     <math:minRange>0.4544</math:minRange>
     <math:maxRange>1.5971</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952774111">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.352771083442 2.86865036093 7.26557394368 -3.21433919435 0.677272164884</math:coefficients>
     <math:minRange>0.4544</math:minRange>
     <math:maxRange>1.5971</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952774152">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4544</math:minRange>
     <math:maxRange>1.5971</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952774176">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5971</math:minRange>
     <math:maxRange>1.5971</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952774171">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5971</math:minRange>
     <math:maxRange>1.5971</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>58.9510</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951233510"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952774183">
   <gml:name>Gelesen aus: PROF0059.0000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552853.1587 5987371.3982 2.85</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952774181">
     <gml:description>Übernommen aus Datei: PROF0059.0000.txt</gml:description>
     <gml:name>0059.0000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952774141">
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
     <om:result><![CDATA[3.232 0.3818230 2.498245 1.000000 1.000000 -0.0003323167 0.0004147724 -2.400000E-7
3.354 0.5038090 3.864117 2.000000 1.000000 0.000001357979 0.0002922523 0E-7
3.445 0.5954220 4.947551 3.000000 0.9999998 0.001159314 -0.007759309 1.800000E-7
3.525 0.6747480 5.914107 4.000000 1.000000 0.001924609 0.007272523 0E-7
3.594 0.7443150 6.781572 5.000000 1.000000 -0.001968496 0.009523718 0E-7
3.657 0.8070370 7.569452 6.000000 1.000000 -0.002580127 -0.002292261 0E-7
3.716 0.8658670 8.311837 7.000000 1.000000 -0.001236282 -0.006777791 -1.200000E-7
3.772 0.9215630 9.017718 8.000000 1.000000 0.0004031904 -0.006651962 0E-7
3.825 0.9747150 9.694109 9.000000 1.000000 0.001539167 -0.003086725 0E-7
3.876 1.025664 10.34499 10.00000 1.000000 0.001868784 0.001085209 0E-7
3.925 1.074747 10.97437 11.00000 0.9999998 0.001402233 0.004977052 1.800000E-7
3.972 1.122184 11.58482 12.00000 0.9999999 0.0003962489 0.006882483 6.000000E-8
4.018 1.168169 12.17863 13.00000 1.000000 -0.0007644845 0.005664858 -1.200000E-7
4.063 1.212851 12.75754 14.00000 1.000000 -0.001596171 0.00009088036 -1.200000E-7
4.106 1.256394 13.32352 15.00000 0.9999999 -0.001508769 -0.01003161 6.000000E-8
4.150 1.299669 13.88809 16.00000 1.000000 -0.0001139247 -0.007761179 0E-7
4.193 1.342821 14.45511 17.00000 0.9999999 0.001405666 0.008157093 1.200000E-7
4.150 1.300002 13.89245 18.00000 0.9999999 -13.89245 -18.00000 -0.9999999
4.180 1.330002 14.28620 19.00000 1.000000 -14.28620 -19.00000 -1.000000
4.220 1.370003 14.81459 20.00000 0.9999999 -14.81459 -20.00000 -0.9999999
4.310 1.460000 16.01742 21.00000 1.000000 -16.01742 -21.00000 -1.000000
4.310 1.460000 16.01742 22.00000 0.9999999 -16.01742 -22.00000 -0.9999999
4.437 1.586732 17.74645 23.00000 0.9999998 -17.74645 -23.00000 -0.9999998
4.360 1.510001 16.69409 24.00000 1.000000 -16.69409 -24.00000 -1.000000
4.400 1.550002 17.23974 25.00000 0.9999999 -17.23974 -25.00000 -0.9999999
4.430 1.580001 17.65301 26.00000 1.000000 -17.65301 -26.00000 -1.000000
4.470 1.620002 18.21218 27.00000 1.000000 -18.21218 -27.00000 -1.000000
4.560 1.709999 19.50418 28.00000 0.9999999 -19.50418 -28.00000 -0.9999999
4.560 1.709999 19.50418 29.00000 0.9999999 -19.50418 -29.00000 -0.9999999
4.580 1.730000 19.79769 30.00000 0.9999999 -19.79769 -30.00000 -0.9999999
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952785119">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.526164467869 -4.46714109125 15.7526322257 -1.95606284304 -0.365319237154</math:coefficients>
     <math:minRange>0.3818</math:minRange>
     <math:maxRange>1.3428</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952785142">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.718712106986 5.31985891558 10.4341973606 -6.66009878905 1.64337944847</math:coefficients>
     <math:minRange>0.3818</math:minRange>
     <math:maxRange>1.3428</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952785122">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3818</math:minRange>
     <math:maxRange>1.3428</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952785111">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3428</math:minRange>
     <math:maxRange>1.3428</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952785191">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3428</math:minRange>
     <math:maxRange>1.3428</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.0000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951244515"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952785139">
   <gml:name>Gelesen aus: PROF0059.0510.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552846.8788 5987422.5508 2.68</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952785136">
     <gml:description>Übernommen aus Datei: PROF0059.0510.txt</gml:description>
     <gml:name>0059.0510</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition11828595278516">
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
     <om:result><![CDATA[3.221 0.5411590 2.207241 1.000000 1.000000 0.005047634 0.01876555 0E-7
3.374 0.6939680 3.499464 2.000000 1.000000 -0.01700301 -0.03952961 0E-7
3.490 0.8101520 4.604652 3.000000 1.000000 0.003952196 -0.03116152 -1.200000E-7
3.588 0.9075210 5.612126 4.000000 0.9999999 0.01585686 0.01665651 6.000000E-8
3.672 0.9924390 6.551244 5.000000 0.9999998 0.006987266 0.08129083 2.400000E-7
3.740 1.060206 7.325627 6.000000 1.000000 -0.004178551 0.02930766 0E-7
3.804 1.123781 8.058417 7.000000 1.000000 -0.007777776 -0.004964342 -1.200000E-7
3.864 1.183911 8.757114 8.000000 1.000000 -0.007204776 -0.02692919 0E-7
3.921 1.241163 9.427452 9.000000 1.000000 -0.004360947 -0.04093515 0E-7
3.976 1.296021 10.07440 10.00000 1.000000 -0.0001878932 -0.04982023 0E-7
4.031 1.350746 10.72607 11.00000 0.9999999 0.003420848 -0.02003294 1.200000E-7
4.084 1.403529 11.36278 12.00000 1.000000 0.004322828 0.007787327 0E-7
4.134 1.454488 11.98508 13.00000 1.000000 0.003305993 0.02796208 -1.200000E-7
4.184 1.503765 12.59397 14.00000 1.000000 0.001245713 0.03620501 0E-7
4.232 1.551520 13.19069 15.00000 0.9999998 -0.0008135769 0.02946305 2.400000E-7
4.278 1.597900 13.77653 16.00000 0.9999999 -0.001846589 0.005175482 1.200000E-7
4.323 1.643021 14.35241 17.00000 0.9999999 -0.0007662187 -0.03924052 6.000000E-8
4.270 1.590000 13.67631 18.00000 0.9999999 -13.67631 -18.00000 -0.9999999
4.310 1.630000 14.18562 19.00000 0.9999999 -14.18562 -19.00000 -0.9999999
4.350 1.670001 14.69955 20.00000 0.9999999 -14.69955 -20.00000 -0.9999999
4.390 1.710002 15.21809 21.00000 1.000000 -15.21809 -21.00000 -1.000000
4.490 1.810000 16.53650 22.00000 1.000000 -16.53650 -22.00000 -1.000000
4.576 1.895534 17.69320 23.00000 1.000000 -17.69320 -23.00000 -1.000000
4.500 1.820000 16.67034 24.00000 1.000000 -16.67034 -24.00000 -1.000000
4.540 1.859999 17.20938 25.00000 0.9999998 -17.20938 -25.00000 -0.9999998
4.570 1.890000 17.61755 26.00000 0.9999998 -17.61755 -26.00000 -0.9999998
4.610 1.930001 18.16695 27.00000 0.9999999 -18.16695 -27.00000 -0.9999999
4.640 1.960002 18.58287 28.00000 1.000000 -18.58287 -28.00000 -1.000000
4.740 2.059999 19.99321 29.00000 1.000000 -19.99321 -29.00000 -1.000000
4.740 2.059999 19.99321 30.00000 0.9999999 -19.99321 -30.00000 -0.9999999
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952785198">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.19132655875 -0.0965782144238 -0.781576996802 8.32258176659 -2.4529596401</math:coefficients>
     <math:minRange>0.5412</math:minRange>
     <math:maxRange>1.643</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952786671">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>1.38906965333 -6.487531848 19.3953296156 -9.48223300796 1.82792869211</math:coefficients>
     <math:minRange>0.5412</math:minRange>
     <math:maxRange>1.643</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952786677">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5412</math:minRange>
     <math:maxRange>1.643</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952786636">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.643</math:minRange>
     <math:maxRange>1.643</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952786637">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.643</math:minRange>
     <math:maxRange>1.643</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.0510</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951247619"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952786654">
   <gml:name>Gelesen aus: PROF0059.1000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552839.4772 5987474.3447 2.46</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952786688">
     <gml:description>Übernommen aus Datei: PROF0059.1000.txt</gml:description>
     <gml:name>0059.1000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952786615">
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
     <om:result><![CDATA[2.974 0.5136950 1.981176 1.000000 1.000000 0.00007507128 0.005708500 0E-7
3.149 0.6892120 3.126121 2.000000 1.000000 -0.002143966 -0.01883406 0E-7
3.284 0.8241340 4.113155 3.000000 1.000000 0.004633001 0.0002325881 -1.200000E-7
3.397 0.9368470 5.008890 4.000000 1.000000 0.001004771 0.02724754 0E-7
3.491 1.030932 5.794219 5.000000 1.000000 -0.003254395 0.004214991 -1.200000E-7
3.577 1.117153 6.535090 6.000000 0.9999998 -0.002900734 -0.007146860 1.800000E-7
3.657 1.197248 7.241453 7.000000 1.000000 -0.001036091 -0.01079458 0E-7
3.732 1.272425 7.920351 8.000000 1.000000 0.0007790309 -0.009137452 -1.200000E-7
3.803 1.343495 8.576313 9.000000 0.9999999 0.001877169 -0.004678689 1.200000E-7
3.871 1.411063 9.212708 10.00000 1.000000 0.002072320 0.0005837367 0E-7
3.936 1.475606 9.832224 11.00000 1.000000 0.001472508 0.005192115 0E-7
3.998 1.537522 10.43719 12.00000 1.000000 0.0003595082 0.008298761 0E-7
4.057 1.597040 11.02858 13.00000 1.000000 -0.0008690582 0.007659466 -1.200000E-7
4.115 1.654513 11.60879 14.00000 1.000000 -0.001740864 0.004159939 0E-7
4.170 1.710046 12.17797 15.00000 0.9999999 -0.001774630 -0.004711768 1.200000E-7
4.224 1.763851 12.73745 16.00000 1.000000 -0.0004559244 -0.01898235 0E-7
4.279 1.818723 13.31720 17.00000 1.000000 0.001902282 0.01098811 -2.400000E-7
4.220 1.760000 12.69715 18.00000 0.9999998 -12.69715 -18.00000 -0.9999998
4.260 1.800001 13.11806 19.00000 0.9999999 -13.11806 -19.00000 -0.9999999
4.310 1.850002 13.65313 20.00000 1.000000 -13.65313 -20.00000 -1.000000
4.420 1.960000 14.86652 21.00000 0.9999999 -14.86652 -21.00000 -0.9999999
4.470 2.010000 15.43457 22.00000 1.000000 -15.43457 -22.00000 -1.000000
4.579 2.119093 16.70980 23.00000 0.9999999 -16.70980 -23.00000 -0.9999999
4.490 2.030000 15.66468 24.00000 0.9999998 -15.66468 -24.00000 -0.9999998
4.530 2.070001 16.12986 25.00000 1.000000 -16.12986 -25.00000 -1.000000
4.570 2.110002 16.60163 26.00000 0.9999999 -16.60163 -26.00000 -0.9999999
4.620 2.160002 17.20164 27.00000 1.000000 -17.20164 -27.00000 -1.000000
4.720 2.259999 18.43877 28.00000 0.9999998 -18.43877 -28.00000 -0.9999998
4.720 2.259999 18.43877 29.00000 0.9999998 -18.43877 -29.00000 -0.9999998
4.740 2.280000 18.69215 30.00000 0.9999998 -18.69215 -30.00000 -0.9999998
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952786654">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.462910962229 -2.28335216364 6.40898461602 0.284753936588 -0.202127656013</math:coefficients>
     <math:minRange>0.5137</math:minRange>
     <math:maxRange>1.8187</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952786680">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.118877251625 0.714862373505 6.94511285315 -2.74573031483 0.497693087777</math:coefficients>
     <math:minRange>0.5137</math:minRange>
     <math:maxRange>1.8187</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952786672">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5137</math:minRange>
     <math:maxRange>1.8187</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952786645">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.8187</math:minRange>
     <math:maxRange>1.8187</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952786653">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.8187</math:minRange>
     <math:maxRange>1.8187</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.1000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951258521"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859527882103">
   <gml:name>Gelesen aus: PROF0059.1500.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552849.6867 5987524.1193 2.78</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952788288">
     <gml:description>Übernommen aus Datei: PROF0059.1500.txt</gml:description>
     <gml:name>0059.1500</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952788223">
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
     <om:result><![CDATA[3.324 0.5435040 2.402705 1.000000 1.000000 -0.002900629 -0.002638453 0E-7
3.470 0.6899270 3.820140 2.000000 0.9999999 0.009416308 0.01350491 1.200000E-7
3.571 0.7905640 4.861600 3.000000 1.000000 -0.002788864 -0.008418147 0E-7
3.657 0.8774850 5.772466 4.000000 1.000000 -0.004834981 -0.01191336 -1.200000E-7
3.736 0.9562670 6.605091 5.000000 1.000000 -0.002830205 -0.004385023 -1.200000E-7
3.809 1.029153 7.381382 6.000000 1.000000 -0.0003417948 0.004966520 0E-7
3.877 1.097492 8.114463 7.000000 1.000000 0.001273223 0.01120044 0E-7
3.942 1.162201 8.813261 8.000000 0.9999999 0.001749458 0.01212488 6.000000E-8
4.004 1.223870 9.483431 9.000000 0.9999999 0.001435625 0.005971097 1.200000E-7
4.063 1.283033 10.13024 10.00000 0.9999999 0.0009969711 -0.006389805 1.200000E-7
4.120 1.340337 10.76040 11.00000 1.000000 0.001279024 -0.01946767 -1.200000E-7
4.177 1.397248 11.39227 12.00000 0.9999999 0.0009849120 -0.01073565 6.000000E-8
4.233 1.452785 12.01636 13.00000 1.000000 -0.00009770238 0.0007228503 -1.200000E-7
4.287 1.506799 12.63215 14.00000 1.000000 -0.002031892 0.01022083 0E-7
4.339 1.559004 13.23576 15.00000 1.000000 -0.003162967 0.01058864 0E-7
4.390 1.609571 13.82835 16.00000 0.9999999 -0.001805357 0.003540371 6.000000E-8
4.439 1.658659 14.41107 17.00000 0.9999998 0.003658873 -0.008892432 1.800000E-7
4.380 1.600001 13.71560 18.00000 1.000000 -13.71560 -18.00000 -1.000000
4.430 1.650002 14.30777 19.00000 1.000000 -14.30777 -19.00000 -1.000000
4.470 1.690003 14.78698 20.00000 1.000000 -14.78698 -20.00000 -1.000000
4.510 1.730003 15.27106 21.00000 0.9999999 -15.27106 -21.00000 -0.9999999
4.590 1.810000 16.25381 22.00000 1.000000 -16.25381 -22.00000 -1.000000
4.707 1.927137 17.72800 23.00000 0.9999999 -17.72800 -23.00000 -0.9999999
4.630 1.850001 16.75253 24.00000 0.9999998 -16.75253 -24.00000 -0.9999998
4.670 1.890002 17.25612 25.00000 1.000000 -17.25612 -25.00000 -1.000000
4.700 1.920002 17.63701 26.00000 1.000000 -17.63701 -26.00000 -1.000000
4.740 1.960002 18.15003 27.00000 0.9999999 -18.15003 -27.00000 -0.9999999
4.840 2.059999 19.46780 28.00000 1.000000 -19.46780 -28.00000 -1.000000
4.840 2.059999 19.46780 29.00000 0.9999998 -19.46780 -29.00000 -0.9999998
4.860 2.080000 19.73745 30.00000 0.9999999 -19.73745 -30.00000 -0.9999999
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859527898116">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>3.71881172509 -17.9258387212 29.6481449312 -11.9702633179 2.12207327093</math:coefficients>
     <math:minRange>0.5435</math:minRange>
     <math:maxRange>1.6587</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952789880">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-1.28672134659 2.89049229924 9.88253807207 -5.70713851601 1.28972346424</math:coefficients>
     <math:minRange>0.5435</math:minRange>
     <math:maxRange>1.6587</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952789817">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5435</math:minRange>
     <math:maxRange>1.6587</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11828595278989">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6587</math:minRange>
     <math:maxRange>1.6587</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11828595278984">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6587</math:minRange>
     <math:maxRange>1.6587</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.1500</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951260129"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952789882">
   <gml:name>Gelesen aus: PROF0059.2000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552889.8488 5987558.463 2.52</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952789861">
     <gml:description>Übernommen aus Datei: PROF0059.2000.txt</gml:description>
     <gml:name>0059.2000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952789847">
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
     <om:result><![CDATA[3.079 0.5588900 1.981511 1.000000 1.000000 0.0006228464 0.01932119 -1.200000E-7
3.261 0.7409110 3.222261 2.000000 0.9999999 -0.0003742501 -0.02459359 6.000000E-8
3.385 0.8646120 4.166576 3.000000 1.000000 -0.003702793 -0.03906542 -1.200000E-7
3.492 0.9717680 5.017440 4.000000 0.9999999 0.001226771 -0.008946614 6.000000E-8
3.587 1.067095 5.798882 5.000000 1.000000 0.002724983 0.02074407 0E-7
3.674 1.154264 6.527534 6.000000 1.000000 0.003118369 0.03922168 -1.200000E-7
3.756 1.235943 7.221564 7.000000 0.9999998 0.001770962 0.04818392 1.800000E-7
3.833 1.313163 7.887735 8.000000 0.9999999 -0.001076915 0.04142664 6.000000E-8
3.907 1.386679 8.530995 9.000000 0.9999999 -0.004069828 0.01528118 6.000000E-8
3.977 1.457077 9.155268 10.00000 1.000000 -0.005283401 -0.03219452 0E-7
4.045 1.524714 9.762674 11.00000 0.9999999 -0.002385121 -0.1033777 6.000000E-8
4.118 1.597949 10.43452 12.00000 0.9999999 0.003144622 -0.08740610 6.000000E-8
4.194 1.674355 11.16070 13.00000 0.9999999 0.007193580 -0.02101229 1.200000E-7
4.273 1.753379 11.95256 14.00000 0.9999998 0.002872067 0.08381701 1.800000E-7
4.346 1.826146 12.72087 15.00000 1.000000 -0.003828234 0.09891861 0E-7
4.414 1.893785 13.46868 16.00000 0.9999999 -0.005429866 0.03772352 1.200000E-7
4.477 1.957213 14.19939 17.00000 0.9999999 0.003476208 -0.08804163 6.000000E-8
4.400 1.880002 13.31367 18.00000 1.000000 -13.31367 -18.00000 -1.000000
4.530 2.010000 14.82925 19.00000 0.9999998 -14.82925 -19.00000 -0.9999998
4.530 2.010000 14.82925 20.00000 0.9999999 -14.82925 -20.00000 -0.9999999
4.560 2.040000 15.19602 21.00000 1.000000 -15.19602 -21.00000 -1.000000
4.610 2.090002 15.82148 22.00000 0.9999999 -15.82148 -22.00000 -0.9999999
4.796 2.276388 18.30898 23.00000 1.000000 -18.30898 -23.00000 -1.000000
4.710 2.190002 17.12553 24.00000 0.9999999 -17.12553 -24.00000 -0.9999999
4.780 2.259999 18.08048 25.00000 1.000000 -18.08048 -25.00000 -1.000000
4.790 2.269999 18.21971 26.00000 1.000000 -18.21971 -26.00000 -1.000000
4.830 2.310000 18.78290 27.00000 1.000000 -18.78290 -27.00000 -1.000000
4.870 2.350000 19.35614 28.00000 1.000000 -19.35614 -28.00000 -1.000000
4.910 2.390001 19.93946 29.00000 0.9999999 -19.93946 -29.00000 -0.9999999
4.940 2.420001 20.38355 30.00000 0.9999999 -20.38355 -30.00000 -0.9999999
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11828595280071">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>3.52518358951 -14.1259779941 20.6511516243 -6.48151621561 0.716992036128</math:coefficients>
     <math:minRange>0.5589</math:minRange>
     <math:maxRange>1.9572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952800784">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.577214932039 -2.76467117408 12.7787937394 -6.7556684459 1.41306937045</math:coefficients>
     <math:minRange>0.5589</math:minRange>
     <math:maxRange>1.9572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952800790">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5589</math:minRange>
     <math:maxRange>1.9572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952800745">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9572</math:minRange>
     <math:maxRange>1.9572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952800723">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9572</math:minRange>
     <math:maxRange>1.9572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.2000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951271014"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952800780">
   <gml:name>Gelesen aus: PROF0059.2560.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552936.5794 5987592.6081 2.31</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952800784">
     <gml:description>Übernommen aus Datei: PROF0059.2560.txt</gml:description>
     <gml:name>0059.2560</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952800720">
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
     <om:result><![CDATA[2.831 0.5213130 1.868768 1.000000 1.000000 0.003817645 0.003503065 -1.200000E-7
3.038 0.7284620 2.984583 2.000000 0.9999999 -0.01216688 -0.01098378 6.000000E-8
3.203 0.8927560 4.007936 3.000000 0.9999999 0.002877620 -0.0001180999 6.000000E-8
3.336 1.026337 4.944645 4.000000 1.000000 0.009275364 0.007872297 0E-7
3.451 1.140722 5.821362 5.000000 0.9999999 0.005743102 0.01835393 6.000000E-8
3.549 1.238877 6.623621 6.000000 0.9999999 -0.001619712 -0.0004678582 6.000000E-8
3.638 1.328270 7.385337 7.000000 0.9999998 -0.004396532 -0.01322256 1.800000E-7
3.721 1.411417 8.120205 8.000000 1.000000 -0.004530575 -0.01466656 0E-7
3.799 1.489386 8.832395 9.000000 1.000000 -0.003315734 -0.008871138 -2.400000E-7
3.873 1.562890 9.524260 10.00000 0.9999999 -0.001575587 -0.0006473696 6.000000E-8
3.943 1.632611 10.19888 11.00000 1.000000 0.0001546249 0.007635876 0E-7
4.009 1.699046 10.85832 12.00000 1.000000 0.001568171 0.01333622 -1.200000E-7
4.073 1.762570 11.50403 13.00000 0.9999998 0.002451694 0.01372877 1.800000E-7
4.134 1.823546 12.13780 14.00000 0.9999999 0.002689885 0.007308886 6.000000E-8
4.192 1.882186 12.76018 15.00000 1.000000 0.002228350 -0.008750210 -1.200000E-7
4.249 1.938734 13.37233 16.00000 0.9999999 0.001053827 -0.03587158 1.200000E-7
4.309 1.998871 14.03943 17.00000 0.9999999 -0.004255261 0.02186010 1.200000E-7
4.270 1.960000 13.60595 18.00000 0.9999999 -13.60595 -18.00000 -0.9999999
4.290 1.980000 13.82795 19.00000 1.000000 -13.82795 -19.00000 -1.000000
4.340 2.030002 14.39259 20.00000 0.9999998 -14.39259 -20.00000 -0.9999998
4.390 2.080003 14.97103 21.00000 1.000000 -14.97103 -21.00000 -1.000000
4.440 2.130002 15.56337 22.00000 1.000000 -15.56337 -22.00000 -1.000000
4.629 2.318751 17.93678 23.00000 0.9999998 -17.93678 -23.00000 -0.9999998
4.540 2.230000 16.79353 24.00000 0.9999998 -16.79353 -24.00000 -0.9999998
4.580 2.270001 17.30281 25.00000 1.000000 -17.30281 -25.00000 -1.000000
4.620 2.310001 17.82191 26.00000 0.9999999 -17.82191 -26.00000 -0.9999999
4.660 2.350002 18.35084 27.00000 1.000000 -18.35084 -27.00000 -1.000000
4.770 2.460000 19.85600 28.00000 1.000000 -19.85600 -28.00000 -1.000000
4.770 2.460000 19.85600 29.00000 1.000000 -19.85600 -29.00000 -1.000000
4.780 2.470000 19.99653 30.00000 0.9999999 -19.99653 -30.00000 -0.9999999
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952802332">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.986322221001 4.50255167116 -3.52761301555 4.72079487412 -0.914547646955</math:coefficients>
     <math:minRange>0.5213</math:minRange>
     <math:maxRange>1.9989</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952802396">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.33054279559 1.0285150496 4.07521655732 -0.765880900296 0.0928951545003</math:coefficients>
     <math:minRange>0.5213</math:minRange>
     <math:maxRange>1.9989</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952802366">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5213</math:minRange>
     <math:maxRange>1.9989</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952802387">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9989</math:minRange>
     <math:maxRange>1.9989</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11828595280234">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9989</math:minRange>
     <math:maxRange>1.9989</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.2560</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951272626"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952802325">
   <gml:name>Gelesen aus: PROF0059.3000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552969.4562 5987619.3316 3.01</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528023102">
     <gml:description>Übernommen aus Datei: PROF0059.3000.txt</gml:description>
     <gml:name>0059.3000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528023131">
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
     <om:result><![CDATA[3.425 0.4154300 2.213070 1.000000 0.9999999 0.0004033742 0.006336723 1.200000E-7
3.573 0.5631670 3.462499 2.000000 1.000000 -0.002697282 -0.01906168 -2.400000E-7
3.687 0.6770060 4.532322 3.000000 1.000000 0.003410253 0.001004523 -1.200000E-7
3.781 0.7710650 5.478106 4.000000 0.9999999 0.002401287 0.01748409 6.000000E-8
3.862 0.8516810 6.323665 5.000000 1.000000 -0.002405710 0.009821794 0E-7
3.934 0.9243600 7.101624 6.000000 1.000000 -0.002633885 -0.003554116 0E-7
4.002 0.9922050 7.840298 7.000000 0.9999998 -0.001090675 -0.007586425 1.800000E-7
4.066 1.056152 8.547541 8.000000 1.000000 0.0004345244 -0.006229247 -1.200000E-7
4.127 1.116844 9.228680 9.000000 0.9999999 0.001236516 -0.003196294 1.200000E-7
4.185 1.174810 9.888196 10.00000 0.9999999 0.001299784 -0.0007898678 6.000000E-8
4.240 1.230407 10.52902 11.00000 0.9999999 0.0009454937 -0.001902966 1.200000E-7
4.294 1.284437 11.15972 12.00000 1.000000 0.0005820738 0.0006818313 -1.200000E-7
4.347 1.337233 11.78499 13.00000 1.000000 -0.0004038052 0.007427987 -1.200000E-7
4.398 1.388191 12.39719 14.00000 0.9999999 -0.001226639 0.001301070 1.200000E-7
4.448 1.438367 13.00853 15.00000 0.9999998 -0.0009393898 -0.001978260 1.800000E-7
4.498 1.488418 13.62902 16.00000 1.000000 -0.0004467784 0.006874377 -1.200000E-7
4.547 1.536831 14.23992 17.00000 0.9999998 0.001130859 -0.006633543 1.800000E-7
4.490 1.480000 13.52388 18.00000 0.9999999 -13.52388 -18.00000 -0.9999999
4.540 1.530001 14.15311 19.00000 0.9999999 -14.15311 -19.00000 -0.9999999
4.580 1.570002 14.66458 20.00000 1.000000 -14.66458 -20.00000 -1.000000
4.620 1.610002 15.18324 21.00000 0.9999998 -15.18324 -21.00000 -0.9999998
4.720 1.709999 16.51134 22.00000 0.9999997 -16.51134 -22.00000 -0.9999997
4.809 1.799010 17.73134 23.00000 0.9999998 -17.73134 -23.00000 -0.9999998
4.730 1.720000 16.64663 24.00000 1.000000 -16.64663 -24.00000 -1.000000
4.770 1.760000 17.19228 25.00000 1.000000 -17.19228 -25.00000 -1.000000
4.810 1.800001 17.74513 26.00000 1.000000 -17.74513 -26.00000 -1.000000
4.840 1.830001 18.16448 27.00000 0.9999999 -18.16448 -27.00000 -0.9999999
4.880 1.870002 18.72993 28.00000 1.000000 -18.72993 -28.00000 -1.000000
4.970 1.959999 20.02841 29.00000 0.9999998 -20.02841 -29.00000 -0.9999998
4.970 1.959999 20.02841 30.00000 1.000000 -20.02841 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528023128">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.302453166084 -1.61695180608 7.14735414972 2.48216720886 -1.20372160035</math:coefficients>
     <math:minRange>0.4154</math:minRange>
     <math:maxRange>1.5368</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952802314">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.251947350962 0.813699979773 11.8204906784 -6.41237671735 1.45128930298</math:coefficients>
     <math:minRange>0.4154</math:minRange>
     <math:maxRange>1.5368</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528038118">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4154</math:minRange>
     <math:maxRange>1.5368</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952803835">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5368</math:minRange>
     <math:maxRange>1.5368</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528038105">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5368</math:minRange>
     <math:maxRange>1.5368</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.3000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951283534"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528038111">
   <gml:name>Gelesen aus: PROF0059.3650.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552986.6852 5987678.0098 2.77</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528038143">
     <gml:description>Übernommen aus Datei: PROF0059.3650.txt</gml:description>
     <gml:name>0059.3650</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952803862">
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
     <om:result><![CDATA[3.242 0.4720320 2.166053 1.000000 0.9999998 -0.001764912 -0.008264347 1.800000E-7
3.399 0.6285040 3.465584 2.000000 1.000000 0.006569624 0.02633088 -1.200000E-7
3.507 0.7369500 4.457587 3.000000 0.9999999 -0.003297707 -0.005084253 6.000000E-8
3.601 0.8308050 5.346502 4.000000 0.9999999 -0.003229354 -0.01396753 6.000000E-8
3.685 0.9150080 6.167949 5.000000 0.9999998 -0.001570823 -0.01246403 1.800000E-7
3.762 0.9922650 6.939710 6.000000 1.000000 0.0002836538 -0.006316678 -1.200000E-7
3.834 1.064316 7.674149 7.000000 1.000000 0.001703723 0.002432460 -1.200000E-7
3.902 1.132140 8.378453 8.000000 1.000000 0.002091779 0.01012513 0E-7
3.966 1.196434 9.057704 9.000000 1.000000 0.001477460 0.01409362 0E-7
4.028 1.257824 9.716804 10.00000 1.000000 0.0002580644 0.01412516 -1.200000E-7
4.087 1.316567 10.35713 11.00000 0.9999999 -0.0009918894 0.006440162 6.000000E-8
4.143 1.373026 10.98144 12.00000 0.9999999 -0.001549759 -0.009370852 6.000000E-8
4.198 1.427503 11.59208 13.00000 0.9999999 -0.0006662918 -0.03336598 6.000000E-8
4.254 1.483570 12.23141 14.00000 0.9999999 0.0003734262 -0.003779457 1.200000E-7
4.308 1.537728 12.86213 15.00000 0.9999999 0.000002746692 0.01308173 6.000000E-8
4.360 1.589922 13.48220 16.00000 1.000000 -0.0003385316 0.01215001 -1.200000E-7
4.410 1.640357 14.09276 17.00000 1.000000 0.0006487909 -0.006166010 0E-7
4.350 1.580001 13.36341 18.00000 1.000000 -13.36341 -18.00000 -1.000000
4.400 1.630002 13.96650 19.00000 0.9999999 -13.96650 -19.00000 -0.9999999
4.480 1.710000 14.95428 20.00000 1.000000 -14.95428 -20.00000 -1.000000
4.530 1.760000 15.58599 21.00000 0.9999998 -15.58599 -21.00000 -0.9999998
4.530 1.760000 15.58599 22.00000 1.000000 -15.58599 -22.00000 -1.000000
4.688 1.918137 17.66375 23.00000 1.000000 -17.66375 -23.00000 -1.000000
4.600 1.830001 16.48938 24.00000 0.9999998 -16.48938 -24.00000 -0.9999998
4.640 1.870002 17.01713 25.00000 1.000000 -17.01713 -25.00000 -1.000000
4.730 1.960000 18.23642 26.00000 1.000000 -18.23642 -26.00000 -1.000000
4.780 2.009999 18.93292 27.00000 1.000000 -18.93292 -27.00000 -1.000000
4.780 2.009999 18.93292 28.00000 1.000000 -18.93292 -28.00000 -1.000000
4.790 2.019999 19.07386 29.00000 1.000000 -19.07386 -29.00000 -1.000000
4.840 2.070001 19.79150 30.00000 1.000000 -19.79150 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952814823">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.903264305814 -5.23458575202 12.4450325293 -2.04458448098 0.0296763044097</math:coefficients>
     <math:minRange>0.472</math:minRange>
     <math:maxRange>1.6404</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952814868">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.481667307094 2.63812635503 7.98514324627 -4.02436987201 0.901113146462</math:coefficients>
     <math:minRange>0.472</math:minRange>
     <math:maxRange>1.6404</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528148126">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.472</math:minRange>
     <math:maxRange>1.6404</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952814822">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6404</math:minRange>
     <math:maxRange>1.6404</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528148123">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6404</math:minRange>
     <math:maxRange>1.6404</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.3650</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile11828595128661"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952814891">
   <gml:name>Gelesen aus: PROF0059.4000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552983.6592 5987726.6338 2.68</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952814878">
     <gml:description>Übernommen aus Datei: PROF0059.4000.txt</gml:description>
     <gml:name>0059.4000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952814832">
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
     <om:result><![CDATA[3.294 0.6143140 1.915416 1.000000 1.000000 -0.002034280 0.02172302 -2.400000E-7
3.497 0.8173580 3.142840 2.000000 1.000000 0.004382515 -0.06628124 -1.200000E-7
3.645 0.9651130 4.211022 3.000000 0.9999999 0.004097070 0.01695309 1.200000E-7
3.756 1.076129 5.088624 4.000000 0.9999998 -0.004463167 0.01679185 1.800000E-7
3.855 1.175192 5.901440 5.000000 1.000000 -0.004213697 0.02266050 -1.200000E-7
3.946 1.265910 6.670341 6.000000 1.000000 -0.002239846 0.02607596 -2.400000E-7
4.030 1.350142 7.405269 7.000000 0.9999999 -0.0008635430 0.02091455 1.200000E-7
4.109 1.429085 8.112418 8.000000 1.000000 -0.0002611617 0.004034780 -1.200000E-7
4.184 1.503656 8.796734 9.000000 1.000000 0.0004780588 -0.02480925 -1.200000E-7
4.254 1.574469 9.461234 10.00000 1.000000 0.002830257 -0.06573439 0E-7
4.327 1.646632 10.15503 11.00000 1.000000 0.007156996 -0.05192412 0E-7
4.399 1.719339 10.88222 12.00000 1.000000 0.005264617 0.007056219 -1.200000E-7
4.468 1.787642 11.59279 13.00000 1.000000 -0.0005403980 0.03720816 0E-7
4.532 1.852193 12.28874 14.00000 1.000000 -0.006160204 0.04367881 0E-7
4.593 1.913476 12.97142 15.00000 0.9999998 -0.008258851 0.03051692 1.800000E-7
4.652 1.971886 13.64199 16.00000 1.000000 -0.004028526 0.001377295 -2.400000E-7
4.708 2.027762 14.30165 17.00000 0.9999998 0.008854161 -0.04024216 1.800000E-7
4.690 2.010000 14.09002 18.00000 1.000000 -14.09002 -18.00000 -1.000000
4.740 2.059999 14.69032 19.00000 1.000000 -14.69032 -19.00000 -1.000000
4.740 2.059999 14.69032 20.00000 1.000000 -14.69032 -20.00000 -1.000000
4.790 2.110000 15.30487 21.00000 1.000000 -15.30487 -21.00000 -1.000000
4.830 2.150001 15.80676 22.00000 1.000000 -15.80676 -22.00000 -1.000000
5.001 2.321463 18.06132 23.00000 0.9999998 -18.06132 -23.00000 -0.9999998
4.920 2.240002 16.96932 24.00000 1.000000 -16.96932 -24.00000 -1.000000
4.990 2.309999 17.90536 25.00000 1.000000 -17.90536 -25.00000 -1.000000
5.000 2.320000 18.04136 26.00000 0.9999999 -18.04136 -26.00000 -0.9999999
5.040 2.359999 18.59106 27.00000 1.000000 -18.59106 -27.00000 -1.000000
5.070 2.390000 19.00933 28.00000 1.000000 -19.00933 -28.00000 -1.000000
5.110 2.430001 19.57500 29.00000 1.000000 -19.57500 -29.00000 -1.000000
5.150 2.470002 20.14977 30.00000 1.000000 -20.14977 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528148148">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>4.66990628813 -16.8009053308 21.9975316329 -7.77989217422 1.22879673084</math:coefficients>
     <math:minRange>0.6143</math:minRange>
     <math:maxRange>2.0278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952814870">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>1.42896112099 -5.39787386968 13.3492221722 -6.03776698059 1.14030495305</math:coefficients>
     <math:minRange>0.6143</math:minRange>
     <math:maxRange>2.0278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528148106">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.6143</math:minRange>
     <math:maxRange>2.0278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952814832">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>2.0278</math:minRange>
     <math:maxRange>2.0278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952814862">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>2.0278</math:minRange>
     <math:maxRange>2.0278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.4000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951297621"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528148130">
   <gml:name>Gelesen aus: PROF0059.4480.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3552986.9676 5987774.2405 2.8</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952816328">
     <gml:description>Übernommen aus Datei: PROF0059.4480.txt</gml:description>
     <gml:name>0059.4480</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528163101">
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
     <om:result><![CDATA[3.262 0.4615540 2.050584 1.000000 1.000000 -0.001398470 0.02274797 0E-7
3.428 0.6284130 3.254750 2.000000 1.000000 0.003980638 -0.04494064 -1.200000E-7
3.551 0.7508110 4.225101 3.000000 1.000000 0.0003002373 -0.02375900 0E-7
3.655 0.8549680 5.098333 4.000000 1.000000 -0.002889926 0.01205451 -1.200000E-7
3.747 0.9468550 5.899166 5.000000 0.9999999 -0.003082173 0.02919964 1.200000E-7
3.831 1.030921 6.654668 6.000000 1.000000 -0.001002315 0.03439730 0E-7
3.909 1.108910 7.375072 7.000000 1.000000 0.001347418 0.02765085 -1.200000E-7
3.982 1.182019 8.067456 8.000000 1.000000 0.002788899 0.01297453 -1.200000E-7
4.051 1.251047 8.736326 9.000000 1.000000 0.002616455 -0.004796343 -1.200000E-7
4.116 1.316218 9.381191 10.00000 1.000000 0.0005292166 -0.02583374 -1.200000E-7
4.178 1.377504 9.996777 11.00000 1.000000 -0.0009831229 -0.05329226 -1.200000E-7
4.237 1.436502 10.59670 12.00000 0.9999999 -0.001379848 -0.06269293 1.200000E-7
4.294 1.493544 11.18357 13.00000 1.000000 -0.001138032 -0.04694436 0E-7
4.349 1.548757 11.75803 14.00000 0.9999999 -0.0006094660 -0.001829008 1.200000E-7
4.402 1.602338 12.32151 15.00000 0.9999999 -0.00004993757 0.07852447 1.200000E-7
4.454 1.654409 12.87481 16.00000 0.9999998 0.0003107086 0.1988740 1.800000E-7
4.505 1.705187 13.41975 17.00000 1.000000 0.0003425218 0.3664240 -1.200000E-7
4.510 1.710000 13.47168 18.00000 0.9999999 0.0003171977 -0.5187590 1.200000E-7
4.510 1.710000 13.47168 19.00000 1.000000 -13.47168 -19.00000 -1.000000
4.540 1.740000 13.79645 20.00000 1.000000 -13.79645 -20.00000 -1.000000
4.580 1.780001 14.23236 21.00000 1.000000 -14.23236 -21.00000 -1.000000
4.620 1.820001 14.67156 22.00000 1.000000 -14.67156 -22.00000 -1.000000
4.813 2.013203 16.87511 23.00000 1.000000 -16.87511 -23.00000 -1.000000
4.760 1.959999 16.24764 24.00000 1.000000 -16.24764 -24.00000 -1.000000
4.760 1.959999 16.24764 25.00000 0.9999999 -16.24764 -25.00000 -0.9999999
4.800 2.000000 16.71779 26.00000 0.9999997 -16.71779 -26.00000 -0.9999997
4.850 2.050001 17.31989 27.00000 1.000000 -17.31989 -27.00000 -1.000000
4.900 2.100002 17.93942 28.00000 1.000000 -17.93942 -28.00000 -1.000000
5.010 2.209999 19.36390 29.00000 1.000000 -19.36390 -29.00000 -1.000000
5.010 2.209999 19.36390 30.00000 1.000000 -19.36390 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528163133">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>3.35907770937 -16.675782531 32.4306299887 -17.7094965626 4.25228438503</math:coefficients>
     <math:minRange>0.4616</math:minRange>
     <math:maxRange>1.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528163121">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.4982674708 4.03039263774 3.60273342852 -0.862879766158 0.100361301366</math:coefficients>
     <math:minRange>0.4616</math:minRange>
     <math:maxRange>1.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528163125">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4616</math:minRange>
     <math:maxRange>1.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952816375">
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
    <math:Polynomial1D gml:id="Polynomial1D118285952816358">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.71</math:minRange>
     <math:maxRange>1.71</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.4480</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951308534"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation11828595281636">
   <gml:name>Gelesen aus: PROF0059.5000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553012.0314 5987820.7902 2.85</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528163112">
     <gml:description>Übernommen aus Datei: PROF0059.5000.txt</gml:description>
     <gml:name>0059.5000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528163157">
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
     <om:result><![CDATA[3.199 0.3489500 2.206079 1.000000 1.000000 -0.0008458312 -0.002971022 -2.400000E-7
3.343 0.4934990 3.364952 2.000000 1.000000 0.001955130 0.005229998 -1.200000E-7
3.459 0.6085400 4.338568 3.000000 0.9999999 0.0004887985 0.003187151 1.200000E-7
3.558 0.7075730 5.213126 4.000000 1.000000 -0.0009705150 -0.0004402173 0E-7
3.646 0.7960290 6.022772 5.000000 1.000000 -0.001519105 -0.003540889 -1.200000E-7
3.727 0.8768040 6.785601 6.000000 0.9999999 -0.001164416 -0.005028337 1.200000E-7
3.802 0.9515940 7.511899 7.000000 1.000000 -0.0002219104 -0.005173783 -1.200000E-7
3.872 1.021622 8.209379 8.000000 1.000000 0.0008816713 -0.002917633 -1.200000E-7
3.938 1.087642 8.882363 9.000000 1.000000 0.001701077 0.0009641536 0E-7
4.000 1.150301 9.534953 10.00000 0.9999998 0.001763714 0.007209890 1.800000E-7
4.060 1.210037 10.16965 11.00000 1.000000 0.0006686169 0.01553743 -1.200000E-7
4.116 1.266170 10.77646 12.00000 1.000000 -0.001162551 0.007181392 -1.200000E-7
4.170 1.319760 11.36256 13.00000 1.000000 -0.001550186 -0.005689905 -2.400000E-7
4.222 1.371680 11.93651 14.00000 1.000000 -0.0009886611 -0.01117264 -1.200000E-7
4.272 1.422022 12.49876 15.00000 1.000000 -0.00009248311 -0.01053027 -1.200000E-7
4.321 1.470954 13.05069 16.00000 0.9999999 0.0005654323 -0.003117522 1.200000E-7
4.369 1.518600 13.59324 17.00000 1.000000 0.0004912179 0.01127220 0E-7
4.320 1.470001 13.03989 18.00000 1.000000 -13.03989 -18.00000 -1.000000
4.360 1.510002 13.49496 19.00000 1.000000 -13.49496 -19.00000 -1.000000
4.460 1.610000 14.64820 20.00000 1.000000 -14.64820 -20.00000 -1.000000
4.460 1.610000 14.64820 21.00000 1.000000 -14.64820 -21.00000 -1.000000
4.510 1.660000 15.23320 22.00000 1.000000 -15.23320 -22.00000 -1.000000
4.632 1.781868 16.68243 23.00000 0.9999999 -16.68243 -23.00000 -0.9999999
4.550 1.700001 15.70523 24.00000 1.000000 -15.70523 -24.00000 -1.000000
4.590 1.740002 16.18082 25.00000 0.9999999 -16.18082 -25.00000 -0.9999999
4.710 1.860000 17.63961 26.00000 1.000000 -17.63961 -26.00000 -1.000000
4.710 1.860000 17.63961 27.00000 0.9999999 -17.63961 -27.00000 -0.9999999
4.720 1.870000 17.76559 28.00000 0.9999999 -17.76559 -28.00000 -0.9999999
4.770 1.920001 18.40766 29.00000 0.9999999 -18.40766 -29.00000 -0.9999999
4.810 1.960000 18.93594 30.00000 1.000000 -18.93594 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11828595281793">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.268623564909 1.16643217854 7.28493892948 -0.758762003852 0.256790232214</math:coefficients>
     <math:minRange>0.3489</math:minRange>
     <math:maxRange>1.5186</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952817990">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.380549203741 7.09137468774 0.609518460431 0.978135585642 -0.3057107543</math:coefficients>
     <math:minRange>0.3489</math:minRange>
     <math:maxRange>1.5186</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528179138">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3489</math:minRange>
     <math:maxRange>1.5186</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528179147">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5186</math:minRange>
     <math:maxRange>1.5186</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952817957">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5186</math:minRange>
     <math:maxRange>1.5186</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.5000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951310127"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952817956">
   <gml:name>Gelesen aus: PROF0059.5510.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553038.0353 5987864.9639 2.67</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952817946">
     <gml:description>Übernommen aus Datei: PROF0059.5510.txt</gml:description>
     <gml:name>0059.5510</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952817938">
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
     <om:result><![CDATA[3.208 0.5377800 2.149999 1.000000 0.9999998 -0.002815652 -0.006338748 2.400000E-7
3.366 0.6961220 3.452669 2.000000 1.000000 0.009113446 0.02018879 -1.200000E-7
3.477 0.8070140 4.453146 3.000000 1.000000 -0.002631769 -0.005235946 -1.200000E-7
3.573 0.9031400 5.351203 4.000000 0.9999999 -0.004666092 -0.01039894 1.200000E-7
3.660 0.9895570 6.182901 5.000000 1.000000 -0.003152693 -0.007403524 -1.200000E-7
3.739 1.068888 6.966706 6.000000 1.000000 -0.0007988652 -0.002108324 0E-7
3.813 1.142712 7.713559 7.000000 0.9999998 0.001191804 0.002533321 2.400000E-7
3.882 1.212058 8.430433 8.000000 1.000000 0.002348955 0.004907519 0E-7
3.948 1.277744 9.123165 9.000000 0.9999998 0.002579742 0.005772869 1.800000E-7
4.010 1.340307 9.795360 10.00000 1.000000 0.002000573 0.005222909 -1.200000E-7
4.070 1.400091 10.44898 11.00000 1.000000 0.0008910507 0.002540823 0E-7
4.127 1.457473 11.08672 12.00000 1.000000 -0.0004455092 -0.0008321319 -1.200000E-7
4.183 1.512734 11.71050 13.00000 0.9999999 -0.001640901 -0.003787351 6.000000E-8
4.236 1.566074 12.32154 14.00000 0.9999999 -0.002294024 -0.005740533 6.000000E-8
4.288 1.617718 12.92151 15.00000 1.000000 -0.002031273 -0.004911975 -1.200000E-7
4.338 1.667776 13.51093 16.00000 0.9999999 -0.0004580322 -0.001162884 1.200000E-7
4.386 1.716394 14.09079 17.00000 0.9999999 0.002809240 0.006754125 1.200000E-7
4.330 1.660002 13.41889 18.00000 0.9999999 -13.41889 -18.00000 -0.9999999
4.430 1.760000 14.61710 19.00000 1.000000 -14.61710 -19.00000 -1.000000
4.480 1.810000 15.22779 20.00000 1.000000 -15.22779 -20.00000 -1.000000
4.480 1.810000 15.22779 21.00000 1.000000 -15.22779 -21.00000 -1.000000
4.500 1.830000 15.47423 22.00000 1.000000 -15.47423 -22.00000 -1.000000
4.653 1.983199 17.40291 23.00000 1.000000 -17.40291 -23.00000 -1.000000
4.580 1.910002 16.47237 24.00000 1.000000 -16.47237 -24.00000 -1.000000
4.610 1.940002 16.85175 25.00000 1.000000 -16.85175 -25.00000 -1.000000
4.650 1.980002 17.36193 26.00000 0.9999999 -17.36193 -26.00000 -0.9999999
4.730 2.060000 18.39706 27.00000 1.000000 -18.39706 -27.00000 -1.000000
4.730 2.060000 18.39706 28.00000 1.000000 -18.39706 -28.00000 -1.000000
4.760 2.090000 18.79087 29.00000 1.000000 -18.79087 -29.00000 -1.000000
4.790 2.120001 19.18918 30.00000 1.000000 -19.18918 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528288174">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.90919308276 -9.8952200497 18.3992980319 -6.57391934336 1.28105910322</math:coefficients>
     <math:minRange>0.5378</math:minRange>
     <math:maxRange>1.7164</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952828849">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-1.07849557334 3.60923867619 5.47932168245 -2.16207217378 0.43411149633</math:coefficients>
     <math:minRange>0.5378</math:minRange>
     <math:maxRange>1.7164</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952828859">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5378</math:minRange>
     <math:maxRange>1.7164</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952828893">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7164</math:minRange>
     <math:maxRange>1.7164</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952828839">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7164</math:minRange>
     <math:maxRange>1.7164</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.5510</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951322614"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952828885">
   <gml:name>Gelesen aus: PROF0059.6000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553060.4544 5987908.2036 2.77</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation11828595282883">
     <gml:description>Übernommen aus Datei: PROF0059.6000.txt</gml:description>
     <gml:name>0059.6000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952828861">
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
     <om:result><![CDATA[3.413 0.6432750 2.000517 1.000000 1.000000 0.001265473 0.06635148 0E-7
3.595 0.8249830 3.289836 2.000000 1.000000 -0.006678231 -0.1464921 0E-7
3.718 0.9477280 4.300720 3.000000 1.000000 0.001577297 -0.08231855 0E-7
3.822 1.051664 5.219008 4.000000 0.9999999 0.01013763 0.03941230 1.200000E-7
3.913 1.143038 6.073477 5.000000 1.000000 0.005585511 0.1411146 0E-7
3.990 1.220314 6.820785 6.000000 1.000000 -0.002597628 0.1330109 -1.200000E-7
4.063 1.292504 7.530986 7.000000 1.000000 -0.007407645 0.09688643 -1.200000E-7
4.131 1.360526 8.210876 8.000000 1.000000 -0.009605748 0.03170613 0E-7
4.195 1.425077 8.865664 9.000000 1.000000 -0.008654559 -0.05983001 -2.400000E-7
4.257 1.486747 9.499939 10.00000 1.000000 -0.003289051 -0.1723529 0E-7
4.316 1.545852 10.11584 11.00000 1.000000 0.008119753 -0.3017996 0E-7
4.393 1.622676 10.93753 12.00000 1.000000 0.02726420 -0.1347151 0E-7
4.483 1.712755 11.98875 13.00000 0.9999999 0.008316103 0.3009222 6.000000E-8
4.537 1.766636 12.65712 14.00000 1.000000 -0.01277238 0.2051384 0E-7
4.586 1.816110 13.28131 15.00000 0.9999999 -0.01896334 0.07316934 1.200000E-7
4.634 1.864032 13.89531 16.00000 0.9999999 -0.009534773 -0.04501464 6.000000E-8
4.680 1.910483 14.49928 17.00000 0.9999999 0.01723738 -0.1451889 1.200000E-7
4.630 1.859999 13.84329 18.00000 1.000000 -13.84329 -18.00000 -1.000000
4.670 1.900000 14.36222 19.00000 1.000000 -14.36222 -19.00000 -1.000000
4.710 1.940000 14.88757 20.00000 1.000000 -14.88757 -20.00000 -1.000000
4.750 1.980001 15.41937 21.00000 0.9999999 -15.41937 -21.00000 -0.9999999
4.790 2.020002 15.95761 22.00000 0.9999999 -15.95761 -22.00000 -0.9999999
4.934 2.164385 17.95388 23.00000 1.000000 -17.95388 -23.00000 -1.000000
4.880 2.109999 17.19209 24.00000 1.000000 -17.19209 -24.00000 -1.000000
4.930 2.159999 17.89200 25.00000 0.9999999 -17.89200 -25.00000 -0.9999999
4.930 2.159999 17.89200 26.00000 0.9999998 -17.89200 -26.00000 -0.9999998
4.970 2.200000 18.45918 27.00000 0.9999998 -18.45918 -27.00000 -0.9999998
5.000 2.230000 18.88879 28.00000 0.9999999 -18.88879 -28.00000 -0.9999999
5.030 2.260001 19.32202 29.00000 0.9999999 -19.32202 -29.00000 -0.9999999
5.060 2.290001 19.75886 30.00000 0.9999999 -19.75886 -30.00000 -0.9999999
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952828867">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>14.8949795729 -54.0813084551 68.7871837153 -31.8866692961 5.74703595869</math:coefficients>
     <math:minRange>0.6433</math:minRange>
     <math:maxRange>1.9105</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528288178">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>2.51167088656 -11.0203989719 22.0303045411 -10.9216370324 2.16242431772</math:coefficients>
     <math:minRange>0.6433</math:minRange>
     <math:maxRange>1.9105</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528288190">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.6433</math:minRange>
     <math:maxRange>1.9105</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528288142">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9105</math:minRange>
     <math:maxRange>1.9105</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528288154">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.9105</math:minRange>
     <math:maxRange>1.9105</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.6000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951333541"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528288107">
   <gml:name>Gelesen aus: PROF0059.6520.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553086.5974 5987951.9282 2.94</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952828829">
     <gml:description>Übernommen aus Datei: PROF0059.6520.txt</gml:description>
     <gml:name>0059.6520</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528288190">
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
     <om:result><![CDATA[3.524 0.5843910 2.150636 1.000000 0.9999999 -0.009571577 0.04789663 6.000000E-8
3.685 0.7454930 3.486118 2.000000 1.000000 0.02109884 -0.1003561 -1.200000E-7
3.794 0.8536220 4.467752 3.000000 1.000000 0.004712382 -0.06147185 -2.400000E-7
3.888 0.9483140 5.343908 4.000000 0.9999998 -0.003382289 0.02592676 2.400000E-7
3.974 1.034168 6.151616 5.000000 1.000000 -0.008552181 0.09273219 0E-7
4.054 1.113574 6.909941 6.000000 0.9999999 -0.01184269 0.1134967 6.000000E-8
4.128 1.187987 7.630418 7.000000 1.000000 -0.01255629 0.08177916 0E-7
4.198 1.258400 8.320935 8.000000 0.9999999 -0.009196765 0.0008720992 1.200000E-7
4.265 1.325456 8.986458 9.000000 1.000000 0.0001325498 -0.1217488 -1.200000E-7
4.330 1.389669 9.631004 10.00000 1.000000 0.01753136 -0.2751260 0E-7
4.423 1.482709 10.60064 11.00000 0.9999999 0.04118744 -0.01835507 6.000000E-8
4.509 1.568958 11.59771 12.00000 1.000000 0.01069906 0.2124454 -1.200000E-7
4.566 1.626428 12.29526 13.00000 0.9999999 -0.01245585 0.09151923 6.000000E-8
4.622 1.681752 12.98384 14.00000 0.9999999 -0.02464343 0.001265011 6.000000E-8
4.675 1.734816 13.66006 15.00000 0.9999999 -0.02340750 -0.05054232 1.200000E-7
4.726 1.785895 14.32556 16.00000 0.9999999 -0.006753955 -0.05075936 6.000000E-8
4.775 1.835117 14.98039 17.00000 1.000000 0.02700090 0.01042624 0E-7
4.750 1.810000 14.64457 18.00000 1.000000 -14.64457 -18.00000 -1.000000
4.760 1.820000 14.77785 19.00000 0.9999999 -14.77785 -19.00000 -0.9999999
4.810 1.870001 15.45249 20.00000 1.000000 -15.45249 -20.00000 -1.000000
4.850 1.910000 16.00204 21.00000 0.9999997 -16.00204 -21.00000 -0.9999997
4.890 1.950001 16.56038 22.00000 1.000000 -16.56038 -22.00000 -1.000000
5.040 2.099688 18.72751 23.00000 0.9999999 -18.72751 -23.00000 -0.9999999
5.000 2.059999 18.14094 24.00000 0.9999999 -18.14094 -24.00000 -0.9999999
5.000 2.059999 18.14094 25.00000 0.9999999 -18.14094 -25.00000 -0.9999999
5.050 2.109999 18.88130 26.00000 0.9999999 -18.88130 -26.00000 -0.9999999
5.070 2.129999 19.18128 27.00000 0.9999998 -19.18128 -27.00000 -0.9999998
5.110 2.170000 19.78783 28.00000 1.000000 -19.78783 -28.00000 -1.000000
5.140 2.200001 20.24850 29.00000 0.9999999 -20.24850 -29.00000 -0.9999999
5.170 2.230002 20.71409 30.00000 0.9999999 -20.71409 -30.00000 -0.9999999
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528304136">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>15.6081805598 -64.9875400041 95.1467807882 -51.6217117341 10.516156758</math:coefficients>
     <math:minRange>0.5844</math:minRange>
     <math:maxRange>1.8351</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952830478">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-1.0893577471 1.49348143101 9.85049808078 -5.9007352126 1.46808877306</math:coefficients>
     <math:minRange>0.5844</math:minRange>
     <math:maxRange>1.8351</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528304127">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5844</math:minRange>
     <math:maxRange>1.8351</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952830415">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.8351</math:minRange>
     <math:maxRange>1.8351</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528304145">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.8351</math:minRange>
     <math:maxRange>1.8351</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.6520</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951335110"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952830468">
   <gml:name>Gelesen aus: PROF0059.7000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553111.1638 5987993.6385 3.04</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528304132">
     <gml:description>Übernommen aus Datei: PROF0059.7000.txt</gml:description>
     <gml:name>0059.7000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528304182">
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
     <om:result><![CDATA[3.486 0.4455590 2.198083 1.000000 0.9999999 -0.001773459 0.002727823 6.000000E-8
3.635 0.5954850 3.479247 2.000000 1.000000 0.005783164 -0.01414878 -1.200000E-7
3.747 0.7068170 4.528123 3.000000 0.9999998 -0.0005294675 0.01811050 1.800000E-7
3.836 0.7956860 5.400342 4.000000 1.000000 -0.005386075 0.0001703647 0E-7
3.916 0.8760210 6.201109 5.000000 1.000000 -0.003094687 -0.004787349 -2.400000E-7
3.990 0.9501770 6.950678 6.000000 0.9999999 0.0004790981 -0.003956945 6.000000E-8
4.060 1.019529 7.660710 7.000000 0.9999999 0.002907018 -0.001685066 1.200000E-7
4.125 1.085056 8.339582 8.000000 0.9999999 0.003451048 0.0007694711 1.200000E-7
4.187 1.147362 8.992311 9.000000 1.000000 0.002257731 0.001798167 0E-7
4.247 1.206966 9.623317 10.00000 1.000000 0.00003092222 0.001843030 -2.400000E-7
4.304 1.264208 10.23538 11.00000 1.000000 -0.002220543 0.0007524343 0E-7
4.359 1.319377 10.83089 12.00000 1.000000 -0.003304639 -0.0008751379 -1.200000E-7
4.413 1.372772 11.41251 13.00000 0.9999998 -0.001914080 -0.0009585968 1.800000E-7
4.465 1.424515 11.98107 14.00000 0.9999998 0.003313969 0.0002400794 2.400000E-7
3.908 0.8682480 6.123123 15.00000 0.9999999 -6.123123 -15.00000 -0.9999999
3.936 0.8959870 6.401948 16.00000 1.000000 -6.401948 -16.00000 -1.000000
4.675 1.634631 14.59641 17.00000 1.000000 -14.59641 -17.00000 -1.000000
4.620 1.580001 13.87959 18.00000 0.9999998 -13.87959 -18.00000 -0.9999998
4.660 1.620002 14.40334 19.00000 0.9999999 -14.40334 -19.00000 -0.9999999
4.750 1.710000 15.60409 20.00000 1.000000 -15.60409 -20.00000 -1.000000
4.750 1.710000 15.60409 21.00000 1.000000 -15.60409 -21.00000 -1.000000
4.780 1.740000 16.01126 22.00000 1.000000 -16.01126 -22.00000 -1.000000
4.928 1.887778 18.07224 23.00000 1.000000 -18.07224 -23.00000 -1.000000
4.850 1.810000 16.97583 24.00000 1.000000 -16.97583 -24.00000 -1.000000
4.890 1.850001 17.53647 25.00000 1.000000 -17.53647 -25.00000 -1.000000
4.920 1.880002 17.96146 26.00000 1.000000 -17.96146 -26.00000 -1.000000
5.000 1.959999 19.11359 27.00000 0.9999999 -19.11359 -27.00000 -0.9999999
5.000 1.959999 19.11359 28.00000 0.9999999 -19.11359 -28.00000 -0.9999999
5.030 1.990000 19.55276 29.00000 1.000000 -19.55276 -29.00000 -1.000000
5.060 2.020000 19.99579 30.00000 1.000000 -19.99579 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952832023">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.90886194265 -10.3099462339 22.1493707678 -8.81292144263 1.7744781036</math:coefficients>
     <math:minRange>0.4456</math:minRange>
     <math:maxRange>1.4245</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952832076">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.093390854298 1.17416616425 11.642838991 -6.88126242355 1.61992777489</math:coefficients>
     <math:minRange>0.4456</math:minRange>
     <math:maxRange>1.4245</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952832065">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4456</math:minRange>
     <math:maxRange>1.4245</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528320178">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4245</math:minRange>
     <math:maxRange>1.4245</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952832046">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4245</math:minRange>
     <math:maxRange>1.4245</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.7000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951346047"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952832086">
   <gml:name>Gelesen aus: PROF0059.8000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553158.1544 5988081.3086 2.72</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952832085">
     <gml:description>Übernommen aus Datei: PROF0059.8000.txt</gml:description>
     <gml:name>0059.8000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952832033">
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
     <om:result><![CDATA[3.159 0.4388590 1.951108 1.000000 1.000000 0.004482517 -0.002431385 -1.200000E-7
3.343 0.6229890 3.130516 2.000000 1.000000 -0.01245132 0.01261223 0E-7
3.481 0.7612430 4.153709 3.000000 1.000000 -0.001337908 -0.009499800 -2.400000E-7
3.596 0.8755140 5.088523 4.000000 1.000000 0.008070731 -0.01283551 0E-7
3.693 0.9734400 5.952195 5.000000 1.000000 0.01011924 -0.005967604 -1.200000E-7
3.781 1.060795 6.767963 6.000000 0.9999999 0.006042209 0.01653374 6.000000E-8
3.858 1.137935 7.521866 7.000000 1.000000 -0.003032138 0.01703195 -2.400000E-7
3.928 1.207821 8.221845 8.000000 1.000000 -0.007364150 0.0002414538 0E-7
3.994 1.273980 8.897766 9.000000 0.9999999 -0.007795036 -0.005114024 6.000000E-8
4.057 1.336957 9.553155 10.00000 0.9999999 -0.005881279 -0.004738712 1.200000E-7
4.117 1.397223 10.19129 11.00000 1.000000 -0.002725369 -0.003139474 -1.200000E-7
4.175 1.455060 10.81376 12.00000 0.9999999 0.0009342414 -0.006126377 6.000000E-8
4.231 1.510758 11.42252 13.00000 0.9999999 0.004557622 -0.01799549 6.000000E-8
4.287 1.566921 12.04637 14.00000 1.000000 0.007184468 0.0005131066 -1.200000E-7
4.342 1.622444 12.67538 15.00000 1.000000 0.006027421 0.02306200 -2.400000E-7
4.396 1.675978 13.29359 16.00000 1.000000 0.0009629696 0.01734747 0E-7
4.448 1.727756 13.90247 17.00000 0.9999999 -0.007794217 -0.01949359 6.000000E-8
4.390 1.670000 13.22398 18.00000 1.000000 -13.22398 -18.00000 -1.000000
4.440 1.720001 13.81060 19.00000 1.000000 -13.81060 -19.00000 -1.000000
4.480 1.760001 14.28711 20.00000 1.000000 -14.28711 -20.00000 -1.000000
4.520 1.800002 14.77007 21.00000 0.9999999 -14.77007 -21.00000 -0.9999999
4.630 1.910000 16.13133 22.00000 1.000000 -16.13133 -22.00000 -1.000000
4.729 2.008600 17.39289 23.00000 1.000000 -17.39289 -23.00000 -1.000000
4.650 1.930000 16.38407 24.00000 1.000000 -16.38407 -24.00000 -1.000000
4.690 1.970000 16.89435 25.00000 1.000000 -16.89435 -25.00000 -1.000000
4.720 2.000000 17.28130 26.00000 0.9999999 -17.28130 -26.00000 -0.9999999
4.760 2.040001 17.80286 27.00000 1.000000 -17.80286 -27.00000 -1.000000
4.800 2.080002 18.33085 28.00000 1.000000 -18.33085 -28.00000 -1.000000
4.880 2.159999 19.40608 29.00000 0.9999998 -19.40608 -29.00000 -0.9999998
4.930 2.209999 20.09118 30.00000 1.000000 -20.09118 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952841382">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-1.42181718697 7.84395285613 -9.8310760385 11.5470393992 -2.84566625865</math:coefficients>
     <math:minRange>0.4389</math:minRange>
     <math:maxRange>1.7278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11828595284132">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.473516457076 0.863208467536 6.51408358627 -1.90411935892 0.258663385755</math:coefficients>
     <math:minRange>0.4389</math:minRange>
     <math:maxRange>1.7278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528429149">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4389</math:minRange>
     <math:maxRange>1.7278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952842987">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7278</math:minRange>
     <math:maxRange>1.7278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528429192">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7278</math:minRange>
     <math:maxRange>1.7278</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.8000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951358547"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528429132">
   <gml:name>Gelesen aus: PROF0059.9000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553209.8569 5988169.6769 2.96</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528429144">
     <gml:description>Übernommen aus Datei: PROF0059.9000.txt</gml:description>
     <gml:name>0059.9000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952842942">
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
     <om:result><![CDATA[3.355 0.3951990 2.157012 1.000000 1.000000 0.002172021 -0.003586318 -1.200000E-7
3.505 0.5451960 3.321480 2.000000 0.9999998 -0.005301825 0.008928462 1.800000E-7
3.623 0.6634100 4.308493 3.000000 0.9999999 -0.001226355 -0.0003540793 6.000000E-8
3.724 0.7643920 5.200009 4.000000 0.9999999 0.003320500 -0.005519110 1.200000E-7
3.814 0.8540140 6.028560 5.000000 1.000000 0.004839069 -0.002777116 -2.400000E-7
3.895 0.9353570 6.810974 6.000000 1.000000 0.002026302 0.006269811 0E-7
3.969 1.008708 7.537722 7.000000 1.000000 -0.001713091 -0.003595691 -2.400000E-7
4.038 1.077527 8.233539 8.000000 1.000000 -0.002908195 -0.006935670 -1.200000E-7
4.103 1.142750 8.905476 9.000000 0.9999999 -0.002670441 -0.003910255 6.000000E-8
4.165 1.204869 9.556710 10.00000 1.000000 -0.001711875 0.001843748 -1.200000E-7
4.224 1.264301 10.19008 11.00000 0.9999998 -0.0005255778 0.007530604 1.800000E-7
4.281 1.321426 10.80836 12.00000 1.000000 0.0005721983 0.01142194 -2.400000E-7
4.336 1.376444 11.41263 13.00000 1.000000 0.001383057 0.009919614 0E-7
4.390 1.429582 12.00445 14.00000 1.000000 0.001777515 0.0008455963 0E-7
4.441 1.481045 12.58530 15.00000 0.9999999 0.001682068 -0.01753349 6.000000E-8
4.492 1.532066 13.16870 16.00000 0.9999999 0.0009927662 -0.02609196 6.000000E-8
4.545 1.585267 13.78733 17.00000 1.000000 -0.002708136 0.02354392 0E-7
4.490 1.530001 13.14492 18.00000 1.000000 -13.14492 -18.00000 -1.000000
4.530 1.570002 13.60865 19.00000 0.9999999 -13.60865 -19.00000 -0.9999999
4.620 1.659999 14.67712 20.00000 0.9999999 -14.67712 -20.00000 -0.9999999
4.620 1.659999 14.67712 21.00000 1.000000 -14.67712 -21.00000 -1.000000
4.670 1.710001 15.28983 22.00000 0.9999998 -15.28983 -22.00000 -0.9999998
4.849 1.889338 17.60666 23.00000 1.000000 -17.60666 -23.00000 -1.000000
4.760 1.800002 16.42923 24.00000 0.9999999 -16.42923 -24.00000 -0.9999999
4.870 1.909999 17.88556 25.00000 1.000000 -17.88556 -25.00000 -1.000000
4.870 1.909999 17.88556 26.00000 0.9999999 -17.88556 -26.00000 -0.9999999
4.880 1.920000 18.02144 27.00000 0.9999999 -18.02144 -27.00000 -0.9999999
4.920 1.960001 18.57075 28.00000 0.9999999 -18.57075 -28.00000 -0.9999999
4.960 2.000001 19.12935 29.00000 1.000000 -19.12935 -29.00000 -1.000000
5.000 2.040001 19.69719 30.00000 1.000000 -19.69719 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528429200">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.778736192379 3.70979265965 0.0694724864081 5.45632184794 -1.58192677073</math:coefficients>
     <math:minRange>0.3952</math:minRange>
     <math:maxRange>1.5853</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528429218">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.227508363567 4.61358941505 4.03612903113 -1.1580771481 0.185087703063</math:coefficients>
     <math:minRange>0.3952</math:minRange>
     <math:maxRange>1.5853</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952842945">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3952</math:minRange>
     <math:maxRange>1.5853</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528429199">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5853</math:minRange>
     <math:maxRange>1.5853</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528429119">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5853</math:minRange>
     <math:maxRange>1.5853</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>59.9000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951360136"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952842940">
   <gml:name>Gelesen aus: PROF0060.0000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553279.0441 5988241.8846 2.47</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation11828595284290">
     <gml:description>Übernommen aus Datei: PROF0060.0000.txt</gml:description>
     <gml:name>0060.0000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528429109">
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
     <om:result><![CDATA[3.100 0.6302880 1.853259 1.000000 0.9999999 4.512458E-9 -0.0003698131 1.200000E-7
3.292 0.8223860 2.889611 2.000000 1.000000 0.000001045255 0.001002022 0E-7
3.444 0.9739100 3.777387 3.000000 1.000000 -0.000002619882 -0.00006292626 -1.200000E-7
3.573 1.103293 4.584514 4.000000 1.000000 7.278267E-7 -0.0005642157 0E-7
3.688 1.218026 5.338078 5.000000 1.000000 -5.895374E-7 -0.0005174465 0E-7
3.792 1.322088 6.052298 6.000000 1.000000 0.000002634702 -0.0002094066 0E-7
3.888 1.417904 6.735789 7.000000 1.000000 0.000002078213 0.00009020293 0E-7
3.977 1.507097 7.394324 8.000000 1.000000 -0.000002309549 0.0002950551 -1.200000E-7
4.061 1.590823 8.032040 9.000000 1.000000 -1.764264E-7 0.0004846698 -2.400000E-7
4.140 1.669908 8.651799 10.00000 0.9999999 -0.000002072719 0.0004083183 1.200000E-7
4.215 1.745009 9.255971 11.00000 0.9999998 -4.940720E-7 0.0002027889 2.400000E-7
4.287 1.816623 9.846279 12.00000 0.9999999 0.000001660817 -0.0002514457 1.200000E-7
4.355 1.885206 10.42459 13.00000 1.000000 -0.000002955156 -0.0002082198 -1.200000E-7
4.421 1.951039 10.99166 14.00000 1.000000 0.000004635324 -0.0003562362 -1.200000E-7
4.484 2.014419 11.54867 15.00000 1.000000 -0.000002086682 -0.0003395327 0E-7
4.546 2.075581 12.09646 16.00000 1.000000 0.000002228316 -0.00005381526 0E-7
4.605 2.134716 12.63571 17.00000 1.000000 -0.000001710942 0.0004500007 0E-7
4.540 2.070000 12.04605 18.00000 0.9999999 -12.04605 -18.00000 -0.9999999
4.590 2.120001 12.50064 19.00000 0.9999999 -12.50064 -19.00000 -0.9999999
4.640 2.170001 12.96196 20.00000 1.000000 -12.96196 -20.00000 -1.000000
4.690 2.220002 13.43005 21.00000 1.000000 -13.43005 -21.00000 -1.000000
4.780 2.309999 14.29266 22.00000 0.9999999 -14.29266 -22.00000 -0.9999999
4.988 2.518236 16.46137 23.00000 0.9999999 -16.46137 -23.00000 -0.9999999
4.860 2.390001 15.09387 24.00000 0.9999999 -15.09387 -24.00000 -0.9999999
4.920 2.450001 15.72095 25.00000 0.9999998 -15.72095 -25.00000 -0.9999998
5.030 2.559999 16.92886 26.00000 1.000000 -16.92886 -26.00000 -1.000000
5.030 2.559999 16.92886 27.00000 0.9999999 -16.92886 -27.00000 -0.9999999
5.080 2.610000 17.50288 28.00000 0.9999999 -17.50288 -28.00000 -0.9999999
5.130 2.660001 18.09249 29.00000 0.9999999 -18.09249 -29.00000 -0.9999999
5.180 2.710001 18.69768 30.00000 1.000000 -18.69768 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952844594">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.186257624837 -0.722952097821 4.22782891878 -0.192272441269 0.0642456560932</math:coefficients>
     <math:minRange>0.6303</math:minRange>
     <math:maxRange>2.1347</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952844590">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.847130382903 3.43326160115 1.35031827478 6.49692232908E-5 -1.208392215E-5</math:coefficients>
     <math:minRange>0.6303</math:minRange>
     <math:maxRange>2.1347</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528445138">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.6303</math:minRange>
     <math:maxRange>2.1347</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528445151">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>2.1347</math:minRange>
     <math:maxRange>2.1347</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528445205">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>2.1347</math:minRange>
     <math:maxRange>2.1347</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.0000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951371054"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528445109">
   <gml:name>Gelesen aus: PROF0060.0560.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553277.0202 5988299.2696 3.1</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952844534">
     <gml:description>Übernommen aus Datei: PROF0060.0560.txt</gml:description>
     <gml:name>0060.0560</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952844520">
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
     <om:result><![CDATA[3.497 0.3966590 2.247009 1.000000 1.000000 -0.00003156118 -0.005817895 0E-7
3.636 0.5360520 3.428184 2.000000 1.000000 0.0001828921 0.01164901 -1.200000E-7
3.746 0.6461420 4.407127 3.000000 0.9999998 -0.0001733175 0.004149872 1.800000E-7
3.841 0.7411760 5.283802 4.000000 0.9999998 -0.0001355007 -0.003129731 2.400000E-7
3.926 0.8262680 6.093615 5.000000 1.000000 -0.000005130980 -0.007142807 0E-7
4.004 0.9041440 6.855335 6.000000 1.000000 0.00009905803 -0.007393967 -1.200000E-7
4.076 0.9763910 7.579589 7.000000 1.000000 0.0001360949 -0.005327907 0E-7
4.144 1.044165 8.274384 8.000000 1.000000 0.0001162909 -0.001039660 0E-7
4.208 1.108163 8.944147 9.000000 0.9999999 0.00004484118 0.003319805 1.200000E-7
4.269 1.169000 9.593141 10.00000 1.000000 -0.00004706030 0.007399984 -1.200000E-7
4.327 1.227080 10.22392 11.00000 1.000000 -0.0001319338 0.009697827 -1.200000E-7
4.383 1.282749 10.83878 12.00000 1.000000 -0.0001792730 0.009335734 0E-7
4.436 1.336282 11.43954 13.00000 1.000000 -0.0001638706 0.005449500 -1.200000E-7
4.488 1.387937 12.02804 14.00000 1.000000 -0.00005947523 -0.002113216 -2.400000E-7
4.538 1.437826 12.60462 15.00000 1.000000 0.0001893558 -0.01553433 0E-7
4.586 1.486180 13.17117 16.00000 1.000000 0.0005882403 -0.03404476 -1.200000E-7
4.637 1.537353 13.78067 17.00000 0.9999999 -0.0004296501 0.03054254 1.200000E-7
4.580 1.480000 13.09833 18.00000 0.9999999 -13.09833 -18.00000 -0.9999999
4.620 1.520001 13.57261 19.00000 0.9999998 -13.57261 -19.00000 -0.9999998
4.670 1.570002 14.17600 20.00000 1.000000 -14.17600 -20.00000 -1.000000
4.760 1.659999 15.29184 21.00000 1.000000 -15.29184 -21.00000 -1.000000
4.760 1.659999 15.29184 22.00000 1.000000 -15.29184 -22.00000 -1.000000
4.915 1.814560 17.29753 23.00000 1.000000 -17.29753 -23.00000 -1.000000
4.830 1.730000 16.18622 24.00000 0.9999998 -16.18622 -24.00000 -0.9999998
4.870 1.770001 16.70771 25.00000 1.000000 -16.70771 -25.00000 -1.000000
4.910 1.810002 17.23677 26.00000 1.000000 -17.23677 -26.00000 -1.000000
5.010 1.909999 18.59245 27.00000 0.9999999 -18.59245 -27.00000 -0.9999999
5.010 1.909999 18.59245 28.00000 1.000000 -18.59245 -28.00000 -1.000000
5.020 1.920000 18.73062 29.00000 0.9999999 -18.73062 -29.00000 -0.9999999
5.060 1.959999 19.28803 30.00000 0.9999998 -19.28803 -30.00000 -0.9999998
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528554181">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.530625997273 1.50271121225 5.38868415306 1.50627728381 -0.529535533828</math:coefficients>
     <math:minRange>0.3967</math:minRange>
     <math:maxRange>1.5374</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528554195">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.751199014858 6.85861617564 1.81222479106 -0.133392538767 0.0338158954613</math:coefficients>
     <math:minRange>0.3967</math:minRange>
     <math:maxRange>1.5374</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952855439">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3967</math:minRange>
     <math:maxRange>1.5374</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528554124">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5374</math:minRange>
     <math:maxRange>1.5374</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528554192">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5374</math:minRange>
     <math:maxRange>1.5374</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.0560</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951383537"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952855486">
   <gml:name>Gelesen aus: PROF0060.1000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553254.2697 5988333.2626 3.14</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952855449">
     <gml:description>Übernommen aus Datei: PROF0060.1000.txt</gml:description>
     <gml:name>0060.1000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952855481">
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
     <om:result><![CDATA[3.602 0.4616180 2.197847 1.000000 0.9999999 -0.004271364 -0.01838107 6.000000E-8
3.756 0.6163560 3.516761 2.000000 1.000000 0.01445752 0.04308349 0E-7
3.863 0.7225350 4.534114 3.000000 1.000000 -0.004563539 0.01315818 -1.200000E-7
3.952 0.8117820 5.413807 4.000000 0.9999998 -0.007615715 -0.01728532 1.800000E-7
4.032 0.8920660 6.222812 5.000000 0.9999999 -0.004830643 -0.02732111 6.000000E-8
4.106 0.9658260 6.980802 6.000000 1.000000 -0.0009259837 -0.02360023 -1.200000E-7
4.175 1.034550 7.699745 7.000000 1.000000 0.002104649 -0.01206359 0E-7
4.239 1.099205 8.387306 8.000000 0.9999999 0.003623435 0.001657469 1.200000E-7
4.300 1.160474 9.048856 9.000000 1.000000 0.003704790 0.01254813 -2.400000E-7
4.359 1.218914 9.688945 10.00000 1.000000 0.002782714 0.01704004 -1.200000E-7
4.415 1.275054 10.31218 11.00000 1.000000 0.001545632 0.01375547 -1.200000E-7
4.471 1.330996 10.94251 12.00000 1.000000 -0.0003508904 0.03234408 0E-7
4.525 1.384896 11.55939 13.00000 0.9999998 -0.002551246 0.03209445 1.800000E-7
4.577 1.436897 12.16340 14.00000 1.000000 -0.003831462 0.008123184 -1.200000E-7
4.627 1.487237 12.75643 15.00000 0.9999999 -0.003009942 -0.04168376 1.200000E-7
4.676 1.536033 13.33906 16.00000 0.9999999 0.001077146 -0.1208922 1.200000E-7
4.741 1.600576 14.13283 17.00000 0.9999999 0.002654896 0.08742278 1.200000E-7
4.670 1.530000 13.26661 18.00000 0.9999999 -13.26661 -18.00000 -0.9999999
4.720 1.580001 13.87556 19.00000 1.000000 -13.87556 -19.00000 -1.000000
4.780 1.640003 14.63691 20.00000 1.000000 -14.63691 -20.00000 -1.000000
4.850 1.709999 15.55808 21.00000 1.000000 -15.55808 -21.00000 -1.000000
4.900 1.759999 16.22822 22.00000 1.000000 -16.22822 -22.00000 -1.000000
5.009 1.869029 17.72407 23.00000 1.000000 -17.72407 -23.00000 -1.000000
4.940 1.800000 16.77152 24.00000 1.000000 -16.77152 -24.00000 -1.000000
4.970 1.830001 17.18318 25.00000 1.000000 -17.18318 -25.00000 -1.000000
5.010 1.870002 17.73763 26.00000 0.9999999 -17.73763 -26.00000 -0.9999999
5.100 1.959999 19.00838 27.00000 1.000000 -19.00838 -27.00000 -1.000000
5.070 1.930002 18.58124 28.00000 1.000000 -18.58124 -28.00000 -1.000000
5.150 2.009999 19.72831 29.00000 1.000000 -19.72831 -29.00000 -1.000000
5.150 2.009999 19.72831 30.00000 1.000000 -19.72831 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952855444">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.159216555405 0.296179506935 2.24533143047 6.42036325625 -2.33212363684</math:coefficients>
     <math:minRange>0.4616</math:minRange>
     <math:maxRange>1.6006</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528554103">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.321980766238 1.90486813558 9.89835407632 -5.37886522387 1.23512566744</math:coefficients>
     <math:minRange>0.4616</math:minRange>
     <math:maxRange>1.6006</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528554126">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4616</math:minRange>
     <math:maxRange>1.6006</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952855468">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6006</math:minRange>
     <math:maxRange>1.6006</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952855427">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.6006</math:minRange>
     <math:maxRange>1.6006</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.1000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951394524"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528554165">
   <gml:name>Gelesen aus: PROF0060.1520.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553219.0821 5988371.4247 3.09</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952855461">
     <gml:description>Übernommen aus Datei: PROF0060.1520.txt</gml:description>
     <gml:name>0060.1520</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952855497">
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
     <om:result><![CDATA[3.474 0.3842610 2.311481 1.000000 1.000000 -0.00009897866 0.003026389 0E-7
3.610 0.5199310 3.543257 2.000000 1.000000 0.0002738075 -0.006855666 0E-7
3.716 0.6258990 4.572301 3.000000 0.9999998 -0.0002838927 0.001100569 2.400000E-7
3.805 0.7145200 5.465287 4.000000 0.9999999 0.0002373305 -0.002696944 1.200000E-7
3.884 0.7940880 6.287573 5.000000 0.9999999 0.0004354475 0.005898483 6.000000E-8
3.956 0.8657700 7.042953 6.000000 1.000000 -0.0006739963 0.002617191 0E-7
4.022 0.9324400 7.753786 7.000000 1.000000 -0.0004374519 0.0004832527 -1.200000E-7
4.085 0.9953890 8.432113 8.000000 1.000000 0.00006455882 0.001442264 0E-7
4.145 1.055266 9.083807 9.000000 1.000000 0.0003080832 0.002837108 0E-7
4.203 1.112527 9.712919 10.00000 0.9999999 0.0002263553 0.001881679 6.000000E-8
4.258 1.167526 10.32261 11.00000 1.000000 -0.000002282935 -0.003836194 0E-7
4.311 1.220562 10.91559 12.00000 1.000000 -0.000004984527 -0.01611380 -1.200000E-7
4.363 1.273093 11.50804 13.00000 0.9999998 0.0004682605 -0.01310018 1.800000E-7
4.416 1.325575 12.10705 14.00000 0.9999999 0.0001192870 0.01008077 6.000000E-8
4.466 1.376450 12.69491 15.00000 1.000000 -0.0006053663 0.01863359 -1.200000E-7
4.516 1.425867 13.27269 16.00000 0.9999999 -0.0007189562 0.01067749 6.000000E-8
4.564 1.473925 13.84099 17.00000 0.9999999 0.0006927791 -0.01607601 1.200000E-7
4.510 1.420000 13.20374 18.00000 1.000000 -13.20374 -18.00000 -1.000000
4.550 1.460000 13.67567 19.00000 0.9999999 -13.67567 -19.00000 -0.9999999
4.590 1.500001 14.15197 20.00000 0.9999999 -14.15197 -20.00000 -0.9999999
4.640 1.550002 14.75433 21.00000 0.9999999 -14.75433 -21.00000 -0.9999999
4.680 1.590003 15.24294 22.00000 1.000000 -15.24294 -22.00000 -1.000000
4.840 1.749858 17.25567 23.00000 1.000000 -17.25567 -23.00000 -1.000000
4.760 1.670000 16.23817 24.00000 1.000000 -16.23817 -24.00000 -1.000000
4.800 1.710001 16.74484 25.00000 1.000000 -16.74484 -25.00000 -1.000000
4.840 1.750000 17.25751 26.00000 0.9999999 -17.25751 -26.00000 -0.9999999
4.870 1.780001 17.64597 27.00000 1.000000 -17.64597 -27.00000 -1.000000
4.910 1.820002 18.16919 28.00000 1.000000 -18.16919 -28.00000 -1.000000
5.000 1.909999 19.36838 29.00000 1.000000 -19.36838 -29.00000 -1.000000
5.000 1.909999 19.36838 30.00000 1.000000 -19.36838 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528570126">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.0318835167538 -0.86074086153 8.32495943215 1.71726585946 -1.13646972525</math:coefficients>
     <math:minRange>0.3843</math:minRange>
     <math:maxRange>1.4739</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528570203">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.328726691631 4.5768605813 7.43838894052 -4.20950724488 1.00515374718</math:coefficients>
     <math:minRange>0.3843</math:minRange>
     <math:maxRange>1.4739</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528570177">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3843</math:minRange>
     <math:maxRange>1.4739</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528570136">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4739</math:minRange>
     <math:maxRange>1.4739</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952857098">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4739</math:minRange>
     <math:maxRange>1.4739</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.1520</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951396035"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952857028">
   <gml:name>Gelesen aus: PROF0060.2000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553186.7416 5988407.8925 3.13</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952857087">
     <gml:description>Übernommen aus Datei: PROF0060.2000.txt</gml:description>
     <gml:name>0060.2000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528570133">
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
     <om:result><![CDATA[3.491 0.3612050 2.297122 1.000000 0.9999999 0.0005513183 -0.003702907 6.000000E-8
3.626 0.4959640 3.488455 2.000000 1.000000 -0.0007511339 0.008695635 -1.200000E-7
3.733 0.6033850 4.484865 3.000000 1.000000 -0.0009469370 0.002251695 0E-7
3.826 0.6959930 5.377165 4.000000 0.9999999 -0.0002694522 -0.004665186 6.000000E-8
3.909 0.7788630 6.201773 5.000000 1.000000 0.0007434910 -0.007444140 0E-7
3.985 0.8546180 6.977178 6.000000 0.9999999 0.001555833 -0.005797196 6.000000E-8
4.055 0.9248630 7.714618 7.000000 1.000000 0.001675029 -0.0003693505 -1.200000E-7
4.121 0.9906920 8.421795 8.000000 0.9999999 0.0006684864 0.008261571 6.000000E-8
4.182 1.052336 9.097857 9.000000 1.000000 -0.001545943 0.01033059 0E-7
4.240 1.110252 9.742316 10.00000 1.000000 -0.002381138 0.001738972 0E-7
4.296 1.165821 10.36835 11.00000 1.000000 -0.001821132 -0.002386162 0E-7
4.349 1.219278 10.97770 12.00000 1.000000 -0.0005621211 -0.003982409 -1.200000E-7
4.401 1.270874 11.57245 13.00000 0.9999998 0.0008203900 -0.003689624 1.800000E-7
4.451 1.320830 12.15448 14.00000 1.000000 0.001790378 -0.001815150 -1.200000E-7
4.499 1.369251 12.72445 15.00000 0.9999999 0.001885858 -0.0003155277 6.000000E-8
4.546 1.416323 13.28402 16.00000 0.9999998 0.0007090258 0.001046737 1.800000E-7
4.592 1.462175 13.83429 17.00000 1.000000 -0.002121952 0.001842452 0E-7
4.540 1.410000 13.20855 18.00000 1.000000 -13.20855 -18.00000 -1.000000
4.580 1.450001 13.68769 19.00000 1.000000 -13.68769 -19.00000 -1.000000
4.620 1.490000 14.17072 20.00000 1.000000 -14.17072 -20.00000 -1.000000
4.660 1.530001 14.65798 21.00000 1.000000 -14.65798 -21.00000 -1.000000
4.790 1.659999 16.32591 22.00000 0.9999998 -16.32591 -22.00000 -0.9999998
4.890 1.759740 17.69521 23.00000 1.000000 -17.69521 -23.00000 -1.000000
4.810 1.680000 16.59659 24.00000 1.000000 -16.59659 -24.00000 -1.000000
4.850 1.720001 17.14382 25.00000 0.9999997 -17.14382 -25.00000 -0.9999997
4.890 1.760002 17.69887 26.00000 1.000000 -17.69887 -26.00000 -1.000000
4.990 1.859999 19.12068 27.00000 0.9999999 -19.12068 -27.00000 -0.9999999
4.960 1.830002 18.68903 28.00000 0.9999999 -18.68903 -28.00000 -0.9999999
5.040 1.909999 19.84994 29.00000 1.000000 -19.84994 -29.00000 -1.000000
5.040 1.909999 19.84994 30.00000 1.000000 -19.84994 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528585142">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.544689132842 2.15527222576 5.22083814664 1.95276172501 -0.628161581019</math:coefficients>
     <math:minRange>0.3612</math:minRange>
     <math:maxRange>1.4622</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952858518">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.586828897226 7.434024231 1.35556583112 0.563957866868 -0.243271867018</math:coefficients>
     <math:minRange>0.3612</math:minRange>
     <math:maxRange>1.4622</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952858558">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3612</math:minRange>
     <math:maxRange>1.4622</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528585126">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4622</math:minRange>
     <math:maxRange>1.4622</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952858534">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4622</math:minRange>
     <math:maxRange>1.4622</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.2000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951407042"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528585200">
   <gml:name>Gelesen aus: PROF0060.3000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553115.4608 5988474.0839 3.12</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952858585">
     <gml:description>Übernommen aus Datei: PROF0060.3000.txt</gml:description>
     <gml:name>0060.3000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952858571">
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
     <om:result><![CDATA[3.737 0.6174040 2.026544 1.000000 0.9999999 0.005248759 -0.003358847 1.200000E-7
3.914 0.7940780 3.299040 2.000000 0.9999999 -0.01551336 0.01935226 6.000000E-8
4.043 0.9234220 4.404889 3.000000 1.000000 -0.001750622 -0.01642798 -1.200000E-7
4.149 1.028912 5.415849 4.000000 1.000000 0.01159524 -0.02510264 -1.200000E-7
4.239 1.119491 6.362086 5.000000 1.000000 0.01496584 -0.001090484 -2.400000E-7
4.319 1.198814 7.249908 6.000000 1.000000 0.005476595 0.03628135 0E-7
4.386 1.265841 8.031824 7.000000 1.000000 -0.004437207 0.01733188 -2.400000E-7
4.448 1.327745 8.772421 8.000000 0.9999999 -0.01086407 0.007574309 6.000000E-8
4.505 1.384563 9.462570 9.000000 0.9999998 -0.01165854 -0.01461239 2.400000E-7
4.559 1.438862 10.12963 10.00000 0.9999999 -0.008095697 -0.02153503 6.000000E-8
4.611 1.491032 10.77746 11.00000 0.9999998 -0.002520692 -0.01704683 1.800000E-7
4.661 1.541283 11.40786 12.00000 1.000000 0.003209824 -0.006888582 0E-7
4.710 1.589866 12.02332 13.00000 0.9999998 0.007588066 0.005164898 1.800000E-7
4.757 1.636908 12.62488 14.00000 0.9999999 0.009330953 0.01376276 6.000000E-8
4.803 1.682621 13.21471 15.00000 1.000000 0.007417168 0.01617143 -1.200000E-7
4.847 1.727070 13.79322 16.00000 1.000000 0.0009261138 0.007149452 0E-7
4.890 1.770378 14.36163 17.00000 0.9999999 -0.01091837 -0.01672555 1.200000E-7
4.840 1.720001 13.70088 18.00000 0.9999999 -13.70088 -18.00000 -0.9999999
4.880 1.760002 14.22501 19.00000 0.9999999 -14.22501 -19.00000 -0.9999999
4.980 1.859999 15.55270 20.00000 1.000000 -15.55270 -20.00000 -1.000000
4.980 1.859999 15.55270 21.00000 0.9999999 -15.55270 -21.00000 -0.9999999
5.030 1.909999 16.22801 22.00000 1.000000 -16.22801 -22.00000 -1.000000
5.139 2.018602 17.73090 23.00000 1.000000 -17.73090 -23.00000 -1.000000
5.060 1.939999 16.63824 24.00000 1.000000 -16.63824 -24.00000 -1.000000
5.100 1.980000 17.19107 25.00000 1.000000 -17.19107 -25.00000 -1.000000
5.140 2.020001 17.75059 26.00000 0.9999999 -17.75059 -26.00000 -0.9999999
5.170 2.050001 18.17461 27.00000 1.000000 -18.17461 -27.00000 -1.000000
5.200 2.080001 18.60242 28.00000 0.9999999 -18.60242 -28.00000 -0.9999999
5.280 2.159998 19.76158 29.00000 1.000000 -19.76158 -29.00000 -1.000000
5.280 2.159998 19.76158 30.00000 1.000000 -19.76158 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528695181">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-5.23936842047 21.3831678819 -30.597612941 22.9013187286 -4.76492146563</math:coefficients>
     <math:minRange>0.6174</math:minRange>
     <math:maxRange>1.7704</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528695100">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.798298623229 -2.54836840541 8.01608717027 -0.941683825488 -0.186812106337</math:coefficients>
     <math:minRange>0.6174</math:minRange>
     <math:maxRange>1.7704</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952869514">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.6174</math:minRange>
     <math:maxRange>1.7704</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952869581">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7704</math:minRange>
     <math:maxRange>1.7704</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952869579">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7704</math:minRange>
     <math:maxRange>1.7704</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.3000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951417915"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528695240">
   <gml:name>Gelesen aus: PROF0060.4000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553054.338 5988559.8399 3.22</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952871014">
     <gml:description>Übernommen aus Datei: PROF0060.4000.txt</gml:description>
     <gml:name>0060.4000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528710253">
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
     <om:result><![CDATA[3.845 0.6253720 2.040078 1.000000 1.000000 -0.001367844 0.001740643 0E-7
4.023 0.8034680 3.367500 2.000000 0.9999998 0.003359096 -0.005917938 2.400000E-7
4.144 0.9238100 4.428791 3.000000 0.9999999 0.001596249 -0.007038544 1.200000E-7
4.244 1.024240 5.390802 4.000000 0.9999999 -0.00009314710 0.02101241 6.000000E-8
4.328 1.107956 6.239949 5.000000 0.9999998 -0.005723310 0.008957954 2.400000E-7
4.403 1.182855 7.021039 6.000000 0.9999999 -0.004480646 -0.008874891 6.000000E-8
4.472 1.252468 7.764249 7.000000 1.000000 -0.0006089602 -0.01474403 0E-7
4.538 1.317800 8.476851 8.000000 0.9999999 0.003099795 -0.01203606 1.200000E-7
4.600 1.379586 9.164242 9.000000 1.000000 0.005075846 -0.002882497 -1.200000E-7
4.658 1.438366 9.830328 10.00000 1.000000 0.004525778 0.01106848 0E-7
4.714 1.494018 10.47173 11.00000 1.000000 0.001293426 0.01863659 0E-7
4.766 1.546196 11.07957 12.00000 1.000000 -0.001484129 0.004641937 0E-7
4.817 1.596666 11.67229 13.00000 1.000000 -0.002921166 -0.004167243 -1.200000E-7
4.866 1.645640 12.25194 14.00000 1.000000 -0.003199488 -0.007666608 -2.400000E-7
4.913 1.693238 12.81954 15.00000 1.000000 -0.002305899 -0.006894074 -1.200000E-7
4.960 1.739594 13.37635 16.00000 0.9999999 -0.0001598852 -0.002084307 6.000000E-8
5.005 1.784808 13.92328 17.00000 0.9999999 0.003394287 0.006248174 6.000000E-8
4.960 1.740001 13.38126 18.00000 0.9999999 -13.38126 -18.00000 -0.9999999
4.990 1.770002 13.74376 19.00000 1.000000 -13.74376 -19.00000 -1.000000
5.080 1.859999 14.84115 20.00000 1.000000 -14.84115 -20.00000 -1.000000
5.080 1.859999 14.84115 21.00000 1.000000 -14.84115 -21.00000 -1.000000
5.130 1.909999 15.45728 22.00000 1.000000 -15.45728 -22.00000 -1.000000
5.259 2.039401 17.07428 23.00000 1.000000 -17.07428 -23.00000 -1.000000
5.180 1.960000 16.07805 24.00000 1.000000 -16.07805 -24.00000 -1.000000
5.220 2.000001 16.57798 25.00000 1.000000 -16.57798 -25.00000 -1.000000
5.330 2.109998 17.97929 26.00000 1.000000 -17.97929 -26.00000 -1.000000
5.290 2.070001 17.46404 27.00000 0.9999998 -17.46404 -27.00000 -0.9999998
5.380 2.159999 18.63265 28.00000 1.000000 -18.63265 -28.00000 -1.000000
5.380 2.159999 18.63265 29.00000 1.000000 -18.63265 -29.00000 -1.000000
5.400 2.179999 18.89688 30.00000 1.000000 -18.89688 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528710259">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>2.58025690577 -9.36897156265 11.6596456526 -1.18985196204 0.0759325779127</math:coefficients>
     <math:minRange>0.6254</math:minRange>
     <math:maxRange>1.7848</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528710206">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>1.16575193769 -5.44013686879 13.9738266424 -5.39841841459 0.852359863846</math:coefficients>
     <math:minRange>0.6254</math:minRange>
     <math:maxRange>1.7848</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528710210">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.6254</math:minRange>
     <math:maxRange>1.7848</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952871090">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7848</math:minRange>
     <math:maxRange>1.7848</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D11828595287108">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7848</math:minRange>
     <math:maxRange>1.7848</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.4000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951430455"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528710208">
   <gml:name>Gelesen aus: PROF0060.4510.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553054.7998 5988614.9783 3.54</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528710248">
     <gml:description>Übernommen aus Datei: PROF0060.4510.txt</gml:description>
     <gml:name>0060.4510</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528710244">
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
     <om:result><![CDATA[3.926 0.3857340 2.507696 1.000000 1.000000 -0.0006887753 -0.002750342 0E-7
4.055 0.5148320 3.822631 2.000000 1.000000 0.002011607 0.007811031 -1.200000E-7
4.154 0.6142510 4.874530 3.000000 1.000000 -0.0005081981 -0.001549389 0E-7
4.241 0.7005410 5.808987 4.000000 0.9999999 -0.0009776476 -0.003836163 6.000000E-8
4.318 0.7782040 6.667063 5.000000 0.9999999 -0.0006792493 -0.002942861 6.000000E-8
4.390 0.8495900 7.470039 6.000000 1.000000 -0.0001921161 -0.001076570 -1.200000E-7
4.456 0.9161370 8.230878 7.000000 0.9999999 0.0002299308 0.0008153882 6.000000E-8
4.519 0.9787750 8.957871 8.000000 1.000000 0.0004738496 0.001983685 0E-7
4.578 1.038157 9.656771 9.000000 0.9999999 0.0005327808 0.002021577 6.000000E-8
4.635 1.094839 10.33269 10.00000 0.9999998 0.0004343652 0.002115970 1.800000E-7
4.689 1.149117 10.98803 11.00000 1.000000 0.0001988076 0.001097033 -1.200000E-7
4.741 1.201302 11.62553 12.00000 1.000000 -0.00007544937 -0.0004777611 0E-7
4.792 1.251683 12.24791 13.00000 0.9999999 -0.0003300810 -0.001207782 6.000000E-8
4.840 1.300390 12.85608 14.00000 1.000000 -0.0004762336 -0.001924888 -1.200000E-7
4.888 1.347583 13.45140 15.00000 1.000000 -0.0004309721 -0.002255474 -1.200000E-7
4.933 1.393461 14.03585 16.00000 1.000000 -0.0001050827 -0.0003828772 0E-7
4.978 1.438066 14.60950 17.00000 1.000000 0.0005824639 0.002559423 0E-7
4.950 1.409999 14.24792 18.00000 0.9999998 -14.24792 -18.00000 -0.9999998
5.000 1.459999 14.89352 19.00000 0.9999999 -14.89352 -19.00000 -0.9999999
5.010 1.469999 15.02344 20.00000 1.000000 -15.02344 -20.00000 -1.000000
5.040 1.500000 15.41483 21.00000 1.000000 -15.41483 -21.00000 -1.000000
5.080 1.540001 15.94043 22.00000 1.000000 -15.94043 -22.00000 -1.000000
5.229 1.688843 17.94095 23.00000 1.000000 -17.94095 -23.00000 -1.000000
5.200 1.659999 17.54697 24.00000 0.9999999 -17.54697 -24.00000 -0.9999999
5.200 1.659999 17.54697 25.00000 1.000000 -17.54697 -25.00000 -1.000000
5.250 1.709999 18.23198 26.00000 0.9999999 -18.23198 -26.00000 -0.9999999
5.260 1.719999 18.37022 27.00000 1.000000 -18.37022 -27.00000 -1.000000
5.300 1.760000 18.92749 28.00000 1.000000 -18.92749 -28.00000 -1.000000
5.330 1.790000 19.34996 29.00000 0.9999999 -19.34996 -29.00000 -0.9999999
5.360 1.820001 19.77629 30.00000 0.9999999 -19.77629 -30.00000 -0.9999999
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528726164">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.206123797545 -3.00160482927 14.6992707768 -4.52633462894 0.976329748682</math:coefficients>
     <math:minRange>0.3857</math:minRange>
     <math:maxRange>1.4381</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952872639">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-1.0848259765 8.5559903604 2.15182097716 -0.552185339576 0.136311289872</math:coefficients>
     <math:minRange>0.3857</math:minRange>
     <math:maxRange>1.4381</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528726102">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3857</math:minRange>
     <math:maxRange>1.4381</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952872626">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4381</math:minRange>
     <math:maxRange>1.4381</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952872641">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.4381</math:minRange>
     <math:maxRange>1.4381</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.4510</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951441336"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528726205">
   <gml:name>Gelesen aus: PROF0060.5210.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553087.1005 5988681.3983 3.35</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528726150">
     <gml:description>Übernommen aus Datei: PROF0060.5210.txt</gml:description>
     <gml:name>0060.5210</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528726219">
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
     <om:result><![CDATA[3.889 0.5385270 2.232316 1.000000 1.000000 -0.001180776 -0.003705840 0E-7
4.035 0.6853160 3.501657 2.000000 1.000000 0.003270431 -0.0002117038 -1.200000E-7
4.147 0.7969290 4.577311 3.000000 1.000000 0.0004020770 0.02135763 -1.200000E-7
4.236 0.8858500 5.481461 4.000000 1.000000 -0.002246698 -0.001663305 0E-7
4.315 0.9653610 6.314562 5.000000 1.000000 -0.002435152 -0.007074329 0E-7
4.387 1.036940 7.080629 6.000000 0.9999999 -0.001487838 -0.01700779 1.200000E-7
4.454 1.103773 7.806874 7.000000 1.000000 0.0005643170 -0.01526842 0E-7
4.517 1.166792 8.501390 8.000000 0.9999999 0.002094194 -0.006311374 1.200000E-7
4.577 1.226618 9.169446 9.000000 1.000000 0.002499873 0.005166236 0E-7
4.634 1.283782 9.815714 10.00000 0.9999999 0.001784402 0.01619387 1.200000E-7
4.689 1.338599 10.44272 11.00000 1.000000 0.0003165319 0.02234460 -1.200000E-7
4.741 1.391364 11.05301 12.00000 0.9999999 -0.001333044 0.02047168 6.000000E-8
4.792 1.442332 11.64879 13.00000 1.000000 -0.002433313 0.008023315 -1.200000E-7
4.842 1.491682 12.23154 14.00000 1.000000 -0.002170053 -0.01797620 0E-7
4.890 1.540079 12.80879 15.00000 0.9999999 0.0002441571 -0.04991616 6.000000E-8
4.942 1.592284 13.44253 16.00000 1.000000 0.001909045 0.003623271 0E-7
4.993 1.642508 14.06632 17.00000 0.9999998 0.0009265945 0.02106435 1.800000E-7
5.041 1.690953 14.68107 18.00000 0.9999998 -0.0007247478 0.0008901747 1.800000E-7
4.980 1.630000 13.90967 19.00000 1.000000 -13.90967 -19.00000 -1.000000
5.020 1.670000 14.41362 20.00000 0.9999999 -14.41362 -20.00000 -0.9999999
5.070 1.720002 15.05586 21.00000 0.9999998 -15.05586 -21.00000 -0.9999998
5.160 1.809999 16.24631 22.00000 0.9999998 -16.24631 -22.00000 -0.9999998
5.261 1.911138 17.63704 23.00000 0.9999998 -17.63704 -23.00000 -0.9999998
5.180 1.829999 16.51690 24.00000 1.000000 -16.51690 -24.00000 -1.000000
5.220 1.869999 17.06460 25.00000 0.9999999 -17.06460 -25.00000 -0.9999999
5.260 1.910000 17.62108 26.00000 0.9999998 -17.62108 -26.00000 -0.9999998
5.290 1.940000 18.04419 27.00000 0.9999998 -18.04419 -27.00000 -0.9999998
5.330 1.980001 18.61599 28.00000 0.9999998 -18.61599 -28.00000 -0.9999998
5.410 2.059998 19.78580 29.00000 1.000000 -19.78580 -29.00000 -1.000000
5.410 2.059998 19.78580 30.00000 1.000000 -19.78580 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528741219">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.0954211685586 -0.212935977745 1.11495355122 6.82044671038 -2.16596795975</math:coefficients>
     <math:minRange>0.5385</math:minRange>
     <math:maxRange>1.691</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528741260">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.106453642107 -1.72366355897 13.907552238 -7.0579698284 1.4491069258</math:coefficients>
     <math:minRange>0.5385</math:minRange>
     <math:maxRange>1.691</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528741174">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5385</math:minRange>
     <math:maxRange>1.691</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528741275">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.691</math:minRange>
     <math:maxRange>1.691</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528741191">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.691</math:minRange>
     <math:maxRange>1.691</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.5210</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951442949"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528741198">
   <gml:name>Gelesen aus: PROF0060.5280.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553089.4394 5988686.5515 3.43</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528741206">
     <gml:description>Übernommen aus Datei: PROF0060.5280.txt</gml:description>
     <gml:name>0060.5280</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528741205">
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
     <om:result><![CDATA[3.729 0.2990070 2.370919 1.000000 1.000000 0.001141206 0.00006802162 -1.200000E-7
3.858 0.4275990 3.582747 2.000000 0.9999999 -0.002546340 0.001292881 1.200000E-7
3.960 0.5304290 4.590762 3.000000 0.9999999 -0.0008771864 -0.002137251 1.200000E-7
4.049 0.6193580 5.490424 4.000000 0.9999999 0.001354388 -0.002658812 6.000000E-8
4.129 0.6991270 6.319451 5.000000 1.000000 0.002432859 0.0002039848 -2.400000E-7
4.202 0.7722190 7.097381 6.000000 0.9999999 0.001609505 0.005463921 6.000000E-8
4.270 0.8395800 7.829027 7.000000 1.000000 -0.0006403344 0.003331089 0E-7
4.333 0.9026760 8.524162 8.000000 0.9999999 -0.001603338 -0.001328810 6.000000E-8
4.393 0.9625930 9.192730 9.000000 1.000000 -0.001580036 -0.003179984 -1.200000E-7
4.450 1.019805 9.838800 10.00000 0.9999998 -0.001030781 -0.003195719 1.800000E-7
4.505 1.074717 10.46598 11.00000 1.000000 -0.0003052646 -0.001490979 -1.200000E-7
4.558 1.127570 11.07618 12.00000 1.000000 0.0003872904 0.0003379519 -1.200000E-7
4.609 1.178606 11.67148 13.00000 0.9999999 0.0008760894 0.001771616 6.000000E-8
4.658 1.228006 12.25339 14.00000 0.9999998 0.001044840 0.001983454 2.400000E-7
4.706 1.275988 12.82397 15.00000 1.000000 0.0007907495 0.001633427 0E-7
4.753 1.322625 13.38361 16.00000 0.9999998 0.00009368771 -0.001045027 1.800000E-7
4.798 1.368280 13.93634 17.00000 0.9999999 -0.001147335 -0.001049763 6.000000E-8
4.790 1.359999 13.83572 18.00000 1.000000 -13.83572 -18.00000 -1.000000
4.790 1.359999 13.83572 19.00000 1.000000 -13.83572 -19.00000 -1.000000
4.830 1.400000 14.32358 20.00000 1.000000 -14.32358 -20.00000 -1.000000
4.870 1.440000 14.81597 21.00000 1.000000 -14.81597 -21.00000 -1.000000
4.910 1.480001 15.31294 22.00000 1.000000 -15.31294 -22.00000 -1.000000
5.068 1.637884 17.34108 23.00000 0.9999999 -17.34108 -23.00000 -0.9999999
5.040 1.609999 16.97449 24.00000 1.000000 -16.97449 -24.00000 -1.000000
5.040 1.609999 16.97449 25.00000 1.000000 -16.97449 -25.00000 -1.000000
5.060 1.629999 17.23706 26.00000 0.9999998 -17.23706 -26.00000 -0.9999998
5.100 1.670000 17.76776 27.00000 0.9999999 -17.76776 -27.00000 -0.9999999
5.140 1.710001 18.30586 28.00000 0.9999997 -18.30586 -28.00000 -0.9999997
5.240 1.809999 19.68341 29.00000 1.000000 -19.68341 -29.00000 -1.000000
5.210 1.780002 19.26532 30.00000 1.000000 -19.26532 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528851199">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.252909004561 1.65322890018 8.50267324051 0.0280088707437 -0.285467712175</math:coefficients>
     <math:minRange>0.299</math:minRange>
     <math:maxRange>1.3683</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952885121">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.167302849837 7.78728290645 2.54593957416 -0.655830258702 0.102951112624</math:coefficients>
     <math:minRange>0.299</math:minRange>
     <math:maxRange>1.3683</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528851165">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.299</math:minRange>
     <math:maxRange>1.3683</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528851239">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3683</math:minRange>
     <math:maxRange>1.3683</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952885194">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3683</math:minRange>
     <math:maxRange>1.3683</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.5280</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951455476"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528851221">
   <gml:name>Gelesen aus: PROF0060.6000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553113.4613 5988757.3721 3.12</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952885129">
     <gml:description>Übernommen aus Datei: PROF0060.6000.txt</gml:description>
     <gml:name>0060.6000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528851210">
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
     <om:result><![CDATA[3.717 0.5968810 2.181009 1.000000 0.9999999 0.0002483910 -0.001626786 6.000000E-8
3.883 0.7631940 3.564035 2.000000 1.000000 0.0002977162 0.01293321 -1.200000E-7
3.995 0.8752650 4.631037 3.000000 1.000000 -0.003476355 -0.01622148 -1.200000E-7
4.091 0.9706180 5.596340 4.000000 1.000000 0.001702297 -0.01247627 -2.400000E-7
4.175 1.054969 6.494312 5.000000 1.000000 0.004150582 0.008306347 -2.400000E-7
4.250 1.130211 7.329649 6.000000 1.000000 0.0002831330 0.02135154 -1.200000E-7
4.317 1.197309 8.093111 7.000000 1.000000 -0.002063431 0.004063094 -1.200000E-7
4.380 1.260438 8.824233 8.000000 0.9999999 -0.001973165 -0.004986449 6.000000E-8
4.440 1.320252 9.528434 9.000000 0.9999998 -0.0009837106 -0.008422040 1.800000E-7
4.497 1.377269 10.21009 10.00000 1.000000 0.00006223967 -0.007819690 -2.400000E-7
4.552 1.431909 10.87286 11.00000 1.000000 0.0007330722 -0.004027471 0E-7
4.604 1.484373 11.51799 12.00000 1.000000 0.0009274371 -0.0002302848 0E-7
4.655 1.534946 12.14800 13.00000 0.9999999 0.0006717899 0.003168246 1.200000E-7
4.704 1.583829 12.76453 14.00000 1.000000 0.0001707537 0.005199799 -1.200000E-7
4.751 1.631189 13.36898 15.00000 0.9999999 -0.0003296467 0.005010699 6.000000E-8
4.797 1.677154 13.96231 16.00000 1.000000 -0.0004858419 0.001564843 -1.200000E-7
4.842 1.721846 14.54552 17.00000 1.000000 0.00006473859 -0.005787297 -2.400000E-7
4.790 1.670000 13.86953 18.00000 1.000000 -13.86953 -18.00000 -1.000000
4.830 1.710001 14.39034 19.00000 1.000000 -14.39034 -19.00000 -1.000000
4.870 1.750001 14.91613 20.00000 0.9999999 -14.91613 -20.00000 -0.9999999
4.910 1.790002 15.44693 21.00000 0.9999999 -15.44693 -21.00000 -0.9999999
4.980 1.859999 16.38777 22.00000 1.000000 -16.38777 -22.00000 -1.000000
5.088 1.967988 17.86924 23.00000 0.9999999 -17.86924 -23.00000 -0.9999999
5.020 1.900000 16.93229 24.00000 0.9999999 -16.93229 -24.00000 -0.9999999
5.050 1.929999 17.34393 25.00000 1.000000 -17.34393 -25.00000 -1.000000
5.090 1.970000 17.89719 26.00000 1.000000 -17.89719 -26.00000 -1.000000
5.120 2.000001 18.31541 27.00000 0.9999999 -18.31541 -27.00000 -0.9999999
5.150 2.030002 18.73644 28.00000 0.9999998 -18.73644 -28.00000 -0.9999998
5.230 2.109999 19.87284 29.00000 1.000000 -19.87284 -29.00000 -1.000000
5.230 2.109999 19.87284 30.00000 1.000000 -19.87284 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528866165">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.63815460676 -6.63115825809 9.0767193208 0.622346145523 -0.376953061971</math:coefficients>
     <math:minRange>0.5969</math:minRange>
     <math:maxRange>1.7218</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528866127">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.415296986278 -3.0524878011 12.8649431555 -5.2153784507 0.895193193301</math:coefficients>
     <math:minRange>0.5969</math:minRange>
     <math:maxRange>1.7218</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528866298">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5969</math:minRange>
     <math:maxRange>1.7218</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952886694">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7218</math:minRange>
     <math:maxRange>1.7218</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952886670">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7218</math:minRange>
     <math:maxRange>1.7218</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.6000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951466330"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528866192">
   <gml:name>Gelesen aus: PROF0060.6310.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553128.8758 5988780.332 3.22</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528866197">
     <gml:description>Übernommen aus Datei: PROF0060.6310.txt</gml:description>
     <gml:name>0060.6310</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952886665">
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
     <om:result><![CDATA[3.648 0.4275820 2.211486 1.000000 1.000000 -0.001782840 0.004217944 -1.200000E-7
3.795 0.5753960 3.449372 2.000000 0.9999999 0.003239773 -0.01796193 1.200000E-7
3.910 0.6901420 4.509212 3.000000 1.000000 0.004818985 0.01133438 -1.200000E-7
4.004 0.7836150 5.432979 4.000000 1.000000 -0.003995810 0.01799619 -1.200000E-7
4.084 0.8638110 6.249821 5.000000 0.9999999 -0.005824285 -0.001806679 1.200000E-7
4.157 0.9373940 7.015876 6.000000 1.000000 -0.003258614 -0.01051444 -1.200000E-7
4.226 1.005858 7.742887 7.000000 1.000000 0.0003998692 -0.01135533 0E-7
4.290 1.070207 8.438731 8.000000 0.9999999 0.003308223 -0.006546203 1.200000E-7
4.351 1.131131 9.108709 9.000000 1.000000 0.004462089 0.001907237 0E-7
4.409 1.189147 9.756820 10.00000 0.9999999 0.003307449 0.01252464 1.200000E-7
4.464 1.243722 10.37437 11.00000 1.000000 0.0008173049 0.006833924 -1.200000E-7
4.516 1.296177 10.97293 12.00000 0.9999999 -0.0009946689 0.001363471 1.200000E-7
4.567 1.346896 11.55627 13.00000 1.000000 -0.002319998 -0.001738631 -1.200000E-7
4.616 1.395989 12.12478 14.00000 0.9999999 -0.002865650 -0.004608609 6.000000E-8
4.664 1.443786 12.68139 15.00000 1.000000 -0.002152920 -0.004163564 -1.200000E-7
4.710 1.490394 13.22710 16.00000 1.000000 -0.0001970738 -0.001181513 0E-7
4.756 1.535908 13.76283 17.00000 1.000000 0.003038167 0.003699112 0E-7
4.710 1.490000 13.22248 18.00000 0.9999999 -13.22248 -18.00000 -0.9999999
4.750 1.530000 13.69313 19.00000 0.9999997 -13.69313 -19.00000 -0.9999997
4.790 1.570001 14.16604 20.00000 0.9999999 -14.16604 -20.00000 -0.9999999
4.820 1.600002 14.52259 21.00000 1.000000 -14.52259 -21.00000 -1.000000
4.930 1.709999 15.84416 22.00000 0.9999998 -15.84416 -22.00000 -0.9999998
5.016 1.795565 16.88916 23.00000 1.000000 -16.88916 -23.00000 -1.000000
4.940 1.720000 15.96542 24.00000 0.9999999 -15.96542 -24.00000 -0.9999999
4.970 1.750000 16.33036 25.00000 1.000000 -16.33036 -25.00000 -1.000000
5.010 1.790000 16.82060 26.00000 0.9999999 -16.82060 -26.00000 -0.9999999
5.050 1.830001 17.31533 27.00000 1.000000 -17.31533 -27.00000 -1.000000
5.080 1.860001 17.68932 28.00000 1.000000 -17.68932 -28.00000 -1.000000
5.180 1.959999 18.95409 29.00000 1.000000 -18.95409 -29.00000 -1.000000
5.180 1.959999 18.95409 30.00000 1.000000 -18.95409 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952886628">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.52863563464 -3.17596432906 10.3559907692 -0.732126223915 -0.076228697905</math:coefficients>
     <math:minRange>0.4276</math:minRange>
     <math:maxRange>1.5359</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528882245">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.224100411487 3.03433923673 7.46881060129 -3.17901870856 0.580200898825</math:coefficients>
     <math:minRange>0.4276</math:minRange>
     <math:maxRange>1.5359</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528882204">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.4276</math:minRange>
     <math:maxRange>1.5359</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528882144">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5359</math:minRange>
     <math:maxRange>1.5359</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528882217">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.5359</math:minRange>
     <math:maxRange>1.5359</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.6310</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951477312"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation118285952888293">
   <gml:name>Gelesen aus: PROF0060.7000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553194.98 5988821.9728 3.36</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528882303">
     <gml:description>Übernommen aus Datei: PROF0060.7000.txt</gml:description>
     <gml:name>0060.7000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528882175">
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
     <om:result><![CDATA[3.706 0.3455930 2.467448 1.000000 1.000000 0.001420206 -0.001519828 0E-7
3.826 0.4663490 3.713462 2.000000 1.000000 -0.002587162 0.005591357 -1.200000E-7
3.923 0.5631010 4.745445 3.000000 0.9999998 -0.001682672 -0.001114019 2.400000E-7
4.007 0.6469070 5.663547 4.000000 1.000000 0.0005233545 -0.005745718 0E-7
4.082 0.7221900 6.507429 5.000000 0.9999998 0.002440008 -0.005416272 2.400000E-7
4.151 0.7912890 7.297959 6.000000 0.9999999 0.003130477 -0.0001149219 6.000000E-8
4.216 0.8555860 8.047256 7.000000 0.9999999 0.002013827 0.008618019 1.200000E-7
4.276 0.9158260 8.761256 8.000000 1.000000 -0.001324057 0.01605530 0E-7
4.331 0.9711840 9.423927 9.000000 1.000000 -0.003303988 -0.006679837 0E-7
4.385 1.025174 10.07439 10.00000 1.000000 -0.002965146 -0.007832792 -1.200000E-7
4.437 1.077110 10.70457 11.00000 1.000000 -0.001654800 -0.006146276 0E-7
4.487 1.127254 11.31714 12.00000 1.000000 0.00001884196 -0.002812153 -1.200000E-7
4.536 1.175799 11.91407 13.00000 1.000000 0.001497974 0.0006358823 0E-7
4.583 1.222960 12.49763 14.00000 1.000000 0.002348394 0.003953737 0E-7
4.629 1.268820 13.06855 15.00000 1.000000 0.002164813 0.004752741 0E-7
4.674 1.313515 13.62824 16.00000 0.9999999 0.0006106048 0.002260163 1.200000E-7
4.717 1.357152 14.17781 17.00000 1.000000 -0.002650676 -0.004485384 0E-7
4.670 1.310002 13.58413 18.00000 1.000000 -13.58413 -18.00000 -1.000000
4.770 1.410000 14.84755 19.00000 1.000000 -14.84755 -19.00000 -1.000000
4.770 1.410000 14.84755 20.00000 1.000000 -14.84755 -20.00000 -1.000000
4.820 1.459999 15.48682 21.00000 0.9999999 -15.48682 -21.00000 -0.9999999
4.820 1.459999 15.48682 22.00000 1.000000 -15.48682 -22.00000 -1.000000
4.965 1.604936 17.37499 23.00000 0.9999999 -17.37499 -23.00000 -0.9999999
4.890 1.530001 16.39226 24.00000 0.9999999 -16.39226 -24.00000 -0.9999999
4.930 1.570002 16.91512 25.00000 1.000000 -16.91512 -25.00000 -1.000000
5.020 1.659999 18.10599 26.00000 1.000000 -18.10599 -26.00000 -1.000000
5.020 1.659999 18.10599 27.00000 1.000000 -18.10599 -27.00000 -1.000000
5.030 1.669999 18.23955 28.00000 1.000000 -18.23955 -28.00000 -1.000000
5.060 1.700000 18.64174 29.00000 1.000000 -18.64174 -29.00000 -1.000000
5.090 1.730001 19.04616 30.00000 1.000000 -19.04616 -30.00000 -1.000000
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528976148">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>-0.541753462354 1.83272421547 6.86759334133 2.48770141732 -1.12533464968</math:coefficients>
     <math:minRange>0.3456</math:minRange>
     <math:maxRange>1.3572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528976126">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-0.774605289943 8.69295521335 2.03369960172 -0.0373332597977 -0.147494886226</math:coefficients>
     <math:minRange>0.3456</math:minRange>
     <math:maxRange>1.3572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528976237">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.3456</math:minRange>
     <math:maxRange>1.3572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952897679">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3572</math:minRange>
     <math:maxRange>1.3572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528976301">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.3572</math:minRange>
     <math:maxRange>1.3572</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.7000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile11828595148041"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528991212">
   <gml:name>Gelesen aus: PROF0060.8000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553297.0266 5988827.8428 3.33</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation1182859528991229">
     <gml:description>Übernommen aus Datei: PROF0060.8000.txt</gml:description>
     <gml:name>0060.8000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition118285952899177">
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
     <om:result><![CDATA[3.891 0.5605850 2.149678 1.000000 0.9999998 -0.0001157436 -0.004107918 1.800000E-7
4.044 0.7141030 3.373065 2.000000 0.9999999 0.001286246 0.01338910 6.000000E-8
4.158 0.8282570 4.354283 3.000000 0.9999999 -0.002242036 -0.004432733 1.200000E-7
4.257 0.9267870 5.236771 4.000000 1.000000 -0.0006032681 -0.008448952 -1.200000E-7
4.345 1.014965 6.054472 5.000000 1.000000 0.001434341 -0.004796853 -1.200000E-7
4.426 1.095634 6.825649 6.000000 1.000000 0.001806662 0.003275001 0E-7
4.500 1.169949 7.555204 7.000000 1.000000 0.00002699281 0.005637276 -1.200000E-7
4.569 1.239159 8.247707 8.000000 0.9999999 -0.0008877231 0.0008668402 1.200000E-7
4.635 1.304872 8.916113 9.000000 1.000000 -0.0009085845 -0.0006163663 -1.200000E-7
4.698 1.367594 9.563985 10.00000 0.9999999 -0.0005467250 -0.0001695551 6.000000E-8
4.758 1.427687 10.19376 11.00000 1.000000 -0.0001119339 0.0006209064 0E-7
4.815 1.485474 10.80775 12.00000 0.9999999 0.0002136684 0.001034711 6.000000E-8
4.871 1.541268 11.40834 13.00000 1.000000 0.0003754156 0.001436898 0E-7
4.925 1.595216 11.99634 14.00000 1.000000 0.0003687508 0.0001677954 -1.200000E-7
4.978 1.647501 12.57303 15.00000 0.9999999 0.0002362379 -0.003121461 6.000000E-8
5.028 1.698306 13.13982 16.00000 1.000000 0.00007028781 -0.008152860 -1.200000E-7
5.079 1.748872 13.71061 17.00000 0.9999999 -0.0004025878 0.007418171 6.000000E-8
5.020 1.690001 13.04673 18.00000 1.000000 -13.04673 -18.00000 -1.000000
5.070 1.740002 13.60991 19.00000 1.000000 -13.60991 -19.00000 -1.000000
5.140 1.809999 14.41175 20.00000 0.9999999 -14.41175 -20.00000 -0.9999999
5.150 1.819999 14.52764 21.00000 1.000000 -14.52764 -21.00000 -1.000000
5.200 1.869999 15.11206 22.00000 1.000000 -15.11206 -22.00000 -1.000000
5.369 2.038974 17.15491 23.00000 1.000000 -17.15491 -23.00000 -1.000000
5.280 1.950001 16.06449 24.00000 0.9999998 -16.06449 -24.00000 -0.9999998
5.390 2.059998 17.41929 25.00000 1.000000 -17.41929 -25.00000 -1.000000
5.390 2.059998 17.41929 26.00000 1.000000 -17.41929 -26.00000 -1.000000
5.400 2.069999 17.54596 27.00000 1.000000 -17.54596 -27.00000 -1.000000
5.450 2.119998 18.18817 28.00000 1.000000 -18.18817 -28.00000 -1.000000
5.490 2.159999 18.71259 29.00000 0.9999999 -18.71259 -29.00000 -0.9999999
5.530 2.200000 19.24647 30.00000 0.9999998 -19.24647 -30.00000 -0.9999998
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528991239">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>0.519720668454 -4.03422560454 9.20381258136 -0.885666607138 0.01391137363</math:coefficients>
     <math:minRange>0.5606</math:minRange>
     <math:maxRange>1.7489</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952899192">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>-1.12764870588 3.70925534098 4.55563125097 -1.4606536658 0.238410315632</math:coefficients>
     <math:minRange>0.5606</math:minRange>
     <math:maxRange>1.7489</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528991281">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5606</math:minRange>
     <math:maxRange>1.7489</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D118285952899179">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7489</math:minRange>
     <math:maxRange>1.7489</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859528991189">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7489</math:minRange>
     <math:maxRange>1.7489</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.8000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951491374"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
 <simBase:flowRelationshipMember>
  <wb1d2d:TeschkeFlowRelation gml:id="TeschkeFlowRelation1182859528991116">
   <gml:name>Gelesen aus: PROF0060.9000.txt</gml:name>
   <simBase:position>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gss" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," decimal="." cs=" ">3553379.3268 5988867.2554 3.14</gml:coordinates>
    </gml:Point>
   </simBase:position>
   <wb1d2d:pointsMember>
    <tuhh:WPointsObservation gml:id="WPointsObservation118285952899139">
     <gml:description>Übernommen aus Datei: PROF0060.9000.txt</gml:description>
     <gml:name>0060.9000</gml:name>
     <om:resultDefinition>
      <swe:RecordDefinition gml:id="RecordDefinition1182859528991324">
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
     <om:result><![CDATA[3.710 0.5698760 2.180080 1.000000 1.000000 -0.001035441 0.004730759 0E-7
3.876 0.7355110 3.482982 2.000000 0.9999998 0.002445723 -0.02169752 1.800000E-7
4.001 0.8607250 4.615159 3.000000 0.9999999 0.002565232 0.02345991 6.000000E-8
4.097 0.9570830 5.559974 4.000000 0.9999999 -0.004547752 0.003867584 1.200000E-7
4.182 1.042127 6.424927 5.000000 0.9999999 -0.002806520 -0.006909354 6.000000E-8
4.260 1.119748 7.239632 6.000000 1.000000 0.0003889205 -0.005109710 -2.400000E-7
4.332 1.191637 8.015676 7.000000 1.000000 0.001392098 0.004840421 0E-7
4.397 1.257496 8.742473 8.000000 1.000000 0.0008480696 -0.002311472 0E-7
4.460 1.319763 9.440069 9.000000 0.9999999 0.001162247 -0.005626368 1.200000E-7
4.519 1.379197 10.11535 10.00000 0.9999999 0.001348284 -0.003024965 1.200000E-7
4.576 1.436179 10.77142 11.00000 0.9999999 0.0008616949 0.004032673 6.000000E-8
4.631 1.490503 11.40455 12.00000 0.9999999 -0.0002872233 0.004798234 1.200000E-7
4.683 1.542632 12.01809 13.00000 0.9999999 -0.001102851 0.001480370 1.200000E-7
4.733 1.593112 12.61769 14.00000 1.000000 -0.001453960 -0.0003452174 -1.200000E-7
4.782 1.642115 13.20490 15.00000 0.9999998 -0.001244232 -0.001153631 1.800000E-7
4.830 1.689814 13.78138 16.00000 1.000000 -0.0002746715 -0.0007630964 0E-7
4.876 1.736297 14.34776 17.00000 1.000000 0.001740383 -0.0002686139 -1.200000E-7
4.830 1.690001 13.78364 18.00000 1.000000 -13.78364 -18.00000 -1.000000
4.870 1.730002 14.27080 19.00000 1.000000 -14.27080 -19.00000 -1.000000
4.950 1.809999 15.25520 20.00000 0.9999999 -15.25520 -20.00000 -0.9999999
4.950 1.809999 15.25520 21.00000 1.000000 -15.25520 -21.00000 -1.000000
5.000 1.859999 15.87734 22.00000 0.9999999 -15.87734 -22.00000 -0.9999999
5.134 1.993842 17.56873 23.00000 0.9999998 -17.56873 -23.00000 -0.9999998
5.060 1.920000 16.63090 24.00000 0.9999999 -16.63090 -24.00000 -0.9999999
5.090 1.950001 17.01054 25.00000 0.9999999 -17.01054 -25.00000 -0.9999999
5.200 2.059999 18.41877 26.00000 1.000000 -18.41877 -26.00000 -1.000000
5.200 2.059999 18.41877 27.00000 0.9999999 -18.41877 -27.00000 -0.9999999
5.250 2.109999 19.06733 28.00000 0.9999998 -19.06733 -28.00000 -0.9999998
5.250 2.109999 19.06733 29.00000 1.000000 -19.06733 -29.00000 -1.000000
5.270 2.129999 19.32825 30.00000 0.9999998 -19.32825 -30.00000 -0.9999998
]]></om:result>
    </tuhh:WPointsObservation>
   </wb1d2d:pointsMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859529007165">
     <gml:description>Q1(h)</gml:description>
     <gml:name>Q1(h)</gml:name>
     <math:coefficients>1.37917329784 -5.8514925212 9.22215005408 -0.0911190853293 -0.169973836008</math:coefficients>
     <math:minRange>0.5699</math:minRange>
     <math:maxRange>1.7363</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonRunoff"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859529007206">
     <gml:description>A1(h)</gml:description>
     <gml:name>A1(h)</gml:name>
     <math:coefficients>0.42521913681 -2.17832273618 11.627759002 -4.66264075146 0.776621908554</math:coefficients>
     <math:minRange>0.5699</math:minRange>
     <math:maxRange>1.7363</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonArea"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859529007108">
     <gml:description>al1(h)</gml:description>
     <gml:name>al1(h)</gml:name>
     <math:coefficients>1.0</math:coefficients>
     <math:minRange>0.5699</math:minRange>
     <math:maxRange>1.7363</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859529007316">
     <gml:description>Spl_al</gml:description>
     <gml:name>Spl_al</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7363</math:minRange>
     <math:maxRange>1.7363</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:polynomialMember>
    <math:Polynomial1D gml:id="Polynomial1D1182859529007255">
     <gml:description>al2(h)</gml:description>
     <gml:name>al2(h)</gml:name>
     <math:coefficients>0.0</math:coefficients>
     <math:minRange>1.7363</math:minRange>
     <math:maxRange>1.7363</math:maxRange>
     <math:domainPhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonAlpha"/>
     <math:rangePhenomenon xlink:href="urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#phenomenonWaterlevel"/>
    </math:Polynomial1D>
   </wb1d2d:polynomialMember>
   <wb1d2d:station>60.9000</wb1d2d:station>
   <wb1d2d:profileMember xlink:href="terrain.gml#Profile118285951502343"/>
   <wb1d2d:slope>0.00100</wb1d2d:slope>
  </wb1d2d:TeschkeFlowRelation>
 </simBase:flowRelationshipMember>
</simBase:FlowRelationshipModel>
