<?xml version="1.0" encoding="WINDOWS-1252"?>
<simBase:TerrainModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:swe="http://www.opengis.net/swe" xmlns:ns0="http://www.tu-harburg.de/wb/kalypso/schemata/observation" xmlns:math="org.kalypso.gml.common.math" xmlns:om="http://www.opengis.net/om" xmlns:sweExt="org.kalypso.swe.ext" gml:id="root">
 <simBase:terrainElevationModelSystem>
  <simBase:TerrainElevationModelSystem gml:id="Scenario_Terrain_Elevation_ModelSystem">
   <simBase:terrainElevationModel>
    <simBase:NativeTerrainElevationModelWrapper gml:id="NativeTerrainElevationModelWrapper1183538586681545">
     <gml:description>Keine Information:</gml:description>
     <gml:name>wsp</gml:name>
     <simBase:fileName>native_tem/wsp.hmo</simBase:fileName>
    </simBase:NativeTerrainElevationModelWrapper>
   </simBase:terrainElevationModel>
   <simBase:terrainElevationModel>
    <simBase:NativeTerrainElevationModelWrapper gml:id="NativeTerrainElevationModelWrapper1183539339431256">
     <gml:description>Keine Information:</gml:description>
     <gml:name>vel</gml:name>
     <simBase:fileName>native_tem/output_VELOCITY_5.hmo</simBase:fileName>
    </simBase:NativeTerrainElevationModelWrapper>
   </simBase:terrainElevationModel>
   <simBase:terrainElevationModel>
    <simBase:NativeTerrainElevationModelWrapper gml:id="NativeTerrainElevationModelWrapper118353985414917">
     <gml:description>Keine Information:</gml:description>
     <gml:name>rrr</gml:name>
     <simBase:fileName>native_tem/output_WATERLEVEL.hmo</simBase:fileName>
    </simBase:NativeTerrainElevationModelWrapper>
   </simBase:terrainElevationModel>
  </simBase:TerrainElevationModelSystem>
 </simBase:terrainElevationModelSystem>
	<simBase:roughnessLayerCollection>
		<simBase:RoughnessLayer gml:id="RoughnessLayer11876158294061">
			<gml:name>Korrekturlayer</gml:name>
			<simBase:editable>false</simBase:editable>
			<simBase:basic>false</simBase:basic>
		</simBase:RoughnessLayer>
	</simBase:roughnessLayerCollection>
	<simBase:roughnessLayerCollection>
		<simBase:RoughnessLayer gml:id="RoughnessLayer11876158257812">
			<gml:name>Basisdatenlayer</gml:name>
			<simBase:editable>false</simBase:editable>
			<simBase:basic>true</simBase:basic>
		</simBase:RoughnessLayer>
	</simBase:roughnessLayerCollection>
 <simBase:riverProfileNetworkCollectionMember>
  <simBase:RiverProfileNetworkCollection gml:id="RiverProfileNetworkCollection11708754954840">
   <simBase:riverProfileNetwork>
    <simBase:RiverProfileNetwork gml:id="RiverProfileNetwork11828595109453">
     <gml:description>Importiert aus WSPM-Gewässerstrang: Stoer - Ist
Importiert am 26.06.2007 14:05 aus platform:/resource//01_Will_Pad_georefNeu/modell.gml</gml:description>
     <gml:name>Ist</gml:name>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595109605">
       <gml:name>So Ists</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595109913">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.18 5986894.321 3552967.118 0.35 0.0 0.0 0.0
null null 9.61 6.65 5986894.0306 3552965.8515 0.35 0.0 0.0 0.0
null null 22.41 6.17 5986893.6437 3552964.1647 0.35 0.0 0.0 0.0
null null 63.99 5.07 5986892.387 3552958.685 0.35 0.0 0.0 0.0
null null 78.35 6.609707602339182 5986891.953 3552956.7926 0.35 8.0 8.0 0.1
low null 84.71 4.98 5986891.7608 3552955.9544 0.2 0.0 0.0 0.0
null null 87.5 4.16 5986891.6765 3552955.5867 0.2 0.0 0.0 0.0
null null 87.89 3.58 5986891.6647 3552955.5353 0.12 0.0 0.0 0.0
null null 91.07 2.58 5986891.5686 3552955.1162 0.12 0.0 0.0 0.0
null null 97.43 3.09 5986891.3764 3552954.2781 0.2 0.0 0.0 0.0
null null 99.02 3.44 5986891.3283 3552954.0685 0.2 0.0 0.0 0.0
low null 101.42 5.1 5986891.2558 3552953.7522 0.35 5.0 5.0 0.1
null null 114.19 5.53 5986890.8698 3552952.0693 0.35 0.0 0.0 0.0
null null 126.98 5.57 5986890.4833 3552950.3838 0.35 0.0 0.0 0.0
null null 152.57 6.27 5986889.7099 3552947.0114 0.35 0.0 0.0 0.0
null true 187.44 6.86 5986888.656 3552942.416 0.35 0.0 0.0 0.0]]></om:result>
       <prof:station>58.4500</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552967.118 5986894.321 6.18,3552965.8515 5986894.0306 6.65,3552964.1647 5986893.6437 6.17,3552958.685 5986892.387 5.07,3552956.7926 5986891.953 6.609707602339182,3552955.9544 5986891.7608 4.98,3552955.5867 5986891.6765 4.16,3552955.5353 5986891.6647 3.58,3552955.1162 5986891.5686 2.58,3552954.2781 5986891.3764 3.09,3552954.0685 5986891.3283 3.44,3552953.7522 5986891.2558 5.1,3552952.0693 5986890.8698 5.53,3552950.3838 5986890.4833 5.57,3552947.0114 5986889.7099 6.27,3552942.416 5986888.656 6.86</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595114297">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595114291">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.87 5986916.918 3552961.449 0.35
null null 28.79 7.59 5986916.1415 3552958.4182 0.35
null null 31.98 7.41 5986916.0554 3552958.0823 0.35
null null 53.17 7.76 5986915.4839 3552955.8516 0.35
null null 73.98 7.97 5986914.9226 3552953.6608 0.35
null null 96.34 8.15 5986914.3195 3552951.3069 0.35
null null 97.94 8.5 5986914.2763 3552951.1384 0.35
null null 101.44 8.63 5986914.1819 3552950.77 0.35
null null 101.54 8.37 5986914.1793 3552950.7595 0.35
null null 107.96 5.1 5986914.0061 3552950.0836 0.25
low null 111.15 5.08 5986913.92 3552949.7478 0.12
null null 114.31 4.33 5986913.8348 3552949.4151 0.12
null null 115.5 3.91 5986913.8027 3552949.2898 0.05
null null 115.9 3.38 5986913.7919 3552949.2477 0.12
null null 120.7 2.37 5986913.6625 3552948.7424 0.12
null null 123.88 2.77 5986913.5767 3552948.4076 0.12
null null 125.46 3.15 5986913.5341 3552948.2413 0.05
null null 127.03 3.96 5986913.4917 3552948.076 0.12
low null 132.99 5.0 5986913.331 3552947.4486 0.25
null null 134.97 5.08 5986913.2776 3552947.2401 0.35
null null 135.77 5.64 5986913.256 3552947.1559 0.35
null null 136.96 6.85 5986913.2239 3552947.0306 0.35
null null 138.55 6.83 5986913.181 3552946.8633 0.35
null null 139.35 7.0 5986913.1594 3552946.779 0.35
null null 141.34 8.43 5986913.1058 3552946.5695 0.35
null null 166.9 7.87 5986912.4164 3552943.8787 0.35
null null 179.67 7.84 5986912.0719 3552942.5344 0.35
null true 219.56 6.8 5986910.996 3552938.335 0.35
]]></om:result>
       <prof:station>58.4930</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552961.449 5986916.918 5.87,3552958.4182 5986916.1415 7.59,3552958.0823 5986916.0554 7.41,3552955.8516 5986915.4839 7.76,3552953.6608 5986914.9226 7.97,3552951.3069 5986914.3195 8.15,3552951.1384 5986914.2763 8.5,3552950.77 5986914.1819 8.63,3552950.7595 5986914.1793 8.37,3552950.0836 5986914.0061 5.1,3552949.7478 5986913.92 5.08,3552949.4151 5986913.8348 4.33,3552949.2898 5986913.8027 3.91,3552949.2477 5986913.7919 3.38,3552948.7424 5986913.6625 2.37,3552948.4076 5986913.5767 2.77,3552948.2413 5986913.5341 3.15,3552948.076 5986913.4917 3.96,3552947.4486 5986913.331 5.0,3552947.2401 5986913.2776 5.08,3552947.1559 5986913.256 5.64,3552947.0306 5986913.2239 6.85,3552946.8633 5986913.181 6.83,3552946.779 5986913.1594 7.0,3552946.5695 5986913.1058 8.43,3552943.8787 5986912.4164 7.87,3552942.5344 5986912.0719 7.84,3552938.335 5986910.996 6.8</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595115707">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595115702">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.81 5986958.196 3552939.0525 0.35 0.0 0.0 0.0
null null 25.53 5.45 5986957.1691 3552936.7163 0.35 0.0 0.0 0.0
null null 51.13 4.87 5986956.1394 3552934.3737 0.35 0.0 0.0 0.0
null null 153.53 4.87 5986952.0204 3552925.0033 0.35 3.0 3.0 0.1
low null 174.34 4.74 5986951.1834 3552923.099 0.25 0.0 0.0 0.0
null null 175.93 4.05 5986951.1194 3552922.9535 0.25 0.0 0.0 0.0
null null 176.33 3.52 5986951.1033 3552922.9169 0.12 0.0 0.0 0.0
null null 181.08 2.61 5986950.9123 3552922.4822 0.12 0.0 0.0 0.0
null null 186.24 2.83 5986950.7047 3552922.01 0.25 0.0 0.0 0.0
null null 187.03 3.59 5986950.6729 3552921.9378 0.25 0.0 0.0 0.0
low null 188.62 4.41 5986950.609 3552921.7923 0.35 3.0 3.0 0.1
null null 201.36 5.45 5986950.0965 3552920.6264 0.35 0.0 0.0 0.0
null null 202.16 5.78 5986950.0644 3552920.5532 0.35 0.0 0.0 0.0
null null 211.73 6.72 5986949.6794 3552919.6775 0.35 0.0 0.0 0.0
null null 213.33 7.13 5986949.6151 3552919.5311 0.35 0.0 0.0 0.0
null null 219.72 7.34 5986949.358 3552918.9464 0.35 0.0 0.0 0.0
null null 245.3 6.78 5986948.3291 3552916.6056 0.35 0.0 0.0 0.0
null true 257.36 6.66 5986947.844 3552915.502 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>58.5500</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552939.0525 5986958.196 5.81,3552936.7163 5986957.1691 5.45,3552934.3737 5986956.1394 4.87,3552925.0033 5986952.0204 4.87,3552923.099 5986951.1834 4.74,3552922.9535 5986951.1194 4.05,3552922.9169 5986951.1033 3.52,3552922.4822 5986950.9123 2.61,3552922.01 5986950.7047 2.83,3552921.9378 5986950.6729 3.59,3552921.7923 5986950.609 4.41,3552920.6264 5986950.0965 5.45,3552920.5532 5986950.0644 5.78,3552919.6775 5986949.6794 6.72,3552919.5311 5986949.6151 7.13,3552918.9464 5986949.358 7.34,3552916.6056 5986948.3291 6.78,3552915.502 5986947.844 6.66</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595116165">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595116163">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.83 5986999.474 3552916.656 0.35 0.0 0.0 0.0
null null 51.19 5.22 5986996.8272 3552912.361 0.35 0.0 0.0 0.0
null null 153.59 4.97 5986991.5326 3552903.7694 0.35 0.0 0.0 0.0
null null 179.18 5.05 5986990.2095 3552901.6223 0.35 5.0 5.0 0.3
null null 203.17 5.33 5986988.9691 3552899.6094 0.35 0.0 0.0 0.0
null null 207.13 5.88 5986988.7643 3552899.2772 0.35 5.0 5.0 0.1
low null 211.11 4.46 5986988.5585 3552898.9433 0.25 0.0 0.0 0.0
null null 211.5 3.6 5986988.5383 3552898.9105 0.25 0.0 0.0 0.0
null null 214.69 2.85 5986988.3734 3552898.6429 0.12 0.0 0.0 0.0
null null 221.05 2.76 5986988.0446 3552898.1093 0.12 0.0 0.0 0.0
null null 223.04 2.91 5986987.9417 3552897.9423 0.12 0.0 0.0 0.0
null null 223.84 3.65 5986987.9003 3552897.8752 0.25 0.0 0.0 0.0
null null 224.24 4.23 5986987.8796 3552897.8416 0.25 0.0 0.0 0.0
low null 225.84 5.05 5986987.7969 3552897.7074 0.35 5.0 5.0 0.1
null null 230.0 5.3269 5986987.5818 3552897.3583 0.35 0.0 0.0 0.0
null null 251.38 6.75 5986986.4763 3552895.5645 0.35 0.0 0.0 0.0
null true 285.89 6.54 5986984.692 3552892.669 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>58.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552916.656 5986999.474 5.83,3552912.361 5986996.8272 5.22,3552903.7694 5986991.5326 4.97,3552901.6223 5986990.2095 5.05,3552899.6094 5986988.9691 5.33,3552899.2772 5986988.7643 5.88,3552898.9433 5986988.5585 4.46,3552898.9105 5986988.5383 3.6,3552898.6429 5986988.3734 2.85,3552898.1093 5986988.0446 2.76,3552897.9423 5986987.9417 2.91,3552897.8752 5986987.9003 3.65,3552897.8416 5986987.8796 4.23,3552897.7074 5986987.7969 5.05,3552897.3583 5986987.5818 5.3269,3552895.5645 5986986.4763 6.75,3552892.669 5986984.692 6.54</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595117574">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595117730">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.79 5987035.337 3552892.388 0.35 0.0 0.0 0.0
null null 102.34 5.15 5987030.6556 3552884.2645 0.35 0.0 0.0 0.0
null null 204.71 5.03 5987025.9729 3552876.1385 0.35 0.0 0.0 0.0
null null 231.83 5.22 5987024.7324 3552873.9858 0.35 5.0 5.0 0.1
low null 233.0 4.9685 5987024.6788 3552873.8929 0.25 0.0 0.0 0.0
null null 235.04 4.53 5987024.5855 3552873.731 0.25 0.0 0.0 0.0
null null 235.84 3.44 5987024.5489 3552873.6675 0.12 0.0 0.0 0.0
null null 237.43 2.86 5987024.4762 3552873.5413 0.12 0.0 0.0 0.0
null null 240.59 2.54 5987024.3316 3552873.2904 0.12 0.0 0.0 0.0
null null 243.76 2.56 5987024.1866 3552873.0388 0.12 0.0 0.0 0.0
null null 246.14 2.85 5987024.0778 3552872.8499 0.25 0.0 0.0 0.0
null null 247.73 4.46 5987024.005 3552872.7237 0.25 0.0 0.0 0.0
low null 249.32 5.2 5987023.9323 3552872.5975 0.35 4.0 4.0 0.1
null null 255.0 5.3475 5987023.6725 3552872.1466 0.35 4.0 4.0 0.1
null null 271.65 5.78 5987022.9109 3552870.825 0.35 0.0 0.0 0.0
null true 307.39 6.48 5987021.276 3552867.988 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>58.6470</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552892.388 5987035.337 5.79,3552884.2645 5987030.6556 5.15,3552876.1385 5987025.9729 5.03,3552873.9858 5987024.7324 5.22,3552873.8929 5987024.6788 4.9685,3552873.731 5987024.5855 4.53,3552873.6675 5987024.5489 3.44,3552873.5413 5987024.4762 2.86,3552873.2904 5987024.3316 2.54,3552873.0388 5987024.1866 2.56,3552872.8499 5987024.0778 2.85,3552872.7237 5987024.005 4.46,3552872.5975 5987023.9323 5.2,3552872.1466 5987023.6725 5.3475,3552870.825 5987022.9109 5.78,3552867.988 5987021.276 6.48</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951189812">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595118980">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.74 5987083.415 3552878.232 0.35 0.0 0.0 0.0
null null 102.37 5.21 5987082.0364 3552869.9228 0.35 0.0 0.0 0.0
null null 230.35 5.08 5987080.3129 3552859.5349 0.35 0.0 0.0 0.0
low null 246.24 5.3 5987080.0989 3552858.2451 0.25 0.0 0.0 0.0
null null 247.82 5.07 5987080.0776 3552858.1169 0.25 0.0 0.0 0.0
null null 249.01 4.67 5987080.0616 3552858.0203 0.25 0.0 0.0 0.0
null null 250.01 3.46 5987080.0481 3552857.9391 0.12 0.0 0.0 0.0
null null 251.61 3.03 5987080.0266 3552857.8093 0.12 0.0 0.0 0.0
null null 254.8 2.68 5987079.9836 3552857.5503 0.12 0.0 0.0 0.0
null null 259.62 2.69 5987079.9187 3552857.1591 0.12 0.0 0.0 0.0
null null 261.02 3.2 5987079.8999 3552857.0455 0.25 0.0 0.0 0.0
null null 261.82 4.46 5987079.8891 3552856.9805 0.25 0.0 0.0 0.0
low null 262.62 4.82 5987079.8783 3552856.9156 0.35 4.0 4.0 0.1
null null 275.42 5.8 5987079.7059 3552855.8766 0.35 0.0 0.0 0.0
null null 288.22 6.39 5987079.5336 3552854.8377 0.35 0.0 0.0 0.0
null true 327.32 6.79 5987079.007 3552851.664 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>58.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552878.232 5987083.415 5.74,3552869.9228 5987082.0364 5.21,3552859.5349 5987080.3129 5.08,3552858.2451 5987080.0989 5.3,3552858.1169 5987080.0776 5.07,3552858.0203 5987080.0616 4.67,3552857.9391 5987080.0481 3.46,3552857.8093 5987080.0266 3.03,3552857.5503 5987079.9836 2.68,3552857.1591 5987079.9187 2.69,3552857.0455 5987079.8999 3.2,3552856.9805 5987079.8891 4.46,3552856.9156 5987079.8783 4.82,3552855.8766 5987079.7059 5.8,3552854.8377 5987079.5336 6.39,3552851.664 5987079.007 6.79</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595120237">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595120232">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.92 5987128.737 3552875.397 0.35 0.0 0.0 0.0
null null 102.34 5.24 5987128.7838 3552867.9237 0.35 0.0 0.0 0.0
null null 204.73 5.08 5987128.8305 3552860.4467 0.35 0.0 0.0 0.0
low null 246.29 5.18 5987128.8495 3552857.4118 0.25 0.0 0.0 0.0
null null 248.28 4.73 5987128.8504 3552857.2665 0.25 0.0 0.0 0.0
null null 249.88 3.38 5987128.8512 3552857.1497 0.12 0.0 0.0 0.0
null null 251.47 3.09 5987128.8519 3552857.0336 0.12 0.0 0.0 0.0
null null 261.04 2.68 5987128.8562 3552856.3347 0.12 0.0 0.0 0.0
null null 262.03 3.11 5987128.8567 3552856.2624 0.25 0.0 0.0 0.0
null null 262.63 4.42 5987128.857 3552856.2186 0.25 0.0 0.0 0.0
low null 264.23 5.22 5987128.8577 3552856.1018 0.35 4.0 4.0 0.1
null null 270.0 5.2651 5987128.8603 3552855.6804 0.35 0.0 0.0 0.0
null null 296.2 5.47 5987128.8723 3552853.7672 0.35 0.0 0.0 0.0
null null 309.0 6.5 5987128.8782 3552852.8325 0.35 0.0 0.0 0.0
null null 321.79 6.96 5987128.884 3552851.8985 0.35 0.0 0.0 0.0
null true 343.68 7.3 5987128.894 3552850.3 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>58.7490</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552875.397 5987128.737 5.92,3552867.9237 5987128.7838 5.24,3552860.4467 5987128.8305 5.08,3552857.4118 5987128.8495 5.18,3552857.2665 5987128.8504 4.73,3552857.1497 5987128.8512 3.38,3552857.0336 5987128.8519 3.09,3552856.3347 5987128.8562 2.68,3552856.2624 5987128.8567 3.11,3552856.2186 5987128.857 4.42,3552856.1018 5987128.8577 5.22,3552855.6804 5987128.8603 5.2651,3552853.7672 5987128.8723 5.47,3552852.8325 5987128.8782 6.5,3552851.8985 5987128.884 6.96,3552850.3 5987128.894 7.3</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595120542">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595120548">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.34 5987177.037 3552880.804 0.35
null null 12.79 6.79 5987177.3244 3552879.7785 0.35
null null 38.37 6.84 5987177.8991 3552877.7276 0.35
null null 76.73 5.6 5987178.7609 3552874.6519 0.35
null null 204.63 5.17 5987181.6346 3552864.3972 0.35
null null 235.01 5.34 5987182.3171 3552861.9613 0.35
low null 237.0 5.63 5987182.3618 3552861.8018 0.2
null null 239.77 4.04 5987182.4241 3552861.5797 0.2
null null 240.57 3.11 5987182.442 3552861.5156 0.12
null null 246.12 2.11 5987182.5667 3552861.0706 0.12
null null 252.5 3.48 5987182.7101 3552860.559 0.2
null null 253.09 4.38 5987182.7233 3552860.5117 0.2
low null 256.29 5.4 5987182.7952 3552860.2552 0.35
null null 262.68 5.42 5987182.9388 3552859.7428 0.35
null null 297.84 4.7 5987183.7288 3552856.9238 0.35
null null 310.62 5.63 5987184.0159 3552855.8991 0.35
null true 350.37 7.11 5987184.909 3552852.712 0.35
]]></om:result>
       <prof:station>58.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552880.804 5987177.037 6.34,3552879.7785 5987177.3244 6.79,3552877.7276 5987177.8991 6.84,3552874.6519 5987178.7609 5.6,3552864.3972 5987181.6346 5.17,3552861.9613 5987182.3171 5.34,3552861.8018 5987182.3618 5.63,3552861.5797 5987182.4241 4.04,3552861.5156 5987182.442 3.11,3552861.0706 5987182.5667 2.11,3552860.559 5987182.7101 3.48,3552860.5117 5987182.7233 4.38,3552860.2552 5987182.7952 5.4,3552859.7428 5987182.9388 5.42,3552856.9238 5987183.7288 4.7,3552855.8991 5987184.0159 5.63,3552852.712 5987184.909 7.11</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595121635">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951217910">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.47 5987229.265 3552896.213 0.35 0.0 0.0 0.0
null null 31.99 8.25 5987229.7843 3552893.4062 0.35 0.0 0.0 0.0
null null 57.58 7.57 5987230.1997 3552891.1609 0.35 0.0 0.0 0.0
null null 108.78 5.28 5987231.0309 3552886.6687 0.35 0.0 0.0 0.0
null null 185.58 5.26 5987232.2777 3552879.9302 0.35 0.0 0.0 0.0
null null 212.8 5.52 5987232.7195 3552877.542 0.35 0.0 0.0 0.0
low null 214.38 5.12 5987232.7452 3552877.4033 0.2 0.0 0.0 0.0
null null 216.35 4.05 5987232.7772 3552877.2305 0.2 0.0 0.0 0.0
null null 216.74 3.49 5987232.7835 3552877.1963 0.12 0.0 0.0 0.0
null null 218.32 2.82 5987232.8092 3552877.0576 0.12 0.0 0.0 0.0
null null 221.5 2.75 5987232.8608 3552876.7786 0.12 0.0 0.0 0.0
null null 227.12 3.52 5987232.952 3552876.2855 0.2 0.0 0.0 0.0
null null 227.72 4.36 5987232.9617 3552876.2329 0.2 0.0 0.0 0.0
low null 230.86 5.24 5987233.0127 3552875.9574 0.35 0.0 0.0 0.0
null null 237.22 5.6 5987233.116 3552875.3994 0.35 0.0 0.0 0.0
null null 275.6 5.08 5987233.739 3552872.0319 0.35 7.0 7.0 0.2
null null 301.18 5.43 5987234.1543 3552869.7875 0.35 0.0 0.0 0.0
null true 329.93 6.26 5987234.621 3552867.265 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>58.8530</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552896.213 5987229.265 7.47,3552893.4062 5987229.7843 8.25,3552891.1609 5987230.1997 7.57,3552886.6687 5987231.0309 5.28,3552879.9302 5987232.2777 5.26,3552877.542 5987232.7195 5.52,3552877.4033 5987232.7452 5.12,3552877.2305 5987232.7772 4.05,3552877.1963 5987232.7835 3.49,3552877.0576 5987232.8092 2.82,3552876.7786 5987232.8608 2.75,3552876.2855 5987232.952 3.52,3552876.2329 5987232.9617 4.36,3552875.9574 5987233.0127 5.24,3552875.3994 5987233.116 5.6,3552872.0319 5987233.739 5.08,3552869.7875 5987234.1543 5.43,3552867.265 5987234.621 6.26</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595123044">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951230414">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.68 5987278.632 3552900.592 0.35
null null 51.13 6.4 5987278.3293 3552895.6832 0.35
null null 102.33 5.32 5987278.0263 3552890.7676 0.35
null null 179.13 5.12 5987277.5716 3552883.3943 0.35
null null 195.13 5.44 5987277.4769 3552881.8581 0.35
low null 199.1 5.9 5987277.4534 3552881.477 0.2
null null 201.48 4.92 5987277.4393 3552881.2485 0.2
null null 203.07 3.41 5987277.4299 3552881.0959 0.12
null null 204.66 2.99 5987277.4205 3552880.9432 0.12
null null 207.85 2.72 5987277.4016 3552880.6369 0.12
null null 212.63 2.89 5987277.3733 3552880.178 0.12
null null 214.22 3.44 5987277.3639 3552880.0254 0.2
null null 215.81 4.59 5987277.3545 3552879.8727 0.2
low null 217.41 5.19 5987277.345 3552879.7191 0.35
null null 223.8 5.58 5987277.3072 3552879.1056 0.35
null null 249.41 5.87 5987277.1556 3552876.6469 0.35
null null 262.19 5.77 5987277.08 3552875.4199 0.35
null true 307.29 6.7 5987276.813 3552871.09 0.35
]]></om:result>
       <prof:station>58.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552900.592 5987278.632 7.68,3552895.6832 5987278.3293 6.4,3552890.7676 5987278.0263 5.32,3552883.3943 5987277.5716 5.12,3552881.8581 5987277.4769 5.44,3552881.477 5987277.4534 5.9,3552881.2485 5987277.4393 4.92,3552881.0959 5987277.4299 3.41,3552880.9432 5987277.4205 2.99,3552880.6369 5987277.4016 2.72,3552880.178 5987277.3733 2.89,3552880.0254 5987277.3639 3.44,3552879.8727 5987277.3545 4.59,3552879.7191 5987277.345 5.19,3552879.1056 5987277.3072 5.58,3552876.6469 5987277.1556 5.87,3552875.4199 5987277.08 5.77,3552871.09 5987276.813 6.7</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951233510">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951233520">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.31 5987330.724 3552889.034 0.35
null null 51.12 6.59 5987329.1756 3552884.031 0.35
null null 89.46 5.4 5987328.0143 3552880.2788 0.35
null null 140.59 5.13 5987326.4656 3552875.2748 0.35
null null 166.19 5.26 5987325.6901 3552872.7694 0.35
null null 191.78 5.63 5987324.915 3552870.2649 0.35
low null 193.37 5.46 5987324.8669 3552870.1093 0.2
null null 196.55 4.46 5987324.7705 3552869.7981 0.2
null null 197.35 3.84 5987324.7463 3552869.7198 0.12
null null 199.73 2.79 5987324.6742 3552869.4869 0.12
null null 202.88 2.55 5987324.5788 3552869.1786 0.12
null null 206.07 2.83 5987324.4822 3552868.8664 0.12
null null 208.07 3.25 5987324.4216 3552868.6707 0.2
null null 208.88 3.92 5987324.3971 3552868.5914 0.2
low null 212.08 5.31 5987324.3001 3552868.2782 0.35
null null 218.47 6.09 5987324.1066 3552867.6529 0.35
null null 244.04 6.28 5987323.3321 3552865.1504 0.35
null true 286.07 7.21 5987322.059 3552861.037 0.35
]]></om:result>
       <prof:station>58.9510</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552889.034 5987330.724 7.31,3552884.031 5987329.1756 6.59,3552880.2788 5987328.0143 5.4,3552875.2748 5987326.4656 5.13,3552872.7694 5987325.6901 5.26,3552870.2649 5987324.915 5.63,3552870.1093 5987324.8669 5.46,3552869.7981 5987324.7705 4.46,3552869.7198 5987324.7463 3.84,3552869.4869 5987324.6742 2.79,3552869.1786 5987324.5788 2.55,3552868.8664 5987324.4822 2.83,3552868.6707 5987324.4216 3.25,3552868.5914 5987324.3971 3.92,3552868.2782 5987324.3001 5.31,3552867.6529 5987324.1066 6.09,3552865.1504 5987323.3321 6.28,3552861.037 5987322.059 7.21</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951244515">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951244525">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.24 5987375.834 3552874.402 0.35
null null 19.18 5.87 5987375.4266 3552872.451 0.35
null null 44.79 6.0 5987374.8827 3552869.8459 0.35
null null 95.98 5.39 5987373.7954 3552864.6389 0.35
null null 147.16 5.34 5987372.7083 3552859.4328 0.35
null null 191.87 5.6 5987371.7586 3552854.8849 0.35
null null 195.08 5.47 5987371.6905 3552854.5584 0.35
low null 199.09 5.67 5987371.6053 3552854.1505 0.2
null null 202.05 4.13 5987371.5424 3552853.8494 0.2
null null 202.44 3.31 5987371.5341 3552853.8097 0.12
null null 205.64 2.89 5987371.4662 3552853.4842 0.12
null null 208.84 2.85 5987371.3982 3552853.1587 0.12
null null 213.63 3.24 5987371.2965 3552852.6714 0.12
null null 214.82 3.56 5987371.2712 3552852.5504 0.12
null null 215.22 4.4 5987371.2627 3552852.5097 0.2
null null 216.81 4.81 5987371.2289 3552852.348 0.2
null null 218.39 5.54 5987371.1953 3552852.1873 0.35
low null 221.57 6.4 5987371.1278 3552851.8638 0.35
null null 234.37 7.04 5987370.8559 3552850.5618 0.35
null true 276.08 7.72 5987369.97 3552846.319 0.35
]]></om:result>
       <prof:station>59.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552874.402 5987375.834 7.24,3552872.451 5987375.4266 5.87,3552869.8459 5987374.8827 6.0,3552864.6389 5987373.7954 5.39,3552859.4328 5987372.7083 5.34,3552854.8849 5987371.7586 5.6,3552854.5584 5987371.6905 5.47,3552854.1505 5987371.6053 5.67,3552853.8494 5987371.5424 4.13,3552853.8097 5987371.5341 3.31,3552853.4842 5987371.4662 2.89,3552853.1587 5987371.3982 2.85,3552852.6714 5987371.2965 3.24,3552852.5504 5987371.2712 3.56,3552852.5097 5987371.2627 4.4,3552852.348 5987371.2289 4.81,3552852.1873 5987371.1953 5.54,3552851.8638 5987371.1278 6.4,3552850.5618 5987370.8559 7.04,3552846.319 5987369.97 7.72</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951247619">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951247614">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.89 5987423.79 3552867.546 0.35
null null 19.16 6.47 5987423.6715 3552865.5705 0.35
null null 57.48 5.42 5987423.4346 3552861.6196 0.35
null null 108.67 5.2 5987423.1182 3552856.3417 0.35
null null 172.68 5.29 5987422.7225 3552849.742 0.35
null null 185.46 5.57 5987422.6434 3552848.4243 0.35
null null 195.07 5.55 5987422.584 3552847.4335 0.35
low null 195.87 5.41 5987422.5791 3552847.351 0.2
null null 198.46 3.98 5987422.5631 3552847.0839 0.12
null null 198.86 3.08 5987422.5606 3552847.0427 0.12
null null 200.45 2.68 5987422.5508 3552846.8788 0.12
null null 206.8 3.24 5987422.5115 3552846.2241 0.12
null null 209.97 3.67 5987422.4919 3552845.8972 0.2
null null 210.77 4.42 5987422.487 3552845.8147 0.2
null null 211.56 4.84 5987422.4821 3552845.7333 0.2
low null 213.16 5.26 5987422.4722 3552845.5683 0.35
null null 219.59 5.88 5987422.4324 3552844.9053 0.35
null null 245.18 7.17 5987422.2742 3552842.2669 0.35
null true 267.38 8.47 5987422.137 3552839.978 0.35
]]></om:result>
       <prof:station>59.0510</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552867.546 5987423.79 5.89,3552865.5705 5987423.6715 6.47,3552861.6196 5987423.4346 5.42,3552856.3417 5987423.1182 5.2,3552849.742 5987422.7225 5.29,3552848.4243 5987422.6434 5.57,3552847.4335 5987422.584 5.55,3552847.351 5987422.5791 5.41,3552847.0839 5987422.5631 3.98,3552847.0427 5987422.5606 3.08,3552846.8788 5987422.5508 2.68,3552846.2241 5987422.5115 3.24,3552845.8972 5987422.4919 3.67,3552845.8147 5987422.487 4.42,3552845.7333 5987422.4821 4.84,3552845.5683 5987422.4722 5.26,3552844.9053 5987422.4324 5.88,3552842.2669 5987422.2742 7.17,3552839.978 5987422.137 8.47</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951258521">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951258519">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.45 5987472.874 3552865.416 0.35
null null 51.2 5.5 5987473.2465 3552858.846 0.35
null null 102.36 5.29 5987473.6187 3552852.2811 0.35
null null 153.54 5.36 5987473.9911 3552845.7136 0.35
low null 193.43 5.79 5987474.2813 3552840.5949 0.2
null null 196.6 4.57 5987474.3044 3552840.1881 0.12
null null 199.77 2.78 5987474.3275 3552839.7813 0.12
null null 200.56 2.51 5987474.3332 3552839.6799 0.12
null null 202.14 2.46 5987474.3447 3552839.4772 0.12
null null 205.29 2.91 5987474.3676 3552839.073 0.12
null null 206.89 3.39 5987474.3793 3552838.8677 0.2
null null 207.7 4.24 5987474.3852 3552838.7637 0.2
null null 209.3 4.92 5987474.3968 3552838.5584 0.2
low null 218.87 6.51 5987474.4664 3552837.3304 0.35
null null 225.26 8.1 5987474.5129 3552836.5104 0.35
null true 259.63 9.11 5987474.763 3552832.1 0.35
]]></om:result>
       <prof:station>59.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552865.416 5987472.874 6.45,3552858.846 5987473.2465 5.5,3552852.2811 5987473.6187 5.29,3552845.7136 5987473.9911 5.36,3552840.5949 5987474.2813 5.79,3552840.1881 5987474.3044 4.57,3552839.7813 5987474.3275 2.78,3552839.6799 5987474.3332 2.51,3552839.4772 5987474.3447 2.46,3552839.073 5987474.3676 2.91,3552838.8677 5987474.3793 3.39,3552838.7637 5987474.3852 4.24,3552838.5584 5987474.3968 4.92,3552837.3304 5987474.4664 6.51,3552836.5104 5987474.5129 8.1,3552832.1 5987474.763 9.11</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951260129">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951260119">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.35 5987512.363 3552867.417 0.35
null null 12.79 6.09 5987513.1122 3552866.287 0.35
null null 25.57 6.09 5987513.8609 3552865.158 0.35
null null 76.72 5.36 5987516.8572 3552860.639 0.35
null null 127.88 5.29 5987519.8541 3552856.1192 0.35
null null 153.47 5.41 5987521.3531 3552853.8584 0.35
low null 183.82 5.93 5987523.131 3552851.1771 0.25
null null 190.22 4.96 5987523.5059 3552850.6117 0.15
null null 191.02 4.71 5987523.5528 3552850.541 0.15
null null 191.62 4.2 5987523.5879 3552850.488 0.15
null null 191.92 3.42 5987523.6055 3552850.4615 0.15
null null 200.69 2.78 5987524.1193 3552849.6867 0.15
null null 201.49 3.06 5987524.1661 3552849.616 0.2
null null 202.28 3.55 5987524.2124 3552849.5462 0.2
null null 202.67 4.11 5987524.2352 3552849.5117 0.2
null null 204.26 4.96 5987524.3284 3552849.3713 0.2
null null 205.85 5.57 5987524.4215 3552849.2308 0.35
low null 209.02 6.09 5987524.6072 3552848.9507 0.35
null null 257.8 8.58 5987527.4647 3552844.6412 0.35
null null 257.9 9.05 5987527.4706 3552844.6323 0.35
null true 258.3 9.06 5987527.494 3552844.597 0.35
]]></om:result>
       <prof:station>59.1500</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552867.417 5987512.363 6.35,3552866.287 5987513.1122 6.09,3552865.158 5987513.8609 6.09,3552860.639 5987516.8572 5.36,3552856.1192 5987519.8541 5.29,3552853.8584 5987521.3531 5.41,3552851.1771 5987523.131 5.93,3552850.6117 5987523.5059 4.96,3552850.541 5987523.5528 4.71,3552850.488 5987523.5879 4.2,3552850.4615 5987523.6055 3.42,3552849.6867 5987524.1193 2.78,3552849.616 5987524.1661 3.06,3552849.5462 5987524.2124 3.55,3552849.5117 5987524.2352 4.11,3552849.3713 5987524.3284 4.96,3552849.2308 5987524.4215 5.57,3552848.9507 5987524.6072 6.09,3552844.6412 5987527.4647 8.58,3552844.6323 5987527.4706 9.05,3552844.597 5987527.494 9.06</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951271014">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595127100">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.21 5987543.647 3552901.583 0.35
null null 25.56 6.2 5987545.9544 3552899.7555 0.35
null null 51.14 5.72 5987548.2637 3552897.9266 0.35
null null 102.34 5.34 5987552.8858 3552894.2659 0.35
null null 127.93 5.37 5987555.1959 3552892.4363 0.35
null null 150.27 5.68 5987557.2126 3552890.8391 0.35
low null 155.03 6.07 5987557.6424 3552890.4987 0.2
null null 158.19 4.78 5987557.9276 3552890.2728 0.2
null null 160.56 4.05 5987558.1416 3552890.1034 0.2
null null 160.95 3.54 5987558.1768 3552890.0755 0.12
null null 162.54 2.65 5987558.3203 3552889.9618 0.12
null null 164.12 2.52 5987558.463 3552889.8488 0.12
null null 168.9 3.24 5987558.8945 3552889.5071 0.25
null null 169.7 4.16 5987558.9667 3552889.4499 0.25
low null 174.5 5.41 5987559.4 3552889.1067 0.35
null null 200.06 5.27 5987561.7074 3552887.2792 0.35
null null 212.84 5.55 5987562.8612 3552886.3655 0.35
null null 225.62 6.56 5987564.0149 3552885.4517 0.35
null true 269.31 7.73 5987567.959 3552882.328 0.35
]]></om:result>
       <prof:station>59.2000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552901.583 5987543.647 6.21,3552899.7555 5987545.9544 6.2,3552897.9266 5987548.2637 5.72,3552894.2659 5987552.8858 5.34,3552892.4363 5987555.1959 5.37,3552890.8391 5987557.2126 5.68,3552890.4987 5987557.6424 6.07,3552890.2728 5987557.9276 4.78,3552890.1034 5987558.1416 4.05,3552890.0755 5987558.1768 3.54,3552889.9618 5987558.3203 2.65,3552889.8488 5987558.463 2.52,3552889.5071 5987558.8945 3.24,3552889.4499 5987558.9667 4.16,3552889.1067 5987559.4 5.41,3552887.2792 5987561.7074 5.27,3552886.3655 5987562.8612 5.55,3552885.4517 5987564.0149 6.56,3552882.328 5987567.959 7.73</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951272626">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595127264">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.84 5987582.124 3552944.635 0.35
null null 25.57 5.56 5987584.3911 3552942.8931 0.35
null null 89.52 5.42 5987590.0609 3552938.5366 0.35
null null 102.31 5.84 5987591.1949 3552937.6653 0.35
low null 108.65 5.84 5987591.757 3552937.2334 0.25
null null 111.84 4.42 5987592.0398 3552937.0161 0.25
null null 115.04 2.45 5987592.3235 3552936.7981 0.12
null null 118.25 2.31 5987592.6081 3552936.5794 0.12
null null 119.85 3.03 5987592.75 3552936.4704 0.12
null null 121.45 3.47 5987592.8919 3552936.3614 0.2
null null 123.05 4.25 5987593.0337 3552936.2524 0.2
low null 127.84 5.48 5987593.4584 3552935.9261 0.35
null null 134.16 5.61 5987594.0187 3552935.4955 0.35
null null 153.38 5.33 5987595.7228 3552934.1862 0.35
null true 260.6 8.41 5987605.229 3552926.882 0.35
]]></om:result>
       <prof:station>59.2560</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552944.635 5987582.124 5.84,3552942.8931 5987584.3911 5.56,3552938.5366 5987590.0609 5.42,3552937.6653 5987591.1949 5.84,3552937.2334 5987591.757 5.84,3552937.0161 5987592.0398 4.42,3552936.7981 5987592.3235 2.45,3552936.5794 5987592.6081 2.31,3552936.4704 5987592.75 3.03,3552936.3614 5987592.8919 3.47,3552936.2524 5987593.0337 4.25,3552935.9261 5987593.4584 5.48,3552935.4955 5987594.0187 5.61,3552934.1862 5987595.7228 5.33,3552926.882 5987605.229 8.41</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951283534">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951283535">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.75 5987612.677 3552978.432 0.35
null null 51.23 5.49 5987616.7006 3552973.005 0.35
null null 70.39 5.69 5987618.2054 3552970.9753 0.35
low null 75.99 5.57 5987618.6452 3552970.3821 0.2
null null 77.6 5.06 5987618.7716 3552970.2115 0.2
null null 79.19 4.27 5987618.8965 3552970.0431 0.2
null null 79.98 3.68 5987618.9586 3552969.9594 0.12
null null 81.55 3.16 5987619.0819 3552969.7931 0.12
null null 84.73 3.01 5987619.3316 3552969.4562 0.12
null null 87.9 3.24 5987619.5806 3552969.1204 0.12
null null 90.28 3.81 5987619.7675 3552968.8683 0.2
null null 91.07 4.43 5987619.8296 3552968.7846 0.2
null null 92.66 5.07 5987619.9544 3552968.6161 0.2
low null 94.24 5.46 5987620.0785 3552968.4488 0.35
null null 100.59 5.79 5987620.5773 3552967.7761 0.35
null null 126.12 5.29 5987622.5824 3552965.0716 0.35
null null 202.91 8.01 5987628.6134 3552956.9369 0.35
null true 230.7 8.67 5987630.796 3552953.993 0.35
]]></om:result>
       <prof:station>59.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552978.432 5987612.677 5.75,3552973.005 5987616.7006 5.49,3552970.9753 5987618.2054 5.69,3552970.3821 5987618.6452 5.57,3552970.2115 5987618.7716 5.06,3552970.0431 5987618.8965 4.27,3552969.9594 5987618.9586 3.68,3552969.7931 5987619.0819 3.16,3552969.4562 5987619.3316 3.01,3552969.1204 5987619.5806 3.24,3552968.8683 5987619.7675 3.81,3552968.7846 5987619.8296 4.43,3552968.6161 5987619.9544 5.07,3552968.4488 5987620.0785 5.46,3552967.7761 5987620.5773 5.79,3552965.0716 5987622.5824 5.29,3552956.9369 5987628.6134 8.01,3552953.993 5987630.796 8.67</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595128661">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951286633">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.55 5987678.286 3552997.679 0.35
null null 25.61 5.58 5987678.2 3552994.258 0.35
null null 44.79 5.18 5987678.1357 3552991.6959 0.35
low null 70.35 5.37 5987678.0499 3552988.2815 0.35
null null 74.35 5.14 5987678.0365 3552987.7472 0.2
null null 75.95 4.57 5987678.0311 3552987.5334 0.2
null null 77.53 3.67 5987678.0258 3552987.3224 0.12
null null 79.11 2.96 5987678.0205 3552987.1113 0.12
null null 82.3 2.77 5987678.0098 3552986.6852 0.12
null null 87.09 3.38 5987677.9937 3552986.0453 0.2
null null 87.89 4.2 5987677.991 3552985.9385 0.2
null null 89.48 4.8 5987677.9857 3552985.7261 0.2
low null 95.84 5.54 5987677.9643 3552984.8765 0.35
null null 99.02 5.71 5987677.9537 3552984.4517 0.35
null null 124.58 5.3 5987677.8679 3552981.0373 0.35
null null 137.38 5.62 5987677.8249 3552979.3275 0.35
null null 163.0 6.9 5987677.7389 3552975.9051 0.35
null true 210.35 8.5 5987677.58 3552969.58 0.35
]]></om:result>
       <prof:station>59.3650</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552997.679 5987678.286 5.55,3552994.258 5987678.2 5.58,3552991.6959 5987678.1357 5.18,3552988.2815 5987678.0499 5.37,3552987.7472 5987678.0365 5.14,3552987.5334 5987678.0311 4.57,3552987.3224 5987678.0258 3.67,3552987.1113 5987678.0205 2.96,3552986.6852 5987678.0098 2.77,3552986.0453 5987677.9937 3.38,3552985.9385 5987677.991 4.2,3552985.7261 5987677.9857 4.8,3552984.8765 5987677.9643 5.54,3552984.4517 5987677.9537 5.71,3552981.0373 5987677.8679 5.3,3552979.3275 5987677.8249 5.62,3552975.9051 5987677.7389 6.9,3552969.58 5987677.58 8.5</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951297621">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595129767">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.49 5987726.363 3552998.291 0.35
null null 51.11 5.15 5987726.4985 3552990.9708 0.35
null null 76.66 5.28 5987726.5662 3552987.3114 0.35
null null 89.43 5.49 5987726.6001 3552985.4824 0.35
low null 94.18 5.72 5987726.6127 3552984.8021 0.2
null null 97.0 4.4534 5987726.6201 3552984.3982 0.2
null null 100.57 2.85 5987726.6296 3552983.8869 0.12
null null 102.16 2.68 5987726.6338 3552983.6592 0.12
null null 104.5 3.197 5987726.64 3552983.324 0.12
null null 106.55 3.65 5987726.6455 3552983.0304 0.2
null null 106.95 4.29 5987726.6465 3552982.9731 0.2
low null 110.14 5.21 5987726.655 3552982.5162 0.35
null null 148.47 5.92 5987726.7566 3552977.0264 0.35
null null 174.01 7.92 5987726.8243 3552973.3685 0.35
null true 205.97 8.5 5987726.909 3552968.791 0.35
]]></om:result>
       <prof:station>59.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3552998.291 5987726.363 5.49,3552990.9708 5987726.4985 5.15,3552987.3114 5987726.5662 5.28,3552985.4824 5987726.6001 5.49,3552984.8021 5987726.6127 5.72,3552984.3982 5987726.6201 4.4534,3552983.8869 5987726.6296 2.85,3552983.6592 5987726.6338 2.68,3552983.324 5987726.64 3.197,3552983.0304 5987726.6455 3.65,3552982.9731 5987726.6465 4.29,3552982.5162 5987726.655 5.21,3552977.0264 5987726.7566 5.92,3552973.3685 5987726.8243 7.92,3552968.791 5987726.909 8.5</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951308534">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595130851">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.5 5987768.793 3553000.186 0.35
null null 51.13 5.21 5987771.1657 3552994.4286 0.35
null null 76.7 5.21 5987772.3522 3552991.5494 0.35
low null 105.48 5.48 5987773.6878 3552988.3087 0.35
null null 107.9 5.2 5987773.8001 3552988.0362 0.2
null null 108.69 4.81 5987773.8367 3552987.9472 0.2
null null 109.48 4.1 5987773.8734 3552987.8583 0.12
null null 111.06 3.36 5987773.9467 3552987.6804 0.12
null null 114.22 2.87 5987774.0934 3552987.3246 0.12
null null 117.39 2.8 5987774.2405 3552986.9676 0.12
null null 118.98 3.63 5987774.3142 3552986.7886 0.2
null null 119.98 4.68 5987774.3607 3552986.676 0.2
low null 123.16 5.32 5987774.5082 3552986.3179 0.35
null null 129.51 5.86 5987774.8029 3552985.6029 0.35
null null 167.92 7.9 5987776.5853 3552981.2778 0.35
null true 207.5 9.05 5987778.422 3552976.821 0.35
]]></om:result>
       <prof:station>59.4480</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553000.186 5987768.793 5.5,3552994.4286 5987771.1657 5.21,3552991.5494 5987772.3522 5.21,3552988.3087 5987773.6878 5.48,3552988.0362 5987773.8001 5.2,3552987.9472 5987773.8367 4.81,3552987.8583 5987773.8734 4.1,3552987.6804 5987773.9467 3.36,3552987.3246 5987774.0934 2.87,3552986.9676 5987774.2405 2.8,3552986.7886 5987774.3142 3.63,3552986.676 5987774.3607 4.68,3552986.3179 5987774.5082 5.32,3552985.6029 5987774.8029 5.86,3552981.2778 5987776.5853 7.9,3552976.821 5987778.422 9.05</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951310127">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951310125">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.5 5987812.802 3553024.917 0.35 0.0 0.0 0.0
null null 25.61 5.23 5987814.4938 3553022.1879 0.35 0.0 0.0 0.0
null null 63.99 5.15 5987817.0293 3553018.098 0.35 0.0 0.0 0.0
null null 89.57 5.52 5987818.7192 3553015.3722 0.35 0.0 0.0 0.0
null null 108.0 5.56 5987819.9367 3553013.4082 0.35 0.0 0.0 0.0
low null 109.75 5.82 5987820.0523 3553013.2217 0.2 0.0 0.0 0.0
null null 110.95 4.96 5987820.1316 3553013.0938 0.2 0.0 0.0 0.0
null null 111.75 4.08 5987820.1844 3553013.0086 0.12 0.0 0.0 0.0
null null 114.16 2.94 5987820.3436 3553012.7518 0.12 0.0 0.0 0.0
null null 120.92 2.85 5987820.7902 3553012.0314 0.12 0.0 0.0 0.0
null null 123.3 4.65 5987820.9474 3553011.7778 0.2 0.0 0.0 0.0
null null 124.89 4.87 5987821.0525 3553011.6084 0.2 0.0 0.0 0.0
low null 126.48 5.33 5987821.1575 3553011.4389 0.35 7.0 7.0 0.1
null null 130.0 5.4466 5987821.39 3553011.0638 0.35 0.0 0.0 0.0
null null 132.82 5.54 5987821.5763 3553010.7633 0.35 0.0 0.0 0.0
null null 164.79 7.92 5987823.6883 3553007.3565 0.35 0.0 0.0 0.0
null null 190.38 8.34 5987825.3788 3553004.6296 0.35 0.0 0.0 0.0
null null 203.16 8.37 5987826.2231 3553003.2677 0.35 0.0 0.0 0.0
null true 217.13 8.94 5987827.146 3553001.779 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>59.5000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553024.917 5987812.802 5.5,3553022.1879 5987814.4938 5.23,3553018.098 5987817.0293 5.15,3553015.3722 5987818.7192 5.52,3553013.4082 5987819.9367 5.56,3553013.2217 5987820.0523 5.82,3553013.0938 5987820.1316 4.96,3553013.0086 5987820.1844 4.08,3553012.7518 5987820.3436 2.94,3553012.0314 5987820.7902 2.85,3553011.7778 5987820.9474 4.65,3553011.6084 5987821.0525 4.87,3553011.4389 5987821.1575 5.33,3553011.0638 5987821.39 5.4466,3553010.7633 5987821.5763 5.54,3553007.3565 5987823.6883 7.92,3553004.6296 5987825.3788 8.34,3553003.2677 5987826.2231 8.37,3553001.779 5987827.146 8.94</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951322614">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595132264">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.6 5987856.608 3553051.504 0.35 0.0 0.0 0.0
null null 25.58 5.25 5987858.3993 3553048.6166 0.35 0.0 0.0 0.0
null null 76.72 5.31 5987861.9806 3553042.8439 0.35 0.0 0.0 0.0
low null 114.26 5.53 5987864.6095 3553038.6064 0.2 0.0 0.0 0.0
null null 115.78 4.75 5987864.716 3553038.4349 0.2 0.0 0.0 0.0
null null 116.94 3.38 5987864.7972 3553038.3039 0.12 0.0 0.0 0.0
null null 117.72 2.92 5987864.8518 3553038.2159 0.12 0.0 0.0 0.0
null null 119.32 2.67 5987864.9639 3553038.0353 0.12 0.0 0.0 0.0
null null 125.7 3.32 5987865.4107 3553037.3151 0.2 0.0 0.0 0.0
null null 128.86 4.73 5987865.632 3553036.9584 0.2 0.0 0.0 0.0
low null 130.47 5.22 5987865.7447 3553036.7767 0.35 7.0 7.0 0.1
null null 134.0 5.2533 5987865.9919 3553036.3782 0.35 0.0 0.0 0.0
null null 149.55 5.4 5987867.0809 3553034.6229 0.35 0.0 0.0 0.0
null null 162.32 6.08 5987867.9751 3553033.1815 0.35 0.0 0.0 0.0
null null 175.11 6.98 5987868.8708 3553031.7378 0.35 0.0 0.0 0.0
null true 216.01 8.55 5987871.735 3553027.121 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>59.5510</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553051.504 5987856.608 5.6,3553048.6166 5987858.3993 5.25,3553042.8439 5987861.9806 5.31,3553038.6064 5987864.6095 5.53,3553038.4349 5987864.716 4.75,3553038.3039 5987864.7972 3.38,3553038.2159 5987864.8518 2.92,3553038.0353 5987864.9639 2.67,3553037.3151 5987865.4107 3.32,3553036.9584 5987865.632 4.73,3553036.7767 5987865.7447 5.22,3553036.3782 5987865.9919 5.2533,3553034.6229 5987867.0809 5.4,3553033.1815 5987867.9751 6.08,3553031.7378 5987868.8708 6.98,3553027.121 5987871.735 8.55</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951333541">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595133353">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.73 5987900.765 3553073.984 0.35
null null 102.3 5.2 5987906.7156 3553063.1607 0.35
low null 120.66 5.92 5987907.7836 3553061.2183 0.2
null null 123.05 3.91 5987907.9226 3553060.9654 0.12
null null 127.88 2.77 5987908.2036 3553060.4544 0.12
null null 132.28 3.58 5987908.4595 3553059.9889 0.12
null null 133.09 4.35 5987908.5066 3553059.9032 0.2
null null 134.69 4.49 5987908.5997 3553059.7339 0.2
low null 137.89 5.62 5987908.7859 3553059.3953 0.35
null null 153.81 5.03 5987909.7119 3553057.711 0.35
null null 160.21 5.52 5987910.0842 3553057.0339 0.35
null null 173.0 7.07 5987910.8282 3553055.6807 0.35
null null 179.4 7.66 5987911.2004 3553055.0036 0.35
null true 215.89 8.19 5987913.323 3553051.143 0.35
]]></om:result>
       <prof:station>59.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553073.984 5987900.765 5.73,3553063.1607 5987906.7156 5.2,3553061.2183 5987907.7836 5.92,3553060.9654 5987907.9226 3.91,3553060.4544 5987908.2036 2.77,3553059.9889 5987908.4595 3.58,3553059.9032 5987908.5066 4.35,3553059.7339 5987908.5997 4.49,3553059.3953 5987908.7859 5.62,3553057.711 5987909.7119 5.03,3553057.0339 5987910.0842 5.52,3553055.6807 5987910.8282 7.07,3553055.0036 5987911.2004 7.66,3553051.143 5987913.323 8.19</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951335110">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595133516">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.86 5987945.857 3553097.934 0.35 0.0 0.0 0.0
null null 51.2 5.53 5987948.1058 3553093.735 0.35 0.0 0.0 0.0
null null 57.6 5.27 5987948.3869 3553093.2101 0.35 0.0 0.0 0.0
null null 108.78 5.17 5987950.6348 3553089.0127 0.35 0.0 0.0 0.0
null null 124.77 5.56 5987951.3371 3553087.7013 0.35 0.0 0.0 0.0
low null 127.96 5.4 5987951.4772 3553087.4397 0.25 0.0 0.0 0.0
null null 130.85 4.46 5987951.6041 3553087.2026 0.25 0.0 0.0 0.0
null null 131.05 3.66 5987951.6129 3553087.1862 0.12 0.0 0.0 0.0
null null 134.0 3.3642 5987951.7424 3553086.9443 0.12 0.0 0.0 0.0
null null 138.23 2.94 5987951.9282 3553086.5974 0.12 0.0 0.0 0.0
null null 139.83 3.55 5987951.9985 3553086.4662 0.2 0.0 0.0 0.0
null null 141.02 4.36 5987952.0508 3553086.3686 0.2 0.0 0.0 0.0
null null 142.61 4.48 5987952.1206 3553086.2382 0.2 0.0 0.0 0.0
low null 144.99 5.47 5987952.2251 3553086.043 0.35 6.0 6.0 0.1
null null 146.5 5.5008 5987952.2915 3553085.9191 0.35 0.0 0.0 0.0
null null 151.36 5.6 5987952.5049 3553085.5206 0.35 0.0 0.0 0.0
null null 164.16 5.15 5987953.0671 3553084.4708 0.35 0.0 0.0 0.0
null null 176.95 5.3 5987953.6289 3553083.4219 0.35 0.0 0.0 0.0
null null 189.75 5.66 5987954.1911 3553082.3721 0.35 0.0 0.0 0.0
null null 221.75 7.35 5987955.5965 3553079.7477 0.35 0.0 0.0 0.0
null null 228.15 8.45 5987955.8776 3553079.2228 0.35 0.0 0.0 0.0
null true 263.13 8.66 5987957.414 3553076.354 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>59.6520</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553097.934 5987945.857 5.86,3553093.735 5987948.1058 5.53,3553093.2101 5987948.3869 5.27,3553089.0127 5987950.6348 5.17,3553087.7013 5987951.3371 5.56,3553087.4397 5987951.4772 5.4,3553087.2026 5987951.6041 4.46,3553087.1862 5987951.6129 3.66,3553086.9443 5987951.7424 3.3642,3553086.5974 5987951.9282 2.94,3553086.4662 5987951.9985 3.55,3553086.3686 5987952.0508 4.36,3553086.2382 5987952.1206 4.48,3553086.043 5987952.2251 5.47,3553085.9191 5987952.2915 5.5008,3553085.5206 5987952.5049 5.6,3553084.4708 5987953.0671 5.15,3553083.4219 5987953.6289 5.3,3553082.3721 5987954.1911 5.66,3553079.7477 5987955.5965 7.35,3553079.2228 5987955.8776 8.45,3553076.354 5987957.414 8.66</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951346047">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595134609">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.88 5987986.882 3553122.586 0.35 0.0 0.0 0.0
null null 51.2 5.37 5987989.3366 3553118.4363 0.35 0.0 0.0 0.0
null null 115.22 5.28 5987992.4059 3553113.2476 0.35 0.0 0.0 0.0
null null 131.19 6.01 5987993.1715 3553111.9532 0.35 0.0 0.0 0.0
low null 131.98 5.9 5987993.2094 3553111.8892 0.2 0.0 0.0 0.0
null null 133.58 4.78 5987993.2861 3553111.7595 0.2 0.0 0.0 0.0
null null 134.58 3.74 5987993.334 3553111.6785 0.12 0.0 0.0 0.0
null null 137.75 3.14 5987993.486 3553111.4216 0.12 0.0 0.0 0.0
null null 140.93 3.04 5987993.6385 3553111.1638 0.12 0.0 0.0 0.0
null null 144.14 3.55 5987993.7924 3553110.9037 0.12 0.0 0.0 0.0
null null 144.94 4.49 5987993.8307 3553110.8388 0.2 0.0 0.0 0.0
null null 146.55 4.55 5987993.9079 3553110.7083 0.2 0.0 0.0 0.0
low null 149.76 5.67 5987994.0618 3553110.4482 0.35 6.0 6.0 0.1
null null 152.0 5.6472 5987994.1692 3553110.2666 0.35 0.0 0.0 0.0
null null 188.13 5.28 5987995.9013 3553107.3383 0.35 0.0 0.0 0.0
null null 239.34 6.41 5987998.3564 3553103.1878 0.35 0.0 0.0 0.0
null null 290.5 8.43 5988000.8091 3553099.0414 0.35 0.0 0.0 0.0
null true 308.79 8.79 5988001.686 3553097.559 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>59.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553122.586 5987986.882 5.88,3553118.4363 5987989.3366 5.37,3553113.2476 5987992.4059 5.28,3553111.9532 5987993.1715 6.01,3553111.8892 5987993.2094 5.9,3553111.7595 5987993.2861 4.78,3553111.6785 5987993.334 3.74,3553111.4216 5987993.486 3.14,3553111.1638 5987993.6385 3.04,3553110.9037 5987993.7924 3.55,3553110.8388 5987993.8307 4.49,3553110.7083 5987993.9079 4.55,3553110.4482 5987994.0618 5.67,3553110.2666 5987994.1692 5.6472,3553107.3383 5987995.9013 5.28,3553103.1878 5987998.3564 6.41,3553099.0414 5988000.8091 8.43,3553097.559 5988001.686 8.79</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951358547">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951358510">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.8 5988074.914 3553169.546 0.35 0.0 0.0 0.0
null null 121.52 5.37 5988080.0493 3553160.3978 0.35 0.0 0.0 0.0
low null 139.48 6.06 5988080.8083 3553159.0458 0.25 0.0 0.0 0.0
null null 143.38 3.82 5988080.9731 3553158.7522 0.12 0.0 0.0 0.0
null null 148.15 2.73 5988081.1746 3553158.3931 0.12 0.0 0.0 0.0
null null 151.32 2.72 5988081.3086 3553158.1544 0.12 0.0 0.0 0.0
null null 152.94 3.62 5988081.3771 3553158.0325 0.12 0.0 0.0 0.0
null null 153.7 4.25 5988081.4092 3553157.9753 0.25 0.0 0.0 0.0
low null 156.8 5.61 5988081.5402 3553157.7419 0.35 4.0 4.0 0.1
null null 163.13 5.74 5988081.8077 3553157.2653 0.35 0.0 0.0 0.0
null null 175.91 5.34 5988082.3477 3553156.3033 0.35 0.0 0.0 0.0
null null 201.49 5.13 5988083.4287 3553154.3776 0.35 0.0 0.0 0.0
null null 227.04 5.14 5988084.5084 3553152.4541 0.35 0.0 0.0 0.0
null null 310.2 6.22 5988088.0227 3553146.1937 0.35 0.0 0.0 0.0
null true 357.18 8.46 5988090.008 3553142.657 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>59.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553169.546 5988074.914 5.8,3553160.3978 5988080.0493 5.37,3553159.0458 5988080.8083 6.06,3553158.7522 5988080.9731 3.82,3553158.3931 5988081.1746 2.73,3553158.1544 5988081.3086 2.72,3553158.0325 5988081.3771 3.62,3553157.9753 5988081.4092 4.25,3553157.7419 5988081.5402 5.61,3553157.2653 5988081.8077 5.74,3553156.3033 5988082.3477 5.34,3553154.3776 5988083.4287 5.13,3553152.4541 5988084.5084 5.14,3553146.1937 5988088.0227 6.22,3553142.657 5988090.008 8.46</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951360136">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951360137">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.66 5988160.425 3553219.044 0.35 0.0 0.0 0.0
null null 38.37 5.75 5988163.4491 3553216.0411 0.35 0.0 0.0 0.0
null null 51.16 5.46 5988164.4571 3553215.0402 0.35 0.0 0.0 0.0
null null 76.74 5.56 5988166.4731 3553213.0382 0.35 0.0 0.0 0.0
null null 102.3 5.88 5988168.4876 3553211.0379 0.35 0.0 0.0 0.0
low null 109.45 5.77 5988169.0511 3553210.4783 0.25 0.0 0.0 0.0
null null 112.22 4.58 5988169.2694 3553210.2615 0.25 0.0 0.0 0.0
null null 112.62 3.9 5988169.301 3553210.2302 0.12 0.0 0.0 0.0
null null 114.2 3.15 5988169.4255 3553210.1066 0.12 0.0 0.0 0.0
null null 117.39 2.96 5988169.6769 3553209.8569 0.12 0.0 0.0 0.0
null null 120.58 3.08 5988169.9283 3553209.6073 0.12 0.0 0.0 0.0
null null 123.75 4.48 5988170.1782 3553209.3592 0.25 0.0 0.0 0.0
low null 128.47 5.84 5988170.5502 3553208.9898 0.35 6.0 6.0 0.1
null null 132.0 5.8152 5988170.8284 3553208.7135 0.35 0.0 0.0 0.0
null null 179.65 5.48 5988174.5838 3553204.9844 0.35 0.0 0.0 0.0
null null 230.83 5.39 5988178.6175 3553200.9789 0.35 0.0 0.0 0.0
null null 269.19 5.64 5988181.6408 3553197.9768 0.35 0.0 0.0 0.0
null true 320.58 7.23 5988185.691 3553193.955 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>59.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553219.044 5988160.425 5.66,3553216.0411 5988163.4491 5.75,3553215.0402 5988164.4571 5.46,3553213.0382 5988166.4731 5.56,3553211.0379 5988168.4876 5.88,3553210.4783 5988169.0511 5.77,3553210.2615 5988169.2694 4.58,3553210.2302 5988169.301 3.9,3553210.1066 5988169.4255 3.15,3553209.8569 5988169.6769 2.96,3553209.6073 5988169.9283 3.08,3553209.3592 5988170.1782 4.48,3553208.9898 5988170.5502 5.84,3553208.7135 5988170.8284 5.8152,3553204.9844 5988174.5838 5.48,3553200.9789 5988178.6175 5.39,3553197.9768 5988181.6408 5.64,3553193.955 5988185.691 7.23</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951371054">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951372620">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.59 5988237.496 3553287.148 0.35 0.0 0.0 0.0
null null 38.39 5.96 5988238.9907 3553284.388 0.35 0.0 0.0 0.0
null null 63.98 5.73 5988239.987 3553282.5482 0.35 0.0 0.0 0.0
null null 99.18 5.75 5988241.3574 3553280.0175 0.35 0.0 0.0 0.0
low null 104.8 6.24 5988241.5762 3553279.6135 0.25 0.0 0.0 0.0
null null 107.96 4.69 5988241.6993 3553279.3863 0.12 0.0 0.0 0.0
null null 110.34 2.88 5988241.7919 3553279.2152 0.12 0.0 0.0 0.0
null null 112.72 2.47 5988241.8846 3553279.0441 0.12 0.0 0.0 0.0
null null 115.13 3.06 5988241.9784 3553278.8708 0.12 0.0 0.0 0.0
null null 117.5 4.77 5988242.0707 3553278.7004 0.25 0.0 0.0 0.0
low null 122.2 5.89 5988242.2537 3553278.3625 0.35 6.0 6.0 0.1
null null 125.0 5.8782 5988242.3627 3553278.1612 0.35 0.0 0.0 0.0
null null 224.52 5.46 5988246.2373 3553271.0063 0.35 0.0 0.0 0.0
null null 301.29 5.64 5988249.2263 3553265.487 0.35 0.0 0.0 0.0
null true 357.79 7.13 5988251.426 3553261.425 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553287.148 5988237.496 5.59,3553284.388 5988238.9907 5.96,3553282.5482 5988239.987 5.73,3553280.0175 5988241.3574 5.75,3553279.6135 5988241.5762 6.24,3553279.3863 5988241.6993 4.69,3553279.2152 5988241.7919 2.88,3553279.0441 5988241.8846 2.47,3553278.8708 5988241.9784 3.06,3553278.7004 5988242.0707 4.77,3553278.3625 5988242.2537 5.89,3553278.1612 5988242.3627 5.8782,3553271.0063 5988246.2373 5.46,3553265.487 5988249.2263 5.64,3553261.425 5988251.426 7.13</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951383537">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951383555">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.72 5988303.614 3553290.614 0.35 0.0 0.0 0.0
null null 6.4 5.65 5988303.5018 3553290.2628 0.35 0.0 0.0 0.0
null null 12.8 5.25 5988303.3895 3553289.9116 0.35 0.0 0.0 0.0
null null 115.21 5.58 5988301.5935 3553284.2918 0.35 0.0 0.0 0.0
null null 191.99 5.33 5988300.247 3553280.0785 0.35 0.0 0.0 0.0
null null 230.35 5.86 5988299.5743 3553277.9734 0.35 0.0 0.0 0.0
low null 236.72 5.66 5988299.4625 3553277.6239 0.25 0.0 0.0 0.0
null null 238.27 5.06 5988299.4354 3553277.5388 0.12 0.0 0.0 0.0
null null 241.33 3.29 5988299.3817 3553277.3709 0.12 0.0 0.0 0.0
null null 247.72 3.1 5988299.2696 3553277.0202 0.12 0.0 0.0 0.0
null null 249.3 3.54 5988299.2419 3553276.9335 0.12 0.0 0.0 0.0
null null 250.89 4.59 5988299.214 3553276.8463 0.25 0.0 0.0 0.0
low null 255.66 6.18 5988299.1304 3553276.5845 0.35 4.0 4.0 0.1
null null 260.0 6.1053 5988299.0543 3553276.3464 0.35 0.0 0.0 0.0
null null 281.22 5.74 5988298.6821 3553275.1819 0.35 0.0 0.0 0.0
null null 383.6 5.44 5988296.8867 3553269.5638 0.35 0.0 0.0 0.0
null null 434.77 5.78 5988295.9893 3553266.7558 0.35 0.0 0.0 0.0
null null 447.57 6.3 5988295.7648 3553266.0534 0.35 0.0 0.0 0.0
null null 473.17 6.65 5988295.3158 3553264.6486 0.35 0.0 0.0 0.0
null true 488.67 7.06 5988295.044 3553263.798 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.0560</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553290.614 5988303.614 5.72,3553290.2628 5988303.5018 5.65,3553289.9116 5988303.3895 5.25,3553284.2918 5988301.5935 5.58,3553280.0785 5988300.247 5.33,3553277.9734 5988299.5743 5.86,3553277.6239 5988299.4625 5.66,3553277.5388 5988299.4354 5.06,3553277.3709 5988299.3817 3.29,3553277.0202 5988299.2696 3.1,3553276.9335 5988299.2419 3.54,3553276.8463 5988299.214 4.59,3553276.5845 5988299.1304 6.18,3553276.3464 5988299.0543 6.1053,3553275.1819 5988298.6821 5.74,3553269.5638 5988296.8867 5.44,3553266.7558 5988295.9893 5.78,3553266.0534 5988295.7648 6.3,3553264.6486 5988295.3158 6.65,3553263.798 5988295.044 7.06</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951394524">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951394561">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.66 5988343.825 3553267.182 0.4 0.0 0.0 0.0
null null 163.13 5.5 5988338.9387 3553261.2087 0.15 0.0 0.0 0.0
null null 167.13 4.4 5988338.8189 3553261.0622 0.15 0.0 0.0 0.0
null null 169.53 5.48 5988338.747 3553260.9743 0.4 0.0 0.0 0.0
null null 220.74 5.84 5988337.2131 3553259.0992 0.4 0.0 0.0 0.0
null null 297.49 5.34 5988334.9142 3553256.2888 0.35 0.0 0.0 0.0
null null 335.87 5.76 5988333.7646 3553254.8834 0.35 0.0 0.0 0.0
null null 337.48 5.59 5988333.7164 3553254.8245 0.35 0.0 0.0 0.0
low null 340.67 6.04 5988333.6208 3553254.7077 0.25 0.0 0.0 0.0
null null 343.82 4.41 5988333.5265 3553254.5923 0.12 0.0 0.0 0.0
null null 344.64 3.78 5988333.5019 3553254.5623 0.12 0.0 0.0 0.0
null null 347.84 3.35 5988333.406 3553254.4451 0.12 0.0 0.0 0.0
null null 352.63 3.14 5988333.2626 3553254.2697 0.12 0.0 0.0 0.0
null null 354.18 3.79 5988333.2161 3553254.213 0.12 0.0 0.0 0.0
null null 355.33 4.68 5988333.1817 3553254.1709 0.2 0.0 0.0 0.0
null null 356.15 4.79 5988333.1571 3553254.1409 0.2 0.0 0.0 0.0
low null 357.77 5.58 5988333.1086 3553254.0815 0.2 2.0 2.0 0.1
null null 359.31 6.08 5988333.0625 3553254.0251 0.35 0.0 0.0 0.0
null null 365.72 6.11 5988332.8705 3553253.7904 0.35 0.0 0.0 0.0
null null 378.51 5.81 5988332.4874 3553253.3221 0.35 0.0 0.0 0.0
null null 480.88 5.53 5988329.4211 3553249.5736 0.35 0.0 0.0 0.0
null null 532.03 6.16 5988327.889 3553247.7007 0.35 0.0 0.0 0.0
null true 567.25 6.88 5988326.834 3553246.411 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553267.182 5988343.825 5.66,3553261.2087 5988338.9387 5.5,3553261.0622 5988338.8189 4.4,3553260.9743 5988338.747 5.48,3553259.0992 5988337.2131 5.84,3553256.2888 5988334.9142 5.34,3553254.8834 5988333.7646 5.76,3553254.8245 5988333.7164 5.59,3553254.7077 5988333.6208 6.04,3553254.5923 5988333.5265 4.41,3553254.5623 5988333.5019 3.78,3553254.4451 5988333.406 3.35,3553254.2697 5988333.2626 3.14,3553254.213 5988333.2161 3.79,3553254.1709 5988333.1817 4.68,3553254.1409 5988333.1571 4.79,3553254.0815 5988333.1086 5.58,3553254.0251 5988333.0625 6.08,3553253.7904 5988332.8705 6.11,3553253.3221 5988332.4874 5.81,3553249.5736 5988329.4211 5.53,3553247.7007 5988327.889 6.16,3553246.411 5988326.834 6.88</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951396035">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951396063">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.58 5988383.581 3553233.639 0.4 0.0 0.0 0.0
null null 131.95 5.44 5988379.6057 3553228.8787 0.15 0.0 0.0 0.0
null null 133.94 4.37 5988379.5458 3553228.8069 0.15 0.0 0.0 0.0
null null 135.52 5.07 5988379.4982 3553228.7499 0.4 0.0 0.0 0.0
null null 137.1 5.5 5988379.4506 3553228.6929 0.4 0.0 0.0 0.0
null null 239.41 5.94 5988376.3683 3553225.0019 0.35 0.0 0.0 0.0
null null 290.62 5.87 5988374.8255 3553223.1544 0.35 0.0 0.0 0.0
null null 341.8 5.53 5988373.2835 3553221.308 0.35 0.0 0.0 0.0
null null 380.12 5.59 5988372.1291 3553219.9255 0.35 0.0 0.0 0.0
null null 392.92 5.89 5988371.7434 3553219.4638 0.35 0.0 0.0 0.0
low null 399.25 5.74 5988371.5527 3553219.2354 0.25 0.0 0.0 0.0
null null 400.85 5.09 5988371.5045 3553219.1777 0.25 0.0 0.0 0.0
null null 401.62 4.6 5988371.4813 3553219.1499 0.12 0.0 0.0 0.0
null null 402.0 3.9 5988371.4699 3553219.1362 0.12 0.0 0.0 0.0
null null 403.5 3.09 5988371.4247 3553219.0821 0.12 0.0 0.0 0.0
null null 406.68 3.09 5988371.3289 3553218.9673 0.12 0.0 0.0 0.0
null null 411.46 3.46 5988371.1849 3553218.7949 0.12 0.0 0.0 0.0
null null 412.25 3.69 5988371.1611 3553218.7664 0.12 0.0 0.0 0.0
null null 413.04 4.34 5988371.1373 3553218.7379 0.25 0.0 0.0 0.0
low null 416.96 6.13 5988371.0192 3553218.5965 0.35 3.0 3.0 0.1
null null 420.0 6.1003 5988370.9276 3553218.4868 0.35 0.0 0.0 0.0
null null 468.06 5.63 5988369.4797 3553216.753 0.35 0.0 0.0 0.0
null null 519.26 5.64 5988367.9372 3553214.9058 0.35 0.0 0.0 0.0
null true 579.71 6.57 5988366.116 3553212.725 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.1520</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553233.639 5988383.581 5.58,3553228.8787 5988379.6057 5.44,3553228.8069 5988379.5458 4.37,3553228.7499 5988379.4982 5.07,3553228.6929 5988379.4506 5.5,3553225.0019 5988376.3683 5.94,3553223.1544 5988374.8255 5.87,3553221.308 5988373.2835 5.53,3553219.9255 5988372.1291 5.59,3553219.4638 5988371.7434 5.89,3553219.2354 5988371.5527 5.74,3553219.1777 5988371.5045 5.09,3553219.1499 5988371.4813 4.6,3553219.1362 5988371.4699 3.9,3553219.0821 5988371.4247 3.09,3553218.9673 5988371.3289 3.09,3553218.7949 5988371.1849 3.46,3553218.7664 5988371.1611 3.69,3553218.7379 5988371.1373 4.34,3553218.5965 5988371.0192 6.13,3553218.4868 5988370.9276 6.1003,3553216.753 5988369.4797 5.63,3553214.9058 5988367.9372 5.64,3553212.725 5988366.116 6.57</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951407042">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595140702">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.51 5988421.49 3553202.887 0.4 0.0 0.0 0.0
null null 51.16 5.64 5988419.9957 3553201.1126 0.4 0.0 0.0 0.0
null null 115.15 5.47 5988418.1266 3553198.8933 0.4 0.0 0.0 0.0
null null 121.08 5.2 5988417.9533 3553198.6876 0.15 0.0 0.0 0.0
null null 121.67 4.31 5988417.9361 3553198.6672 0.15 0.0 0.0 0.0
null null 122.46 5.28 5988417.913 3553198.6398 0.4 0.0 0.0 0.0
null null 167.24 5.87 5988416.605 3553197.0867 0.35 5.0 5.0 0.2
null null 173.64 6.58 5988416.4181 3553196.8647 0.25 0.0 0.0 0.0
null null 224.84 7.35 5988414.9226 3553195.089 0.25 0.0 0.0 0.0
null null 373.6 6.83 5988410.5774 3553189.9296 0.35 5.0 5.0 0.2
null null 378.4 5.89 5988410.4372 3553189.7631 0.35 0.0 0.0 0.0
null null 416.81 5.34 5988409.3153 3553188.431 0.35 0.0 0.0 0.0
null null 442.42 5.51 5988408.5672 3553187.5428 0.35 0.0 0.0 0.0
low null 456.83 6.16 5988408.1463 3553187.043 0.2 0.0 0.0 0.0
null null 460.35 4.64 5988408.0435 3553186.9209 0.2 0.0 0.0 0.0
null null 460.75 4.16 5988408.0318 3553186.907 0.12 0.0 0.0 0.0
null null 462.36 3.35 5988407.9848 3553186.8512 0.12 0.0 0.0 0.0
null null 465.52 3.13 5988407.8925 3553186.7416 0.12 0.0 0.0 0.0
null null 470.29 3.25 5988407.7532 3553186.5762 0.12 0.0 0.0 0.0
null null 472.62 4.7 5988407.6851 3553186.4953 0.2 0.0 0.0 0.0
null null 473.41 4.76 5988407.6621 3553186.4679 0.2 0.0 0.0 0.0
low null 476.63 6.01 5988407.568 3553186.3563 0.35 3.0 3.0 0.1
null null 480.0 5.9949 5988407.4696 3553186.2394 0.35 0.0 0.0 0.0
null null 527.8 5.78 5988406.0734 3553184.5816 0.35 0.0 0.0 0.0
null true 596.9 6.21 5988404.055 3553182.185 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.2000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553202.887 5988421.49 5.51,3553201.1126 5988419.9957 5.64,3553198.8933 5988418.1266 5.47,3553198.6876 5988417.9533 5.2,3553198.6672 5988417.9361 4.31,3553198.6398 5988417.913 5.28,3553197.0867 5988416.605 5.87,3553196.8647 5988416.4181 6.58,3553195.089 5988414.9226 7.35,3553189.9296 5988410.5774 6.83,3553189.7631 5988410.4372 5.89,3553188.431 5988409.3153 5.34,3553187.5428 5988408.5672 5.51,3553187.043 5988408.1463 6.16,3553186.9209 5988408.0435 4.64,3553186.907 5988408.0318 4.16,3553186.8512 5988407.9848 3.35,3553186.7416 5988407.8925 3.13,3553186.5762 5988407.7532 3.25,3553186.4953 5988407.6851 4.7,3553186.4679 5988407.6621 4.76,3553186.3563 5988407.568 6.01,3553186.2394 5988407.4696 5.9949,3553184.5816 5988406.0734 5.78,3553182.185 5988404.055 6.21</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951417915">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951417914">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.33 5988494.527 3553133.332 0.4 0.0 0.0 0.0
null null 17.59 5.25 5988493.8754 3553132.7624 0.4 0.0 0.0 0.0
null null 19.18 5.66 5988493.8165 3553132.7109 0.4 0.0 0.0 0.0
null null 116.71 5.58 5988490.2034 3553129.5524 0.15 0.0 0.0 0.0
null null 119.11 4.4 5988490.1145 3553129.4746 0.15 0.0 0.0 0.0
null null 120.71 5.42 5988490.0553 3553129.4228 0.4 0.0 0.0 0.0
null null 159.08 6.11 5988488.6338 3553128.1802 0.35 5.0 5.0 0.25
null null 165.47 6.96 5988488.3971 3553127.9733 0.4 5.0 5.0 0.25
null null 216.64 7.22 5988486.5015 3553126.3162 0.4 5.0 5.0 0.25
null null 433.29 6.6 5988478.4756 3553119.3 0.35 5.0 5.0 0.25
null null 434.89 6.04 5988478.4164 3553119.2482 0.35 0.0 0.0 0.0
null null 438.09 5.61 5988478.2978 3553119.1445 0.35 0.0 0.0 0.0
null null 463.69 5.35 5988477.3495 3553118.3155 0.35 0.0 0.0 0.0
null null 514.9 5.49 5988475.4524 3553116.6571 0.35 0.0 0.0 0.0
null null 540.48 6.09 5988474.5048 3553115.8287 0.35 0.0 0.0 0.0
low null 542.08 6.33 5988474.4455 3553115.7769 0.35 0.0 0.0 0.0
null null 543.66 5.94 5988474.387 3553115.7257 0.2 0.0 0.0 0.0
null null 545.86 4.98 5988474.3055 3553115.6544 0.2 0.0 0.0 0.0
null null 546.26 4.31 5988474.2906 3553115.6415 0.12 0.0 0.0 0.0
null null 551.84 3.12 5988474.0839 3553115.4608 0.12 0.0 0.0 0.0
null null 555.01 3.65 5988473.9665 3553115.3581 0.12 0.0 0.0 0.0
null null 558.22 4.43 5988473.8476 3553115.2542 0.2 0.0 0.0 0.0
null null 559.83 5.28 5988473.7879 3553115.202 0.2 0.0 0.0 0.0
low null 561.44 5.9 5988473.7283 3553115.1499 0.35 7.0 7.0 0.1
null null 563.05 6.19 5988473.6686 3553115.0977 0.35 0.0 0.0 0.0
null true 632.55 6.31 5988471.094 3553112.847 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553133.332 5988494.527 5.33,3553132.7624 5988493.8754 5.25,3553132.7109 5988493.8165 5.66,3553129.5524 5988490.2034 5.58,3553129.4746 5988490.1145 4.4,3553129.4228 5988490.0553 5.42,3553128.1802 5988488.6338 6.11,3553127.9733 5988488.3971 6.96,3553126.3162 5988486.5015 7.22,3553119.3 5988478.4756 6.6,3553119.2482 5988478.4164 6.04,3553119.1445 5988478.2978 5.61,3553118.3155 5988477.3495 5.35,3553116.6571 5988475.4524 5.49,3553115.8287 5988474.5048 6.09,3553115.7769 5988474.4455 6.33,3553115.7257 5988474.387 5.94,3553115.6544 5988474.3055 4.98,3553115.6415 5988474.2906 4.31,3553115.4608 5988474.0839 3.12,3553115.3581 5988473.9665 3.65,3553115.2542 5988473.8476 4.43,3553115.202 5988473.7879 5.28,3553115.1499 5988473.7283 5.9,3553115.0977 5988473.6686 6.19,3553112.847 5988471.094 6.31</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951430455">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595143048">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.34 5988566.366 3553081.025 0.4 0.0 0.0 0.0
null null 105.57 5.58 5988565.2095 3553076.2958 0.4 0.0 0.0 0.0
null null 107.17 5.4 5988565.192 3553076.2242 0.15 0.0 0.0 0.0
null null 109.17 4.38 5988565.1701 3553076.1346 0.15 0.0 0.0 0.0
null null 111.57 5.6 5988565.1438 3553076.0271 0.4 0.0 0.0 0.0
null null 149.96 6.33 5988564.7232 3553074.3073 0.35 5.0 5.0 0.25
null null 153.16 7.05 5988564.6882 3553074.164 0.35 5.0 5.0 0.25
null null 204.35 7.13 5988564.1274 3553071.8708 0.35 5.0 5.0 0.25
null null 306.76 6.74 5988563.0056 3553067.2832 0.35 5.0 5.0 0.25
null null 460.31 6.6 5988561.3235 3553060.4047 0.35 5.0 5.0 0.25
null null 466.71 5.76 5988561.2534 3553060.118 0.35 0.0 0.0 0.0
null null 517.93 5.26 5988560.6923 3553057.8236 0.35 0.0 0.0 0.0
null null 543.53 5.34 5988560.4118 3553056.6768 0.35 0.0 0.0 0.0
null null 581.93 6.29 5988559.9912 3553054.9566 0.35 0.0 0.0 0.0
null null 587.0 6.26 5988559.9356 3553054.7295 0.35 0.0 0.0 0.0
low null 587.79 6.55 5988559.927 3553054.6941 0.35 0.0 0.0 0.0
null null 589.35 5.67 5988559.9099 3553054.6242 0.2 0.0 0.0 0.0
null null 590.93 4.27 5988559.8926 3553054.5534 0.12 0.0 0.0 0.0
null null 595.74 3.22 5988559.8399 3553054.338 0.12 0.0 0.0 0.0
null null 600.48 4.03 5988559.788 3553054.1256 0.12 0.0 0.0 0.0
null null 602.02 4.7 5988559.7711 3553054.0566 0.2 0.0 0.0 0.0
null null 602.4 5.23 5988559.7669 3553054.0396 0.2 0.0 0.0 0.0
low null 603.98 5.76 5988559.7496 3553053.9688 0.35 8.0 8.0 0.1
null null 610.37 6.32 5988559.6796 3553053.6826 0.35 0.0 0.0 0.0
null true 648.31 7.02 5988559.264 3553051.983 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553081.025 5988566.366 5.34,3553076.2958 5988565.2095 5.58,3553076.2242 5988565.192 5.4,3553076.1346 5988565.1701 4.38,3553076.0271 5988565.1438 5.6,3553074.3073 5988564.7232 6.33,3553074.164 5988564.6882 7.05,3553071.8708 5988564.1274 7.13,3553067.2832 5988563.0056 6.74,3553060.4047 5988561.3235 6.6,3553060.118 5988561.2534 5.76,3553057.8236 5988560.6923 5.26,3553056.6768 5988560.4118 5.34,3553054.9566 5988559.9912 6.29,3553054.7295 5988559.9356 6.26,3553054.6941 5988559.927 6.55,3553054.6242 5988559.9099 5.67,3553054.5534 5988559.8926 4.27,3553054.338 5988559.8399 3.22,3553054.1256 5988559.788 4.03,3553054.0566 5988559.7711 4.7,3553054.0396 5988559.7669 5.23,3553053.9688 5988559.7496 5.76,3553053.6826 5988559.6796 6.32,3553051.983 5988559.264 7.02</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951441336">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951441327">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.48 5988609.985 3553077.539 0.4 0.0 0.0 0.0
null null 95.16 5.51 5988610.7862 3553073.8902 0.15 0.0 0.0 0.0
null null 97.51 4.34 5988610.806 3553073.8001 0.15 0.0 0.0 0.0
null null 100.64 5.6 5988610.8324 3553073.6801 0.4 0.0 0.0 0.0
null null 126.22 6.06 5988611.0477 3553072.6993 0.4 0.0 0.0 0.0
null null 145.41 6.18 5988611.2093 3553071.9635 0.4 0.0 0.0 0.0
null null 148.61 6.45 5988611.2363 3553071.8408 0.35 5.0 5.0 0.25
null null 151.8 7.03 5988611.2631 3553071.7185 0.35 5.0 5.0 0.25
null null 356.52 6.64 5988612.9868 3553063.8688 0.35 5.0 5.0 0.25
null null 510.06 6.61 5988614.2796 3553057.9815 0.35 5.0 5.0 0.25
null null 522.86 5.48 5988614.3874 3553057.4907 0.35 0.0 5.0 0.0
null null 580.37 5.99 5988614.8716 3553055.2856 0.35 5.0 5.0 0.1
low null 584.34 5.65 5988614.905 3553055.1334 0.2 0.0 0.0 0.0
null null 585.11 5.23 5988614.9115 3553055.1039 0.15 0.0 0.0 0.0
null null 586.66 3.96 5988614.9245 3553055.0444 0.15 0.0 0.0 0.0
null null 589.85 3.62 5988614.9514 3553054.9221 0.15 0.0 0.0 0.0
null null 593.04 3.54 5988614.9783 3553054.7998 0.15 0.0 0.0 0.0
null null 596.2 3.73 5988615.0049 3553054.6786 0.15 0.0 0.0 0.0
null null 596.97 4.04 5988615.0113 3553054.6491 0.2 0.0 0.0 0.0
null null 598.53 5.11 5988615.0245 3553054.5893 0.2 0.0 0.0 0.0
low null 600.11 5.75 5988615.0378 3553054.5287 0.35 5.0 5.0 0.1
null null 602.0 5.7679 5988615.0537 3553054.4562 0.35 0.0 0.0 0.0
null null 606.46 5.81 5988615.0912 3553054.2852 0.35 0.0 0.0 0.0
null null 632.02 7.02 5988615.3065 3553053.3052 0.35 0.0 0.0 0.0
null true 656.67 7.67 5988615.514 3553052.36 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.4510</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553077.539 5988609.985 5.48,3553073.8902 5988610.7862 5.51,3553073.8001 5988610.806 4.34,3553073.6801 5988610.8324 5.6,3553072.6993 5988611.0477 6.06,3553071.9635 5988611.2093 6.18,3553071.8408 5988611.2363 6.45,3553071.7185 5988611.2631 7.03,3553063.8688 5988612.9868 6.64,3553057.9815 5988614.2796 6.61,3553057.4907 5988614.3874 5.48,3553055.2856 5988614.8716 5.99,3553055.1334 5988614.905 5.65,3553055.1039 5988614.9115 5.23,3553055.0444 5988614.9245 3.96,3553054.9221 5988614.9514 3.62,3553054.7998 5988614.9783 3.54,3553054.6786 5988615.0049 3.73,3553054.6491 5988615.0113 4.04,3553054.5893 5988615.0245 5.11,3553054.5287 5988615.0378 5.75,3553054.4562 5988615.0537 5.7679,3553054.2852 5988615.0912 5.81,3553053.3052 5988615.3065 7.02,3553052.36 5988615.514 7.67</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951442949">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951442958">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.57 5988675.931 3553099.516 0.4 0.0 0.0 0.0
null null 25.61 5.6 5988676.1737 3553098.9649 0.4 0.0 0.0 0.0
null null 51.21 5.37 5988676.4163 3553098.414 0.4 0.0 0.0 0.0
null null 64.02 5.56 5988676.5377 3553098.1383 0.4 0.0 0.0 0.0
null null 95.2 5.49 5988676.8331 3553097.4673 0.15 0.0 0.0 0.0
null null 97.6 4.42 5988676.8559 3553097.4157 0.15 0.0 0.0 0.0
null null 100.0 5.52 5988676.8786 3553097.364 0.4 0.0 0.0 0.0
null null 157.56 6.19 5988677.4241 3553096.1254 0.35 5.0 5.0 0.25
null null 163.96 6.93 5988677.4847 3553095.9877 0.35 5.0 5.0 0.25
null null 368.7 6.65 5988679.4249 3553091.5818 0.35 5.0 5.0 0.25
null null 522.29 6.71 5988680.8804 3553088.2766 0.35 0.0 0.0 0.0
null null 535.1 6.85 5988681.0018 3553088.0009 0.35 0.0 0.0 0.0
null null 538.3 7.07 5988681.0321 3553087.932 0.35 0.0 0.0 0.0
null null 545.49 7.0 5988681.1003 3553087.7773 0.35 0.0 0.0 0.0
low null 563.82 7.71 5988681.274 3553087.3829 0.2 0.0 0.0 0.0
null null 571.3 4.89 5988681.3448 3553087.2219 0.12 0.0 0.0 0.0
null null 571.47 4.31 5988681.3464 3553087.2182 0.12 0.0 0.0 0.0
null null 572.19 3.86 5988681.3533 3553087.2028 0.12 0.0 0.0 0.0
null null 576.94 3.35 5988681.3983 3553087.1005 0.12 0.0 0.0 0.0
null null 581.7 4.13 5988681.4434 3553086.9981 0.12 0.0 0.0 0.0
null null 583.24 4.87 5988681.458 3553086.965 0.12 0.0 0.0 0.0
null null 586.37 5.98 5988681.4876 3553086.8976 0.2 0.0 0.0 0.0
low null 590.39 7.67 5988681.5257 3553086.8111 0.35 0.0 0.0 0.0
null null 602.42 7.56 5988681.6397 3553086.5522 0.35 0.0 0.0 0.0
null null 621.63 7.46 5988681.8218 3553086.1388 0.35 0.0 0.0 0.0
null null 640.79 7.38 5988682.0033 3553085.7265 0.35 0.0 0.0 0.0
null true 680.22 8.01 5988682.377 3553084.878 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.5210</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553099.516 5988675.931 5.57,3553098.9649 5988676.1737 5.6,3553098.414 5988676.4163 5.37,3553098.1383 5988676.5377 5.56,3553097.4673 5988676.8331 5.49,3553097.4157 5988676.8559 4.42,3553097.364 5988676.8786 5.52,3553096.1254 5988677.4241 6.19,3553095.9877 5988677.4847 6.93,3553091.5818 5988679.4249 6.65,3553088.2766 5988680.8804 6.71,3553088.0009 5988681.0018 6.85,3553087.932 5988681.0321 7.07,3553087.7773 5988681.1003 7.0,3553087.3829 5988681.274 7.71,3553087.2219 5988681.3448 4.89,3553087.2182 5988681.3464 4.31,3553087.2028 5988681.3533 3.86,3553087.1005 5988681.3983 3.35,3553086.9981 5988681.4434 4.13,3553086.965 5988681.458 4.87,3553086.8976 5988681.4876 5.98,3553086.8111 5988681.5257 7.67,3553086.5522 5988681.6397 7.56,3553086.1388 5988681.8218 7.46,3553085.7265 5988682.0033 7.38,3553084.878 5988682.377 8.01</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951455476">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951455431">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.6 5988681.027 3553101.681 0.4 0.0 0.0 0.0
null null 25.6 5.61 5988681.2723 3553101.1375 0.4 0.0 0.0 0.0
null null 51.19 5.41 5988681.5175 3553100.5942 0.4 0.0 0.0 0.0
null null 57.59 5.54 5988681.5788 3553100.4583 0.4 0.0 0.0 0.0
null null 91.98 5.53 5988681.9083 3553099.7281 0.15 0.0 0.0 0.0
null null 94.38 4.38 5988681.9313 3553099.6772 0.15 0.0 0.0 0.0
null null 96.78 5.44 5988681.9543 3553099.6262 0.15 0.0 0.0 0.0
null null 97.58 5.55 5988681.962 3553099.6092 0.4 0.0 0.0 0.0
null null 155.17 6.18 5988682.5138 3553098.3865 0.35 0.0 0.0 0.0
null null 158.37 6.44 5988682.5444 3553098.3186 0.35 0.0 0.0 0.0
null null 161.57 6.83 5988682.5751 3553098.2506 0.35 0.0 0.0 0.0
null null 163.17 6.93 5988682.5904 3553098.2167 0.35 5.0 5.0 0.25
null null 214.37 6.77 5988683.081 3553097.1296 0.35 5.0 5.0 0.25
null null 419.11 6.67 5988685.0427 3553092.7827 0.35 5.0 5.0 0.25
null null 527.9 6.75 5988686.085 3553090.4729 0.35 0.0 0.0 0.0
null null 530.3 6.53 5988686.108 3553090.422 0.35 0.0 0.0 0.0
null null 531.1 6.28 5988686.1157 3553090.405 0.35 0.0 0.0 0.0
null null 537.5 5.87 5988686.177 3553090.2691 0.35 0.0 0.0 0.0
null null 559.85 6.18 5988686.3912 3553089.7946 0.35 0.0 0.0 0.0
null null 563.06 6.05 5988686.4219 3553089.7264 0.35 5.0 5.0 0.15
low null 566.26 5.8 5988686.4526 3553089.6585 0.2 0.0 0.0 0.0
null null 567.81 5.24 5988686.4674 3553089.6256 0.2 0.0 0.0 0.0
null null 568.6 4.79 5988686.475 3553089.6088 0.12 0.0 0.0 0.0
null null 570.17 3.49 5988686.4901 3553089.5755 0.12 0.0 0.0 0.0
null null 576.58 3.43 5988686.5515 3553089.4394 0.12 0.0 0.0 0.0
null null 578.58 3.48 5988686.5706 3553089.3969 0.12 0.0 0.0 0.0
null null 580.13 4.23 5988686.5855 3553089.364 0.2 0.0 0.0 0.0
null null 580.87 4.91 5988686.5926 3553089.3483 0.2 0.0 0.0 0.0
low null 582.42 5.45 5988686.6074 3553089.3154 0.35 0.0 0.0 0.0
null null 589.55 5.97 5988686.6757 3553089.164 0.35 0.0 0.0 0.0
null null 589.95 6.08 5988686.6796 3553089.1555 0.35 0.0 0.0 0.0
null null 596.29 5.77 5988686.7403 3553089.0209 0.35 0.0 0.0 0.0
null null 615.45 5.48 5988686.9239 3553088.6141 0.35 0.0 0.0 0.0
null null 631.4 5.73 5988687.0767 3553088.2755 0.35 0.0 0.0 0.0
null null 644.13 7.56 5988687.1987 3553088.0052 0.35 0.0 0.0 0.0
null null 669.7 7.79 5988687.4437 3553087.4623 0.35 0.0 0.0 0.0
null true 680.69 8.01 5988687.549 3553087.229 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.5280</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553101.681 5988681.027 5.6,3553101.1375 5988681.2723 5.61,3553100.5942 5988681.5175 5.41,3553100.4583 5988681.5788 5.54,3553099.7281 5988681.9083 5.53,3553099.6772 5988681.9313 4.38,3553099.6262 5988681.9543 5.44,3553099.6092 5988681.962 5.55,3553098.3865 5988682.5138 6.18,3553098.3186 5988682.5444 6.44,3553098.2506 5988682.5751 6.83,3553098.2167 5988682.5904 6.93,3553097.1296 5988683.081 6.77,3553092.7827 5988685.0427 6.67,3553090.4729 5988686.085 6.75,3553090.422 5988686.108 6.53,3553090.405 5988686.1157 6.28,3553090.2691 5988686.177 5.87,3553089.7946 5988686.3912 6.18,3553089.7264 5988686.4219 6.05,3553089.6585 5988686.4526 5.8,3553089.6256 5988686.4674 5.24,3553089.6088 5988686.475 4.79,3553089.5755 5988686.4901 3.49,3553089.4394 5988686.5515 3.43,3553089.3969 5988686.5706 3.48,3553089.364 5988686.5855 4.23,3553089.3483 5988686.5926 4.91,3553089.3154 5988686.6074 5.45,3553089.164 5988686.6757 5.97,3553089.1555 5988686.6796 6.08,3553089.0209 5988686.7403 5.77,3553088.6141 5988686.9239 5.48,3553088.2755 5988687.0767 5.73,3553088.0052 5988687.1987 7.56,3553087.4623 5988687.4437 7.79,3553087.229 5988687.549 8.01</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951466330">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951466342">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.67 5988745.906 3553134.558 0.4 0.0 0.0 0.0
null null 86.31 5.42 5988747.6642 3553131.323 0.15 0.0 0.0 0.0
null null 88.31 4.41 5988747.705 3553131.248 0.15 0.0 0.0 0.0
null null 90.7 5.44 5988747.7537 3553131.1584 0.4 0.0 0.0 0.0
null null 154.67 6.26 5988749.0568 3553128.7608 0.35 0.0 0.0 0.0
null null 161.07 6.87 5988749.1872 3553128.5209 0.35 5.0 5.0 0.25
null null 410.55 6.7 5988754.2694 3553119.17 0.35 0.0 0.0 0.0
null null 423.34 6.17 5988754.53 3553118.6907 0.35 0.0 0.0 0.0
null null 474.53 6.07 5988755.5728 3553116.772 0.35 0.0 0.0 0.0
null null 525.71 6.23 5988756.6154 3553114.8537 0.35 0.0 0.0 0.0
null null 544.89 6.14 5988757.0061 3553114.1348 0.35 0.0 0.0 0.0
null null 548.09 5.97 5988757.0713 3553114.0149 0.35 7.0 7.0 0.15
low null 550.1 6.19 5988757.1122 3553113.9395 0.2 0.0 0.0 0.0
null null 552.0 5.0262 5988757.1509 3553113.8683 0.2 0.0 0.0 0.0
null null 553.3 4.23 5988757.1774 3553113.8196 0.12 0.0 0.0 0.0
null null 554.9 3.86 5988757.21 3553113.7596 0.12 0.0 0.0 0.0
null null 562.86 3.12 5988757.3721 3553113.4613 0.2 0.0 0.0 0.0
low null 566.86 5.81 5988757.4536 3553113.3113 0.35 0.0 0.0 0.0
null null 592.46 5.53 5988757.9751 3553112.3518 0.35 0.0 0.0 0.0
null null 618.06 5.54 5988758.4966 3553111.3923 0.35 0.0 0.0 0.0
null null 650.07 5.83 5988759.1487 3553110.1925 0.35 0.0 0.0 0.0
null null 659.67 7.18 5988759.3443 3553109.8327 0.35 0.0 0.0 0.0
null true 696.08 8.18 5988760.086 3553108.468 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553134.558 5988745.906 5.67,3553131.323 5988747.6642 5.42,3553131.248 5988747.705 4.41,3553131.1584 5988747.7537 5.44,3553128.7608 5988749.0568 6.26,3553128.5209 5988749.1872 6.87,3553119.17 5988754.2694 6.7,3553118.6907 5988754.53 6.17,3553116.772 5988755.5728 6.07,3553114.8537 5988756.6154 6.23,3553114.1348 5988757.0061 6.14,3553114.0149 5988757.0713 5.97,3553113.9395 5988757.1122 6.19,3553113.8683 5988757.1509 5.0262,3553113.8196 5988757.1774 4.23,3553113.7596 5988757.21 3.86,3553113.4613 5988757.3721 3.12,3553113.3113 5988757.4536 5.81,3553112.3518 5988757.9751 5.53,3553111.3923 5988758.4966 5.54,3553110.1925 5988759.1487 5.83,3553109.8327 5988759.3443 7.18,3553108.468 5988760.086 8.18</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951477312">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951478851">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.72 5988765.92 3553148.28 0.4 0.0 0.0 0.0
null null 25.57 5.56 5988766.5928 3553147.3742 0.4 0.0 0.0 0.0
null null 83.49 5.46 5988768.1167 3553145.3223 0.15 0.0 0.0 0.0
null null 85.71 4.39 5988768.1751 3553145.2437 0.15 0.0 0.0 0.0
null null 88.13 5.5 5988768.2388 3553145.158 0.4 0.0 0.0 0.0
null null 152.1 6.15 5988769.922 3553142.8918 0.35 0.0 0.0 0.0
null null 160.1 6.83 5988770.1324 3553142.6084 0.35 5.0 5.0 0.25
null null 384.03 6.66 5988776.0243 3553134.6756 0.35 0.0 0.0 0.0
null null 393.63 6.23 5988776.2769 3553134.3355 0.35 0.0 0.0 0.0
null null 470.41 6.08 5988778.2971 3553131.6156 0.35 0.0 0.0 0.0
null null 508.8 6.2 5988779.3072 3553130.2556 0.35 0.0 0.0 0.0
null null 515.2 6.15 5988779.4756 3553130.0289 0.35 0.0 0.0 0.0
null null 526.37 5.79 5988779.7695 3553129.6332 0.35 0.0 0.0 0.0
null null 532.74 6.05 5988779.9371 3553129.4075 0.35 7.0 7.0 0.15
low null 539.13 6.2 5988780.1052 3553129.1812 0.35 0.0 0.0 0.0
null null 540.66 5.82 5988780.1455 3553129.127 0.2 0.0 0.0 0.0
null null 541.45 5.54 5988780.1663 3553129.099 0.2 0.0 0.0 0.0
null null 542.61 4.96 5988780.1968 3553129.0579 0.2 0.0 0.0 0.0
null null 542.81 4.77 5988780.202 3553129.0508 0.2 0.0 0.0 0.0
null null 543.0 4.42 5988780.207 3553129.0441 0.12 0.0 0.0 0.0
null null 543.79 3.96 5988780.2278 3553129.0161 0.12 0.0 0.0 0.0
null null 547.75 3.22 5988780.332 3553128.8758 0.12 0.0 0.0 0.0
null null 552.95 3.43 5988780.4688 3553128.6916 0.12 0.0 0.0 0.0
null null 553.33 3.63 5988780.4788 3553128.6781 0.12 0.0 0.0 0.0
null null 554.47 4.57 5988780.5088 3553128.6377 0.2 0.0 0.0 0.0
null null 555.03 5.27 5988780.5236 3553128.6179 0.2 0.0 0.0 0.0
null null 556.6 5.84 5988780.5649 3553128.5623 0.2 0.0 0.0 0.0
low null 558.15 6.08 5988780.6057 3553128.5074 0.35 0.0 0.0 0.0
null null 564.52 5.84 5988780.7733 3553128.2817 0.35 0.0 0.0 0.0
null null 590.1 5.63 5988781.4463 3553127.3755 0.35 0.0 0.0 0.0
null null 615.68 5.63 5988782.1193 3553126.4694 0.35 0.0 0.0 0.0
null null 641.26 5.86 5988782.7924 3553125.5632 0.35 0.0 0.0 0.0
null null 652.46 5.81 5988783.0871 3553125.1664 0.35 0.0 0.0 0.0
null null 671.65 7.83 5988783.592 3553124.4866 0.35 0.0 0.0 0.0
null true 717.03 8.52 5988784.786 3553122.879 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.6310</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553148.28 5988765.92 5.72,3553147.3742 5988766.5928 5.56,3553145.3223 5988768.1167 5.46,3553145.2437 5988768.1751 4.39,3553145.158 5988768.2388 5.5,3553142.8918 5988769.922 6.15,3553142.6084 5988770.1324 6.83,3553134.6756 5988776.0243 6.66,3553134.3355 5988776.2769 6.23,3553131.6156 5988778.2971 6.08,3553130.2556 5988779.3072 6.2,3553130.0289 5988779.4756 6.15,3553129.6332 5988779.7695 5.79,3553129.4075 5988779.9371 6.05,3553129.1812 5988780.1052 6.2,3553129.127 5988780.1455 5.82,3553129.099 5988780.1663 5.54,3553129.0579 5988780.1968 4.96,3553129.0508 5988780.202 4.77,3553129.0441 5988780.207 4.42,3553129.0161 5988780.2278 3.96,3553128.8758 5988780.332 3.22,3553128.6916 5988780.4688 3.43,3553128.6781 5988780.4788 3.63,3553128.6377 5988780.5088 4.57,3553128.6179 5988780.5236 5.27,3553128.5623 5988780.5649 5.84,3553128.5074 5988780.6057 6.08,3553128.2817 5988780.7733 5.84,3553127.3755 5988781.4463 5.63,3553126.4694 5988782.1193 5.63,3553125.5632 5988782.7924 5.86,3553125.1664 5988783.0871 5.81,3553124.4866 5988783.592 7.83,3553122.879 5988784.786 8.52</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595148041">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951480460">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.81 5988801.354 3553199.265 0.4 0.0 0.0 0.0
null null 80.77 5.52 5988804.5519 3553198.6004 0.15 0.0 0.0 0.0
null null 82.77 4.52 5988804.6311 3553198.584 0.15 0.0 0.0 0.0
null null 85.17 5.52 5988804.7261 3553198.5642 0.4 0.0 0.0 0.0
null null 149.18 6.13 5988807.2605 3553198.0375 0.35 0.0 0.0 0.0
null null 158.78 6.8 5988807.6406 3553197.9585 0.35 5.0 5.0 0.25
null null 350.76 6.7 5988815.2416 3553196.3789 0.35 0.0 0.0 0.0
null null 363.56 6.2 5988815.7484 3553196.2736 0.35 0.0 0.0 0.0
null null 465.89 6.31 5988819.8 3553195.4316 0.35 0.0 0.0 0.0
null null 478.68 6.21 5988820.3064 3553195.3264 0.35 0.0 0.0 0.0
null null 494.66 5.64 5988820.9391 3553195.1949 0.35 0.0 0.0 0.0
null null 508.0 5.9825 5988821.4672 3553195.0851 0.35 5.0 5.0 0.15
low null 510.63 6.05 5988821.5714 3553195.0635 0.2 0.0 0.0 0.0
null null 512.21 5.22 5988821.6339 3553195.0505 0.2 0.0 0.0 0.0
null null 512.7 4.76 5988821.6533 3553195.0464 0.12 0.0 0.0 0.0
null null 512.8 4.27 5988821.6573 3553195.0456 0.12 0.0 0.0 0.0
null null 514.37 3.56 5988821.7194 3553195.0327 0.12 0.0 0.0 0.0
null null 520.77 3.36 5988821.9728 3553194.98 0.12 0.0 0.0 0.0
null null 523.99 3.52 5988822.1003 3553194.9536 0.12 0.0 0.0 0.0
null null 524.79 4.33 5988822.132 3553194.947 0.2 0.0 0.0 0.0
null null 526.39 5.46 5988822.1954 3553194.9338 0.2 0.0 0.0 0.0
low null 528.0 6.07 5988822.2591 3553194.9206 0.35 10.0 10.0 0.1
null null 530.0 5.9731 5988822.3383 3553194.9041 0.35 0.0 0.0 0.0
null null 534.4 5.76 5988822.5125 3553194.8679 0.35 0.0 0.0 0.0
null null 560.01 5.52 5988823.5265 3553194.6572 0.35 0.0 0.0 0.0
null null 591.97 5.72 5988824.7919 3553194.3942 0.35 0.0 0.0 0.0
null null 604.75 6.16 5988825.2979 3553194.289 0.35 0.0 0.0 0.0
null null 623.93 6.21 5988826.0573 3553194.1312 0.35 0.0 0.0 0.0
null null 643.12 7.14 5988826.817 3553193.9733 0.35 0.0 0.0 0.0
null null 662.32 7.76 5988827.5772 3553193.8154 0.35 0.0 0.0 0.0
null null 675.11 7.77 5988828.0836 3553193.7101 0.35 0.0 0.0 0.0
null null 700.71 6.94 5988829.0972 3553193.4995 0.35 0.0 0.0 0.0
null null 726.31 7.88 5988830.1108 3553193.2888 0.35 0.0 0.0 0.0
null true 740.51 8.2 5988830.673 3553193.172 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553199.265 5988801.354 5.81,3553198.6004 5988804.5519 5.52,3553198.584 5988804.6311 4.52,3553198.5642 5988804.7261 5.52,3553198.0375 5988807.2605 6.13,3553197.9585 5988807.6406 6.8,3553196.3789 5988815.2416 6.7,3553196.2736 5988815.7484 6.2,3553195.4316 5988819.8 6.31,3553195.3264 5988820.3064 6.21,3553195.1949 5988820.9391 5.64,3553195.0851 5988821.4672 5.9825,3553195.0635 5988821.5714 6.05,3553195.0505 5988821.6339 5.22,3553195.0464 5988821.6533 4.76,3553195.0456 5988821.6573 4.27,3553195.0327 5988821.7194 3.56,3553194.98 5988821.9728 3.36,3553194.9536 5988822.1003 3.52,3553194.947 5988822.132 4.33,3553194.9338 5988822.1954 5.46,3553194.9206 5988822.2591 6.07,3553194.9041 5988822.3383 5.9731,3553194.8679 5988822.5125 5.76,3553194.6572 5988823.5265 5.52,3553194.3942 5988824.7919 5.72,3553194.289 5988825.2979 6.16,3553194.1312 5988826.0573 6.21,3553193.9733 5988826.817 7.14,3553193.8154 5988827.5772 7.76,3553193.7101 5988828.0836 7.77,3553193.4995 5988829.0972 6.94,3553193.2888 5988830.1108 7.88,3553193.172 5988830.673 8.2</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951491374">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951491370">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 5.96 5988811.057 3553297.022 0.4 0.0 0.0 0.0
null null 76.76 5.71 5988814.2242 3553297.0229 0.4 0.0 0.0 0.0
null null 140.75 6.25 5988816.8645 3553297.0236 0.35 0.0 0.0 0.0
null null 153.55 6.75 5988817.3926 3553297.0237 0.35 5.0 5.0 0.25
null null 243.1 6.61 5988821.0875 3553297.0248 0.35 0.0 0.0 0.0
null null 255.89 6.17 5988821.6153 3553297.0249 0.35 0.0 0.0 0.0
null null 358.19 6.34 5988825.8363 3553297.0261 0.35 0.0 0.0 0.0
null null 396.0 6.0706 5988827.3964 3553297.0265 0.35 5.0 5.0 0.1
low null 398.89 6.05 5988827.5156 3553297.0265 0.2 0.0 0.0 0.0
null null 401.27 5.05 5988827.6138 3553297.0266 0.2 0.0 0.0 0.0
null null 402.85 4.0 5988827.679 3553297.0266 0.12 0.0 0.0 0.0
null null 406.82 3.33 5988827.8428 3553297.0266 0.12 0.0 0.0 0.0
null null 410.8 3.84 5988828.007 3553297.0267 0.12 0.0 0.0 0.0
null null 411.99 4.47 5988828.0561 3553297.0267 0.2 0.0 0.0 0.0
null null 412.78 5.3 5988828.0887 3553297.0267 0.2 0.0 0.0 0.0
low null 414.0 5.6453 5988828.1391 3553297.0267 0.35 10.0 10.0 0.1
null null 415.96 6.2 5988828.2199 3553297.0267 0.35 0.0 0.0 0.0
null null 441.52 5.65 5988829.2746 3553297.027 0.35 0.0 0.0 0.0
null null 479.92 5.65 5988830.859 3553297.0274 0.35 0.0 0.0 0.0
null null 492.72 6.25 5988831.3871 3553297.0276 0.35 0.0 0.0 0.0
null null 505.5 7.15 5988831.9144 3553297.0277 0.35 0.0 0.0 0.0
null null 511.9 7.43 5988832.1785 3553297.0278 0.35 0.0 0.0 0.0
null null 518.29 7.42 5988832.4422 3553297.0279 0.35 0.0 0.0 0.0
null null 524.68 7.68 5988832.7058 3553297.028 0.35 0.0 0.0 0.0
null null 601.48 8.04 5988835.8747 3553297.0288 0.35 0.0 0.0 0.0
null null 691.0 7.45 5988839.5683 3553297.0298 0.35 0.0 0.0 0.0
null true 705.0 7.61 5988840.146 3553297.03 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553297.022 5988811.057 5.96,3553297.0229 5988814.2242 5.71,3553297.0236 5988816.8645 6.25,3553297.0237 5988817.3926 6.75,3553297.0248 5988821.0875 6.61,3553297.0249 5988821.6153 6.17,3553297.0261 5988825.8363 6.34,3553297.0265 5988827.3964 6.0706,3553297.0265 5988827.5156 6.05,3553297.0266 5988827.6138 5.05,3553297.0266 5988827.679 4.0,3553297.0266 5988827.8428 3.33,3553297.0267 5988828.007 3.84,3553297.0267 5988828.0561 4.47,3553297.0267 5988828.0887 5.3,3553297.0267 5988828.1391 5.6453,3553297.0267 5988828.2199 6.2,3553297.027 5988829.2746 5.65,3553297.0274 5988830.859 5.65,3553297.0276 5988831.3871 6.25,3553297.0277 5988831.9144 7.15,3553297.0278 5988832.1785 7.43,3553297.0279 5988832.4422 7.42,3553297.028 5988832.7058 7.68,3553297.0288 5988835.8747 8.04,3553297.0298 5988839.5683 7.45,3553297.03 5988840.146 7.61</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951502343">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951502362">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.16 5988859.05 3553392.804 0.4 0.0 0.0 0.0
null null 25.55 5.79 5988859.6353 3553391.8427 0.4 0.0 0.0 0.0
null null 76.68 5.95 5988860.8066 3553389.9189 0.4 0.0 0.0 0.0
null null 102.28 5.87 5988861.393 3553388.9556 0.4 0.0 0.0 0.0
null null 153.43 6.2 5988862.5648 3553387.0311 0.4 0.0 0.0 0.0
null null 179.02 6.69 5988863.151 3553386.0682 0.35 0.0 0.0 0.0
null null 198.18 5.91 5988863.5899 3553385.3473 0.35 0.0 0.0 0.0
null null 223.76 6.17 5988864.1759 3553384.3848 0.35 0.0 0.0 0.0
null null 300.51 5.89 5988865.9341 3553381.4971 0.35 0.0 0.0 0.0
null null 353.2 6.29 5988867.1411 3553379.5146 0.35 8.0 8.0 0.1
low null 353.99 6.62 5988867.1592 3553379.4848 0.25 0.0 0.0 0.0
null null 356.34 5.3 5988867.2131 3553379.3964 0.25 0.0 0.0 0.0
null null 357.12 4.6 5988867.2309 3553379.3671 0.25 0.0 0.0 0.0
null null 358.1 3.99 5988867.2534 3553379.3302 0.12 0.0 0.0 0.0
null null 358.19 3.14 5988867.2554 3553379.3268 0.12 0.0 0.0 0.0
null null 364.53 3.61 5988867.4007 3553379.0883 0.12 0.0 0.0 0.0
null null 367.72 4.01 5988867.4737 3553378.9682 0.12 0.0 0.0 0.0
null null 368.51 4.34 5988867.4918 3553378.9385 0.2 0.0 0.0 0.0
low null 369.7 5.53 5988867.5191 3553378.8937 0.2 8.0 8.0 0.1
null null 372.07 6.34 5988867.5734 3553378.8046 0.35 0.0 0.0 0.0
null null 397.6 5.98 5988868.1582 3553377.844 0.35 0.0 0.0 0.0
null null 416.77 5.96 5988868.5974 3553377.1227 0.35 0.0 0.0 0.0
null null 423.16 6.26 5988868.7438 3553376.8822 0.35 0.0 0.0 0.0
null null 439.14 7.59 5988869.1098 3553376.281 0.35 0.0 0.0 0.0
null null 618.21 7.94 5988873.212 3553369.5433 0.35 0.0 0.0 0.0
null true 681.42 6.96 5988874.66 3553367.165 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>60.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553392.804 5988859.05 6.16,3553391.8427 5988859.6353 5.79,3553389.9189 5988860.8066 5.95,3553388.9556 5988861.393 5.87,3553387.0311 5988862.5648 6.2,3553386.0682 5988863.151 6.69,3553385.3473 5988863.5899 5.91,3553384.3848 5988864.1759 6.17,3553381.4971 5988865.9341 5.89,3553379.5146 5988867.1411 6.29,3553379.4848 5988867.1592 6.62,3553379.3964 5988867.2131 5.3,3553379.3671 5988867.2309 4.6,3553379.3302 5988867.2534 3.99,3553379.3268 5988867.2554 3.14,3553379.0883 5988867.4007 3.61,3553378.9682 5988867.4737 4.01,3553378.9385 5988867.4918 4.34,3553378.8937 5988867.5191 5.53,3553378.8046 5988867.5734 6.34,3553377.844 5988868.1582 5.98,3553377.1227 5988868.5974 5.96,3553376.8822 5988868.7438 6.26,3553376.281 5988869.1098 7.59,3553369.5433 5988873.212 7.94,3553367.165 5988874.66 6.96</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951514856">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595151637">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.3 5988958.958 3553422.898 0.4 0.0 0.0 0.0
null null 25.6 5.86 5988959.1627 3553421.8527 0.4 0.0 0.0 0.0
null null 76.8 5.69 5988959.572 3553419.7621 0.4 0.0 0.0 0.0
null null 134.39 5.77 5988960.0325 3553417.4105 0.4 0.0 0.0 0.0
null null 140.79 6.0 5988960.0836 3553417.1492 0.35 0.0 0.0 0.0
null null 153.58 5.8 5988960.1859 3553416.627 0.35 0.0 0.0 0.0
null null 358.35 5.73 5988961.8231 3553408.2657 0.35 0.0 0.0 0.0
null null 396.7 6.17 5988962.1297 3553406.6998 0.35 0.0 0.0 0.0
low null 399.1 6.57 5988962.1489 3553406.6018 0.2 0.0 0.0 0.0
null null 402.27 5.28 5988962.1742 3553406.4724 0.12 0.0 0.0 0.0
null null 402.57 3.99 5988962.1766 3553406.4601 0.12 0.0 0.0 0.0
null null 405.78 3.58 5988962.2023 3553406.3291 0.12 0.0 0.0 0.0
null null 413.69 3.59 5988962.2655 3553406.0061 0.12 0.0 0.0 0.0
null null 413.9 4.5 5988962.2672 3553405.9975 0.2 0.0 0.0 0.0
null null 414.7 4.37 5988962.2736 3553405.9648 0.2 0.0 0.0 0.0
null null 416.3 5.15 5988962.2864 3553405.8995 0.2 0.0 0.0 0.0
low null 422.72 6.02 5988962.3377 3553405.6374 0.35 5.0 5.0 0.1
null null 425.0 6.0102 5988962.3559 3553405.5443 0.35 0.0 0.0 0.0
null null 448.28 5.91 5988962.5421 3553404.5937 0.35 0.0 0.0 0.0
null null 467.47 7.08 5988962.6955 3553403.8101 0.35 0.0 0.0 0.0
null null 518.68 7.29 5988963.1049 3553401.7191 0.35 0.0 0.0 0.0
null null 569.89 7.86 5988963.5144 3553399.6281 0.35 0.0 0.0 0.0
null null 595.5 7.85 5988963.7191 3553398.5824 0.35 0.0 0.0 0.0
null null 659.47 7.15 5988964.2306 3553395.9703 0.35 0.0 0.0 0.0
null true 677.16 7.26 5988964.372 3553395.248 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553422.898 5988958.958 6.3,3553421.8527 5988959.1627 5.86,3553419.7621 5988959.572 5.69,3553417.4105 5988960.0325 5.77,3553417.1492 5988960.0836 6.0,3553416.627 5988960.1859 5.8,3553408.2657 5988961.8231 5.73,3553406.6998 5988962.1297 6.17,3553406.6018 5988962.1489 6.57,3553406.4724 5988962.1742 5.28,3553406.4601 5988962.1766 3.99,3553406.3291 5988962.2023 3.58,3553406.0061 5988962.2655 3.59,3553405.9975 5988962.2672 4.5,3553405.9648 5988962.2736 4.37,3553405.8995 5988962.2864 5.15,3553405.6374 5988962.3377 6.02,3553405.5443 5988962.3559 6.0102,3553404.5937 5988962.5421 5.91,3553403.8101 5988962.6955 7.08,3553401.7191 5988963.1049 7.29,3553399.6281 5988963.5144 7.86,3553398.5824 5988963.7191 7.85,3553395.9703 5988964.2306 7.15,3553395.248 5988964.372 7.26</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951527321">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951527359">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.36 5989032.782 3553454.861 0.35 0.0 0.0 0.0
null null 38.38 5.64 5989034.8022 3553454.5515 0.35 0.0 0.0 0.0
null null 140.71 5.85 5989040.1886 3553453.7263 0.35 0.0 0.0 0.0
null null 409.43 5.83 5989054.3332 3553451.5592 0.35 0.0 0.0 0.0
null null 431.86 6.53 5989055.5138 3553451.3784 0.35 0.0 0.0 0.0
low null 435.06 6.94 5989055.6823 3553451.3525 0.25 0.0 0.0 0.0
null null 436.65 5.9 5989055.766 3553451.3397 0.25 0.0 0.0 0.0
null null 438.25 4.23 5989055.8502 3553451.3268 0.12 0.0 0.0 0.0
null null 439.05 3.79 5989055.8923 3553451.3204 0.12 0.0 0.0 0.0
null null 440.65 3.5 5989055.9765 3553451.3075 0.12 0.0 0.0 0.0
null null 447.04 3.45 5989056.3129 3553451.2559 0.12 0.0 0.0 0.0
null null 449.44 3.62 5989056.4392 3553451.2366 0.12 0.0 0.0 0.0
null null 449.83 4.72 5989056.4597 3553451.2334 0.25 0.0 0.0 0.0
null null 450.13 4.68 5989056.4755 3553451.231 0.25 0.0 0.0 0.0
low null 450.91 5.5 5989056.5166 3553451.2247 0.25 5.0 5.0 0.1
null null 451.7 5.85 5989056.5581 3553451.2184 0.35 0.0 0.0 0.0
null null 554.0 5.9 5989061.9429 3553450.3934 0.35 0.0 0.0 0.0
null null 566.79 6.13 5989062.6161 3553450.2902 0.35 0.0 0.0 0.0
null true 600.3 6.12 5989064.38 3553450.02 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553454.861 5989032.782 6.36,3553454.5515 5989034.8022 5.64,3553453.7263 5989040.1886 5.85,3553451.5592 5989054.3332 5.83,3553451.3784 5989055.5138 6.53,3553451.3525 5989055.6823 6.94,3553451.3397 5989055.766 5.9,3553451.3268 5989055.8502 4.23,3553451.3204 5989055.8923 3.79,3553451.3075 5989055.9765 3.5,3553451.2559 5989056.3129 3.45,3553451.2366 5989056.4392 3.62,3553451.2334 5989056.4597 4.72,3553451.231 5989056.4755 4.68,3553451.2247 5989056.5166 5.5,3553451.2184 5989056.5581 5.85,3553450.3934 5989061.9429 5.9,3553450.2902 5989062.6161 6.13,3553450.02 5989064.38 6.12</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595152889">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951528884">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.39 5989031.778 3553485.965 0.35 0.0 0.0 0.0
null null 38.37 5.63 5989033.8154 3553486.3144 0.35 0.0 0.0 0.0
null null 51.17 5.87 5989034.4951 3553486.431 0.35 0.0 0.0 0.0
null null 102.34 5.7 5989037.2123 3553486.8969 0.35 0.0 0.0 0.0
null null 383.86 5.89 5989052.161 3553489.4604 0.35 0.0 0.0 0.0
null null 415.81 6.26 5989053.8575 3553489.7514 0.35 5.0 5.0 0.15
low null 417.41 5.89 5989053.9425 3553489.7659 0.25 0.0 0.0 0.0
null null 418.21 5.51 5989053.985 3553489.7732 0.15 0.0 0.0 0.0
null null 418.81 4.95 5989054.0168 3553489.7787 0.15 0.0 0.0 0.0
null null 419.21 4.1 5989054.0381 3553489.7823 0.15 0.0 0.0 0.0
null null 422.41 3.52 5989054.208 3553489.8115 0.15 0.0 0.0 0.0
null null 424.81 2.74 5989054.3354 3553489.8333 0.15 0.0 0.0 0.0
null null 429.62 3.32 5989054.5908 3553489.8771 0.15 0.0 0.0 0.0
null null 432.81 4.56 5989054.7602 3553489.9062 0.15 0.0 0.0 0.0
null null 434.01 5.41 5989054.8239 3553489.9171 0.15 0.0 0.0 0.0
null null 434.81 5.41 5989054.8664 3553489.9244 0.25 0.0 0.0 0.0
low null 436.41 5.84 5989054.9514 3553489.9389 0.35 0.0 0.0 0.0
null null 442.81 6.39 5989055.2912 3553489.9972 0.35 0.0 0.0 0.0
null null 494.03 5.9 5989058.011 3553490.4636 0.35 0.0 0.0 0.0
null null 545.22 6.15 5989060.7292 3553490.9298 0.35 0.0 0.0 0.0
null true 579.51 6.14 5989062.55 3553491.242 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.1270</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553485.965 5989031.778 6.39,3553486.3144 5989033.8154 5.63,3553486.431 5989034.4951 5.87,3553486.8969 5989037.2123 5.7,3553489.4604 5989052.161 5.89,3553489.7514 5989053.8575 6.26,3553489.7659 5989053.9425 5.89,3553489.7732 5989053.985 5.51,3553489.7787 5989054.0168 4.95,3553489.7823 5989054.0381 4.1,3553489.8115 5989054.208 3.52,3553489.8333 5989054.3354 2.74,3553489.8771 5989054.5908 3.32,3553489.9062 5989054.7602 4.56,3553489.9171 5989054.8239 5.41,3553489.9244 5989054.8664 5.41,3553489.9389 5989054.9514 5.84,3553489.9972 5989055.2912 6.39,3553490.4636 5989058.011 5.9,3553490.9298 5989060.7292 6.15,3553491.242 5989062.55 6.14</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951539886">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595153988">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.41 5989031.106 3553497.106 0.35 0.0 0.0 0.0
null null 25.6 5.98 5989032.0818 3553497.4065 0.35 0.0 0.0 0.0
null null 102.38 5.67 5989035.0084 3553498.3077 0.35 0.0 0.0 0.0
null null 358.21 5.93 5989044.76 3553501.3105 0.35 0.0 0.0 0.0
null null 402.0 6.1948 5989046.4291 3553501.8245 0.35 5.0 5.0 0.1
low null 404.51 6.21 5989046.5248 3553501.8539 0.25 0.0 0.0 0.0
null null 407.27 5.03 5989046.63 3553501.8863 0.25 0.0 0.0 0.0
null null 408.02 4.19 5989046.6586 3553501.8951 0.15 0.0 0.0 0.0
null null 409.58 3.62 5989046.718 3553501.9134 0.15 0.0 0.0 0.0
null null 417.44 4.18 5989047.0176 3553502.0057 0.15 0.0 0.0 0.0
null null 418.99 4.53 5989047.0767 3553502.0239 0.15 0.0 0.0 0.0
null null 420.15 5.45 5989047.1209 3553502.0375 0.25 0.0 0.0 0.0
low null 423.33 5.92 5989047.2421 3553502.0748 0.35 0.0 0.0 0.0
null null 436.04 6.41 5989047.7266 3553502.224 0.35 0.0 0.0 0.0
null null 487.11 5.85 5989049.6733 3553502.8235 0.35 0.0 0.0 0.0
null null 512.72 6.17 5989050.6494 3553503.1241 0.35 0.0 0.0 0.0
null true 547.05 6.14 5989051.958 3553503.527 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.1390</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553497.106 5989031.106 6.41,3553497.4065 5989032.0818 5.98,3553498.3077 5989035.0084 5.67,3553501.3105 5989044.76 5.93,3553501.8245 5989046.4291 6.1948,3553501.8539 5989046.5248 6.21,3553501.8863 5989046.63 5.03,3553501.8951 5989046.6586 4.19,3553501.9134 5989046.718 3.62,3553502.0057 5989047.0176 4.18,3553502.0239 5989047.0767 4.53,3553502.0375 5989047.1209 5.45,3553502.0748 5989047.2421 5.92,3553502.224 5989047.7266 6.41,3553502.8235 5989049.6733 5.85,3553503.1241 5989050.6494 6.17,3553503.527 5989051.958 6.14</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951550793">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951550799">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.44 5989002.829 3553547.175 0.35 0.0 0.0 0.0
null null 25.58 6.01 5989004.2424 3553548.0961 0.35 0.0 0.0 0.0
null null 31.97 6.13 5989004.5954 3553548.3262 0.35 0.0 0.0 0.0
null null 57.56 5.76 5989006.0093 3553549.2477 0.35 0.0 0.0 0.0
null null 83.14 5.86 5989007.4227 3553550.1688 0.35 0.0 0.0 0.0
null null 108.71 5.72 5989008.8355 3553551.0896 0.35 0.0 0.0 0.0
null null 339.0 5.96 5989021.5595 3553559.3822 0.35 0.0 0.0 0.0
null null 365.0 6.1926 5989022.996 3553560.3184 0.35 15.0 15.0 0.15
low null 366.95 6.21 5989023.1038 3553560.3886 0.25 0.0 0.0 0.0
null null 368.0 5.6469 5989023.1618 3553560.4265 0.15 0.0 0.0 0.0
null null 370.12 4.51 5989023.2789 3553560.5028 0.15 0.0 0.0 0.0
null null 371.7 4.21 5989023.3662 3553560.5597 0.15 0.0 0.0 0.0
null null 379.65 4.46 5989023.8055 3553560.846 0.15 0.0 0.0 0.0
null null 381.22 5.05 5989023.8922 3553560.9025 0.25 0.0 0.0 0.0
low null 382.79 5.84 5989023.979 3553560.959 0.35 0.0 0.0 0.0
null null 395.6 6.12 5989024.6868 3553561.4203 0.35 0.0 0.0 0.0
null null 472.35 5.92 5989028.9274 3553564.184 0.35 0.0 0.0 0.0
null true 506.84 6.15 5989030.833 3553565.426 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.2000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553547.175 5989002.829 6.44,3553548.0961 5989004.2424 6.01,3553548.3262 5989004.5954 6.13,3553549.2477 5989006.0093 5.76,3553550.1688 5989007.4227 5.86,3553551.0896 5989008.8355 5.72,3553559.3822 5989021.5595 5.96,3553560.3184 5989022.996 6.1926,3553560.3886 5989023.1038 6.21,3553560.4265 5989023.1618 5.6469,3553560.5028 5989023.2789 4.51,3553560.5597 5989023.3662 4.21,3553560.846 5989023.8055 4.46,3553560.9025 5989023.8922 5.05,3553560.959 5989023.979 5.84,3553561.4203 5989024.6868 6.12,3553564.184 5989028.9274 5.92,3553565.426 5989030.833 6.15</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951561658">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859515616101">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#OBERKANTEWEHR"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.4 5988983.166 3553577.468 0.35 0.0 0.0 0.0 6.4
null null 38.4 5.9 5988985.5663 3553579.0261 0.35 0.0 0.0 0.0 5.9
null null 44.8 6.11 5988985.9664 3553579.2858 0.35 0.0 0.0 0.0 6.11
null null 57.6 5.82 5988986.7665 3553579.8051 0.35 0.0 0.0 0.0 5.82
null null 287.85 6.19 5989001.1589 3553589.1475 0.35 0.0 0.0 0.0 6.19
null null 298.21 6.5 5989001.8065 3553589.5679 0.25 0.0 0.0 0.0 6.5
null null 299.81 5.64 5989001.9065 3553589.6328 0.25 0.0 0.0 0.0 5.64
low null 302.14 4.65 5989002.0522 3553589.7273 0.15 0.0 0.0 0.0 4.65
null null 303.72 3.96 5989002.1509 3553589.7915 0.15 0.0 0.0 0.0 4.65
null null 306.89 4.06 5989002.3491 3553589.9201 0.15 0.0 0.0 0.0 4.65
null null 311.66 3.93 5989002.6473 3553590.1136 0.15 0.0 0.0 0.0 4.65
null null 313.25 4.19 5989002.7466 3553590.1781 0.15 0.0 0.0 0.0 4.65
low null 313.8 4.65 5989002.781 3553590.2005 0.15 0.0 0.0 0.0 4.65
null null 315.24 5.5 5989002.871 3553590.2589 0.25 0.0 0.0 0.0 5.5
null null 316.05 6.5 5989002.9217 3553590.2917 0.35 0.0 0.0 0.0 6.5
null null 323.23 6.32 5989003.3705 3553590.5831 0.35 0.0 0.0 0.0 6.32
null null 348.81 6.14 5989004.9694 3553591.621 0.35 0.0 0.0 0.0 6.14
null null 387.22 6.32 5989007.3704 3553593.1795 0.35 0.0 0.0 0.0 6.32
null null 412.76 5.97 5989008.9668 3553594.2158 0.35 0.0 0.0 0.0 5.97
null true 466.74 6.18 5989012.341 3553596.406 0.35 0.0 0.0 0.0 6.18
]]></om:result>
       <prof:station>61.2410</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553577.468 5988983.166 6.4,3553579.0261 5988985.5663 5.9,3553579.2858 5988985.9664 6.11,3553579.8051 5988986.7665 5.82,3553589.1475 5989001.1589 6.19,3553589.5679 5989001.8065 6.5,3553589.6328 5989001.9065 5.64,3553589.7273 5989002.0522 4.65,3553589.7915 5989002.1509 3.96,3553589.9201 5989002.3491 4.06,3553590.1136 5989002.6473 3.93,3553590.1781 5989002.7466 4.19,3553590.2005 5989002.781 4.65,3553590.2589 5989002.871 5.5,3553590.2917 5989002.9217 6.5,3553590.5831 5989003.3705 6.32,3553591.621 5989004.9694 6.14,3553593.1795 5989007.3704 6.32,3553594.2158 5989008.9668 5.97,3553596.406 5989012.341 6.18</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
       <prof:member>
        <om:Observation gml:id="Observation118285951580473">
         <gml:description>Bauwerk-Observation</gml:description>
         <gml:name>urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR</gml:name>
         <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR"/>
         <om:resultDefinition>
          <swe:RecordDefinition gml:id="RecordDefinition118285951580456">
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#WEHRART"/>
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#FORMBEIWERT"/>
          </swe:RecordDefinition>
         </om:resultDefinition>
         <om:result><![CDATA[org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_BREITKRONIG 5.0
]]></om:result>
        </om:Observation>
       </prof:member>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951585169">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951585142">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.63 5988955.749 3553630.615 0.35 5.0 5.0 0.2
null null 102.37 5.99 5988963.6195 3553633.8014 0.35 0.0 0.0 0.0
low null 225.45 6.03 5988973.0822 3553637.6325 0.25 0.0 0.0 0.0
null null 227.03 5.5 5988973.2036 3553637.6816 0.25 0.0 0.0 0.0
null null 228.61 4.67 5988973.3251 3553637.7308 0.12 0.0 0.0 0.0
null null 230.19 4.49 5988973.4466 3553637.78 0.12 0.0 0.0 0.0
null null 235.73 4.73 5988973.8725 3553637.9524 0.12 0.0 0.0 0.0
null null 237.32 5.6 5988973.9948 3553638.0019 0.25 0.0 0.0 0.0
low null 240.48 6.03 5988974.2377 3553638.1003 0.35 0.0 0.0 0.0
null null 304.44 6.0 5988979.1551 3553640.0911 0.35 0.0 0.0 0.0
null null 317.24 6.41 5988980.1392 3553640.4896 0.35 0.0 0.0 0.0
null null 330.04 6.22 5988981.1233 3553640.888 0.35 5.0 5.0 0.25
null true 375.95 6.28 5988984.653 3553642.317 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553630.615 5988955.749 6.63,3553633.8014 5988963.6195 5.99,3553637.6325 5988973.0822 6.03,3553637.6816 5988973.2036 5.5,3553637.7308 5988973.3251 4.67,3553637.78 5988973.4466 4.49,3553637.9524 5988973.8725 4.73,3553638.0019 5988973.9948 5.6,3553638.1003 5988974.2377 6.03,3553640.0911 5988979.1551 6.0,3553640.4896 5988980.1392 6.41,3553640.888 5988981.1233 6.22,3553642.317 5988984.653 6.28</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951599176">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951599121">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.54 5988961.554 3553732.952 0.35 0.0 0.0 0.0
null null 51.13 6.15 5988967.9308 3553731.5877 0.35 0.0 0.0 0.0
null null 93.93 6.63 5988973.2687 3553730.4456 0.35 0.0 0.0 0.0
null null 95.93 6.63 5988973.5182 3553730.3922 0.35 0.7 7.0 0.05
low null 96.73 6.07 5988973.618 3553730.3709 0.25 0.0 0.0 0.0
null null 97.94 5.54 5988973.7689 3553730.3386 0.25 0.0 0.0 0.0
null null 98.34 5.11 5988973.8187 3553730.3279 0.12 0.0 0.0 0.0
null null 99.93 4.55 5988974.017 3553730.2855 0.12 0.0 0.0 0.0
null null 106.73 4.24 5988974.8651 3553730.104 0.12 0.0 0.0 0.0
null null 107.73 5.28 5988974.9898 3553730.0773 0.25 0.0 0.0 0.0
null null 108.73 5.74 5988975.1146 3553730.0507 0.25 0.0 0.0 0.0
low null 111.9 6.23 5988975.5099 3553729.9661 0.35 0.0 0.0 0.0
null null 118.24 6.36 5988976.3006 3553729.7969 0.35 0.0 0.0 0.0
null null 143.82 6.1 5988979.4909 3553729.1143 0.35 0.0 0.0 0.0
null null 169.39 6.22 5988982.6799 3553728.432 0.35 0.0 0.0 0.0
null null 182.18 6.5 5988984.2751 3553728.0907 0.35 5.0 5.0 0.25
null true 216.16 6.36 5988988.513 3553727.184 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553732.952 5988961.554 6.54,3553731.5877 5988967.9308 6.15,3553730.4456 5988973.2687 6.63,3553730.3922 5988973.5182 6.63,3553730.3709 5988973.618 6.07,3553730.3386 5988973.7689 5.54,3553730.3279 5988973.8187 5.11,3553730.2855 5988974.017 4.55,3553730.104 5988974.8651 4.24,3553730.0773 5988974.9898 5.28,3553730.0507 5988975.1146 5.74,3553729.9661 5988975.5099 6.23,3553729.7969 5988976.3006 6.36,3553729.1143 5988979.4909 6.1,3553728.432 5988982.6799 6.22,3553728.0907 5988984.2751 6.5,3553727.184 5988988.513 6.36</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951603832">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951603818">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.63 5988974.737 3553830.527 0.35 0.0 0.0 0.0
null null 38.38 6.59 5988979.3374 3553830.7897 0.35 0.0 0.0 0.0
null null 51.13 6.25 5988980.8656 3553830.8769 0.35 0.0 0.0 0.0
null null 62.0 6.3061 5988982.1686 3553830.9513 0.35 8.0 8.0 0.15
low null 64.7 6.32 5988982.4922 3553830.9698 0.25 0.0 0.0 0.0
null null 66.0 5.7959 5988982.648 3553830.9787 0.15 0.0 0.0 0.0
null null 67.85 5.05 5988982.8698 3553830.9913 0.15 0.0 0.0 0.0
null null 69.41 4.73 5988983.0568 3553831.002 0.15 0.0 0.0 0.0
null null 75.77 5.01 5988983.8191 3553831.0455 0.15 0.0 0.0 0.0
null null 77.37 5.69 5988984.0109 3553831.0565 0.25 0.0 0.0 0.0
low null 78.97 6.09 5988984.2027 3553831.0674 0.35 0.0 0.0 0.0
null null 91.71 6.44 5988985.7297 3553831.1546 0.35 0.0 0.0 0.0
null null 142.87 6.38 5988991.862 3553831.5048 0.35 5.0 5.0 0.25
null true 227.8 6.62 5989002.042 3553832.086 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.5000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553830.527 5988974.737 6.63,3553830.7897 5988979.3374 6.59,3553830.8769 5988980.8656 6.25,3553830.9513 5988982.1686 6.3061,3553830.9698 5988982.4922 6.32,3553830.9787 5988982.648 5.7959,3553830.9913 5988982.8698 5.05,3553831.002 5988983.0568 4.73,3553831.0455 5988983.8191 5.01,3553831.0565 5988984.0109 5.69,3553831.0674 5988984.2027 6.09,3553831.1546 5988985.7297 6.44,3553831.5048 5988991.862 6.38,3553832.086 5989002.042 6.62</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951616364">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951616378">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.8 5988966.88 3553927.527 0.35 4.0 4.0 0.25
null null 25.59 6.61 5988971.1698 3553928.3639 0.35 0.0 0.0 0.0
null null 31.99 6.49 5988972.2426 3553928.5732 0.35 0.0 0.0 0.0
null null 51.18 6.33 5988975.4595 3553929.2008 0.35 0.0 0.0 0.0
null null 60.78 6.31 5988977.0688 3553929.5148 0.35 0.0 0.0 0.0
null null 62.38 6.28 5988977.337 3553929.5671 0.35 0.0 0.0 0.0
null null 63.98 6.22 5988977.6053 3553929.6194 0.35 0.0 0.0 0.0
low null 67.77 6.63 5988978.2406 3553929.7434 0.35 0.0 0.0 0.0
null null 68.3 6.2961 5988978.3294 3553929.7607 0.25 0.0 0.0 0.0
null null 68.97 5.91 5988978.4418 3553929.7826 0.25 0.0 0.0 0.0
null null 69.87 5.58 5988978.5926 3553929.812 0.25 0.0 0.0 0.0
null null 70.47 5.03 5988978.6932 3553929.8317 0.12 0.0 0.0 0.0
null null 71.27 4.87 5988978.8273 3553929.8578 0.12 0.0 0.0 0.0
null null 72.07 4.75 5988978.9614 3553929.884 0.12 0.0 0.0 0.0
null null 74.07 4.64 5988979.2967 3553929.9494 0.12 0.0 0.0 0.0
null null 75.66 4.66 5988979.5632 3553930.0014 0.12 0.0 0.0 0.0
null null 78.06 4.64 5988979.9656 3553930.0799 0.12 0.0 0.0 0.0
null null 79.26 4.69 5988980.1667 3553930.1191 0.12 0.0 0.0 0.0
null null 79.36 4.78 5988980.1835 3553930.1224 0.25 0.0 0.0 0.0
null null 80.15 5.67 5988980.3159 3553930.1482 0.25 0.0 0.0 0.0
null null 80.25 5.74 5988980.3327 3553930.1515 0.25 0.0 0.0 0.0
null null 81.45 6.07 5988980.5338 3553930.1908 0.25 0.0 0.0 0.0
low null 82.85 6.35 5988980.7685 3553930.2365 0.35 0.0 0.0 0.0
null null 87.64 6.39 5988981.5715 3553930.3932 0.35 0.0 0.0 0.0
null null 89.24 6.43 5988981.8397 3553930.4455 0.35 0.0 0.0 0.0
null null 100.42 6.18 5988983.7139 3553930.8111 0.35 0.0 0.0 0.0
null null 113.22 6.3 5988985.8596 3553931.2298 0.35 0.0 0.0 0.0
null null 119.62 6.35 5988986.9324 3553931.4391 0.35 0.0 0.0 0.0
null null 126.02 6.37 5988988.0053 3553931.6484 0.35 0.0 0.0 0.0
null null 132.42 6.49 5988989.0782 3553931.8577 0.35 4.0 4.0 0.25
null null 142.01 6.69 5988990.6858 3553932.1713 0.35 4.0 4.0 0.25
null true 159.46 6.64 5988993.611 3553932.742 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3553927.527 5988966.88 6.8,3553928.3639 5988971.1698 6.61,3553928.5732 5988972.2426 6.49,3553929.2008 5988975.4595 6.33,3553929.5148 5988977.0688 6.31,3553929.5671 5988977.337 6.28,3553929.6194 5988977.6053 6.22,3553929.7434 5988978.2406 6.63,3553929.7607 5988978.3294 6.2961,3553929.7826 5988978.4418 5.91,3553929.812 5988978.5926 5.58,3553929.8317 5988978.6932 5.03,3553929.8578 5988978.8273 4.87,3553929.884 5988978.9614 4.75,3553929.9494 5988979.2967 4.64,3553930.0014 5988979.5632 4.66,3553930.0799 5988979.9656 4.64,3553930.1191 5988980.1667 4.69,3553930.1224 5988980.1835 4.78,3553930.1482 5988980.3159 5.67,3553930.1515 5988980.3327 5.74,3553930.1908 5988980.5338 6.07,3553930.2365 5988980.7685 6.35,3553930.3932 5988981.5715 6.39,3553930.4455 5988981.8397 6.43,3553930.8111 5988983.7139 6.18,3553931.2298 5988985.8596 6.3,3553931.4391 5988986.9324 6.35,3553931.6484 5988988.0053 6.37,3553931.8577 5988989.0782 6.49,3553932.1713 5988990.6858 6.69,3553932.742 5988993.611 6.64</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859516304101">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951630477">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.06 5988950.463 3554026.327 0.35 3.0 3.0 0.15
null null 12.79 6.96 5988952.4013 3554026.7007 0.35 3.0 3.0 0.15
null null 38.37 6.71 5988956.278 3554027.4482 0.35 3.0 3.0 0.15
null null 44.76 6.62 5988957.2464 3554027.6349 0.35 3.0 3.0 0.15
null null 70.33 6.44 5988961.1216 3554028.3821 0.35 3.0 3.0 0.15
null null 73.51 6.4 5988961.6035 3554028.475 0.35 3.0 3.0 0.15
null null 73.91 6.38 5988961.6642 3554028.4867 0.35 0.0 0.0 0.0
null null 79.52 6.6 5988962.5144 3554028.6506 0.35 15.0 15.0 0.15
low null 80.42 5.97 5988962.6508 3554028.6769 0.25 0.0 0.0 0.0
null null 81.43 5.52 5988962.8038 3554028.7064 0.15 0.0 0.0 0.0
null null 81.93 5.19 5988962.8796 3554028.721 0.15 0.0 0.0 0.0
null null 83.72 4.72 5988963.1509 3554028.7734 0.15 0.0 0.0 0.0
null null 85.31 4.85 5988963.3918 3554028.8198 0.15 0.0 0.0 0.0
null null 87.71 5.0 5988963.7556 3554028.8899 0.15 0.0 0.0 0.0
null null 90.1 5.03 5988964.1178 3554028.9598 0.15 0.0 0.0 0.0
null null 90.9 5.07 5988964.239 3554028.9832 0.15 0.0 0.0 0.0
null null 91.0 5.12 5988964.2542 3554028.9861 0.15 0.0 0.0 0.0
null null 91.69 5.67 5988964.3587 3554029.0062 0.15 0.0 0.0 0.0
null null 92.49 5.94 5988964.48 3554029.0296 0.25 0.0 0.0 0.0
low null 94.28 6.27 5988964.7513 3554029.0819 0.35 0.0 0.0 0.0
null null 100.67 6.49 5988965.7197 3554029.2686 0.35 0.0 0.0 0.0
null null 101.47 6.48 5988965.8409 3554029.292 0.35 0.0 0.0 0.0
null null 103.06 6.41 5988966.0819 3554029.3385 0.35 0.0 0.0 0.0
null null 103.86 6.43 5988966.2031 3554029.3619 0.35 0.0 0.0 0.0
null null 124.62 6.25 5988969.3493 3554029.9685 0.35 0.0 0.0 0.0
null null 137.42 6.34 5988971.2892 3554030.3425 0.35 0.0 0.0 0.0
null null 156.62 6.42 5988974.199 3554030.9035 0.35 0.0 0.0 0.0
null true 178.23 6.68 5988977.474 3554031.535 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554026.327 5988950.463 7.06,3554026.7007 5988952.4013 6.96,3554027.4482 5988956.278 6.71,3554027.6349 5988957.2464 6.62,3554028.3821 5988961.1216 6.44,3554028.475 5988961.6035 6.4,3554028.4867 5988961.6642 6.38,3554028.6506 5988962.5144 6.6,3554028.6769 5988962.6508 5.97,3554028.7064 5988962.8038 5.52,3554028.721 5988962.8796 5.19,3554028.7734 5988963.1509 4.72,3554028.8198 5988963.3918 4.85,3554028.8899 5988963.7556 5.0,3554028.9598 5988964.1178 5.03,3554028.9832 5988964.239 5.07,3554028.9861 5988964.2542 5.12,3554029.0062 5988964.3587 5.67,3554029.0296 5988964.48 5.94,3554029.0819 5988964.7513 6.27,3554029.2686 5988965.7197 6.49,3554029.292 5988965.8409 6.48,3554029.3385 5988966.0819 6.41,3554029.3619 5988966.2031 6.43,3554029.9685 5988969.3493 6.25,3554030.3425 5988971.2892 6.34,3554030.9035 5988974.199 6.42,3554031.535 5988977.474 6.68</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951642966">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951642942">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.42 5988941.498 3554128.811 0.35 3.0 3.0 0.15
null null 25.58 6.77 5988945.336 3554128.656 0.35 3.0 3.0 0.15
null null 28.78 6.66 5988945.8161 3554128.6366 0.35 3.0 3.0 0.15
null null 30.38 6.63 5988946.0562 3554128.6269 0.35 3.0 3.0 0.15
null null 49.56 6.75 5988948.9339 3554128.5106 0.35 3.0 3.0 0.15
null null 55.96 6.77 5988949.8942 3554128.4719 0.35 3.0 3.0 0.15
null null 78.32 6.42 5988953.249 3554128.3363 0.35 3.0 3.0 0.15
null null 79.91 6.35 5988953.4876 3554128.3267 0.35 0.0 0.0 0.0
low null 84.3 6.84 5988954.1463 3554128.3001 0.25 0.0 0.0 0.0
null null 85.59 6.06 5988954.3398 3554128.2923 0.25 0.0 0.0 0.0
null null 86.39 5.68 5988954.4598 3554128.2874 0.25 0.0 0.0 0.0
null null 87.19 5.27 5988954.5799 3554128.2826 0.12 0.0 0.0 0.0
null null 87.99 5.04 5988954.6999 3554128.2777 0.12 0.0 0.0 0.0
null null 88.79 4.83 5988954.8199 3554128.2729 0.12 0.0 0.0 0.0
null null 89.99 4.63 5988955.0 3554128.2656 0.12 0.0 0.0 0.0
null null 92.39 4.61 5988955.3601 3554128.2511 0.12 0.0 0.0 0.0
null null 95.59 4.75 5988955.8402 3554128.2317 0.12 0.0 0.0 0.0
null null 96.59 4.84 5988955.9902 3554128.2256 0.12 0.0 0.0 0.0
null null 97.29 5.69 5988956.0953 3554128.2214 0.25 0.0 0.0 0.0
null null 98.59 6.1 5988956.2903 3554128.2135 0.25 0.0 0.0 0.0
null null 99.39 6.26 5988956.4104 3554128.2087 0.25 0.0 0.0 0.0
low null 99.79 6.33 5988956.4704 3554128.2062 0.35 0.0 0.0 0.0
null null 104.59 6.4 5988957.1906 3554128.1771 0.35 0.0 0.0 0.0
null null 106.18 6.46 5988957.4291 3554128.1675 0.35 0.0 0.0 0.0
null null 149.35 6.47 5988963.9063 3554127.9059 0.35 0.0 0.0 0.0
null null 168.51 6.89 5988966.781 3554127.7898 0.35 0.0 0.0 0.0
null null 179.68 6.97 5988968.457 3554127.7221 0.35 0.0 0.0 0.0
null true 180.68 6.96 5988968.607 3554127.716 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554128.811 5988941.498 7.42,3554128.656 5988945.336 6.77,3554128.6366 5988945.8161 6.66,3554128.6269 5988946.0562 6.63,3554128.5106 5988948.9339 6.75,3554128.4719 5988949.8942 6.77,3554128.3363 5988953.249 6.42,3554128.3267 5988953.4876 6.35,3554128.3001 5988954.1463 6.84,3554128.2923 5988954.3398 6.06,3554128.2874 5988954.4598 5.68,3554128.2826 5988954.5799 5.27,3554128.2777 5988954.6999 5.04,3554128.2729 5988954.8199 4.83,3554128.2656 5988955.0 4.63,3554128.2511 5988955.3601 4.61,3554128.2317 5988955.8402 4.75,3554128.2256 5988955.9902 4.84,3554128.2214 5988956.0953 5.69,3554128.2135 5988956.2903 6.1,3554128.2087 5988956.4104 6.26,3554128.2062 5988956.4704 6.33,3554128.1771 5988957.1906 6.4,3554128.1675 5988957.4291 6.46,3554127.9059 5988963.9063 6.47,3554127.7898 5988966.781 6.89,3554127.7221 5988968.457 6.97,3554127.716 5988968.607 6.96</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951646015">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951646063">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.25 5988965.197 3554228.739 0.35 3.0 3.0 0.15
null null 44.78 6.68 5988971.4069 3554226.2739 0.35 3.0 3.0 0.15
null null 51.18 6.67 5988972.2944 3554225.9216 0.35 3.0 3.0 0.15
null null 63.98 6.61 5988974.0694 3554225.217 0.35 3.0 3.0 0.15
null null 73.58 6.52 5988975.4007 3554224.6885 0.35 0.0 0.0 0.0
null null 86.39 6.59 5988977.1771 3554223.9834 0.35 0.0 0.0 0.0
null null 87.99 6.61 5988977.399 3554223.8953 0.35 30.0 30.0 0.05
low null 93.28 6.84 5988978.1326 3554223.6041 0.25 0.0 0.0 0.0
null null 96.77 5.78 5988978.6165 3554223.412 0.25 0.0 0.0 0.0
null null 97.36 5.36 5988978.6984 3554223.3795 0.12 0.0 0.0 0.0
null null 97.46 5.31 5988978.7122 3554223.374 0.12 0.0 0.0 0.0
null null 99.86 4.79 5988979.0451 3554223.2419 0.12 0.0 0.0 0.0
null null 100.66 4.71 5988979.156 3554223.1978 0.12 0.0 0.0 0.0
null null 102.26 4.67 5988979.3779 3554223.1098 0.12 0.0 0.0 0.0
null null 106.25 4.72 5988979.9312 3554222.8901 0.12 0.0 0.0 0.0
null null 107.15 5.63 5988980.056 3554222.8406 0.25 0.0 0.0 0.0
null null 108.55 6.17 5988980.2501 3554222.7635 0.25 0.0 0.0 0.0
low null 109.95 6.49 5988980.4443 3554222.6864 0.35 0.0 0.0 0.0
null null 116.35 6.4 5988981.3318 3554222.3341 0.35 0.0 0.0 0.0
null null 129.14 6.37 5988983.1054 3554221.6301 0.35 0.0 0.0 0.0
null null 141.93 6.4 5988984.8791 3554220.926 0.35 0.0 0.0 0.0
null null 154.73 6.46 5988986.6541 3554220.2214 0.35 0.0 0.0 0.0
null null 167.53 6.55 5988988.4292 3554219.5168 0.35 0.0 0.0 0.0
null true 194.52 6.71 5988992.172 3554218.031 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>61.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554228.739 5988965.197 7.25,3554226.2739 5988971.4069 6.68,3554225.9216 5988972.2944 6.67,3554225.217 5988974.0694 6.61,3554224.6885 5988975.4007 6.52,3554223.9834 5988977.1771 6.59,3554223.8953 5988977.399 6.61,3554223.6041 5988978.1326 6.84,3554223.412 5988978.6165 5.78,3554223.3795 5988978.6984 5.36,3554223.374 5988978.7122 5.31,3554223.2419 5988979.0451 4.79,3554223.1978 5988979.156 4.71,3554223.1098 5988979.3779 4.67,3554222.8901 5988979.9312 4.72,3554222.8406 5988980.056 5.63,3554222.7635 5988980.2501 6.17,3554222.6864 5988980.4443 6.49,3554222.3341 5988981.3318 6.4,3554221.6301 5988983.1054 6.37,3554220.926 5988984.8791 6.4,3554220.2214 5988986.6541 6.46,3554219.5168 5988988.4292 6.55,3554218.031 5988992.172 6.71</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951660121">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595166015">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.51 5989001.386 3554315.683 0.35 0.0 0.0 0.0
null null 22.39 6.37 5989004.9333 3554315.4655 0.35 0.0 0.0 0.0
null null 35.19 6.45 5989006.9612 3554315.3412 0.35 0.0 0.0 0.0
null null 47.98 6.49 5989008.9875 3554315.217 0.35 0.0 0.0 0.0
null null 65.57 6.49 5989011.7743 3554315.0461 0.35 0.0 0.0 0.0
null null 73.57 6.34 5989013.0417 3554314.9684 0.35 0.0 0.0 0.0
null null 83.17 6.3 5989014.5627 3554314.8751 0.35 30.0 30.0 0.1
low null 89.07 6.38 5989015.4974 3554314.8178 0.25 0.0 0.0 0.0
null null 90.27 6.01 5989015.6875 3554314.8062 0.15 0.0 0.0 0.0
null null 91.17 5.81 5989015.8301 3554314.7974 0.15 0.0 0.0 0.0
null null 91.77 5.38 5989015.9252 3554314.7916 0.15 0.0 0.0 0.0
null null 93.97 4.75 5989016.2737 3554314.7702 0.15 0.0 0.0 0.0
null null 95.58 4.68 5989016.5288 3554314.7546 0.15 0.0 0.0 0.0
null null 98.78 4.61 5989017.0358 3554314.7235 0.15 0.0 0.0 0.0
null null 99.58 4.62 5989017.1625 3554314.7158 0.15 0.0 0.0 0.0
null null 100.69 4.78 5989017.3384 3554314.705 0.15 0.0 0.0 0.0
null null 100.79 4.86 5989017.3542 3554314.704 0.15 0.0 0.0 0.0
null null 101.59 5.83 5989017.4809 3554314.6962 0.25 0.0 0.0 0.0
null null 103.19 6.28 5989017.7344 3554314.6807 0.35 0.0 0.0 0.0
low null 104.19 6.54 5989017.8929 3554314.671 0.35 0.0 0.0 0.0
null null 108.98 6.71 5989018.6517 3554314.6244 0.35 0.0 0.0 0.0
null null 110.58 6.81 5989018.9052 3554314.6089 0.35 0.0 0.0 0.0
null null 111.38 6.83 5989019.032 3554314.6011 0.35 0.0 0.0 0.0
null null 117.77 6.87 5989020.0443 3554314.5391 0.35 0.0 0.0 0.0
null null 130.55 7.02 5989022.0691 3554314.4149 0.35 0.0 0.0 0.0
null null 156.15 7.53 5989026.1249 3554314.1663 0.35 0.0 0.0 0.0
null null 165.75 7.56 5989027.6458 3554314.073 0.35 0.0 0.0 0.0
null true 172.65 7.52 5989028.739 3554314.006 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>62.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554315.683 5989001.386 6.51,3554315.4655 5989004.9333 6.37,3554315.3412 5989006.9612 6.45,3554315.217 5989008.9875 6.49,3554315.0461 5989011.7743 6.49,3554314.9684 5989013.0417 6.34,3554314.8751 5989014.5627 6.3,3554314.8178 5989015.4974 6.38,3554314.8062 5989015.6875 6.01,3554314.7974 5989015.8301 5.81,3554314.7916 5989015.9252 5.38,3554314.7702 5989016.2737 4.75,3554314.7546 5989016.5288 4.68,3554314.7235 5989017.0358 4.61,3554314.7158 5989017.1625 4.62,3554314.705 5989017.3384 4.78,3554314.704 5989017.3542 4.86,3554314.6962 5989017.4809 5.83,3554314.6807 5989017.7344 6.28,3554314.671 5989017.8929 6.54,3554314.6244 5989018.6517 6.71,3554314.6089 5989018.9052 6.81,3554314.6011 5989019.032 6.83,3554314.5391 5989020.0443 6.87,3554314.4149 5989022.0691 7.02,3554314.1663 5989026.1249 7.53,3554314.073 5989027.6458 7.56,3554314.006 5989028.739 7.52</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951671062">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951671018">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.67 5989004.211 3554413.178 0.35 3.0 3.0 0.15
null null 6.4 6.64 5989005.204 3554413.3178 0.35 3.0 3.0 0.15
null null 31.99 6.63 5989009.1743 3554413.8766 0.35 3.0 3.0 0.15
null null 38.39 6.65 5989010.1673 3554414.0163 0.35 3.0 3.0 0.15
null null 44.78 6.72 5989011.1587 3554414.1559 0.35 3.0 3.0 0.15
null null 57.58 6.78 5989013.1447 3554414.4354 0.35 0.0 0.0 0.0
null null 63.98 6.72 5989014.1377 3554414.5751 0.35 0.0 0.0 0.0
null null 70.37 6.69 5989015.1291 3554414.7147 0.35 0.0 0.0 0.0
null null 73.57 6.66 5989015.6256 3554414.7846 0.35 0.0 0.0 0.0
null null 79.97 6.53 5989016.6186 3554414.9243 0.35 0.0 0.0 0.0
null null 90.37 6.8 5989018.2322 3554415.1514 0.35 0.0 0.0 0.0
null null 93.56 7.07 5989018.7271 3554415.2211 0.35 15.0 15.0 0.1
low null 95.86 7.21 5989019.0839 3554415.2713 0.25 0.0 0.0 0.0
null null 96.17 6.9 5989019.132 3554415.2781 0.25 0.0 0.0 0.0
null null 97.86 6.27 5989019.3942 3554415.315 0.25 0.0 0.0 0.0
null null 99.46 5.63 5989019.6425 3554415.3499 0.12 0.0 0.0 0.0
null null 100.06 5.33 5989019.7356 3554415.363 0.12 0.0 0.0 0.0
null null 102.26 4.81 5989020.0769 3554415.4111 0.12 0.0 0.0 0.0
null null 107.86 4.69 5989020.9458 3554415.5334 0.12 0.0 0.0 0.0
null null 108.96 4.87 5989021.1164 3554415.5574 0.12 0.0 0.0 0.0
null null 109.46 5.82 5989021.194 3554415.5683 0.25 0.0 0.0 0.0
low null 112.96 6.7 5989021.7371 3554415.6447 0.35 0.0 0.0 0.0
null null 118.56 6.92 5989022.6059 3554415.767 0.35 0.0 0.0 0.0
null null 119.36 6.97 5989022.73 3554415.7845 0.35 0.0 0.0 0.0
null null 122.56 7.01 5989023.2265 3554415.8544 0.35 0.0 0.0 0.0
null null 148.17 6.73 5989027.2 3554416.4136 0.35 3.0 3.0 0.15
null null 156.17 6.62 5989028.4412 3554416.5883 0.35 3.0 3.0 0.15
null null 175.37 6.94 5989031.4201 3554417.0076 0.35 3.0 3.0 0.15
null true 187.57 7.22 5989033.313 3554417.274 0.35 3.0 3.0 0.15
]]></om:result>
       <prof:station>62.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554413.178 5989004.211 6.67,3554413.3178 5989005.204 6.64,3554413.8766 5989009.1743 6.63,3554414.0163 5989010.1673 6.65,3554414.1559 5989011.1587 6.72,3554414.4354 5989013.1447 6.78,3554414.5751 5989014.1377 6.72,3554414.7147 5989015.1291 6.69,3554414.7846 5989015.6256 6.66,3554414.9243 5989016.6186 6.53,3554415.1514 5989018.2322 6.8,3554415.2211 5989018.7271 7.07,3554415.2713 5989019.0839 7.21,3554415.2781 5989019.132 6.9,3554415.315 5989019.3942 6.27,3554415.3499 5989019.6425 5.63,3554415.363 5989019.7356 5.33,3554415.4111 5989020.0769 4.81,3554415.5334 5989020.9458 4.69,3554415.5574 5989021.1164 4.87,3554415.5683 5989021.194 5.82,3554415.6447 5989021.7371 6.7,3554415.767 5989022.6059 6.92,3554415.7845 5989022.73 6.97,3554415.8544 5989023.2265 7.01,3554416.4136 5989027.2 6.73,3554416.5883 5989028.4412 6.62,3554417.0076 5989031.4201 6.94,3554417.274 5989033.313 7.22</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951683578">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951683538">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 6.71 5988991.84 3554506.19 0.45 0.0 0.0 0.0
null null 0.8 6.69 5988991.9551 3554506.185 0.35 0.0 0.0 0.0
null null 7.2 6.36 5988992.8755 3554506.1448 0.35 0.0 0.0 0.0
null null 8.0 6.33 5988992.9905 3554506.1398 0.35 0.0 0.0 0.0
null null 11.2 6.37 5988993.4507 3554506.1197 0.35 0.0 0.0 0.0
null null 27.8 6.43 5988995.8381 3554506.0156 0.35 0.0 0.0 0.0
null null 30.0 6.74 5988996.1545 3554506.0018 0.35 0.0 0.0 0.0
null null 33.2 6.63 5988996.6147 3554505.9817 0.35 0.0 0.0 0.0
null null 43.0 7.53 5988998.0241 3554505.9203 0.35 0.0 0.0 0.0
null null 54.2 7.1 5988999.6348 3554505.85 0.35 0.0 0.0 0.0
null null 59.0 6.78 5989000.3251 3554505.8199 0.35 0.0 0.0 0.0
null null 82.97 6.36 5989003.7724 3554505.6696 0.35 0.0 0.0 0.0
low null 89.37 6.84 5989004.6928 3554505.6294 0.25 0.0 0.0 0.0
null null 89.77 6.43 5989004.7503 3554505.6269 0.25 0.0 0.0 0.0
null null 90.67 6.23 5989004.8798 3554505.6213 0.25 0.0 0.0 0.0
null null 91.86 5.93 5989005.0509 3554505.6138 0.25 0.0 0.0 0.0
null null 92.16 5.46 5989005.094 3554505.6119 0.25 0.0 0.0 0.0
null null 92.26 5.35 5989005.1084 3554505.6113 0.15 0.0 0.0 0.0
null null 93.86 5.11 5989005.3385 3554505.6012 0.15 0.0 0.0 0.0
null null 95.46 4.94 5989005.5686 3554505.5912 0.15 0.0 0.0 0.0
null null 96.25 4.87 5989005.6823 3554505.5863 0.15 0.0 0.0 0.0
null null 97.85 4.86 5989005.9124 3554505.5762 0.15 0.0 0.0 0.0
null null 100.25 5.07 5989006.2575 3554505.5612 0.15 0.0 0.0 0.0
null null 101.44 5.31 5989006.4287 3554505.5537 0.15 0.0 0.0 0.0
null null 102.24 5.87 5989006.5437 3554505.5487 0.15 0.0 0.0 0.0
null null 102.44 5.99 5989006.5725 3554505.5474 0.25 0.0 0.0 0.0
low null 104.64 6.48 5989006.8889 3554505.5336 0.35 0.0 0.0 0.0
null null 107.83 6.46 5989007.3476 3554505.5136 0.35 0.0 0.0 0.0
null null 120.61 6.34 5989009.1856 3554505.4335 0.35 0.0 0.0 0.0
null null 127.0 6.34 5989010.1046 3554505.3934 0.35 0.0 0.0 0.0
null null 133.38 6.36 5989011.0221 3554505.3533 0.35 0.0 0.0 0.0
null null 150.96 6.36 5989013.5504 3554505.2431 0.35 3.0 3.0 0.15
null null 176.54 6.87 5989017.2292 3554505.0826 0.35 3.0 3.0 0.15
null true 203.9 7.3 5989021.164 3554504.911 0.35 3.0 3.0 0.15
]]></om:result>
       <prof:station>62.2000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554506.19 5988991.84 6.71,3554506.185 5988991.9551 6.69,3554506.1448 5988992.8755 6.36,3554506.1398 5988992.9905 6.33,3554506.1197 5988993.4507 6.37,3554506.0156 5988995.8381 6.43,3554506.0018 5988996.1545 6.74,3554505.9817 5988996.6147 6.63,3554505.9203 5988998.0241 7.53,3554505.85 5988999.6348 7.1,3554505.8199 5989000.3251 6.78,3554505.6696 5989003.7724 6.36,3554505.6294 5989004.6928 6.84,3554505.6269 5989004.7503 6.43,3554505.6213 5989004.8798 6.23,3554505.6138 5989005.0509 5.93,3554505.6119 5989005.094 5.46,3554505.6113 5989005.1084 5.35,3554505.6012 5989005.3385 5.11,3554505.5912 5989005.5686 4.94,3554505.5863 5989005.6823 4.87,3554505.5762 5989005.9124 4.86,3554505.5612 5989006.2575 5.07,3554505.5537 5989006.4287 5.31,3554505.5487 5989006.5437 5.87,3554505.5474 5989006.5725 5.99,3554505.5336 5989006.8889 6.48,3554505.5136 5989007.3476 6.46,3554505.4335 5989009.1856 6.34,3554505.3934 5989010.1046 6.34,3554505.3533 5989011.0221 6.36,3554505.2431 5989013.5504 6.36,3554505.0826 5989017.2292 6.87,3554504.911 5989021.164 7.3</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595168666">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951686666">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.4 5989037.453 3554613.713 0.35 0.0 0.0 0.0
null null 19.18 7.01 5989039.3792 3554611.958 0.35 0.0 0.0 0.0
null null 51.16 6.93 5989042.5907 3554609.0319 0.35 0.0 0.0 0.0
null null 63.95 7.0 5989043.8752 3554607.8616 0.35 0.0 0.0 0.0
null null 76.74 7.04 5989045.1596 3554606.6913 0.35 0.0 0.0 0.0
null null 95.12 7.03 5989047.0054 3554605.0095 0.35 0.0 0.0 0.0
null null 102.32 7.15 5989047.7285 3554604.3507 0.35 15.0 15.0 0.15
low null 103.62 6.41 5989047.859 3554604.2318 0.25 0.0 0.0 0.0
null null 105.02 6.05 5989047.9996 3554604.1037 0.25 0.0 0.0 0.0
null null 105.52 5.53 5989048.0498 3554604.0579 0.12 0.0 0.0 0.0
null null 107.52 4.87 5989048.2507 3554603.8749 0.12 0.0 0.0 0.0
null null 109.11 4.84 5989048.4104 3554603.7294 0.12 0.0 0.0 0.0
null null 109.71 4.79 5989048.4706 3554603.6746 0.12 0.0 0.0 0.0
null null 111.71 4.78 5989048.6715 3554603.4916 0.12 0.0 0.0 0.0
null null 112.51 4.86 5989048.7518 3554603.4184 0.12 0.0 0.0 0.0
null null 113.91 4.88 5989048.8924 3554603.2903 0.12 0.0 0.0 0.0
null null 114.61 5.16 5989048.9627 3554603.2262 0.12 0.0 0.0 0.0
null null 115.01 5.51 5989049.0029 3554603.1896 0.25 0.0 0.0 0.0
null null 115.41 5.97 5989049.0431 3554603.153 0.25 0.0 0.0 0.0
null null 116.81 6.37 5989049.1836 3554603.0249 0.25 0.0 0.0 0.0
low null 118.22 6.57 5989049.3252 3554602.8959 0.35 0.0 0.0 0.0
null null 124.62 6.62 5989049.968 3554602.3103 0.35 0.0 0.0 0.0
null null 135.8 6.51 5989051.0907 3554601.2873 0.35 0.0 0.0 0.0
null null 138.2 6.54 5989051.3317 3554601.0677 0.35 0.0 0.0 0.0
null null 166.95 6.49 5989054.219 3554598.4371 0.35 3.0 3.0 0.15
null null 195.7 7.11 5989057.1062 3554595.8065 0.35 3.0 3.0 0.15
null true 218.82 7.35 5989059.428 3554593.691 0.35 3.0 3.0 0.15
]]></om:result>
       <prof:station>62.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554613.713 5989037.453 7.4,3554611.958 5989039.3792 7.01,3554609.0319 5989042.5907 6.93,3554607.8616 5989043.8752 7.0,3554606.6913 5989045.1596 7.04,3554605.0095 5989047.0054 7.03,3554604.3507 5989047.7285 7.15,3554604.2318 5989047.859 6.41,3554604.1037 5989047.9996 6.05,3554604.0579 5989048.0498 5.53,3554603.8749 5989048.2507 4.87,3554603.7294 5989048.4104 4.84,3554603.6746 5989048.4706 4.79,3554603.4916 5989048.6715 4.78,3554603.4184 5989048.7518 4.86,3554603.2903 5989048.8924 4.88,3554603.2262 5989048.9627 5.16,3554603.1896 5989049.0029 5.51,3554603.153 5989049.0431 5.97,3554603.0249 5989049.1836 6.37,3554602.8959 5989049.3252 6.57,3554602.3103 5989049.968 6.62,3554601.2873 5989051.0907 6.51,3554601.0677 5989051.3317 6.54,3554598.4371 5989054.219 6.49,3554595.8065 5989057.1062 7.11,3554593.691 5989059.428 7.35</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951697684">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951697675">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.27 5989116.46 3554677.045 0.35 0.0 0.0 0.0
null null 8.0 7.29 5989117.1635 3554676.2678 0.35 0.0 0.0 0.0
null null 20.8 7.12 5989118.2892 3554675.0243 0.35 0.0 0.0 0.0
null null 33.59 6.92 5989119.414 3554673.7817 0.35 0.0 0.0 0.0
null null 39.99 6.8 5989119.9769 3554673.16 0.35 0.0 0.0 0.0
null null 49.59 6.8 5989120.8211 3554672.2273 0.35 0.0 0.0 0.0
null null 84.78 6.5 5989123.9158 3554668.8086 0.35 0.0 0.0 0.0
null null 97.56 6.59 5989125.0398 3554667.567 0.35 0.0 0.0 0.0
null null 102.35 6.61 5989125.461 3554667.1017 0.35 0.0 0.0 0.0
null null 104.74 6.53 5989125.6712 3554666.8695 0.35 0.0 0.0 0.0
null null 108.84 6.52 5989126.0318 3554666.4712 0.35 15.0 15.0 0.15
low null 109.04 6.79 5989126.0493 3554666.4517 0.25 0.0 0.0 0.0
null null 109.84 6.36 5989126.1197 3554666.374 0.25 0.0 0.0 0.0
null null 110.74 6.0 5989126.1989 3554666.2866 0.25 0.0 0.0 0.0
null null 111.34 5.17 5989126.2516 3554666.2283 0.12 0.0 0.0 0.0
null null 112.15 4.98 5989126.3229 3554666.1496 0.12 0.0 0.0 0.0
null null 112.55 4.9 5989126.358 3554666.1107 0.12 0.0 0.0 0.0
null null 114.15 4.88 5989126.4987 3554665.9553 0.12 0.0 0.0 0.0
null null 115.55 4.79 5989126.6219 3554665.8193 0.12 0.0 0.0 0.0
null null 117.15 4.83 5989126.7626 3554665.6638 0.12 0.0 0.0 0.0
null null 118.55 4.75 5989126.8857 3554665.5278 0.12 0.0 0.0 0.0
null null 119.66 4.97 5989126.9833 3554665.42 0.12 0.0 0.0 0.0
null null 120.16 5.85 5989127.0273 3554665.3714 0.25 0.0 0.0 0.0
null null 120.56 6.09 5989127.0625 3554665.3326 0.25 0.0 0.0 0.0
low null 122.86 6.83 5989127.2647 3554665.1091 0.35 0.0 0.0 0.0
null null 128.45 7.2 5989127.7563 3554664.566 0.35 0.0 0.0 0.0
null null 150.81 7.11 5989129.7227 3554662.3938 0.35 0.0 0.0 0.0
null null 157.21 7.13 5989130.2856 3554661.772 0.35 0.0 0.0 0.0
null null 182.81 7.13 5989132.5369 3554659.285 0.35 3.0 3.0 0.15
null null 190.01 7.37 5989133.1701 3554658.5855 0.35 3.0 3.0 0.15
null true 200.72 7.36 5989134.112 3554657.545 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>62.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554677.045 5989116.46 7.27,3554676.2678 5989117.1635 7.29,3554675.0243 5989118.2892 7.12,3554673.7817 5989119.414 6.92,3554673.16 5989119.9769 6.8,3554672.2273 5989120.8211 6.8,3554668.8086 5989123.9158 6.5,3554667.567 5989125.0398 6.59,3554667.1017 5989125.461 6.61,3554666.8695 5989125.6712 6.53,3554666.4712 5989126.0318 6.52,3554666.4517 5989126.0493 6.79,3554666.374 5989126.1197 6.36,3554666.2866 5989126.1989 6.0,3554666.2283 5989126.2516 5.17,3554666.1496 5989126.3229 4.98,3554666.1107 5989126.358 4.9,3554665.9553 5989126.4987 4.88,3554665.8193 5989126.6219 4.79,3554665.6638 5989126.7626 4.83,3554665.5278 5989126.8857 4.75,3554665.42 5989126.9833 4.97,3554665.3714 5989127.0273 5.85,3554665.3326 5989127.0625 6.09,3554665.1091 5989127.2647 6.83,3554664.566 5989127.7563 7.2,3554662.3938 5989129.7227 7.11,3554661.772 5989130.2856 7.13,3554659.285 5989132.5369 7.13,3554658.5855 5989133.1701 7.37,3554657.545 5989134.112 7.36</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951708588">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859517085124">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null null 0.0 7.64 5989183.184 3554745.583 0.35 3.0 3.0 0.15
null null 12.8 7.69 5989184.621 3554744.6108 0.35 3.0 3.0 0.15
null true 19.19 7.67 5989185.3384 3554744.1255 0.35 3.0 3.0 0.15
null null 41.59 6.81 5989187.8532 3554742.4242 0.35 0.0 0.0 0.0
null null 60.79 6.8 5989190.0088 3554740.9659 0.35 0.0 0.0 0.0
null null 86.39 6.92 5989192.8828 3554739.0216 0.35 0.0 0.0 0.0
null null 111.17 7.13 5989195.6648 3554737.1395 0.35 0.0 0.0 0.0
null null 111.57 7.16 5989195.7097 3554737.1091 0.35 0.0 0.0 0.0
null null 115.56 7.27 5989196.1577 3554736.8061 0.35 0.0 0.0 0.0
null null 115.76 7.29 5989196.1801 3554736.7909 0.35 0.0 0.0 0.0
null null 115.96 7.45 5989196.2026 3554736.7757 0.35 10.0 10.0 0.15
low null 117.45 7.7 5989196.3699 3554736.6626 0.3 0.0 0.0 0.0
null null 119.05 6.29 5989196.5495 3554736.541 0.3 0.0 0.0 0.0
null null 119.45 5.5 5989196.5944 3554736.5107 0.15 0.0 0.0 0.0
null null 119.55 5.41 5989196.6056 3554736.5031 0.15 0.0 0.0 0.0
null null 120.14 5.29 5989196.6719 3554736.4582 0.15 0.0 0.0 0.0
null null 120.94 5.23 5989196.7617 3554736.3975 0.15 0.0 0.0 0.0
null null 124.11 4.92 5989197.1176 3554736.1567 0.15 0.0 0.0 0.0
null null 126.7 4.76 5989197.4084 3554735.96 0.15 0.0 0.0 0.0
null null 127.99 5.24 5989197.5532 3554735.862 0.3 0.0 0.0 0.0
null null 128.68 6.0 5989197.6306 3554735.8096 0.3 0.0 0.0 0.0
low null 130.67 6.62 5989197.8541 3554735.6585 0.35 0.0 0.0 0.0
null null 137.03 6.86 5989198.5681 3554735.1754 0.35 0.0 0.0 0.0
null null 140.22 6.9 5989198.9262 3554734.9332 0.35 0.0 0.0 0.0
null null 159.4 7.03 5989201.0795 3554733.4764 0.35 0.0 0.0 0.0
null null 172.18 7.02 5989202.5143 3554732.5058 0.35 0.0 0.0 0.0
null null 178.57 7.1 5989203.2317 3554732.0204 0.35 0.0 0.0 0.0
null true 195.06 7.22 5989205.083 3554730.768 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>62.5000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554745.583 5989183.184 7.64,3554744.6108 5989184.621 7.69,3554744.1255 5989185.3384 7.67,3554742.4242 5989187.8532 6.81,3554740.9659 5989190.0088 6.8,3554739.0216 5989192.8828 6.92,3554737.1395 5989195.6648 7.13,3554737.1091 5989195.7097 7.16,3554736.8061 5989196.1577 7.27,3554736.7909 5989196.1801 7.29,3554736.7757 5989196.2026 7.45,3554736.6626 5989196.3699 7.7,3554736.541 5989196.5495 6.29,3554736.5107 5989196.5944 5.5,3554736.5031 5989196.6056 5.41,3554736.4582 5989196.6719 5.29,3554736.3975 5989196.7617 5.23,3554736.1567 5989197.1176 4.92,3554735.96 5989197.4084 4.76,3554735.862 5989197.5532 5.24,3554735.8096 5989197.6306 6.0,3554735.6585 5989197.8541 6.62,3554735.1754 5989198.5681 6.86,3554734.9332 5989198.9262 6.9,3554733.4764 5989201.0795 7.03,3554732.5058 5989202.5143 7.02,3554732.0204 5989203.2317 7.1,3554730.768 5989205.083 7.22</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859517195127">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951719555">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#OBERKANTEWEHR"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.62 5989202.312 3554773.965 0.35 3.0 3.0 0.15 7.62
null null 124.7 6.95 5989212.6513 3554768.0814 0.35 10.0 10.0 0.15 6.95
null null 127.02 6.7 5989212.8437 3554767.9719 0.3 0.0 0.0 0.0 6.7
low null 128.5 5.66 5989212.9664 3554767.9021 0.15 0.0 0.0 0.0 5.66
null null 128.92 5.32 5989213.0012 3554767.8822 0.15 0.0 0.0 0.0 5.66
null null 132.08 5.16 5989213.2632 3554767.7331 0.15 0.0 0.0 0.0 5.66
null null 136.87 5.33 5989213.6604 3554767.5071 0.15 0.0 0.0 0.0 5.66
low null 137.5 5.66 5989213.7126 3554767.4774 0.3 0.0 0.0 0.0 5.66
null null 139.18 6.63 5989213.8519 3554767.3981 0.35 0.0 0.0 0.0 6.63
null null 142.35 6.9 5989214.1147 3554767.2486 0.35 0.0 0.0 0.0 6.9
null true 254.12 7.24 5989223.382 3554761.975 0.35 0.0 0.0 0.0 7.24
]]></om:result>
       <prof:station>62.5930</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554773.965 5989202.312 7.62,3554768.0814 5989212.6513 6.95,3554767.9719 5989212.8437 6.7,3554767.9021 5989212.9664 5.66,3554767.8822 5989213.0012 5.32,3554767.7331 5989213.2632 5.16,3554767.5071 5989213.6604 5.33,3554767.4774 5989213.7126 5.66,3554767.3981 5989213.8519 6.63,3554767.2486 5989214.1147 6.9,3554761.975 5989223.382 7.24</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
       <prof:member>
        <om:Observation gml:id="Observation118285951732040">
         <gml:description>Bauwerk-Observation</gml:description>
         <gml:name>urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR</gml:name>
         <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR"/>
         <om:resultDefinition>
          <swe:RecordDefinition gml:id="RecordDefinition1182859517320111">
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#WEHRART"/>
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#FORMBEIWERT"/>
          </swe:RecordDefinition>
         </om:resultDefinition>
         <om:result><![CDATA[org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_BREITKRONIG 5.0
]]></om:result>
        </om:Observation>
       </prof:member>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951733528">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951733582">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.6 5989225.396 3554834.589 0.35 0.0 0.0 0.0
null null 6.39 7.49 5989226.3352 3554834.3552 0.35 0.0 0.0 0.0
null null 19.18 7.37 5989228.2152 3554833.8871 0.35 0.0 0.0 0.0
null null 27.18 7.25 5989229.3911 3554833.5944 0.35 0.0 0.0 0.0
null null 36.77 7.32 5989230.8007 3554833.2435 0.35 0.0 0.0 0.0
null null 49.56 7.28 5989232.6807 3554832.7754 0.35 0.0 0.0 0.0
null null 62.35 7.17 5989234.5606 3554832.3074 0.35 0.0 0.0 0.0
null null 78.33 7.07 5989236.9095 3554831.7226 0.35 0.0 0.0 0.0
null null 79.93 7.09 5989237.1447 3554831.6641 0.35 0.0 0.0 0.0
null null 85.33 7.46 5989237.9384 3554831.4665 0.35 0.0 0.0 0.0
null null 85.63 7.11 5989237.9825 3554831.4555 0.35 10.0 10.0 0.15
low null 87.43 6.56 5989238.2471 3554831.3896 0.3 0.0 0.0 0.0
null null 88.23 6.1 5989238.3647 3554831.3604 0.3 0.0 0.0 0.0
null null 88.73 5.85 5989238.4382 3554831.3421 0.12 0.0 0.0 0.0
null null 90.53 5.49 5989238.7027 3554831.2762 0.12 0.0 0.0 0.0
null null 92.13 5.5 5989238.9379 3554831.2176 0.12 0.0 0.0 0.0
null null 94.74 5.66 5989239.3216 3554831.1221 0.12 0.0 0.0 0.0
null null 95.74 5.97 5989239.4685 3554831.0855 0.3 0.0 0.0 0.0
null null 96.74 6.54 5989239.6155 3554831.0489 0.3 0.0 0.0 0.0
low null 98.04 7.07 5989239.8066 3554831.0014 0.35 0.0 0.0 0.0
null null 106.82 7.37 5989241.0972 3554830.6801 0.35 0.0 0.0 0.0
null null 108.42 7.37 5989241.3323 3554830.6215 0.35 0.0 0.0 0.0
null null 119.6 7.19 5989242.9756 3554830.2124 0.35 0.0 0.0 0.0
null null 124.39 7.25 5989243.6797 3554830.0371 0.35 0.0 0.0 0.0
null null 143.56 7.13 5989246.4975 3554829.3356 0.35 0.0 0.0 0.0
null null 162.74 7.26 5989249.3167 3554828.6338 0.35 0.0 0.0 0.0
null null 169.14 7.27 5989250.2574 3554828.3996 0.35 0.0 0.0 0.0
null null 188.35 7.23 5989253.081 3554827.6966 0.35 0.0 0.0 0.0
null true 194.16 7.25 5989253.935 3554827.484 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>62.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554834.589 5989225.396 7.6,3554834.3552 5989226.3352 7.49,3554833.8871 5989228.2152 7.37,3554833.5944 5989229.3911 7.25,3554833.2435 5989230.8007 7.32,3554832.7754 5989232.6807 7.28,3554832.3074 5989234.5606 7.17,3554831.7226 5989236.9095 7.07,3554831.6641 5989237.1447 7.09,3554831.4665 5989237.9384 7.46,3554831.4555 5989237.9825 7.11,3554831.3896 5989238.2471 6.56,3554831.3604 5989238.3647 6.1,3554831.3421 5989238.4382 5.85,3554831.2762 5989238.7027 5.49,3554831.2176 5989238.9379 5.5,3554831.1221 5989239.3216 5.66,3554831.0855 5989239.4685 5.97,3554831.0489 5989239.6155 6.54,3554831.0014 5989239.8066 7.07,3554830.6801 5989241.0972 7.37,3554830.6215 5989241.3323 7.37,3554830.2124 5989242.9756 7.19,3554830.0371 5989243.6797 7.25,3554829.3356 5989246.4975 7.13,3554828.6338 5989249.3167 7.26,3554828.3996 5989250.2574 7.27,3554827.6966 5989253.081 7.23,3554827.484 5989253.935 7.25</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951744571">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951746059">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.58 5989241.028 3554925.057 0.35 0.0 0.0 0.0
null null 4.8 7.49 5989241.7454 3554925.0638 0.35 0.0 0.0 0.0
null null 14.39 7.46 5989243.1786 3554925.0774 0.35 0.0 0.0 0.0
null null 46.37 6.96 5989247.9579 3554925.1227 0.35 0.0 0.0 0.0
null null 65.56 6.85 5989250.8258 3554925.1498 0.35 0.0 0.0 0.0
null null 71.16 7.0 5989251.6627 3554925.1578 0.35 8.0 8.0 0.15
null null 71.96 6.87 5989251.7823 3554925.1589 0.25 0.0 0.0 0.0
low null 73.16 6.59 5989251.9616 3554925.1606 0.25 0.0 0.0 0.0
null null 73.26 6.52 5989251.9766 3554925.1607 0.25 0.0 0.0 0.0
null null 73.46 5.93 5989252.0065 3554925.161 0.25 0.0 0.0 0.0
null null 73.56 5.69 5989252.0214 3554925.1612 0.12 0.0 0.0 0.0
null null 75.95 5.13 5989252.3786 3554925.1646 0.12 0.0 0.0 0.0
null null 78.35 4.96 5989252.7373 3554925.168 0.12 0.0 0.0 0.0
null null 78.95 5.08 5989252.8269 3554925.1688 0.12 0.0 0.0 0.0
null null 80.15 5.18 5989253.0063 3554925.1705 0.12 0.0 0.0 0.0
null null 80.85 5.3 5989253.1109 3554925.1715 0.12 0.0 0.0 0.0
null null 81.55 5.74 5989253.2155 3554925.1725 0.12 0.0 0.0 0.0
null null 81.95 6.07 5989253.2753 3554925.173 0.2 0.0 0.0 0.0
null null 82.15 6.44 5989253.3052 3554925.1733 0.2 0.0 0.0 0.0
low null 82.95 6.7 5989253.4247 3554925.1745 0.35 0.0 0.0 0.0
null null 89.35 6.85 5989254.3812 3554925.1835 0.35 0.0 0.0 0.0
null null 92.55 6.91 5989254.8594 3554925.1881 0.35 0.0 0.0 0.0
null null 105.36 6.96 5989256.7738 3554925.2062 0.35 0.0 0.0 0.0
null null 126.16 7.2 5989259.8824 3554925.2357 0.35 0.0 0.0 0.0
null null 138.96 7.09 5989261.7953 3554925.2538 0.35 0.0 0.0 0.0
null null 150.16 6.96 5989263.4691 3554925.2696 0.35 0.0 0.0 0.0
null true 173.01 7.24 5989266.884 3554925.302 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>62.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3554925.057 5989241.028 7.58,3554925.0638 5989241.7454 7.49,3554925.0774 5989243.1786 7.46,3554925.1227 5989247.9579 6.96,3554925.1498 5989250.8258 6.85,3554925.1578 5989251.6627 7.0,3554925.1589 5989251.7823 6.87,3554925.1606 5989251.9616 6.59,3554925.1607 5989251.9766 6.52,3554925.161 5989252.0065 5.93,3554925.1612 5989252.0214 5.69,3554925.1646 5989252.3786 5.13,3554925.168 5989252.7373 4.96,3554925.1688 5989252.8269 5.08,3554925.1705 5989253.0063 5.18,3554925.1715 5989253.1109 5.3,3554925.1725 5989253.2155 5.74,3554925.173 5989253.2753 6.07,3554925.1733 5989253.3052 6.44,3554925.1745 5989253.4247 6.7,3554925.1835 5989254.3812 6.85,3554925.1881 5989254.8594 6.91,3554925.2062 5989256.7738 6.96,3554925.2357 5989259.8824 7.2,3554925.2538 5989261.7953 7.09,3554925.2696 5989263.4691 6.96,3554925.302 5989266.884 7.24</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859517476105">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951747669">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.74 5989233.468 3555023.29 0.35 0.0 0.0 0.0
null null 20.8 7.67 5989236.6287 3555023.5308 0.35 0.0 0.0 0.0
null null 28.8 7.3 5989237.8444 3555023.6235 0.35 0.0 0.0 0.0
null null 35.2 7.18 5989238.8169 3555023.6976 0.35 0.0 0.0 0.0
null null 41.6 7.16 5989239.7895 3555023.7717 0.35 0.0 0.0 0.0
null null 76.8 6.86 5989245.1384 3555024.1793 0.35 0.0 0.0 0.0
null null 79.2 6.87 5989245.5031 3555024.2071 0.35 0.0 0.0 0.0
null null 82.3 7.18 5989245.9742 3555024.243 0.35 8.0 8.0 0.15
null null 82.5 6.88 5989246.0046 3555024.2453 0.25 0.0 0.0 0.0
low null 84.1 6.68 5989246.2477 3555024.2638 0.25 0.0 0.0 0.0
null null 84.8 5.85 5989246.3541 3555024.2719 0.12 0.0 0.0 0.0
null null 86.0 5.43 5989246.5364 3555024.2858 0.12 0.0 0.0 0.0
null null 87.6 5.44 5989246.7796 3555024.3043 0.12 0.0 0.0 0.0
null null 90.0 5.16 5989247.1443 3555024.3321 0.12 0.0 0.0 0.0
null null 91.9 5.07 5989247.433 3555024.3541 0.12 0.0 0.0 0.0
null null 92.3 5.22 5989247.4938 3555024.3587 0.12 0.0 0.0 0.0
null null 93.5 5.76 5989247.6761 3555024.3726 0.2 0.0 0.0 0.0
low null 94.3 6.65 5989247.7977 3555024.3819 0.35 0.0 0.0 0.0
null null 96.0 7.14 5989248.056 3555024.4016 0.35 0.0 0.0 0.0
null null 102.4 7.25 5989249.0285 3555024.4757 0.35 0.0 0.0 0.0
null null 105.6 7.29 5989249.5148 3555024.5127 0.35 0.0 0.0 0.0
null null 124.8 7.2 5989252.4324 3555024.7351 0.35 0.0 0.0 0.0
null null 142.4 7.06 5989255.1069 3555024.9389 0.35 3.0 3.0 0.15
null null 161.59 7.28 5989258.023 3555025.1611 0.35 3.0 3.0 0.15
null true 186.63 7.39 5989261.828 3555025.451 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>62.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555023.29 5989233.468 7.74,3555023.5308 5989236.6287 7.67,3555023.6235 5989237.8444 7.3,3555023.6976 5989238.8169 7.18,3555023.7717 5989239.7895 7.16,3555024.1793 5989245.1384 6.86,3555024.2071 5989245.5031 6.87,3555024.243 5989245.9742 7.18,3555024.2453 5989246.0046 6.88,3555024.2638 5989246.2477 6.68,3555024.2719 5989246.3541 5.85,3555024.2858 5989246.5364 5.43,3555024.3043 5989246.7796 5.44,3555024.3321 5989247.1443 5.16,3555024.3541 5989247.433 5.07,3555024.3587 5989247.4938 5.22,3555024.3726 5989247.6761 5.76,3555024.3819 5989247.7977 6.65,3555024.4016 5989248.056 7.14,3555024.4757 5989249.0285 7.25,3555024.5127 5989249.5148 7.29,3555024.7351 5989252.4324 7.2,3555024.9389 5989255.1069 7.06,3555025.1611 5989258.023 7.28,3555025.451 5989261.828 7.39</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859517601131">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951760188">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.05 5989237.239 3555126.622 0.35 0.0 0.0 0.0
null null 6.4 8.0 5989238.0878 3555126.4605 0.35 0.0 0.0 0.0
null null 12.79 7.85 5989238.9353 3555126.2992 0.35 0.0 0.0 0.0
null null 33.58 7.26 5989241.6926 3555125.7746 0.35 0.0 0.0 0.0
null null 49.57 7.29 5989243.8133 3555125.371 0.35 0.0 0.0 0.0
null null 68.76 7.49 5989246.3584 3555124.8867 0.35 0.0 0.0 0.0
null null 94.36 7.29 5989249.7537 3555124.2407 0.35 5.0 5.0 0.15
low null 94.76 7.27 5989249.8067 3555124.2306 0.25 0.0 0.0 0.0
null null 97.66 6.71 5989250.1914 3555124.1574 0.25 0.0 0.0 0.0
null null 97.76 6.59 5989250.2046 3555124.1549 0.25 0.0 0.0 0.0
null null 97.86 6.27 5989250.2179 3555124.1524 0.25 0.0 0.0 0.0
null null 97.96 6.13 5989250.2311 3555124.1498 0.25 0.0 0.0 0.0
null null 98.36 5.92 5989250.2842 3555124.1398 0.12 0.0 0.0 0.0
null null 99.16 5.54 5989250.3903 3555124.1196 0.12 0.0 0.0 0.0
null null 100.16 5.1 5989250.5229 3555124.0943 0.12 0.0 0.0 0.0
null null 102.56 5.13 5989250.8412 3555124.0338 0.12 0.0 0.0 0.0
null null 103.36 5.19 5989250.9473 3555124.0136 0.12 0.0 0.0 0.0
null null 105.45 5.15 5989251.2245 3555123.9608 0.12 0.0 0.0 0.0
null null 106.65 5.64 5989251.3837 3555123.9305 0.2 0.0 0.0 0.0
null null 107.25 6.59 5989251.4632 3555123.9154 0.2 0.0 0.0 0.0
null null 108.05 7.02 5989251.5693 3555123.8952 0.2 0.0 0.0 0.0
low null 108.45 7.21 5989251.6224 3555123.8851 0.35 0.0 0.0 0.0
null null 118.05 7.16 5989252.8956 3555123.6428 0.35 0.0 0.0 0.0
null null 127.64 7.03 5989254.1675 3555123.4008 0.35 0.0 0.0 0.0
null null 134.03 7.0 5989255.015 3555123.2396 0.35 0.0 0.0 0.0
null null 166.01 7.23 5989259.2564 3555122.4325 0.35 0.0 0.0 0.0
null null 172.4 7.2 5989260.1039 3555122.2713 0.35 0.0 0.0 0.0
null null 191.59 7.18 5989262.649 3555121.787 0.35 0.0 0.0 0.0
null true 214.73 7.29 5989265.718 3555121.203 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>62.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555126.622 5989237.239 8.05,3555126.4605 5989238.0878 8.0,3555126.2992 5989238.9353 7.85,3555125.7746 5989241.6926 7.26,3555125.371 5989243.8133 7.29,3555124.8867 5989246.3584 7.49,3555124.2407 5989249.7537 7.29,3555124.2306 5989249.8067 7.27,3555124.1574 5989250.1914 6.71,3555124.1549 5989250.2046 6.59,3555124.1524 5989250.2179 6.27,3555124.1498 5989250.2311 6.13,3555124.1398 5989250.2842 5.92,3555124.1196 5989250.3903 5.54,3555124.0943 5989250.5229 5.1,3555124.0338 5989250.8412 5.13,3555124.0136 5989250.9473 5.19,3555123.9608 5989251.2245 5.15,3555123.9305 5989251.3837 5.64,3555123.9154 5989251.4632 6.59,3555123.8952 5989251.5693 7.02,3555123.8851 5989251.6224 7.21,3555123.6428 5989252.8956 7.16,3555123.4008 5989254.1675 7.03,3555123.2396 5989255.015 7.0,3555122.4325 5989259.2564 7.23,3555122.2713 5989260.1039 7.2,3555121.787 5989262.649 7.18,3555121.203 5989265.718 7.29</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951772691">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859517726105">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.1 5989270.184 3555223.484 0.35 0.0 0.0 0.0
null null 15.19 7.9 5989272.2191 3555222.5166 0.35 0.0 0.0 0.0
null null 15.99 7.93 5989272.3263 3555222.4657 0.35 0.0 0.0 0.0
null null 35.17 7.73 5989274.896 3555221.2442 0.35 0.0 0.0 0.0
null null 54.36 7.43 5989277.4671 3555220.0221 0.35 0.0 0.0 0.0
null null 87.65 7.28 5989281.9273 3555217.902 0.35 5.0 5.0 0.15
low null 89.85 6.82 5989282.222 3555217.7618 0.25 0.0 0.0 0.0
null null 89.95 6.69 5989282.2354 3555217.7555 0.25 0.0 0.0 0.0
null null 90.25 6.08 5989282.2756 3555217.7364 0.12 0.0 0.0 0.0
null null 90.55 5.74 5989282.3158 3555217.7173 0.12 0.0 0.0 0.0
null null 92.16 5.43 5989282.5315 3555217.6147 0.12 0.0 0.0 0.0
null null 93.36 5.34 5989282.6923 3555217.5383 0.12 0.0 0.0 0.0
null null 94.96 5.35 5989282.9066 3555217.4364 0.12 0.0 0.0 0.0
null null 96.56 5.32 5989283.121 3555217.3345 0.12 0.0 0.0 0.0
null null 98.66 5.25 5989283.4024 3555217.2008 0.12 0.0 0.0 0.0
null null 99.36 6.14 5989283.4961 3555217.1562 0.2 0.0 0.0 0.0
null null 99.46 6.39 5989283.5095 3555217.1498 0.2 0.0 0.0 0.0
null null 99.56 6.7 5989283.5229 3555217.1435 0.2 0.0 0.0 0.0
low null 101.36 7.49 5989283.7641 3555217.0288 0.35 0.0 0.0 0.0
null null 109.36 7.37 5989284.8359 3555216.5193 0.35 0.0 0.0 0.0
null null 134.93 7.42 5989288.2618 3555214.8909 0.35 0.0 0.0 0.0
null null 147.71 7.38 5989289.974 3555214.077 0.35 0.0 0.0 0.0
null null 160.5 7.39 5989291.6876 3555213.2625 0.35 0.0 0.0 0.0
null null 166.9 7.42 5989292.5451 3555212.8549 0.35 0.0 0.0 0.0
null null 176.49 7.57 5989293.8299 3555212.2441 0.35 0.0 0.0 0.0
null null 195.68 7.55 5989296.401 3555211.022 0.35 0.0 0.0 0.0
null true 208.27 7.46 5989298.0878 3555210.2202 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>63.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555223.484 5989270.184 8.1,3555222.5166 5989272.2191 7.9,3555222.4657 5989272.3263 7.93,3555221.2442 5989274.896 7.73,3555220.0221 5989277.4671 7.43,3555217.902 5989281.9273 7.28,3555217.7618 5989282.222 6.82,3555217.7555 5989282.2354 6.69,3555217.7364 5989282.2756 6.08,3555217.7173 5989282.3158 5.74,3555217.6147 5989282.5315 5.43,3555217.5383 5989282.6923 5.34,3555217.4364 5989282.9066 5.35,3555217.3345 5989283.121 5.32,3555217.2008 5989283.4024 5.25,3555217.1562 5989283.4961 6.14,3555217.1498 5989283.5095 6.39,3555217.1435 5989283.5229 6.7,3555217.0288 5989283.7641 7.49,3555216.5193 5989284.8359 7.37,3555214.8909 5989288.2618 7.42,3555214.077 5989289.974 7.38,3555213.2625 5989291.6876 7.39,3555212.8549 5989292.5451 7.42,3555212.2441 5989293.8299 7.57,3555211.022 5989296.401 7.55,3555210.2202 5989298.0878 7.46</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859517851131">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951785137">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.03 5989328.536 3555308.415 0.35
null null 12.8 7.96 5989330.2413 3555306.9204 0.35
null null 32.01 7.91 5989332.8005 3555304.6773 0.35
null null 33.61 7.87 5989333.0136 3555304.4905 0.35
null null 46.41 7.76 5989334.7189 3555302.9959 0.35
null null 68.82 7.7 5989337.7044 3555300.3792 0.35
null null 71.22 7.58 5989338.0241 3555300.0989 0.35
null null 72.02 7.5 5989338.1307 3555300.0055 0.35
low null 78.02 7.22 5989338.93 3555299.3049 0.25
null null 79.62 6.64 5989339.1432 3555299.1181 0.25
null null 80.02 6.42 5989339.1965 3555299.0714 0.25
null null 80.52 5.92 5989339.2631 3555299.013 0.12
null null 82.72 5.34 5989339.5562 3555298.7561 0.12
null null 85.92 5.47 5989339.9825 3555298.3825 0.12
null null 88.32 5.49 5989340.3022 3555298.1023 0.12
null null 89.22 5.57 5989340.4221 3555297.9972 0.2
null null 90.12 6.52 5989340.542 3555297.8921 0.2
null null 90.52 6.89 5989340.5953 3555297.8454 0.2
low null 91.72 7.17 5989340.7552 3555297.7053 0.35
null null 101.31 7.08 5989342.0328 3555296.5855 0.35
null null 133.31 7.12 5989346.2959 3555292.849 0.35
null null 139.71 7.08 5989347.1485 3555292.1017 0.35
null null 142.91 7.21 5989347.5749 3555291.728 0.35
null null 144.51 7.27 5989347.788 3555291.5412 0.35
null null 162.12 7.44 5989350.1341 3555289.485 0.35
null null 168.52 7.39 5989350.9867 3555288.7377 0.35
null null 171.72 7.38 5989351.413 3555288.364 0.35
null true 189.07 7.46 5989353.7244 3555286.3381 0.35
]]></om:result>
       <prof:station>63.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555308.415 5989328.536 8.03,3555306.9204 5989330.2413 7.96,3555304.6773 5989332.8005 7.91,3555304.4905 5989333.0136 7.87,3555302.9959 5989334.7189 7.76,3555300.3792 5989337.7044 7.7,3555300.0989 5989338.0241 7.58,3555300.0055 5989338.1307 7.5,3555299.3049 5989338.93 7.22,3555299.1181 5989339.1432 6.64,3555299.0714 5989339.1965 6.42,3555299.013 5989339.2631 5.92,3555298.7561 5989339.5562 5.34,3555298.3825 5989339.9825 5.47,3555298.1023 5989340.3022 5.49,3555297.9972 5989340.4221 5.57,3555297.8921 5989340.542 6.52,3555297.8454 5989340.5953 6.89,3555297.7053 5989340.7552 7.17,3555296.5855 5989342.0328 7.08,3555292.849 5989346.2959 7.12,3555292.1017 5989347.1485 7.08,3555291.728 5989347.5749 7.21,3555291.5412 5989347.788 7.27,3555289.485 5989350.1341 7.44,3555288.7377 5989350.9867 7.39,3555288.364 5989351.413 7.38,3555286.3381 5989353.7244 7.46</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859517866109">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951786671">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.07 5989410.268 3555372.669 0.35 0.0 0.0 0.0
null null 14.4 7.94 5989411.6191 3555370.6626 0.35 0.0 0.0 0.0
null null 40.01 7.32 5989414.022 3555367.0944 0.35 0.0 0.0 0.0
null null 72.01 7.35 5989417.0245 3555362.6358 0.35 0.0 0.0 0.0
null null 87.98 7.27 5989418.5229 3555360.4106 0.35 0.0 0.0 0.0
null null 109.52 7.39 5989420.5439 3555357.4095 0.35 0.0 0.0 0.0
null null 114.71 7.31 5989421.0309 3555356.6863 0.35 0.0 0.0 0.0
low null 117.01 7.48 5989421.2467 3555356.3659 0.35 0.0 0.0 0.0
null null 117.41 7.34 5989421.2842 3555356.3101 0.2 0.0 0.0 0.0
null null 118.31 7.01 5989421.3686 3555356.1847 0.2 0.0 0.0 0.0
null null 119.31 6.32 5989421.4625 3555356.0454 0.12 0.0 0.0 0.0
null null 119.71 5.95 5989421.5 3555355.9897 0.12 0.0 0.0 0.0
null null 121.91 5.37 5989421.7064 3555355.6831 0.12 0.0 0.0 0.0
null null 124.31 5.43 5989421.9316 3555355.3487 0.12 0.0 0.0 0.0
null null 127.31 5.4 5989422.2131 3555354.9308 0.12 0.0 0.0 0.0
null null 128.41 5.62 5989422.3163 3555354.7775 0.25 0.0 0.0 0.0
null null 129.21 6.67 5989422.3913 3555354.666 0.25 0.0 0.0 0.0
low null 131.31 7.42 5989422.5884 3555354.3734 0.35 0.0 0.0 0.0
null null 137.71 7.68 5989423.1889 3555353.4817 0.35 0.0 0.0 0.0
null null 140.11 7.75 5989423.4141 3555353.1473 0.35 0.0 0.0 0.0
null null 165.7 7.42 5989425.8151 3555349.5818 0.35 3.0 3.0 0.15
null null 178.51 7.45 5989427.017 3555347.797 0.35 3.0 3.0 0.15
null true 212.24 7.57 5989430.1818 3555343.0974 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>63.2000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555372.669 5989410.268 8.07,3555370.6626 5989411.6191 7.94,3555367.0944 5989414.022 7.32,3555362.6358 5989417.0245 7.35,3555360.4106 5989418.5229 7.27,3555357.4095 5989420.5439 7.39,3555356.6863 5989421.0309 7.31,3555356.3659 5989421.2467 7.48,3555356.3101 5989421.2842 7.34,3555356.1847 5989421.3686 7.01,3555356.0454 5989421.4625 6.32,3555355.9897 5989421.5 5.95,3555355.6831 5989421.7064 5.37,3555355.3487 5989421.9316 5.43,3555354.9308 5989422.2131 5.4,3555354.7775 5989422.3163 5.62,3555354.666 5989422.3913 6.67,3555354.3734 5989422.5884 7.42,3555353.4817 5989423.1889 7.68,3555353.1473 5989423.4141 7.75,3555349.5818 5989425.8151 7.42,3555347.797 5989427.017 7.45,3555343.0974 5989430.1818 7.57</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951797668">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951797644">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.35 5989495.5 3555424.493 0.35 0.0 0.0 0.0
null null 19.2 7.18 5989498.3245 3555420.1253 0.35 0.0 0.0 0.0
null null 44.79 7.32 5989502.089 3555414.304 0.35 0.0 0.0 0.0
null null 75.99 7.55 5989506.6789 3555407.2066 0.35 0.0 0.0 0.0
null null 82.89 7.28 5989507.6939 3555405.6369 0.35 0.0 0.0 0.0
null null 83.59 7.69 5989507.7969 3555405.4777 0.35 0.0 0.0 0.0
low null 83.69 7.67 5989507.8116 3555405.4549 0.2 0.0 0.0 0.0
null null 84.28 7.01 5989507.8984 3555405.3207 0.2 0.0 0.0 0.0
null null 85.08 6.41 5989508.0161 3555405.1387 0.2 0.0 0.0 0.0
null null 85.38 6.03 5989508.0602 3555405.0705 0.12 0.0 0.0 0.0
null null 87.58 5.47 5989508.3839 3555404.57 0.12 0.0 0.0 0.0
null null 89.18 5.6 5989508.6193 3555404.2061 0.12 0.0 0.0 0.0
null null 89.98 5.54 5989508.7369 3555404.0241 0.12 0.0 0.0 0.0
null null 93.67 5.37 5989509.2798 3555403.1847 0.12 0.0 0.0 0.0
null null 94.07 5.99 5989509.3386 3555403.0937 0.25 0.0 0.0 0.0
null null 94.17 6.14 5989509.3533 3555403.0709 0.25 0.0 0.0 0.0
null null 94.67 6.74 5989509.4269 3555402.9572 0.25 0.0 0.0 0.0
low null 97.06 7.59 5989509.7785 3555402.4135 0.35 20.0 20.0 0.15
null null 105.01 7.72 5989510.948 3555400.605 0.35 0.0 0.0 0.0
null true 152.71 7.7 5989517.9651 3555389.7541 0.35 3.0 3.0 0.15
]]></om:result>
       <prof:station>63.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555424.493 5989495.5 7.35,3555420.1253 5989498.3245 7.18,3555414.304 5989502.089 7.32,3555407.2066 5989506.6789 7.55,3555405.6369 5989507.6939 7.28,3555405.4777 5989507.7969 7.69,3555405.4549 5989507.8116 7.67,3555405.3207 5989507.8984 7.01,3555405.1387 5989508.0161 6.41,3555405.0705 5989508.0602 6.03,3555404.57 5989508.3839 5.47,3555404.2061 5989508.6193 5.6,3555404.0241 5989508.7369 5.54,3555403.1847 5989509.2798 5.37,3555403.0937 5989509.3386 5.99,3555403.0709 5989509.3533 6.14,3555402.9572 5989509.4269 6.74,3555402.4135 5989509.7785 7.59,3555400.605 5989510.948 7.72,3555389.7541 5989517.9651 7.7</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859518085108">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951808585">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.76 5989575.898 3555480.295 0.35 0.0 0.0 0.0
null null 6.4 7.78 5989576.5345 3555479.526 0.35 0.0 0.0 0.0
null null 19.2 7.86 5989577.8075 3555477.9879 0.35 0.0 0.0 0.0
null null 25.6 7.72 5989578.444 3555477.2188 0.35 0.0 0.0 0.0
null null 49.59 7.3 5989580.8299 3555474.3361 0.35 0.0 0.0 0.0
null null 77.59 7.52 5989583.6146 3555470.9715 0.35 0.0 0.0 0.0
null null 79.19 7.44 5989583.7737 3555470.7792 0.35 0.0 0.0 0.0
null null 80.39 7.35 5989583.8931 3555470.635 0.35 0.0 0.0 0.0
low null 86.78 7.38 5989584.5286 3555469.8672 0.2 0.0 0.0 0.0
null null 88.28 6.66 5989584.6778 3555469.6869 0.2 0.0 0.0 0.0
null null 89.08 6.16 5989584.7573 3555469.5908 0.12 0.0 0.0 0.0
null null 89.48 5.65 5989584.7971 3555469.5427 0.12 0.0 0.0 0.0
null null 91.08 5.48 5989584.9563 3555469.3505 0.12 0.0 0.0 0.0
null null 92.68 5.47 5989585.1154 3555469.1582 0.12 0.0 0.0 0.0
null null 95.88 5.41 5989585.4336 3555468.7737 0.12 0.0 0.0 0.0
null null 97.78 5.71 5989585.6226 3555468.5454 0.25 0.0 0.0 0.0
null null 98.48 6.7 5989585.6922 3555468.4613 0.25 0.0 0.0 0.0
low null 100.87 7.76 5989585.9299 3555468.1741 0.35 20.0 20.0 0.2
null null 107.27 7.79 5989586.5664 3555467.405 0.35 0.0 0.0 0.0
null null 113.66 7.79 5989587.2019 3555466.6372 0.35 0.0 0.0 0.0
null null 132.85 7.63 5989589.1104 3555464.3312 0.35 0.0 0.0 0.0
null null 136.05 7.57 5989589.4287 3555463.9467 0.35 0.0 0.0 0.0
null null 140.85 7.63 5989589.9061 3555463.3699 0.35 0.0 0.0 0.0
null null 179.24 7.66 5989593.7241 3555458.7568 0.35 0.0 0.0 0.0
null null 190.43 7.59 5989594.837 3555457.4122 0.35 3.0 3.0 0.15
null null 216.03 7.99 5989597.383 3555454.336 0.35 3.0 3.0 0.15
null true 222.62 8.01 5989598.0384 3555453.5441 0.35 3.0 3.0 0.15
]]></om:result>
       <prof:station>63.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555480.295 5989575.898 7.76,3555479.526 5989576.5345 7.78,3555477.9879 5989577.8075 7.86,3555477.2188 5989578.444 7.72,3555474.3361 5989580.8299 7.3,3555470.9715 5989583.6146 7.52,3555470.7792 5989583.7737 7.44,3555470.635 5989583.8931 7.35,3555469.8672 5989584.5286 7.38,3555469.6869 5989584.6778 6.66,3555469.5908 5989584.7573 6.16,3555469.5427 5989584.7971 5.65,3555469.3505 5989584.9563 5.48,3555469.1582 5989585.1154 5.47,3555468.7737 5989585.4336 5.41,3555468.5454 5989585.6226 5.71,3555468.4613 5989585.6922 6.7,3555468.1741 5989585.9299 7.76,3555467.405 5989586.5664 7.79,3555466.6372 5989587.2019 7.79,3555464.3312 5989589.1104 7.63,3555463.9467 5989589.4287 7.57,3555463.3699 5989589.9061 7.63,3555458.7568 5989593.7241 7.66,3555457.4122 5989594.837 7.59,3555454.336 5989597.383 7.99,3555453.5441 5989598.0384 8.01</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859518195100">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859518195152">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.8 5989645.842 3555546.236 0.35 0.0 0.0 0.0
null null 3.2 7.8 5989646.3255 3555545.8429 0.35 0.0 0.0 0.0
null null 28.79 7.89 5989650.192 3555542.6993 0.35 0.0 0.0 0.0
null null 35.18 7.9 5989651.1575 3555541.9144 0.35 0.0 0.0 0.0
null null 51.17 7.85 5989653.5735 3555539.9501 0.35 0.0 0.0 0.0
low null 57.27 7.71 5989654.4952 3555539.2007 0.2 0.0 0.0 0.0
null null 58.97 6.98 5989654.752 3555538.9919 0.2 0.0 0.0 0.0
null null 59.57 6.12 5989654.8427 3555538.9182 0.12 0.0 0.0 0.0
null null 59.97 5.86 5989654.9031 3555538.8691 0.12 0.0 0.0 0.0
null null 61.77 5.39 5989655.1751 3555538.6479 0.12 0.0 0.0 0.0
null null 61.97 5.36 5989655.2053 3555538.6234 0.12 0.0 0.0 0.0
null null 63.57 5.36 5989655.4471 3555538.4268 0.12 0.0 0.0 0.0
null null 66.37 5.25 5989655.8701 3555538.0829 0.12 0.0 0.0 0.0
null null 67.67 5.77 5989656.0666 3555537.9232 0.12 0.0 0.0 0.0
null null 68.47 6.82 5989656.1874 3555537.8249 0.25 0.0 0.0 0.0
low null 71.27 7.86 5989656.6105 3555537.4809 0.35 12.0 12.0 0.15
null null 79.26 8.03 5989657.8178 3555536.4994 0.35 0.0 0.0 0.0
null null 95.25 7.86 5989660.2338 3555534.5351 0.35 0.0 0.0 0.0
null null 114.44 7.86 5989663.1333 3555532.1778 0.35 0.0 0.0 0.0
null null 119.23 7.79 5989663.857 3555531.5893 0.35 3.0 3.0 0.15
null null 132.02 7.9 5989665.7895 3555530.0182 0.35 3.0 3.0 0.15
null null 144.81 7.98 5989667.722 3555528.447 0.35 3.0 3.0 0.15
null true 165.39 8.05 5989670.8315 3555525.9189 0.35 3.0 3.0 0.15
]]></om:result>
       <prof:station>63.5000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555546.236 5989645.842 7.8,3555545.8429 5989646.3255 7.8,3555542.6993 5989650.192 7.89,3555541.9144 5989651.1575 7.9,3555539.9501 5989653.5735 7.85,3555539.2007 5989654.4952 7.71,3555538.9919 5989654.752 6.98,3555538.9182 5989654.8427 6.12,3555538.8691 5989654.9031 5.86,3555538.6479 5989655.1751 5.39,3555538.6234 5989655.2053 5.36,3555538.4268 5989655.4471 5.36,3555538.0829 5989655.8701 5.25,3555537.9232 5989656.0666 5.77,3555537.8249 5989656.1874 6.82,3555537.4809 5989656.6105 7.86,3555536.4994 5989657.8178 8.03,3555534.5351 5989660.2338 7.86,3555532.1778 5989663.1333 7.86,3555531.5893 5989663.857 7.79,3555530.0182 5989665.7895 7.9,3555528.447 5989667.722 7.98,3555525.9189 5989670.8315 8.05</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951822612">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859518226150">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.01 5989700.163 3555627.786 0.35 0.0 0.0 0.0
null null 19.19 7.98 5989702.8076 3555626.451 0.35 0.0 0.0 0.0
null null 84.75 7.67 5989711.8424 3555621.8901 0.35 0.0 0.0 0.0
null null 86.35 7.6 5989712.0629 3555621.7787 0.35 0.0 0.0 0.0
null null 87.15 7.52 5989712.1731 3555621.7231 0.35 0.0 0.0 0.0
low null 91.34 7.52 5989712.7505 3555621.4316 0.25 0.0 0.0 0.0
null null 93.53 6.87 5989713.0523 3555621.2792 0.25 0.0 0.0 0.0
null null 94.03 6.32 5989713.1212 3555621.2445 0.25 0.0 0.0 0.0
null null 94.23 5.71 5989713.1488 3555621.2305 0.12 0.0 0.0 0.0
null null 95.82 5.42 5989713.3679 3555621.1199 0.12 0.0 0.0 0.0
null null 96.22 5.38 5989713.423 3555621.0921 0.12 0.0 0.0 0.0
null null 97.81 5.35 5989713.6421 3555620.9815 0.12 0.0 0.0 0.0
null null 101.1 5.42 5989714.0955 3555620.7526 0.12 0.0 0.0 0.0
null null 102.1 5.88 5989714.2334 3555620.683 0.25 0.0 0.0 0.0
null null 102.9 6.87 5989714.3436 3555620.6274 0.25 0.0 0.0 0.0
low null 104.9 7.28 5989714.6192 3555620.4883 0.35 12.0 12.0 0.15
null null 117.69 7.34 5989716.3818 3555619.5985 0.35 0.0 0.0 0.0
null null 130.49 7.43 5989718.1458 3555618.708 0.35 0.0 0.0 0.0
null null 156.1 7.55 5989721.6751 3555616.9263 0.35 3.0 3.0 0.15
null null 172.1 7.91 5989723.88 3555615.8132 0.35 3.0 3.0 0.15
null null 178.5 7.97 5989724.762 3555615.368 0.35 3.0 3.0 0.15
null true 193.7 7.99 5989726.8567 3555614.3106 0.35 3.0 3.0 0.15
]]></om:result>
       <prof:station>63.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555627.786 5989700.163 8.01,3555626.451 5989702.8076 7.98,3555621.8901 5989711.8424 7.67,3555621.7787 5989712.0629 7.6,3555621.7231 5989712.1731 7.52,3555621.4316 5989712.7505 7.52,3555621.2792 5989713.0523 6.87,3555621.2445 5989713.1212 6.32,3555621.2305 5989713.1488 5.71,3555621.1199 5989713.3679 5.42,3555621.0921 5989713.423 5.38,3555620.9815 5989713.6421 5.35,3555620.7526 5989714.0955 5.42,3555620.683 5989714.2334 5.88,3555620.6274 5989714.3436 6.87,3555620.4883 5989714.6192 7.28,3555619.5985 5989716.3818 7.34,3555618.708 5989718.1458 7.43,3555616.9263 5989721.6751 7.55,3555615.8132 5989723.88 7.91,3555615.368 5989724.762 7.97,3555614.3106 5989726.8567 7.99</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951835170">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951835114">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.23 5989735.319 3555717.883 0.35
null null 6.39 8.24 5989736.2641 3555717.6034 0.35
null null 44.74 7.7 5989741.9364 3555715.9252 0.35
null null 72.72 7.48 5989746.0748 3555714.7008 0.35
null null 91.91 8.02 5989748.9132 3555713.861 0.35
null null 93.51 8.02 5989749.1498 3555713.791 0.35
null null 94.31 7.97 5989749.2681 3555713.756 0.35
low null 99.9 8.0 5989750.0949 3555713.5114 0.35
null null 100.7 7.65 5989750.2133 3555713.4763 0.25
null null 102.3 7.12 5989750.4499 3555713.4063 0.25
null null 102.69 6.69 5989750.5076 3555713.3893 0.12
null null 103.39 5.85 5989750.6111 3555713.3586 0.12
null null 104.99 5.61 5989750.8478 3555713.2886 0.12
null null 106.59 5.64 5989751.0844 3555713.2186 0.12
null null 108.19 5.7 5989751.3211 3555713.1486 0.12
null null 110.78 5.74 5989751.7042 3555713.0352 0.12
null null 111.98 5.97 5989751.8817 3555712.9827 0.12
null null 112.08 6.02 5989751.8965 3555712.9784 0.2
null null 112.98 7.05 5989752.0296 3555712.939 0.2
low null 114.78 7.78 5989752.2958 3555712.8602 0.35
null null 140.35 7.95 5989756.0778 3555711.7413 0.35
null null 146.74 8.07 5989757.0229 3555711.4616 0.35
null null 159.53 8.37 5989758.9147 3555710.9019 0.35
null null 170.72 8.79 5989760.5697 3555710.4123 0.35
null null 183.5 8.95 5989762.46 3555709.853 0.35
null true 190.22 9.01 5989763.4539 3555709.5589 0.35
]]></om:result>
       <prof:station>63.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555717.883 5989735.319 8.23,3555717.6034 5989736.2641 8.24,3555715.9252 5989741.9364 7.7,3555714.7008 5989746.0748 7.48,3555713.861 5989748.9132 8.02,3555713.791 5989749.1498 8.02,3555713.756 5989749.2681 7.97,3555713.5114 5989750.0949 8.0,3555713.4763 5989750.2133 7.65,3555713.4063 5989750.4499 7.12,3555713.3893 5989750.5076 6.69,3555713.3586 5989750.6111 5.85,3555713.2886 5989750.8478 5.61,3555713.2186 5989751.0844 5.64,3555713.1486 5989751.3211 5.7,3555713.0352 5989751.7042 5.74,3555712.9827 5989751.8817 5.97,3555712.9784 5989751.8965 6.02,3555712.939 5989752.0296 7.05,3555712.8602 5989752.2958 7.78,3555711.7413 5989756.0778 7.95,3555711.4616 5989757.0229 8.07,3555710.9019 5989758.9147 8.37,3555710.4123 5989760.5697 8.79,3555709.853 5989762.46 8.95,3555709.5589 5989763.4539 9.01</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859518460126">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859518460153">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null null 0.0 8.39 5989757.159 3555814.46 0.35 0.0 0.0 0.0
null true 13.6 8.43 5989760.4082 3555813.737 0.35 0.0 0.0 0.0
null null 23.19 8.18 5989762.6994 3555813.2271 0.35 0.0 0.0 0.0
null null 38.39 7.61 5989766.331 3555812.419 0.35 0.0 0.0 0.0
null null 63.98 7.61 5989772.4448 3555811.0586 0.35 0.0 0.0 0.0
null null 84.76 7.78 5989777.4095 3555809.9538 0.35 0.0 0.0 0.0
null null 85.56 7.75 5989777.6006 3555809.9113 0.35 0.0 0.0 0.0
low null 94.36 8.18 5989779.703 3555809.4435 0.25 0.0 0.0 0.0
null null 97.06 7.04 5989780.3481 3555809.2999 0.25 0.0 0.0 0.0
null null 97.56 6.36 5989780.4676 3555809.2733 0.25 0.0 0.0 0.0
null null 97.76 5.84 5989780.5154 3555809.2627 0.12 0.0 0.0 0.0
null null 97.86 5.72 5989780.5392 3555809.2574 0.12 0.0 0.0 0.0
null null 99.46 5.64 5989780.9215 3555809.1723 0.12 0.0 0.0 0.0
null null 102.66 5.57 5989781.686 3555809.0022 0.12 0.0 0.0 0.0
null null 104.86 5.56 5989782.2117 3555808.8852 0.12 0.0 0.0 0.0
null null 106.16 5.98 5989782.5222 3555808.8161 0.2 0.0 0.0 0.0
null null 106.75 7.07 5989782.6632 3555808.7848 0.2 0.0 0.0 0.0
null null 106.86 7.23 5989782.6895 3555808.7789 0.2 0.0 0.0 0.0
low null 107.75 7.84 5989782.9021 3555808.7316 0.35 0.0 0.0 0.0
null null 117.36 7.94 5989785.1981 3555808.2207 0.35 3.0 3.0 0.15
null null 134.96 8.33 5989789.403 3555807.285 0.35 3.0 3.0 0.15
null true 166.73 8.27 5989796.9933 3555805.596 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>63.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555814.46 5989757.159 8.39,3555813.737 5989760.4082 8.43,3555813.2271 5989762.6994 8.18,3555812.419 5989766.331 7.61,3555811.0586 5989772.4448 7.61,3555809.9538 5989777.4095 7.78,3555809.9113 5989777.6006 7.75,3555809.4435 5989779.703 8.18,3555809.2999 5989780.3481 7.04,3555809.2733 5989780.4676 6.36,3555809.2627 5989780.5154 5.84,3555809.2574 5989780.5392 5.72,3555809.1723 5989780.9215 5.64,3555809.0022 5989781.686 5.57,3555808.8852 5989782.2117 5.56,3555808.8161 5989782.5222 5.98,3555808.7848 5989782.6632 7.07,3555808.7789 5989782.6895 7.23,3555808.7316 5989782.9021 7.84,3555808.2207 5989785.1981 7.94,3555807.285 5989789.403 8.33,3555805.596 5989796.9933 8.27</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951847685">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951847681">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.19 5989779.928 3555911.701 0.35 3.0 3.0 0.15
null null 6.4 8.13 5989781.1627 3555911.4712 0.35 3.0 3.0 0.15
null null 19.21 7.91 5989783.634 3555911.0114 0.35 3.0 3.0 0.15
null null 36.81 7.64 5989787.0294 3555910.3796 0.35 3.0 3.0 0.15
null null 40.01 7.7 5989787.6467 3555910.2647 0.35 3.0 3.0 0.15
null null 59.21 7.6 5989791.3508 3555909.5754 0.35 3.0 3.0 0.15
null null 84.8 7.81 5989796.2876 3555908.6568 0.35 3.0 3.0 0.15
null null 85.6 7.84 5989796.4419 3555908.6281 0.35 0.0 0.0 0.0
null null 86.8 7.95 5989796.6734 3555908.585 0.35 0.0 0.0 0.0
null null 88.0 7.88 5989796.905 3555908.5419 0.35 4.0 4.0 0.15
low null 93.8 8.02 5989798.0239 3555908.3337 0.25 0.0 0.0 0.0
null null 96.19 7.18 5989798.485 3555908.2479 0.25 0.0 0.0 0.0
null null 96.3 7.08 5989798.5062 3555908.2439 0.25 0.0 0.0 0.0
null null 96.69 6.5 5989798.5814 3555908.2299 0.25 0.0 0.0 0.0
null null 97.09 6.25 5989798.6586 3555908.2156 0.12 0.0 0.0 0.0
null null 99.29 5.69 5989799.083 3555908.1366 0.12 0.0 0.0 0.0
null null 101.29 5.59 5989799.4689 3555908.0648 0.12 0.0 0.0 0.0
null null 103.88 5.66 5989799.9685 3555907.9718 0.12 0.0 0.0 0.0
null null 104.78 6.04 5989800.1421 3555907.9395 0.2 0.0 0.0 0.0
null null 106.18 7.26 5989800.4122 3555907.8893 0.2 0.0 0.0 0.0
low null 107.78 7.9 5989800.7209 3555907.8318 0.35 0.0 0.0 0.0
null null 114.17 8.0 5989801.9537 3555907.6024 0.35 0.0 0.0 0.0
null null 126.95 8.47 5989804.4192 3555907.1436 0.35 0.0 0.0 0.0
null null 152.54 8.57 5989809.356 3555906.225 0.35 0.0 0.0 0.0
null true 163.85 8.83 5989811.5379 3555905.819 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>63.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3555911.701 5989779.928 8.19,3555911.4712 5989781.1627 8.13,3555911.0114 5989783.634 7.91,3555910.3796 5989787.0294 7.64,3555910.2647 5989787.6467 7.7,3555909.5754 5989791.3508 7.6,3555908.6568 5989796.2876 7.81,3555908.6281 5989796.4419 7.84,3555908.585 5989796.6734 7.95,3555908.5419 5989796.905 7.88,3555908.3337 5989798.0239 8.02,3555908.2479 5989798.485 7.18,3555908.2439 5989798.5062 7.08,3555908.2299 5989798.5814 6.5,3555908.2156 5989798.6586 6.25,3555908.1366 5989799.083 5.69,3555908.0648 5989799.4689 5.59,3555907.9718 5989799.9685 5.66,3555907.9395 5989800.1421 6.04,3555907.8893 5989800.4122 7.26,3555907.8318 5989800.7209 7.9,3555907.6024 5989801.9537 8.0,3555907.1436 5989804.4192 8.47,3555906.225 5989809.356 8.57,3555905.819 5989811.5379 8.83</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859518585112">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951858543">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.98 5989831.802 3556005.354 0.35 0.0 0.0 0.0
null null 44.79 8.25 5989837.7885 3555999.4113 0.35 0.0 0.0 0.0
null null 70.39 8.18 5989841.2102 3555996.0148 0.35 0.0 0.0 0.0
null null 92.13 8.25 5989844.1159 3555993.1304 0.35 0.0 0.0 0.0
null null 92.53 8.34 5989844.1693 3555993.0773 0.35 30.0 30.0 0.15
low null 97.12 8.36 5989844.7828 3555992.4683 0.35 0.0 0.0 0.0
null null 97.22 8.24 5989844.7962 3555992.455 0.25 0.0 0.0 0.0
null null 99.62 7.24 5989845.117 3555992.1366 0.25 0.0 0.0 0.0
null null 100.32 6.14 5989845.2105 3555992.0437 0.12 0.0 0.0 0.0
null null 101.32 5.86 5989845.3442 3555991.911 0.12 0.0 0.0 0.0
null null 102.92 5.64 5989845.558 3555991.6988 0.12 0.0 0.0 0.0
null null 104.11 5.58 5989845.7171 3555991.5409 0.12 0.0 0.0 0.0
null null 105.7 5.62 5989845.9296 3555991.3299 0.12 0.0 0.0 0.0
null null 107.29 5.77 5989846.1421 3555991.119 0.12 0.0 0.0 0.0
null null 107.69 5.89 5989846.1956 3555991.0659 0.2 0.0 0.0 0.0
null null 108.19 6.58 5989846.2624 3555990.9995 0.2 0.0 0.0 0.0
null null 109.78 7.43 5989846.4749 3555990.7886 0.2 0.0 0.0 0.0
low null 110.27 7.72 5989846.5404 3555990.7236 0.35 0.0 0.0 0.0
null null 111.07 7.64 5989846.6473 3555990.6174 0.35 0.0 0.0 0.0
null null 118.26 8.02 5989847.6083 3555989.6635 0.35 0.0 0.0 0.0
null null 143.84 8.36 5989851.0273 3555986.2696 0.35 3.0 3.0 0.15
null null 147.04 8.42 5989851.455 3555985.845 0.35 3.0 3.0 0.15
null true 175.61 9.57 5989855.2736 3555982.0544 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556005.354 5989831.802 7.98,3555999.4113 5989837.7885 8.25,3555996.0148 5989841.2102 8.18,3555993.1304 5989844.1159 8.25,3555993.0773 5989844.1693 8.34,3555992.4683 5989844.7828 8.36,3555992.455 5989844.7962 8.24,3555992.1366 5989845.117 7.24,3555992.0437 5989845.2105 6.14,3555991.911 5989845.3442 5.86,3555991.6988 5989845.558 5.64,3555991.5409 5989845.7171 5.58,3555991.3299 5989845.9296 5.62,3555991.119 5989846.1421 5.77,3555991.0659 5989846.1956 5.89,3555990.9995 5989846.2624 6.58,3555990.7886 5989846.4749 7.43,3555990.7236 5989846.5404 7.72,3555990.6174 5989846.6473 7.64,3555989.6635 5989847.6083 8.02,3555986.2696 5989851.0273 8.36,3555985.845 5989851.455 8.42,3555982.0544 5989855.2736 9.57</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595186957">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859518695124">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 7.99 5989926.551 3556051.288 0.35 0.0 0.0 0.0
null null 19.2 7.87 5989927.0599 3556047.8979 0.35 0.0 0.0 0.0
null null 28.79 7.89 5989927.3141 3556046.2047 0.35 0.0 0.0 0.0
null null 66.39 8.4 5989928.3107 3556039.5658 0.35 0.0 0.0 0.0
null null 69.59 8.34 5989928.3955 3556039.0008 0.35 0.0 0.0 0.0
null null 87.56 8.56 5989928.8718 3556035.8279 0.35 0.0 0.0 0.0
null null 87.96 8.53 5989928.8825 3556035.7573 0.35 0.0 0.0 0.0
null null 91.95 8.65 5989928.9882 3556035.0528 0.35 0.0 0.0 0.0
null null 92.15 8.32 5989928.9935 3556035.0175 0.35 0.0 0.0 0.0
null null 92.25 8.26 5989928.9962 3556034.9998 0.35 30.0 30.0 0.2
low null 94.35 7.57 5989929.0518 3556034.629 0.35 0.0 0.0 0.0
null null 94.55 7.41 5989929.0571 3556034.5937 0.25 0.0 0.0 0.0
null null 95.44 6.85 5989929.0807 3556034.4366 0.25 0.0 0.0 0.0
null null 95.55 6.81 5989929.0836 3556034.4171 0.25 0.0 0.0 0.0
null null 96.24 6.23 5989929.1019 3556034.2953 0.12 0.0 0.0 0.0
null null 97.24 5.92 5989929.1284 3556034.1188 0.12 0.0 0.0 0.0
null null 98.04 5.78 5989929.1496 3556033.9775 0.12 0.0 0.0 0.0
null null 99.64 5.66 5989929.192 3556033.695 0.12 0.0 0.0 0.0
null null 102.03 5.61 5989929.2554 3556033.273 0.12 0.0 0.0 0.0
null null 103.63 5.92 5989929.2978 3556032.9905 0.2 0.0 0.0 0.0
null null 104.92 7.33 5989929.332 3556032.7627 0.2 0.0 0.0 0.0
low null 107.12 8.58 5989929.3903 3556032.3743 0.35 0.0 0.0 0.0
null null 116.7 8.79 5989929.6442 3556030.6828 0.35 0.0 0.0 0.0
null null 139.08 9.54 5989930.2374 3556026.7312 0.35 0.0 0.0 0.0
null null 151.86 10.12 5989930.5762 3556024.4747 0.35 0.0 0.0 0.0
null null 158.24 10.39 5989930.7453 3556023.3482 0.35 0.0 0.0 0.0
null null 159.84 10.48 5989930.7877 3556023.0657 0.35 0.0 0.0 0.0
null true 167.02 10.77 5989930.978 3556021.798 0.35 0.0 0.0 0.0
null null 181.21 10.46 5989931.3541 3556019.2925 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556051.288 5989926.551 7.99,3556047.8979 5989927.0599 7.87,3556046.2047 5989927.3141 7.89,3556039.5658 5989928.3107 8.4,3556039.0008 5989928.3955 8.34,3556035.8279 5989928.8718 8.56,3556035.7573 5989928.8825 8.53,3556035.0528 5989928.9882 8.65,3556035.0175 5989928.9935 8.32,3556034.9998 5989928.9962 8.26,3556034.629 5989929.0518 7.57,3556034.5937 5989929.0571 7.41,3556034.4366 5989929.0807 6.85,3556034.4171 5989929.0836 6.81,3556034.2953 5989929.1019 6.23,3556034.1188 5989929.1284 5.92,3556033.9775 5989929.1496 5.78,3556033.695 5989929.192 5.66,3556033.273 5989929.2554 5.61,3556032.9905 5989929.2978 5.92,3556032.7627 5989929.332 7.33,3556032.3743 5989929.3903 8.58,3556030.6828 5989929.6442 8.79,3556026.7312 5989930.2374 9.54,3556024.4747 5989930.5762 10.12,3556023.3482 5989930.7453 10.39,3556023.0657 5989930.7877 10.48,3556021.798 5989930.978 10.77,3556019.2925 5989931.3541 10.46</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951882095">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859518820143">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#OBERKANTEWEHR"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.37 5989968.112 3556052.377 0.35 8.37
null null 25.59 8.73 5989967.8741 3556049.3849 0.35 8.73
null null 51.18 8.29 5989967.6363 3556046.3928 0.35 8.29
null null 89.56 7.94 5989967.2795 3556041.9052 0.35 7.94
null null 150.29 8.35 5989966.715 3556034.8043 0.35 8.35
null null 157.49 8.01 5989966.6481 3556033.9625 0.35 8.01
null null 158.65 7.59 5989966.6373 3556033.8268 0.3 7.59
null null 159.39 6.86 5989966.6305 3556033.7403 0.3 6.86
low null 160.0 6.75 5989966.6248 3556033.669 0.15 6.75
null null 160.16 6.63 5989966.6233 3556033.6503 0.15 6.75
null null 163.38 6.37 5989966.5934 3556033.2738 0.15 6.75
null null 166.57 6.39 5989966.5637 3556032.9008 0.15 6.75
null null 168.15 6.31 5989966.549 3556032.716 0.15 6.75
null null 168.54 6.38 5989966.5454 3556032.6704 0.15 6.75
low null 169.5 6.75 5989966.5365 3556032.5582 0.2 6.75
null null 170.12 7.14 5989966.5307 3556032.4857 0.2 7.14
null null 172.1 8.24 5989966.5123 3556032.2542 0.35 8.24
null null 210.46 8.17 5989966.1558 3556027.7689 0.35 8.17
null null 223.25 8.46 5989966.0369 3556026.2735 0.35 8.46
null null 236.04 9.09 5989965.918 3556024.778 0.35 9.09
null true 269.73 9.6 5989965.6049 3556020.8388 0.35 9.6
]]></om:result>
       <prof:station>64.1930</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556052.377 5989968.112 8.37,3556049.3849 5989967.8741 8.73,3556046.3928 5989967.6363 8.29,3556041.9052 5989967.2795 7.94,3556034.8043 5989966.715 8.35,3556033.9625 5989966.6481 8.01,3556033.8268 5989966.6373 7.59,3556033.7403 5989966.6305 6.86,3556033.669 5989966.6248 6.75,3556033.6503 5989966.6233 6.63,3556033.2738 5989966.5934 6.37,3556032.9008 5989966.5637 6.39,3556032.716 5989966.549 6.31,3556032.6704 5989966.5454 6.38,3556032.5582 5989966.5365 6.75,3556032.4857 5989966.5307 7.14,3556032.2542 5989966.5123 8.24,3556027.7689 5989966.1558 8.17,3556026.2735 5989966.0369 8.46,3556024.778 5989965.918 9.09,3556020.8388 5989965.6049 9.6</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
       <prof:member>
        <om:Observation gml:id="Observation118285951883597">
         <gml:description>Bauwerk-Observation</gml:description>
         <gml:name>urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR</gml:name>
         <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR"/>
         <om:resultDefinition>
          <swe:RecordDefinition gml:id="RecordDefinition118285951883592">
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#WEHRART"/>
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#FORMBEIWERT"/>
          </swe:RecordDefinition>
         </om:resultDefinition>
         <om:result><![CDATA[org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_BREITKRONIG 5.0
]]></om:result>
        </om:Observation>
       </prof:member>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859518835127">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859518835105">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.73 5990044.601 3556033.962 0.35 0.0 0.0 0.0
null null 44.81 8.91 5990043.9195 3556029.7253 0.35 0.0 0.0 0.0
null null 57.62 8.45 5990043.7247 3556028.5141 0.35 0.0 0.0 0.0
null null 76.82 8.01 5990043.4327 3556026.6987 0.35 0.0 0.0 0.0
null null 128.04 8.32 5990042.6538 3556021.8559 0.35 0.0 0.0 0.0
null null 148.78 8.22 5990042.3384 3556019.895 0.35 30.0 30.0 0.2
low null 150.37 7.96 5990042.3142 3556019.7447 0.3 0.0 0.0 0.0
null null 152.69 7.08 5990042.2789 3556019.5253 0.3 0.0 0.0 0.0
null null 154.28 6.62 5990042.2547 3556019.375 0.15 0.0 0.0 0.0
null null 158.22 6.45 5990042.1948 3556019.0024 0.15 0.0 0.0 0.0
null null 159.76 6.81 5990042.1714 3556018.8568 0.25 0.0 0.0 0.0
low null 161.3 7.83 5990042.148 3556018.7112 0.35 30.0 30.0 0.2
null null 162.1 8.01 5990042.1358 3556018.6356 0.35 0.0 0.0 0.0
null null 168.48 8.16 5990042.0388 3556018.0324 0.35 0.0 0.0 0.0
null null 200.43 8.08 5990041.5529 3556015.0115 0.35 0.0 0.0 0.0
null null 219.63 8.71 5990041.2609 3556013.1962 0.35 0.0 0.0 0.0
null null 270.78 9.13 5990040.483 3556008.36 0.35 0.0 0.0 0.0
null true 278.67 9.07 5990040.363 3556007.614 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.2200</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556033.962 5990044.601 8.73,3556029.7253 5990043.9195 8.91,3556028.5141 5990043.7247 8.45,3556026.6987 5990043.4327 8.01,3556021.8559 5990042.6538 8.32,3556019.895 5990042.3384 8.22,3556019.7447 5990042.3142 7.96,3556019.5253 5990042.2789 7.08,3556019.375 5990042.2547 6.62,3556019.0024 5990042.1948 6.45,3556018.8568 5990042.1714 6.81,3556018.7112 5990042.148 7.83,3556018.6356 5990042.1358 8.01,3556018.0324 5990042.0388 8.16,3556015.0115 5990041.5529 8.08,3556013.1962 5990041.2609 8.71,3556008.36 5990040.483 9.13,3556007.614 5990040.363 9.07</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859518945122">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859518945154">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 9.79 5990123.384 3556023.402 0.35 0.0 0.0 0.0
null null 3.2 9.63 5990123.4224 3556022.9072 0.35 0.0 0.0 0.0
null null 28.81 8.12 5990123.7295 3556018.9472 0.35 0.0 0.0 0.0
null null 30.41 8.04 5990123.7486 3556018.6998 0.35 0.0 0.0 0.0
null null 49.61 8.05 5990123.9789 3556015.731 0.35 0.0 0.0 0.0
null null 77.58 8.41 5990124.3143 3556011.4061 0.35 0.0 0.0 0.0
null null 82.76 8.72 5990124.3764 3556010.6052 0.35 0.0 0.0 0.0
null null 82.86 8.7 5990124.3776 3556010.5897 0.35 5.0 5.0 0.2
low null 84.45 7.89 5990124.3966 3556010.3438 0.3 0.0 0.0 0.0
null null 85.25 7.51 5990124.4062 3556010.2201 0.3 0.0 0.0 0.0
null null 86.04 7.03 5990124.4157 3556010.098 0.3 0.0 0.0 0.0
null null 87.94 5.68 5990124.4385 3556009.8042 0.12 0.0 0.0 0.0
null null 88.93 5.75 5990124.4504 3556009.6511 0.12 0.0 0.0 0.0
null null 91.32 6.22 5990124.479 3556009.2816 0.12 0.0 0.0 0.0
null null 93.11 6.38 5990124.5005 3556009.0048 0.25 0.0 0.0 0.0
null null 94.31 7.16 5990124.5149 3556008.8192 0.25 0.0 0.0 0.0
null null 94.71 7.57 5990124.5197 3556008.7574 0.25 0.0 0.0 0.0
low null 96.0 8.03 5990124.5351 3556008.5579 0.35 12.0 12.0 0.2
null null 106.4 8.44 5990124.6598 3556006.9498 0.35 0.0 0.0 0.0
null null 125.59 8.36 5990124.89 3556003.9825 0.35 0.0 0.0 0.0
null null 128.79 8.33 5990124.9283 3556003.4877 0.35 0.0 0.0 0.0
null null 143.17 8.0 5990125.1008 3556001.2642 0.35 0.0 0.0 0.0
null null 155.95 8.03 5990125.254 3555999.2881 0.35 0.0 0.0 0.0
null null 171.93 7.96 5990125.4456 3555996.8172 0.35 0.0 0.0 0.0
null true 188.61 8.35 5990125.6456 3555994.238 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556023.402 5990123.384 9.79,3556022.9072 5990123.4224 9.63,3556018.9472 5990123.7295 8.12,3556018.6998 5990123.7486 8.04,3556015.731 5990123.9789 8.05,3556011.4061 5990124.3143 8.41,3556010.6052 5990124.3764 8.72,3556010.5897 5990124.3776 8.7,3556010.3438 5990124.3966 7.89,3556010.2201 5990124.4062 7.51,3556010.098 5990124.4157 7.03,3556009.8042 5990124.4385 5.68,3556009.6511 5990124.4504 5.75,3556009.2816 5990124.479 6.22,3556009.0048 5990124.5005 6.38,3556008.8192 5990124.5149 7.16,3556008.7574 5990124.5197 7.57,3556008.5579 5990124.5351 8.03,3556006.9498 5990124.6598 8.44,3556003.9825 5990124.89 8.36,3556003.4877 5990124.9283 8.33,3556001.2642 5990125.1008 8.0,3555999.2881 5990125.254 8.03,3555996.8172 5990125.4456 7.96,3555994.238 5990125.6456 8.35</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951905431">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951907062">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null null 0.0 9.73 5990215.525 3556042.681 0.35 0.0 0.0 0.0
null true 16.0 9.83 5990216.7492 3556040.1465 0.35 0.0 0.0 0.0
null null 35.2 9.1 5990218.2182 3556037.105 0.35 0.0 0.0 0.0
null null 44.8 8.63 5990218.9527 3556035.5843 0.35 0.0 0.0 0.0
null null 60.16 8.42 5990220.128 3556033.1512 0.35 0.0 0.0 0.0
null null 60.76 8.47 5990220.1739 3556033.0561 0.35 0.0 0.0 0.0
null null 61.55 8.44 5990220.2343 3556032.931 0.35 0.0 0.0 0.0
null null 65.23 8.53 5990220.5159 3556032.348 0.35 0.0 0.0 0.0
null null 65.33 8.49 5990220.5235 3556032.3322 0.35 0.0 0.0 0.0
null null 65.63 8.25 5990220.5465 3556032.2847 0.35 5.0 5.0 0.2
low null 66.42 8.01 5990220.6069 3556032.1595 0.3 0.0 0.0 0.0
null null 68.11 7.22 5990220.7362 3556031.8918 0.15 0.0 0.0 0.0
null null 69.71 6.95 5990220.8587 3556031.6384 0.15 0.0 0.0 0.0
null null 70.5 6.84 5990220.9191 3556031.5132 0.15 0.0 0.0 0.0
null null 70.9 6.81 5990220.9497 3556031.4499 0.15 0.0 0.0 0.0
null null 72.5 6.79 5990221.0721 3556031.1964 0.15 0.0 0.0 0.0
null null 75.29 6.81 5990221.2856 3556030.7544 0.15 0.0 0.0 0.0
null null 76.49 7.13 5990221.3774 3556030.5644 0.25 0.0 0.0 0.0
low null 77.48 7.85 5990221.4532 3556030.4075 0.25 10.0 10.0 0.2
null null 78.38 8.11 5990221.522 3556030.265 0.35 0.0 0.0 0.0
null null 91.16 8.22 5990222.4998 3556028.2405 0.35 0.0 0.0 0.0
null null 116.71 8.04 5990224.4547 3556024.1932 0.35 0.0 0.0 0.0
null null 142.27 7.96 5990226.4104 3556020.1443 0.35 0.0 0.0 0.0
null null 147.06 8.09 5990226.7769 3556019.3855 0.35 0.0 0.0 0.0
null true 170.13 8.08 5990228.542 3556015.731 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556042.681 5990215.525 9.73,3556040.1465 5990216.7492 9.83,3556037.105 5990218.2182 9.1,3556035.5843 5990218.9527 8.63,3556033.1512 5990220.128 8.42,3556033.0561 5990220.1739 8.47,3556032.931 5990220.2343 8.44,3556032.348 5990220.5159 8.53,3556032.3322 5990220.5235 8.49,3556032.2847 5990220.5465 8.25,3556032.1595 5990220.6069 8.01,3556031.8918 5990220.7362 7.22,3556031.6384 5990220.8587 6.95,3556031.5132 5990220.9191 6.84,3556031.4499 5990220.9497 6.81,3556031.1964 5990221.0721 6.79,3556030.7544 5990221.2856 6.81,3556030.5644 5990221.3774 7.13,3556030.4075 5990221.4532 7.85,3556030.265 5990221.522 8.11,3556028.2405 5990222.4998 8.22,3556024.1932 5990224.4547 8.04,3556020.1443 5990226.4104 7.96,3556019.3855 5990226.7769 8.09,3556015.731 5990228.542 8.08</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859519179173">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951917965">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.72 5990292.525 3556096.644 0.35 0.0 0.0 0.0
null null 27.17 8.52 5990296.1627 3556093.0944 0.35 0.0 0.0 0.0
null null 50.3 8.64 5990299.2595 3556090.0726 0.35 0.0 0.0 0.0
null null 51.88 8.6 5990299.471 3556089.8662 0.35 0.0 0.0 0.0
null null 58.05 8.53 5990300.2971 3556089.0602 0.35 5.0 5.0 0.15
low null 59.15 8.16 5990300.4444 3556088.9164 0.2 0.0 0.0 0.0
null null 60.44 7.63 5990300.6171 3556088.7479 0.2 0.0 0.0 0.0
null null 61.04 7.16 5990300.6974 3556088.6695 0.12 0.0 0.0 0.0
null null 63.03 6.62 5990300.9638 3556088.4095 0.12 0.0 0.0 0.0
null null 65.01 6.4 5990301.2289 3556088.1509 0.12 0.0 0.0 0.0
null null 66.6 6.4 5990301.4418 3556087.9431 0.12 0.0 0.0 0.0
null null 68.78 6.34 5990301.7337 3556087.6583 0.12 0.0 0.0 0.0
null null 70.37 7.2 5990301.9466 3556087.4506 0.25 0.0 0.0 0.0
low null 71.46 8.0 5990302.0925 3556087.3082 0.35 5.0 5.0 0.2
null null 72.45 8.36 5990302.2251 3556087.1789 0.35 0.0 0.0 0.0
null null 83.67 8.57 5990303.7273 3556085.7131 0.35 0.0 0.0 0.0
null null 102.87 8.38 5990306.2979 3556083.2047 0.35 0.0 0.0 0.0
null null 112.47 8.36 5990307.5832 3556081.9505 0.35 0.0 0.0 0.0
null null 128.47 8.11 5990309.7254 3556079.8602 0.35 0.0 0.0 0.0
null null 154.06 8.05 5990313.1515 3556076.5171 0.35 0.0 0.0 0.0
null null 160.45 8.05 5990314.007 3556075.6823 0.35 0.0 0.0 0.0
null null 162.05 8.07 5990314.2213 3556075.4732 0.35 0.0 0.0 0.0
null null 165.25 8.16 5990314.6497 3556075.0552 0.35 0.0 0.0 0.0
null null 174.85 8.31 5990315.935 3556073.801 0.35 0.0 0.0 0.0
null null 178.04 8.39 5990316.3621 3556073.3842 0.35 0.0 0.0 0.0
null null 181.24 8.4 5990316.7905 3556072.9662 0.35 0.0 0.0 0.0
null true 185.74 8.46 5990317.393 3556072.3783 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.5000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556096.644 5990292.525 8.72,3556093.0944 5990296.1627 8.52,3556090.0726 5990299.2595 8.64,3556089.8662 5990299.471 8.6,3556089.0602 5990300.2971 8.53,3556088.9164 5990300.4444 8.16,3556088.7479 5990300.6171 7.63,3556088.6695 5990300.6974 7.16,3556088.4095 5990300.9638 6.62,3556088.1509 5990301.2289 6.4,3556087.9431 5990301.4418 6.4,3556087.6583 5990301.7337 6.34,3556087.4506 5990301.9466 7.2,3556087.3082 5990302.0925 8.0,3556087.1789 5990302.2251 8.36,3556085.7131 5990303.7273 8.57,3556083.2047 5990306.2979 8.38,3556081.9505 5990307.5832 8.36,3556079.8602 5990309.7254 8.11,3556076.5171 5990313.1515 8.05,3556075.6823 5990314.007 8.05,3556075.4732 5990314.2213 8.07,3556075.0552 5990314.6497 8.16,3556073.801 5990315.935 8.31,3556073.3842 5990316.3621 8.39,3556072.9662 5990316.7905 8.4,3556072.3783 5990317.393 8.46</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859519288162">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859519288137">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.52 5990345.211 3556174.844 0.35 0.0 0.0 0.0
null null 25.58 8.56 5990348.5144 3556173.5329 0.35 0.0 0.0 0.0
null null 38.38 8.74 5990350.1674 3556172.8768 0.35 0.0 0.0 0.0
null null 44.77 8.75 5990350.9926 3556172.5492 0.35 0.0 0.0 0.0
null null 54.37 8.33 5990352.2324 3556172.0572 0.35 0.0 0.0 0.0
null null 79.95 8.42 5990355.5358 3556170.746 0.35 0.0 0.0 0.0
null null 111.95 8.68 5990359.6682 3556169.1058 0.35 0.0 0.0 0.0
null null 117.73 9.0 5990360.4147 3556168.8096 0.35 5.0 5.0 0.15
null null 118.49 8.53 5990360.5128 3556168.7706 0.2 0.0 0.0 0.0
low null 120.77 7.48 5990360.8073 3556168.6537 0.2 0.0 0.0 0.0
null null 121.17 7.14 5990360.8589 3556168.6332 0.12 0.0 0.0 0.0
null null 122.77 6.62 5990361.0655 3556168.5512 0.12 0.0 0.0 0.0
null null 123.57 6.41 5990361.1689 3556168.5102 0.12 0.0 0.0 0.0
null null 128.76 6.45 5990361.8391 3556168.2442 0.25 0.0 0.0 0.0
null null 130.36 7.45 5990362.0457 3556168.1622 0.25 0.0 0.0 0.0
low null 131.96 8.15 5990362.2523 3556168.0802 0.35 5.0 5.0 0.2
null null 138.36 8.43 5990363.0788 3556167.7521 0.35 0.0 0.0 0.0
null null 144.76 8.58 5990363.9053 3556167.4241 0.35 0.0 0.0 0.0
null null 170.35 8.51 5990367.21 3556166.1124 0.35 0.0 0.0 0.0
null null 202.35 8.18 5990371.3425 3556164.4722 0.35 0.0 0.0 0.0
null null 234.35 8.86 5990375.475 3556162.832 0.35 0.0 0.0 0.0
null null 285.57 10.96 5990382.0896 3556160.2066 0.35 0.0 0.0 0.0
null null 304.78 9.97 5990384.5703 3556159.222 0.35 0.0 0.0 0.0
null true 308.98 9.84 5990385.1127 3556159.0067 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556174.844 5990345.211 8.52,3556173.5329 5990348.5144 8.56,3556172.8768 5990350.1674 8.74,3556172.5492 5990350.9926 8.75,3556172.0572 5990352.2324 8.33,3556170.746 5990355.5358 8.42,3556169.1058 5990359.6682 8.68,3556168.8096 5990360.4147 9.0,3556168.7706 5990360.5128 8.53,3556168.6537 5990360.8073 7.48,3556168.6332 5990360.8589 7.14,3556168.5512 5990361.0655 6.62,3556168.5102 5990361.1689 6.41,3556168.2442 5990361.8391 6.45,3556168.1622 5990362.0457 7.45,3556168.0802 5990362.2523 8.15,3556167.7521 5990363.0788 8.43,3556167.4241 5990363.9053 8.58,3556166.1124 5990367.21 8.51,3556164.4722 5990371.3425 8.18,3556162.832 5990375.475 8.86,3556160.2066 5990382.0896 10.96,3556159.222 5990384.5703 9.97,3556159.0067 5990385.1127 9.84</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859519304153">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859519304150">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.45 5990386.694 3556264.261 0.35 0.0 0.0 0.0
null null 12.8 8.28 5990388.7391 3556263.4214 0.35 0.0 0.0 0.0
null null 30.41 8.19 5990391.5527 3556262.2664 0.35 0.0 0.0 0.0
null null 46.42 8.4 5990394.1106 3556261.2162 0.35 0.0 0.0 0.0
null null 56.02 8.44 5990395.6444 3556260.5866 0.35 0.0 0.0 0.0
null null 59.22 8.33 5990396.1557 3556260.3767 0.35 0.0 0.0 0.0
null null 72.0 8.28 5990398.1976 3556259.5384 0.35 0.0 0.0 0.0
null null 78.4 8.24 5990399.2201 3556259.1186 0.35 0.0 0.0 0.0
null null 80.0 8.21 5990399.4757 3556259.0137 0.35 0.0 0.0 0.0
null null 86.81 8.21 5990400.5638 3556258.567 0.35 0.0 0.0 0.0
null null 87.01 8.19 5990400.5957 3556258.5539 0.35 4.0 4.0 0.15
low null 88.01 7.94 5990400.7555 3556258.4883 0.2 0.0 0.0 0.0
null null 88.91 7.47 5990400.8993 3556258.4293 0.2 0.0 0.0 0.0
null null 89.21 7.08 5990400.9472 3556258.4096 0.2 0.0 0.0 0.0
null null 89.31 7.0 5990400.9632 3556258.403 0.12 0.0 0.0 0.0
null null 91.11 6.39 5990401.2508 3556258.285 0.12 0.0 0.0 0.0
null null 93.91 6.56 5990401.6981 3556258.1013 0.12 0.0 0.0 0.0
null null 96.52 6.5 5990402.1152 3556257.9301 0.12 0.0 0.0 0.0
null null 97.52 6.83 5990402.2749 3556257.8645 0.2 0.0 0.0 0.0
low null 99.22 8.04 5990402.5465 3556257.753 0.35 4.0 4.0 0.15
null null 100.02 8.26 5990402.6744 3556257.7005 0.35 0.0 0.0 0.0
null null 106.41 8.56 5990403.6953 3556257.2814 0.35 0.0 0.0 0.0
null null 110.39 8.71 5990404.3312 3556257.0204 0.35 0.0 0.0 0.0
null null 135.95 8.59 5990408.4149 3556255.3438 0.35 0.0 0.0 0.0
null null 148.72 8.55 5990410.4552 3556254.5062 0.35 0.0 0.0 0.0
null null 158.3 8.54 5990411.9858 3556253.8779 0.35 0.0 0.0 0.0
null null 161.49 8.57 5990412.4955 3556253.6686 0.35 0.0 0.0 0.0
null null 164.68 8.62 5990413.0052 3556253.4594 0.35 0.0 0.0 0.0
null null 183.85 9.08 5990416.068 3556252.202 0.35 0.0 0.0 0.0
null true 200.62 9.71 5990418.7474 3556251.102 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556264.261 5990386.694 8.45,3556263.4214 5990388.7391 8.28,3556262.2664 5990391.5527 8.19,3556261.2162 5990394.1106 8.4,3556260.5866 5990395.6444 8.44,3556260.3767 5990396.1557 8.33,3556259.5384 5990398.1976 8.28,3556259.1186 5990399.2201 8.24,3556259.0137 5990399.4757 8.21,3556258.567 5990400.5638 8.21,3556258.5539 5990400.5957 8.19,3556258.4883 5990400.7555 7.94,3556258.4293 5990400.8993 7.47,3556258.4096 5990400.9472 7.08,3556258.403 5990400.9632 7.0,3556258.285 5990401.2508 6.39,3556258.1013 5990401.6981 6.56,3556257.9301 5990402.1152 6.5,3556257.8645 5990402.2749 6.83,3556257.753 5990402.5465 8.04,3556257.7005 5990402.6744 8.26,3556257.2814 5990403.6953 8.56,3556257.0204 5990404.3312 8.71,3556255.3438 5990408.4149 8.59,3556254.5062 5990410.4552 8.55,3556253.8779 5990411.9858 8.54,3556253.6686 5990412.4955 8.57,3556253.4594 5990413.0052 8.62,3556252.202 5990416.068 9.08,3556251.102 5990418.7474 9.71</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859519413158">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951941362">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#OBERKANTEBRUECKE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#UNTERKANTEBRUECKE"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.65 5990427.675 3556332.484 0.35 10.65 10.65
null null 11.19 10.42 5990428.465 3556332.1487 0.35 10.42 10.42
null null 11.98 10.35 5990428.5207 3556332.125 0.35 10.35 10.35
null null 24.77 10.3 5990429.4237 3556331.7418 0.35 10.3 10.3
null null 36.05 10.22 5990430.22 3556331.4038 0.35 10.22 10.22
null null 38.15 10.0 5990430.3682 3556331.3409 0.35 10.0 10.0
null null 46.94 10.0 5990430.9888 3556331.0775 0.35 10.0 10.0
null null 50.54 10.12 5990431.2429 3556330.9696 0.35 10.12 10.12
null null 52.54 9.98 5990431.3841 3556330.9097 0.35 9.98 9.98
null null 61.53 9.98 5990432.0188 3556330.6403 0.35 9.98 9.98
null null 63.33 9.97 5990432.1458 3556330.5864 0.35 9.97 9.97
null null 75.72 9.9 5990433.0205 3556330.2151 0.35 9.97 9.9
null null 85.01 9.9 5990433.6763 3556329.9368 0.2 9.97 9.9
low null 85.11 9.67 5990433.6834 3556329.9338 0.2 9.97 9.67
null null 86.31 8.58 5990433.7681 3556329.8978 0.15 9.97 9.67
null null 86.61 8.2 5990433.7893 3556329.8888 0.15 9.97 9.67
null null 88.01 7.24 5990433.8881 3556329.8469 0.15 9.97 9.67
null null 89.71 7.07 5990434.0081 3556329.7959 0.15 9.97 9.67
null null 91.11 6.98 5990434.107 3556329.754 0.15 9.97 9.67
null null 97.01 7.47 5990434.5235 3556329.5772 0.15 9.97 9.67
null null 97.11 7.52 5990434.5305 3556329.5742 0.15 9.97 9.67
null null 98.11 8.27 5990434.6011 3556329.5442 0.2 9.97 9.67
null null 98.81 9.17 5990434.6506 3556329.5232 0.2 9.97 9.67
low null 98.91 9.75 5990434.6576 3556329.5203 0.35 9.97 9.75
null null 99.81 9.75 5990434.7212 3556329.4933 0.35 9.97 9.75
null null 99.91 9.8 5990434.7282 3556329.4903 0.35 9.97 9.8
null null 107.22 9.88 5990435.2443 3556329.2713 0.35 9.97 9.88
null null 111.82 9.86 5990435.569 3556329.1334 0.35 9.97 9.86
null null 112.52 10.07 5990435.6184 3556329.1124 0.35 10.07 10.07
null null 128.72 9.9 5990436.7621 3556328.627 0.35 9.9 9.9
null null 130.71 10.0 5990436.9026 3556328.5674 0.35 10.0 10.0
null null 138.61 10.0 5990437.4603 3556328.3307 0.35 10.0 10.0
null null 139.61 9.93 5990437.5309 3556328.3007 0.35 9.93 9.93
null null 146.6 10.0 5990438.0243 3556328.0913 0.35 10.0 10.0
null null 150.6 10.23 5990438.3067 3556327.9714 0.35 10.23 10.23
null null 157.0 10.34 5990438.7585 3556327.7796 0.35 10.34 10.34
null null 160.2 10.38 5990438.9844 3556327.6838 0.35 10.38 10.38
null null 164.99 10.36 5990439.3226 3556327.5402 0.35 10.36 10.36
null null 189.36 10.71 5990441.043 3556326.81 0.35 10.71 10.71
null null 200.55 10.17 5990441.833 3556326.4747 0.35 10.17 10.17
null true 208.04 10.0 5990442.3617 3556326.2503 0.35 10.0 10.0
]]></om:result>
       <prof:station>64.7820</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556332.484 5990427.675 10.65,3556332.1487 5990428.465 10.42,3556332.125 5990428.5207 10.35,3556331.7418 5990429.4237 10.3,3556331.4038 5990430.22 10.22,3556331.3409 5990430.3682 10.0,3556331.0775 5990430.9888 10.0,3556330.9696 5990431.2429 10.12,3556330.9097 5990431.3841 9.98,3556330.6403 5990432.0188 9.98,3556330.5864 5990432.1458 9.97,3556330.2151 5990433.0205 9.9,3556329.9368 5990433.6763 9.9,3556329.9338 5990433.6834 9.67,3556329.8978 5990433.7681 8.58,3556329.8888 5990433.7893 8.2,3556329.8469 5990433.8881 7.24,3556329.7959 5990434.0081 7.07,3556329.754 5990434.107 6.98,3556329.5772 5990434.5235 7.47,3556329.5742 5990434.5305 7.52,3556329.5442 5990434.6011 8.27,3556329.5232 5990434.6506 9.17,3556329.5203 5990434.6576 9.75,3556329.4933 5990434.7212 9.75,3556329.4903 5990434.7282 9.8,3556329.2713 5990435.2443 9.88,3556329.1334 5990435.569 9.86,3556329.1124 5990435.6184 10.07,3556328.627 5990436.7621 9.9,3556328.5674 5990436.9026 10.0,3556328.3307 5990437.4603 10.0,3556328.3007 5990437.5309 9.93,3556328.0913 5990438.0243 10.0,3556327.9714 5990438.3067 10.23,3556327.7796 5990438.7585 10.34,3556327.6838 5990438.9844 10.38,3556327.5402 5990439.3226 10.36,3556326.81 5990441.043 10.71,3556326.4747 5990441.833 10.17,3556326.2503 5990442.3617 10.0</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
       <prof:member>
        <om:Observation gml:id="Observation118285951955465">
         <gml:description>Bauwerk-Observation</gml:description>
         <gml:name>urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#BRUECKE</gml:name>
         <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#BRUECKE"/>
         <om:resultDefinition>
          <swe:RecordDefinition gml:id="RecordDefinition118285951955412">
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#BREITE"/>
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#UNTERWASSER"/>
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#FORMBEIWERT"/>
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#RAUHEIT"/>
          </swe:RecordDefinition>
         </om:resultDefinition>
         <om:result><![CDATA[6.0 8.0 1.0 0.3
]]></om:result>
        </om:Observation>
       </prof:member>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951955470">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859519554168">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 9.59 5990433.4 3556353.949 0.3 0.0 0.0 0.0
null null 0.4 9.58 5990433.451 3556353.9274 0.35 0.0 0.0 0.0
null null 9.19 8.63 5990434.5708 3556353.4526 0.35 0.0 0.0 0.0
null null 21.98 8.55 5990436.2002 3556352.7618 0.35 0.0 0.0 0.0
null null 34.77 8.48 5990437.8296 3556352.0709 0.35 0.0 0.0 0.0
null null 47.56 8.44 5990439.459 3556351.3801 0.35 0.0 0.0 0.0
null null 60.33 8.43 5990441.0859 3556350.6903 0.35 0.0 0.0 0.0
null null 74.69 8.33 5990442.9153 3556349.9147 0.35 0.0 0.0 0.0
null null 75.49 8.3 5990443.0172 3556349.8715 0.35 0.0 0.0 0.0
null null 76.08 8.27 5990443.0924 3556349.8396 0.35 8.0 8.0 0.15
low null 78.46 8.56 5990443.3956 3556349.7111 0.35 0.0 0.0 0.0
null null 78.66 8.31 5990443.4211 3556349.7002 0.2 0.0 0.0 0.0
null null 79.85 8.05 5990443.5727 3556349.636 0.2 0.0 0.0 0.0
null null 80.64 7.73 5990443.6733 3556349.5933 0.2 0.0 0.0 0.0
null null 81.33 7.29 5990443.7612 3556349.556 0.12 0.0 0.0 0.0
null null 82.52 6.99 5990443.9128 3556349.4918 0.12 0.0 0.0 0.0
null null 84.92 6.9 5990444.2186 3556349.3621 0.12 0.0 0.0 0.0
null null 85.31 6.86 5990444.2683 3556349.3411 0.12 0.0 0.0 0.0
null null 88.51 6.96 5990444.6759 3556349.1682 0.12 0.0 0.0 0.0
null null 89.51 7.13 5990444.8033 3556349.1142 0.2 0.0 0.0 0.0
null null 90.21 7.74 5990444.8925 3556349.0764 0.2 0.0 0.0 0.0
null null 91.01 8.07 5990444.9944 3556349.0332 0.2 0.0 0.0 0.0
null null 91.41 8.21 5990445.0454 3556349.0116 0.2 0.0 0.0 0.0
low null 92.5 8.51 5990445.1843 3556348.9527 0.35 0.0 0.0 0.0
null null 98.89 8.61 5990445.9983 3556348.6075 0.35 0.0 0.0 0.0
null null 100.48 8.66 5990446.2009 3556348.5217 0.35 0.0 0.0 0.0
null null 101.68 8.73 5990446.3538 3556348.4568 0.35 0.0 0.0 0.0
null null 103.28 8.69 5990446.5576 3556348.3704 0.35 0.0 0.0 0.0
null null 128.84 8.69 5990449.8139 3556346.9898 0.35 0.0 0.0 0.0
null null 132.03 8.71 5990450.2203 3556346.8175 0.35 0.0 0.0 0.0
null null 144.81 8.94 5990451.8484 3556346.1272 0.35 0.0 0.0 0.0
null null 146.41 9.0 5990452.0523 3556346.0408 0.35 0.0 0.0 0.0
null null 160.79 10.0 5990453.8842 3556345.2641 0.35 0.0 0.0 0.0
null true 189.58 10.41 5990457.552 3556343.709 0.35 0.0 0.0 0.0
null null 194.98 10.15 5990458.2399 3556343.4173 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556353.949 5990433.4 9.59,3556353.9274 5990433.451 9.58,3556353.4526 5990434.5708 8.63,3556352.7618 5990436.2002 8.55,3556352.0709 5990437.8296 8.48,3556351.3801 5990439.459 8.44,3556350.6903 5990441.0859 8.43,3556349.9147 5990442.9153 8.33,3556349.8715 5990443.0172 8.3,3556349.8396 5990443.0924 8.27,3556349.7111 5990443.3956 8.56,3556349.7002 5990443.4211 8.31,3556349.636 5990443.5727 8.05,3556349.5933 5990443.6733 7.73,3556349.556 5990443.7612 7.29,3556349.4918 5990443.9128 6.99,3556349.3621 5990444.2186 6.9,3556349.3411 5990444.2683 6.86,3556349.1682 5990444.6759 6.96,3556349.1142 5990444.8033 7.13,3556349.0764 5990444.8925 7.74,3556349.0332 5990444.9944 8.07,3556349.0116 5990445.0454 8.21,3556348.9527 5990445.1843 8.51,3556348.6075 5990445.9983 8.61,3556348.5217 5990446.2009 8.66,3556348.4568 5990446.3538 8.73,3556348.3704 5990446.5576 8.69,3556346.9898 5990449.8139 8.69,3556346.8175 5990450.2203 8.71,3556346.1272 5990451.8484 8.94,3556346.0408 5990452.0523 9.0,3556345.2641 5990453.8842 10.0,3556343.709 5990457.552 10.41,3556343.4173 5990458.2399 10.15</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859519710186">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951972619">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.56 5990495.424 3556436.196 0.35 0.0 0.0 0.0
null null 24.76 8.25 5990498.8453 3556432.4574 0.35 0.0 0.0 0.0
null null 43.93 8.67 5990501.4942 3556429.5629 0.35 0.0 0.0 0.0
null null 58.33 8.8 5990503.484 3556427.3886 0.35 0.0 0.0 0.0
null null 59.13 8.78 5990503.5945 3556427.2678 0.35 8.0 8.0 0.15
low null 63.62 8.89 5990504.2149 3556426.5898 0.35 0.0 0.0 0.0
null null 63.72 8.87 5990504.2288 3556426.5747 0.35 0.0 0.0 0.0
null null 64.92 8.22 5990504.3946 3556426.3935 0.3 0.0 0.0 0.0
null null 66.11 7.7 5990504.559 3556426.2138 0.3 0.0 0.0 0.0
null null 66.51 7.2 5990504.6143 3556426.1534 0.12 0.0 0.0 0.0
null null 67.91 6.83 5990504.8077 3556425.942 0.12 0.0 0.0 0.0
null null 69.5 6.78 5990505.0274 3556425.702 0.12 0.0 0.0 0.0
null null 69.9 6.75 5990505.0827 3556425.6416 0.12 0.0 0.0 0.0
null null 70.7 6.66 5990505.1932 3556425.5208 0.12 0.0 0.0 0.0
null null 71.5 6.62 5990505.3038 3556425.4 0.12 0.0 0.0 0.0
null null 73.89 6.67 5990505.634 3556425.0391 0.12 0.0 0.0 0.0
null null 75.19 6.61 5990505.8137 3556424.8428 0.2 0.0 0.0 0.0
null null 75.99 7.75 5990505.9242 3556424.722 0.2 0.0 0.0 0.0
low null 77.59 8.39 5990506.1453 3556424.4804 0.35 0.0 0.0 0.0
null null 83.2 8.64 5990506.9205 3556423.6334 0.35 0.0 0.0 0.0
null null 108.8 8.6 5990510.4579 3556419.7679 0.35 0.0 0.0 0.0
null null 115.2 8.6 5990511.3422 3556418.8016 0.35 0.0 0.0 0.0
null null 137.6 8.72 5990514.4374 3556415.4193 0.35 0.0 0.0 0.0
null true 149.6 9.35 5990516.0955 3556413.6074 0.35 0.0 0.0 0.0
null null 169.81 9.34 5990518.8881 3556410.5558 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>64.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556436.196 5990495.424 8.56,3556432.4574 5990498.8453 8.25,3556429.5629 5990501.4942 8.67,3556427.3886 5990503.484 8.8,3556427.2678 5990503.5945 8.78,3556426.5898 5990504.2149 8.89,3556426.5747 5990504.2288 8.87,3556426.3935 5990504.3946 8.22,3556426.2138 5990504.559 7.7,3556426.1534 5990504.6143 7.2,3556425.942 5990504.8077 6.83,3556425.702 5990505.0274 6.78,3556425.6416 5990505.0827 6.75,3556425.5208 5990505.1932 6.66,3556425.4 5990505.3038 6.62,3556425.0391 5990505.634 6.67,3556424.8428 5990505.8137 6.61,3556424.722 5990505.9242 7.75,3556424.4804 5990506.1453 8.39,3556423.6334 5990506.9205 8.64,3556419.7679 5990510.4579 8.6,3556418.8016 5990511.3422 8.6,3556415.4193 5990514.4374 8.72,3556413.6074 5990516.0955 9.35,3556410.5558 5990518.8881 9.34</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859519851182">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859519851173">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.64 5990570.589 3556499.688 0.35 0.0 0.0 0.0
null null 21.59 8.78 5990573.1137 3556497.0318 0.35 0.0 0.0 0.0
null null 47.17 8.5 5990576.1049 3556493.8847 0.35 0.0 0.0 0.0
null null 53.97 8.46 5990576.9001 3556493.0481 0.35 0.0 0.0 0.0
null null 55.57 8.6 5990577.0872 3556492.8513 0.35 0.0 0.0 0.0
null null 57.57 8.52 5990577.321 3556492.6052 0.35 8.0 8.0 0.2
low null 58.37 8.52 5990577.4146 3556492.5068 0.3 0.0 0.0 0.0
null null 61.17 7.41 5990577.742 3556492.1623 0.3 0.0 0.0 0.0
null null 62.77 6.94 5990577.9291 3556491.9655 0.12 0.0 0.0 0.0
null null 63.37 6.78 5990577.9993 3556491.8917 0.12 0.0 0.0 0.0
null null 65.37 6.66 5990578.2331 3556491.6456 0.12 0.0 0.0 0.0
null null 68.57 6.78 5990578.6073 3556491.2519 0.12 0.0 0.0 0.0
null null 69.27 6.83 5990578.6892 3556491.1658 0.2 0.0 0.0 0.0
null null 70.06 7.74 5990578.7816 3556491.0686 0.2 0.0 0.0 0.0
low null 71.67 8.39 5990578.9698 3556490.8705 0.35 0.0 0.0 0.0
null null 78.06 8.51 5990579.7171 3556490.0844 0.35 0.0 0.0 0.0
null null 103.64 8.87 5990582.7083 3556486.9373 0.35 0.0 0.0 0.0
null null 110.04 8.88 5990583.4567 3556486.1499 0.35 0.0 0.0 0.0
null null 142.01 8.77 5990587.1952 3556482.2167 0.35 0.0 0.0 0.0
null null 161.19 8.81 5990589.438 3556479.857 0.35 0.0 0.0 0.0
null true 172.28 9.11 5990590.7348 3556478.4926 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>65.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556499.688 5990570.589 8.64,3556497.0318 5990573.1137 8.78,3556493.8847 5990576.1049 8.5,3556493.0481 5990576.9001 8.46,3556492.8513 5990577.0872 8.6,3556492.6052 5990577.321 8.52,3556492.5068 5990577.4146 8.52,3556492.1623 5990577.742 7.41,3556491.9655 5990577.9291 6.94,3556491.8917 5990577.9993 6.78,3556491.6456 5990578.2331 6.66,3556491.2519 5990578.6073 6.78,3556491.1658 5990578.6892 6.83,3556491.0686 5990578.7816 7.74,3556490.8705 5990578.9698 8.39,3556490.0844 5990579.7171 8.51,3556486.9373 5990582.7083 8.87,3556486.1499 5990583.4567 8.88,3556482.2167 5990587.1952 8.77,3556479.857 5990589.438 8.81,3556478.4926 5990590.7348 9.11</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285951988215">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285951988253">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.74 5990635.216 3556571.334 0.35 0.0 0.0 0.0
null null 12.77 8.41 5990637.49 3556570.1359 0.35 0.0 0.0 0.0
null null 41.52 8.73 5990642.6096 3556567.4385 0.35 0.0 0.0 0.0
null null 47.91 8.79 5990643.7475 3556566.839 0.35 0.0 0.0 0.0
null null 57.11 8.95 5990645.3858 3556565.9758 0.35 0.0 0.0 0.0
null null 57.71 8.9 5990645.4926 3556565.9195 0.35 0.0 0.0 0.0
null null 61.71 9.47 5990646.2049 3556565.5442 0.35 0.0 0.0 0.0
null null 61.81 9.43 5990646.2227 3556565.5348 0.35 8.0 8.0 0.2
low null 63.41 8.29 5990646.5076 3556565.3847 0.2 0.0 0.0 0.0
null null 64.72 7.74 5990646.7409 3556565.2618 0.2 0.0 0.0 0.0
null null 64.81 7.66 5990646.757 3556565.2534 0.2 0.0 0.0 0.0
null null 65.11 7.26 5990646.8104 3556565.2252 0.12 0.0 0.0 0.0
null null 66.22 6.93 5990647.008 3556565.1211 0.12 0.0 0.0 0.0
null null 67.82 6.83 5990647.293 3556564.971 0.12 0.0 0.0 0.0
null null 70.22 6.56 5990647.7203 3556564.7458 0.12 0.0 0.0 0.0
null null 72.22 6.5 5990648.0765 3556564.5582 0.12 0.0 0.0 0.0
null null 73.42 7.02 5990648.2902 3556564.4456 0.25 0.0 0.0 0.0
null null 73.92 7.82 5990648.3792 3556564.3987 0.25 0.0 0.0 0.0
low null 75.52 8.39 5990648.6641 3556564.2485 0.35 30.0 30.0 0.25
null null 76.72 8.77 5990648.8778 3556564.136 0.35 0.0 0.0 0.0
null null 78.72 8.93 5990649.234 3556563.9483 0.35 0.0 0.0 0.0
null null 82.71 9.11 5990649.9445 3556563.574 0.35 0.0 0.0 0.0
null null 83.51 9.18 5990650.0869 3556563.4989 0.35 0.0 0.0 0.0
null null 85.11 9.19 5990650.3718 3556563.3488 0.35 0.0 0.0 0.0
null null 110.65 8.78 5990654.9198 3556560.9526 0.35 3.0 3.0 0.15
null null 131.44 8.52 5990658.622 3556559.002 0.35 0.0 0.0 0.0
null null 173.01 9.32 5990666.0245 3556555.1018 0.35 0.0 0.0 0.0
null true 174.31 9.36 5990666.256 3556554.9798 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>65.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556571.334 5990635.216 8.74,3556570.1359 5990637.49 8.41,3556567.4385 5990642.6096 8.73,3556566.839 5990643.7475 8.79,3556565.9758 5990645.3858 8.95,3556565.9195 5990645.4926 8.9,3556565.5442 5990646.2049 9.47,3556565.5348 5990646.2227 9.43,3556565.3847 5990646.5076 8.29,3556565.2618 5990646.7409 7.74,3556565.2534 5990646.757 7.66,3556565.2252 5990646.8104 7.26,3556565.1211 5990647.008 6.93,3556564.971 5990647.293 6.83,3556564.7458 5990647.7203 6.56,3556564.5582 5990648.0765 6.5,3556564.4456 5990648.2902 7.02,3556564.3987 5990648.3792 7.82,3556564.2485 5990648.6641 8.39,3556564.136 5990648.8778 8.77,3556563.9483 5990649.234 8.93,3556563.574 5990649.9445 9.11,3556563.4989 5990650.0869 9.18,3556563.3488 5990650.3718 9.19,3556560.9526 5990654.9198 8.78,3556559.002 5990658.622 8.52,3556555.1018 5990666.0245 9.32,3556554.9798 5990666.256 9.36</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859520007189">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859520023156">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 8.86 5990667.272 3556660.754 0.35 0.0 0.0 0.0
null null 7.99 8.83 5990669.8995 3556660.2967 0.35 0.0 0.0 0.0
null null 9.58 8.8 5990670.4224 3556660.2057 0.35 0.0 0.0 0.0
null null 28.75 8.82 5990676.7264 3556659.1085 0.35 0.0 0.0 0.0
null null 31.94 8.85 5990677.7754 3556658.9259 0.35 0.0 0.0 0.0
null null 44.73 9.13 5990681.9814 3556658.1938 0.35 0.0 0.0 0.0
null null 59.12 9.31 5990686.7135 3556657.3702 0.35 0.0 0.0 0.0
null null 60.72 9.24 5990687.2396 3556657.2786 0.35 0.0 0.0 0.0
null null 64.11 9.26 5990688.3544 3556657.0846 0.35 0.0 0.0 0.0
null null 64.31 9.23 5990688.4202 3556657.0731 0.35 10.0 10.0 0.2
low null 68.71 9.36 5990689.8671 3556656.8213 0.2 0.0 0.0 0.0
null null 70.11 8.37 5990690.3275 3556656.7412 0.2 0.0 0.0 0.0
null null 71.21 7.79 5990690.6893 3556656.6782 0.2 0.0 0.0 0.0
null null 71.61 7.15 5990690.8208 3556656.6553 0.12 0.0 0.0 0.0
null null 73.21 6.87 5990691.3469 3556656.5637 0.12 0.0 0.0 0.0
null null 74.41 6.76 5990691.7416 3556656.4951 0.12 0.0 0.0 0.0
null null 77.61 6.64 5990692.7939 3556656.3119 0.12 0.0 0.0 0.0
null null 79.21 6.59 5990693.32 3556656.2203 0.12 0.0 0.0 0.0
null null 80.41 7.03 5990693.7147 3556656.1516 0.25 0.0 0.0 0.0
null null 81.11 7.86 5990693.9448 3556656.1116 0.25 0.0 0.0 0.0
low null 83.71 8.88 5990694.7999 3556655.9628 0.35 20.0 20.0 0.2
null null 118.93 8.43 5990706.3819 3556653.9469 0.35 3.0 3.0 0.15
null true 158.31 8.93 5990719.3319 3556651.6929 0.35 3.0 3.0 0.15
]]></om:result>
       <prof:station>65.2000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556660.754 5990667.272 8.86,3556660.2967 5990669.8995 8.83,3556660.2057 5990670.4224 8.8,3556659.1085 5990676.7264 8.82,3556658.9259 5990677.7754 8.85,3556658.1938 5990681.9814 9.13,3556657.3702 5990686.7135 9.31,3556657.2786 5990687.2396 9.24,3556657.0846 5990688.3544 9.26,3556657.0731 5990688.4202 9.23,3556656.8213 5990689.8671 9.36,3556656.7412 5990690.3275 8.37,3556656.6782 5990690.6893 7.79,3556656.6553 5990690.8208 7.15,3556656.5637 5990691.3469 6.87,3556656.4951 5990691.7416 6.76,3556656.3119 5990692.7939 6.64,3556656.2203 5990693.32 6.59,3556656.1516 5990693.7147 7.03,3556656.1116 5990693.9448 7.86,3556655.9628 5990694.7999 8.88,3556653.9469 5990706.3819 8.43,3556651.6929 5990719.3319 8.93</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859520163125">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859520163156">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 9.28 5990663.913 3556755.99 0.35 0.0 0.0 0.0
null null 12.79 8.93 5990666.3435 3556756.2006 0.35 0.0 0.0 0.0
null null 19.18 8.94 5990667.5578 3556756.3058 0.35 0.0 0.0 0.0
null null 31.97 8.81 5990669.9883 3556756.5164 0.35 0.0 0.0 0.0
null null 47.94 9.18 5990673.023 3556756.7794 0.35 0.0 0.0 0.0
null null 57.52 9.5 5990674.8435 3556756.9371 0.35 0.0 0.0 0.0
null null 70.29 9.63 5990677.2702 3556757.1474 0.35 0.0 0.0 0.0
null null 76.67 9.68 5990678.4826 3556757.2525 0.35 0.0 0.0 0.0
null null 83.06 9.67 5990679.6969 3556757.3577 0.35 0.0 0.0 0.0
null null 84.06 9.69 5990679.8869 3556757.3742 0.35 0.0 0.0 0.0
null null 84.25 9.63 5990679.923 3556757.3773 0.35 10.0 10.0 0.2
low null 88.65 9.32 5990680.7592 3556757.4497 0.2 0.0 0.0 0.0
null null 90.24 8.42 5990681.0613 3556757.4759 0.2 0.0 0.0 0.0
null null 91.14 7.94 5990681.2323 3556757.4907 0.2 0.0 0.0 0.0
null null 91.64 7.39 5990681.3274 3556757.499 0.12 0.0 0.0 0.0
null null 94.03 6.76 5990681.7815 3556757.5383 0.12 0.0 0.0 0.0
null null 94.23 6.72 5990681.8195 3556757.5416 0.12 0.0 0.0 0.0
null null 96.62 6.74 5990682.2737 3556757.581 0.12 0.0 0.0 0.0
null null 99.41 6.93 5990682.8039 3556757.6269 0.12 0.0 0.0 0.0
null null 100.7 7.11 5990683.049 3556757.6482 0.12 0.0 0.0 0.0
null null 100.8 7.18 5990683.068 3556757.6498 0.2 0.0 0.0 0.0
null null 101.2 7.84 5990683.144 3556757.6564 0.2 0.0 0.0 0.0
null null 101.3 7.96 5990683.163 3556757.658 0.2 0.0 0.0 0.0
low null 103.89 9.14 5990683.6552 3556757.7007 0.35 0.0 0.0 0.0
null null 113.47 9.23 5990685.4757 3556757.8584 0.35 3.0 3.0 0.15
null null 121.46 9.14 5990686.994 3556757.99 0.35 3.0 3.0 0.15
null null 159.78 9.17 5990694.276 3556758.621 0.35 3.0 3.0 0.15
null true 162.08 9.15 5990694.7131 3556758.6589 0.35 3.0 3.0 0.15
]]></om:result>
       <prof:station>65.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556755.99 5990663.913 9.28,3556756.2006 5990666.3435 8.93,3556756.3058 5990667.5578 8.94,3556756.5164 5990669.9883 8.81,3556756.7794 5990673.023 9.18,3556756.9371 5990674.8435 9.5,3556757.1474 5990677.2702 9.63,3556757.2525 5990678.4826 9.68,3556757.3577 5990679.6969 9.67,3556757.3742 5990679.8869 9.69,3556757.3773 5990679.923 9.63,3556757.4497 5990680.7592 9.32,3556757.4759 5990681.0613 8.42,3556757.4907 5990681.2323 7.94,3556757.499 5990681.3274 7.39,3556757.5383 5990681.7815 6.76,3556757.5416 5990681.8195 6.72,3556757.581 5990682.2737 6.74,3556757.6269 5990682.8039 6.93,3556757.6482 5990683.049 7.11,3556757.6498 5990683.068 7.18,3556757.6564 5990683.144 7.84,3556757.658 5990683.163 7.96,3556757.7007 5990683.6552 9.14,3556757.8584 5990685.4757 9.23,3556757.99 5990686.994 9.14,3556758.621 5990694.276 9.17,3556758.6589 5990694.7131 9.15</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859520288180">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952028873">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 9.25 5990651.883 3556856.284 0.35 0.0 0.0 0.0
null null 12.79 9.21 5990653.6117 3556856.475 0.35 0.0 0.0 0.0
null null 19.18 9.16 5990654.4754 3556856.5704 0.35 0.0 0.0 0.0
null null 22.38 9.13 5990654.9079 3556856.6181 0.35 0.0 0.0 0.0
null null 32.76 8.84 5990656.3108 3556856.7731 0.35 0.0 0.0 0.0
null null 45.53 9.03 5990658.0368 3556856.9638 0.35 0.0 0.0 0.0
null null 80.65 9.37 5990662.7837 3556857.4881 0.35 0.0 0.0 0.0
null null 85.83 9.32 5990663.4838 3556857.5655 0.35 0.0 0.0 0.0
null null 86.33 9.21 5990663.5514 3556857.5729 0.35 0.0 0.0 0.0
null null 88.72 9.43 5990663.8744 3556857.6086 0.35 12.0 12.0 0.15
low null 88.83 9.4 5990663.8893 3556857.6103 0.35 0.0 0.0 0.0
null null 90.12 8.41 5990664.0636 3556857.6295 0.25 0.0 0.0 0.0
null null 90.72 8.17 5990664.1447 3556857.6385 0.25 0.0 0.0 0.0
null null 91.22 7.56 5990664.2123 3556857.6459 0.25 0.0 0.0 0.0
null null 92.81 6.9 5990664.4272 3556857.6697 0.12 0.0 0.0 0.0
null null 93.61 6.6 5990664.5353 3556857.6816 0.12 0.0 0.0 0.0
null null 96.0 6.64 5990664.8584 3556857.7173 0.12 0.0 0.0 0.0
null null 99.19 6.91 5990665.2895 3556857.7649 0.12 0.0 0.0 0.0
null null 100.49 7.1 5990665.4652 3556857.7843 0.3 0.0 0.0 0.0
null null 100.89 7.99 5990665.5193 3556857.7903 0.3 0.0 0.0 0.0
null null 102.48 8.78 5990665.7342 3556857.814 0.35 0.0 0.0 0.0
low null 103.98 9.29 5990665.937 3556857.8364 0.35 0.0 0.0 0.0
null null 110.36 9.34 5990666.7993 3556857.9317 0.35 0.0 0.0 0.0
null null 112.76 9.53 5990667.1237 3556857.9675 0.35 0.0 0.0 0.0
null null 119.15 9.68 5990667.9873 3556858.0629 0.35 0.0 0.0 0.0
null true 138.31 9.67 5990670.577 3556858.349 0.35 0.0 0.0 0.0
null null 158.58 9.46 5990673.3167 3556858.6516 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>65.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556856.284 5990651.883 9.25,3556856.475 5990653.6117 9.21,3556856.5704 5990654.4754 9.16,3556856.6181 5990654.9079 9.13,3556856.7731 5990656.3108 8.84,3556856.9638 5990658.0368 9.03,3556857.4881 5990662.7837 9.37,3556857.5655 5990663.4838 9.32,3556857.5729 5990663.5514 9.21,3556857.6086 5990663.8744 9.43,3556857.6103 5990663.8893 9.4,3556857.6295 5990664.0636 8.41,3556857.6385 5990664.1447 8.17,3556857.6459 5990664.2123 7.56,3556857.6697 5990664.4272 6.9,3556857.6816 5990664.5353 6.6,3556857.7173 5990664.8584 6.64,3556857.7649 5990665.2895 6.91,3556857.7843 5990665.4652 7.1,3556857.7903 5990665.5193 7.99,3556857.814 5990665.7342 8.78,3556857.8364 5990665.937 9.29,3556857.9317 5990666.7993 9.34,3556857.9675 5990667.1237 9.53,3556858.0629 5990667.9873 9.68,3556858.349 5990670.577 9.67,3556858.6516 5990673.3167 9.46</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952113251">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952113221">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 9.86 5990655.266 3556959.369 0.35 0.0 0.0 0.0
null null 25.58 9.79 5990659.1114 3556958.4001 0.35 0.0 0.0 0.0
null null 51.17 9.57 5990662.9584 3556957.4308 0.35 0.0 0.0 0.0
null null 70.38 9.44 5990665.8462 3556956.7032 0.35 0.0 0.0 0.0
null null 71.18 9.4 5990665.9665 3556956.6729 0.35 0.0 0.0 0.0
null null 71.98 9.3 5990666.0868 3556956.6426 0.35 0.0 0.0 0.0
null null 75.58 9.57 5990666.628 3556956.5062 0.35 12.0 12.0 0.15
low null 77.08 8.49 5990666.8534 3556956.4494 0.25 0.0 0.0 0.0
null null 77.98 8.12 5990666.9887 3556956.4153 0.25 0.0 0.0 0.0
null null 78.68 7.37 5990667.094 3556956.3888 0.12 0.0 0.0 0.0
null null 81.38 6.47 5990667.4999 3556956.2865 0.12 0.0 0.0 0.0
null null 83.78 6.6 5990667.8607 3556956.1956 0.12 0.0 0.0 0.0
null null 86.39 6.8 5990668.253 3556956.0968 0.12 0.0 0.0 0.0
null null 87.29 7.03 5990668.3883 3556956.0627 0.25 0.0 0.0 0.0
null null 87.59 7.64 5990668.4334 3556956.0513 0.25 0.0 0.0 0.0
low null 89.59 8.79 5990668.7341 3556955.9755 0.35 12.0 12.0 0.15
null null 97.59 9.02 5990669.9367 3556955.6725 0.35 0.0 0.0 0.0
null null 126.38 9.16 5990674.2647 3556954.582 0.35 0.0 0.0 0.0
null null 142.35 9.48 5990676.6655 3556953.9771 0.35 0.0 0.0 0.0
null null 171.13 9.55 5990680.992 3556952.887 0.35 0.0 0.0 0.0
null true 177.03 9.61 5990681.8789 3556952.6635 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>65.5000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3556959.369 5990655.266 9.86,3556958.4001 5990659.1114 9.79,3556957.4308 5990662.9584 9.57,3556956.7032 5990665.8462 9.44,3556956.6729 5990665.9665 9.4,3556956.6426 5990666.0868 9.3,3556956.5062 5990666.628 9.57,3556956.4494 5990666.8534 8.49,3556956.4153 5990666.9887 8.12,3556956.3888 5990667.094 7.37,3556956.2865 5990667.4999 6.47,3556956.1956 5990667.8607 6.6,3556956.0968 5990668.253 6.8,3556956.0627 5990668.3883 7.03,3556956.0513 5990668.4334 7.64,3556955.9755 5990668.7341 8.79,3556955.6725 5990669.9367 9.02,3556954.582 5990674.2647 9.16,3556953.9771 5990676.6655 9.48,3556952.887 5990680.992 9.55,3556952.6635 5990681.8789 9.61</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859521163143">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859521163123">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#OBERKANTEWEHR"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.5 5990695.33 3557076.329 0.35 0.0 0.0 0.0 10.5
null null 8.0 10.88 5990696.0061 3557076.0989 0.35 0.0 0.0 0.0 10.88
null null 11.2 10.57 5990696.2766 3557076.0068 0.35 0.0 0.0 0.0 10.57
null null 23.99 10.48 5990697.3576 3557075.6389 0.35 0.0 0.0 0.0 10.48
null null 49.58 10.11 5990699.5204 3557074.9027 0.35 0.0 0.0 0.0 10.11
null null 87.94 9.78 5990702.7625 3557073.7992 0.35 0.0 0.0 0.0 9.78
null null 96.7 9.57 5990703.5029 3557073.5472 0.2 12.0 12.0 0.15 9.57
null null 98.27 8.7 5990703.6356 3557073.5021 0.2 0.0 0.0 0.0 8.7
low null 99.0 8.08 5990703.6973 3557073.4811 0.15 0.0 0.0 0.0 8.08
null null 100.0 7.49 5990703.7818 3557073.4523 0.15 0.0 0.0 0.0 8.08
null null 104.71 7.15 5990704.1799 3557073.3168 0.15 0.0 0.0 0.0 8.08
null null 106.31 7.17 5990704.3151 3557073.2708 0.15 0.0 0.0 0.0 8.08
null null 109.08 7.49 5990704.5493 3557073.1911 0.15 0.0 0.0 0.0 8.08
low null 110.0 8.08 5990704.627 3557073.1646 0.25 0.0 0.0 0.0 8.08
null null 110.67 8.46 5990704.6836 3557073.1453 0.25 0.0 0.0 0.0 8.46
null null 112.27 9.26 5990704.8189 3557073.0993 0.35 0.0 0.0 0.0 9.26
null null 112.67 9.4 5990704.8527 3557073.0878 0.35 0.0 0.0 0.0 9.4
null null 138.27 9.42 5990707.0163 3557072.3514 0.35 0.0 0.0 0.0 9.42
null null 154.26 9.31 5990708.3678 3557071.8914 0.35 0.0 0.0 0.0 9.31
null null 176.66 10.0 5990710.261 3557071.247 0.35 0.0 0.0 0.0 10.0
null true 218.12 9.74 5990713.7651 3557070.0543 0.35 0.0 0.0 0.0 9.74
]]></om:result>
       <prof:station>65.6230</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557076.329 5990695.33 10.5,3557076.0989 5990696.0061 10.88,3557076.0068 5990696.2766 10.57,3557075.6389 5990697.3576 10.48,3557074.9027 5990699.5204 10.11,3557073.7992 5990702.7625 9.78,3557073.5472 5990703.5029 9.57,3557073.5021 5990703.6356 8.7,3557073.4811 5990703.6973 8.08,3557073.4523 5990703.7818 7.49,3557073.3168 5990704.1799 7.15,3557073.2708 5990704.3151 7.17,3557073.1911 5990704.5493 7.49,3557073.1646 5990704.627 8.08,3557073.1453 5990704.6836 8.46,3557073.0993 5990704.8189 9.26,3557073.0878 5990704.8527 9.4,3557072.3514 5990707.0163 9.42,3557071.8914 5990708.3678 9.31,3557071.247 5990710.261 10.0,3557070.0543 5990713.7651 9.74</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
       <prof:member>
        <om:Observation gml:id="Observation118285952128815">
         <gml:description>Bauwerk-Observation</gml:description>
         <gml:name>urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR</gml:name>
         <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR"/>
         <om:resultDefinition>
          <swe:RecordDefinition gml:id="RecordDefinition118285952128890">
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#WEHRART"/>
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#FORMBEIWERT"/>
          </swe:RecordDefinition>
         </om:resultDefinition>
         <om:result><![CDATA[org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_BREITKRONIG 5.0
]]></om:result>
        </om:Observation>
       </prof:member>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859521288129">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859521288127">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null null 0.0 10.74 5990705.31 3557149.594 0.35
null null 25.6 10.79 5990709.5545 3557149.4111 0.35
null true 32.0 10.78 5990710.6156 3557149.3653 0.35
null null 44.8 10.65 5990712.7378 3557149.2739 0.35
null null 57.6 10.41 5990714.8601 3557149.1824 0.35
null null 60.8 10.33 5990715.3907 3557149.1596 0.35
null null 73.6 9.94 5990717.5129 3557149.0681 0.35
null null 97.6 9.74 5990721.4921 3557148.8966 0.35
null null 99.0 9.66 5990721.7242 3557148.8866 0.35
low null 103.9 10.12 5990722.5366 3557148.8516 0.35
null null 105.3 9.1 5990722.7688 3557148.8416 0.2
null null 106.2 8.79 5990722.918 3557148.8351 0.2
null null 106.7 8.35 5990723.0009 3557148.8316 0.2
null null 108.0 7.77 5990723.2164 3557148.8223 0.12
null null 110.4 7.57 5990723.6143 3557148.8051 0.12
null null 111.6 7.36 5990723.8133 3557148.7966 0.12
null null 113.6 7.3 5990724.1449 3557148.7823 0.12
null null 115.2 7.94 5990724.4102 3557148.7708 0.25
null null 115.9 8.77 5990724.5262 3557148.7658 0.25
null null 116.0 8.84 5990724.5428 3557148.7651 0.25
low null 118.1 9.63 5990724.891 3557148.7501 0.35
null null 129.3 9.79 5990726.748 3557148.6701 0.35
null null 143.69 9.68 5990729.1338 3557148.5673 0.35
null null 150.09 9.73 5990730.195 3557148.5215 0.35
null null 175.7 9.8 5990734.4411 3557148.3385 0.35
null null 188.51 9.86 5990736.565 3557148.247 0.35
null true 191.01 9.9 5990736.9795 3557148.2291 0.35
]]></om:result>
       <prof:station>65.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557149.594 5990705.31 10.74,3557149.4111 5990709.5545 10.79,3557149.3653 5990710.6156 10.78,3557149.2739 5990712.7378 10.65,3557149.1824 5990714.8601 10.41,3557149.1596 5990715.3907 10.33,3557149.0681 5990717.5129 9.94,3557148.8966 5990721.4921 9.74,3557148.8866 5990721.7242 9.66,3557148.8516 5990722.5366 10.12,3557148.8416 5990722.7688 9.1,3557148.8351 5990722.918 8.79,3557148.8316 5990723.0009 8.35,3557148.8223 5990723.2164 7.77,3557148.8051 5990723.6143 7.57,3557148.7966 5990723.8133 7.36,3557148.7823 5990724.1449 7.3,3557148.7708 5990724.4102 7.94,3557148.7658 5990724.5262 8.77,3557148.7651 5990724.5428 8.84,3557148.7501 5990724.891 9.63,3557148.6701 5990726.748 9.79,3557148.5673 5990729.1338 9.68,3557148.5215 5990730.195 9.73,3557148.3385 5990734.4411 9.8,3557148.247 5990736.565 9.86,3557148.2291 5990736.9795 9.9</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952139853">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859521398146">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 11.66 5990696.462 3557245.732 0.35
null null 6.39 11.4 5990697.751 3557245.9475 0.35
null null 23.98 10.62 5990701.2993 3557246.5408 0.35
null null 36.77 10.24 5990703.8793 3557246.9721 0.35
null null 49.56 9.84 5990706.4593 3557247.4035 0.35
null null 52.76 9.77 5990707.1049 3557247.5114 0.35
null null 64.75 9.65 5990709.5235 3557247.9158 0.35
null null 66.35 9.78 5990709.8463 3557247.9697 0.35
null null 68.74 9.72 5990710.3284 3557248.0503 0.35
null null 71.14 9.75 5990710.8125 3557248.1313 0.35
low null 71.33 9.68 5990710.8508 3557248.1377 0.35
null null 71.44 9.67 5990710.873 3557248.1414 0.35
null null 71.73 9.7 5990710.9315 3557248.1512 0.35
null null 72.23 9.3 5990711.0324 3557248.1681 0.2
null null 73.23 8.67 5990711.2341 3557248.2018 0.2
null null 73.83 8.15 5990711.3551 3557248.222 0.2
null null 74.83 7.71 5990711.5569 3557248.2557 0.12
null null 76.82 7.55 5990711.9583 3557248.3229 0.12
null null 77.62 7.39 5990712.1197 3557248.3498 0.12
null null 78.02 7.33 5990712.2004 3557248.3633 0.12
null null 80.31 7.21 5990712.6623 3557248.4406 0.12
null null 81.81 8.05 5990712.9649 3557248.4912 0.25
null null 81.91 8.15 5990712.9851 3557248.4945 0.25
null null 82.2 8.55 5990713.0436 3557248.5043 0.25
low null 84.8 9.67 5990713.568 3557248.592 0.35
null null 93.58 9.83 5990715.3391 3557248.8881 0.35
null null 111.17 9.64 5990718.8874 3557249.4814 0.35
null null 136.75 9.88 5990724.0475 3557250.3441 0.35
null null 139.95 9.89 5990724.693 3557250.452 0.35
null true 156.94 9.78 5990728.1203 3557251.025 0.35
]]></om:result>
       <prof:station>65.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557245.732 5990696.462 11.66,3557245.9475 5990697.751 11.4,3557246.5408 5990701.2993 10.62,3557246.9721 5990703.8793 10.24,3557247.4035 5990706.4593 9.84,3557247.5114 5990707.1049 9.77,3557247.9158 5990709.5235 9.65,3557247.9697 5990709.8463 9.78,3557248.0503 5990710.3284 9.72,3557248.1313 5990710.8125 9.75,3557248.1377 5990710.8508 9.68,3557248.1414 5990710.873 9.67,3557248.1512 5990710.9315 9.7,3557248.1681 5990711.0324 9.3,3557248.2018 5990711.2341 8.67,3557248.222 5990711.3551 8.15,3557248.2557 5990711.5569 7.71,3557248.3229 5990711.9583 7.55,3557248.3498 5990712.1197 7.39,3557248.3633 5990712.2004 7.33,3557248.4406 5990712.6623 7.21,3557248.4912 5990712.9649 8.05,3557248.4945 5990712.9851 8.15,3557248.5043 5990713.0436 8.55,3557248.592 5990713.568 9.67,3557248.8881 5990715.3391 9.83,3557249.4814 5990718.8874 9.64,3557250.3441 5990724.0475 9.88,3557250.452 5990724.693 9.89,3557251.025 5990728.1203 9.78</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859521429140">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859521429132">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.3 5990679.676 3557344.758 0.35
null null 27.18 10.06 5990686.6335 3557346.0352 0.35
null null 36.77 9.76 5990689.0884 3557346.4859 0.35
null null 38.37 9.72 5990689.4979 3557346.5611 0.35
null null 46.76 9.75 5990691.6456 3557346.9553 0.35
null null 47.56 9.71 5990691.8504 3557346.9929 0.35
low null 51.95 9.63 5990692.9741 3557347.1992 0.35
null null 52.75 9.39 5990693.1789 3557347.2368 0.25
null null 54.05 8.93 5990693.5117 3557347.2979 0.25
null null 54.25 8.35 5990693.5629 3557347.3073 0.12
null null 56.45 6.95 5990694.126 3557347.4107 0.12
null null 56.55 6.94 5990694.1516 3557347.4154 0.12
null null 60.94 7.56 5990695.2754 3557347.6217 0.12
null null 61.84 7.84 5990695.5058 3557347.664 0.2
null null 61.94 7.89 5990695.5314 3557347.6687 0.2
null null 62.54 8.56 5990695.685 3557347.6969 0.2
null null 62.94 8.86 5990695.7873 3557347.7157 0.2
low null 65.13 9.51 5990696.3479 3557347.8186 0.35
null null 74.7 9.79 5990698.7977 3557348.2683 0.35
null null 93.87 9.67 5990703.7048 3557349.1691 0.35
null true 109.04 10.41 5990707.588 3557349.882 0.35
null null 124.08 10.3 5990711.4379 3557350.5888 0.35
]]></om:result>
       <prof:station>65.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557344.758 5990679.676 10.3,3557346.0352 5990686.6335 10.06,3557346.4859 5990689.0884 9.76,3557346.5611 5990689.4979 9.72,3557346.9553 5990691.6456 9.75,3557346.9929 5990691.8504 9.71,3557347.1992 5990692.9741 9.63,3557347.2368 5990693.1789 9.39,3557347.2979 5990693.5117 8.93,3557347.3073 5990693.5629 8.35,3557347.4107 5990694.126 6.95,3557347.4154 5990694.1516 6.94,3557347.6217 5990695.2754 7.56,3557347.664 5990695.5058 7.84,3557347.6687 5990695.5314 7.89,3557347.6969 5990695.685 8.56,3557347.7157 5990695.7873 8.86,3557347.8186 5990696.3479 9.51,3557348.2683 5990698.7977 9.79,3557349.1691 5990703.7048 9.67,3557349.882 5990707.588 10.41,3557350.5888 5990711.4379 10.3</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859521538149">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952153860">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null null 0.0 11.18 5990660.185 3557443.363 0.35
null null 3.2 11.34 5990661.0035 3557443.4451 0.35
null true 12.59 11.54 5990663.4054 3557443.686 0.35
null null 23.79 10.43 5990666.2703 3557443.9733 0.35
null null 38.18 9.71 5990669.9512 3557444.3424 0.35
null null 42.98 9.58 5990671.179 3557444.4656 0.35
null null 55.77 9.66 5990674.4506 3557444.7937 0.35
low null 68.76 9.68 5990677.7734 3557445.1269 0.35
null null 71.16 9.14 5990678.3873 3557445.1885 0.3
null null 71.26 9.04 5990678.4129 3557445.1911 0.3
null null 71.76 8.26 5990678.5408 3557445.2039 0.3
null null 71.86 8.17 5990678.5664 3557445.2065 0.3
null null 72.66 7.86 5990678.771 3557445.227 0.3
null null 73.46 7.56 5990678.9756 3557445.2475 0.12
null null 74.26 7.39 5990679.1803 3557445.268 0.12
null null 74.66 7.29 5990679.2826 3557445.2783 0.12
null null 75.76 6.96 5990679.5639 3557445.3065 0.12
null null 76.95 6.96 5990679.8683 3557445.337 0.12
null null 79.75 8.9 5990680.5846 3557445.4089 0.2
null null 81.35 9.62 5990680.9938 3557445.4499 0.35
null null 81.75 9.81 5990681.0962 3557445.4602 0.35
null null 82.15 10.12 5990681.1985 3557445.4704 0.35
low null 82.45 10.3 5990681.2752 3557445.4781 0.35
null null 88.44 9.88 5990682.8074 3557445.6318 0.35
null null 89.24 9.9 5990683.0121 3557445.6523 0.35
null null 103.61 9.72 5990686.6878 3557446.021 0.35
null null 116.4 9.81 5990689.9594 3557446.3491 0.35
null null 132.38 9.96 5990694.047 3557446.759 0.35
null true 145.47 9.9 5990697.3953 3557447.0948 0.35
]]></om:result>
       <prof:station>66.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557443.363 5990660.185 11.18,3557443.4451 5990661.0035 11.34,3557443.686 5990663.4054 11.54,3557443.9733 5990666.2703 10.43,3557444.3424 5990669.9512 9.71,3557444.4656 5990671.179 9.58,3557444.7937 5990674.4506 9.66,3557445.1269 5990677.7734 9.68,3557445.1885 5990678.3873 9.14,3557445.1911 5990678.4129 9.04,3557445.2039 5990678.5408 8.26,3557445.2065 5990678.5664 8.17,3557445.227 5990678.771 7.86,3557445.2475 5990678.9756 7.56,3557445.268 5990679.1803 7.39,3557445.2783 5990679.2826 7.29,3557445.3065 5990679.5639 6.96,3557445.337 5990679.8683 6.96,3557445.4089 5990680.5846 8.9,3557445.4499 5990680.9938 9.62,3557445.4602 5990681.0962 9.81,3557445.4704 5990681.1985 10.12,3557445.4781 5990681.2752 10.3,3557445.6318 5990682.8074 9.88,3557445.6523 5990683.0121 9.9,3557446.021 5990686.6878 9.72,3557446.3491 5990689.9594 9.81,3557446.759 5990694.047 9.96,3557447.0948 5990697.3953 9.9</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952155474">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952155414">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 9.73 5990666.07 3557546.726 0.35
null null 12.8 9.83 5990668.7686 3557545.9528 0.35
null null 25.59 9.95 5990671.4651 3557545.1801 0.35
null null 41.59 9.89 5990674.8383 3557544.2136 0.35
null null 54.39 9.96 5990677.5369 3557543.4404 0.35
null null 60.78 9.95 5990678.8841 3557543.0543 0.35
null null 62.58 9.89 5990679.2636 3557542.9456 0.35
null null 65.18 10.14 5990679.8117 3557542.7885 0.35
null null 66.78 10.01 5990680.149 3557542.6919 0.35
low null 68.68 9.99 5990680.5496 3557542.5771 0.35
null null 70.27 9.5 5990680.8848 3557542.4811 0.3
null null 71.67 9.02 5990681.18 3557542.3965 0.3
null null 72.07 8.37 5990681.2643 3557542.3723 0.3
null null 74.47 7.5 5990681.7703 3557542.2273 0.12
null null 76.87 7.7 5990682.2763 3557542.0824 0.12
null null 78.47 7.76 5990682.6136 3557541.9857 0.12
null null 79.56 8.11 5990682.8434 3557541.9199 0.2
null null 80.36 9.09 5990683.0121 3557541.8715 0.2
null null 81.56 9.21 5990683.2651 3557541.799 0.35
low null 82.66 9.69 5990683.497 3557541.7326 0.35
null null 89.86 9.79 5990685.0149 3557541.2977 0.35
null null 91.45 9.87 5990685.3502 3557541.2016 0.35
null null 107.44 9.88 5990688.7213 3557540.2357 0.35
null null 113.83 10.32 5990690.0685 3557539.8497 0.35
null null 120.23 10.72 5990691.4178 3557539.463 0.35
null true 137.43 11.92 5990695.044 3557538.424 0.35
null null 164.22 11.19 5990700.6921 3557536.8056 0.35
]]></om:result>
       <prof:station>66.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557546.726 5990666.07 9.73,3557545.9528 5990668.7686 9.83,3557545.1801 5990671.4651 9.95,3557544.2136 5990674.8383 9.89,3557543.4404 5990677.5369 9.96,3557543.0543 5990678.8841 9.95,3557542.9456 5990679.2636 9.89,3557542.7885 5990679.8117 10.14,3557542.6919 5990680.149 10.01,3557542.5771 5990680.5496 9.99,3557542.4811 5990680.8848 9.5,3557542.3965 5990681.18 9.02,3557542.3723 5990681.2643 8.37,3557542.2273 5990681.7703 7.5,3557542.0824 5990682.2763 7.7,3557541.9857 5990682.6136 7.76,3557541.9199 5990682.8434 8.11,3557541.8715 5990683.0121 9.09,3557541.799 5990683.2651 9.21,3557541.7326 5990683.497 9.69,3557541.2977 5990685.0149 9.79,3557541.2016 5990685.3502 9.87,3557540.2357 5990688.7213 9.88,3557539.8497 5990690.0685 10.32,3557539.463 5990691.4178 10.72,3557538.424 5990695.044 11.92,3557536.8056 5990700.6921 11.19</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile11828595216632">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859521663143">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 9.87 5990704.357 3557643.822 0.35 0.0 0.0 0.0
null null 17.59 9.77 5990708.8035 3557641.313 0.35 0.0 0.0 0.0
null null 19.2 9.69 5990709.2105 3557641.0834 0.35 0.0 0.0 0.0
null null 26.39 9.52 5990711.028 3557640.0578 0.35 0.0 0.0 0.0
null null 27.59 9.45 5990711.3314 3557639.8866 0.35 0.0 0.0 0.0
null null 30.79 9.54 5990712.1403 3557639.4302 0.35 0.0 0.0 0.0
null null 43.59 9.82 5990715.3759 3557637.6044 0.35 0.0 0.0 0.0
null null 62.78 10.19 5990720.2269 3557634.8672 0.35 0.0 0.0 0.0
null null 64.38 10.13 5990720.6313 3557634.639 0.35 0.0 0.0 0.0
null null 65.98 10.05 5990721.0358 3557634.4108 0.35 0.0 0.0 0.0
null null 69.18 9.81 5990721.8447 3557633.9543 0.35 3.0 3.0 0.2
low null 73.97 9.96 5990723.0555 3557633.2711 0.35 0.0 0.0 0.0
null null 75.17 9.33 5990723.3589 3557633.0999 0.3 0.0 0.0 0.0
null null 76.37 8.96 5990723.6622 3557632.9288 0.3 0.0 0.0 0.0
null null 77.07 8.04 5990723.8392 3557632.8289 0.3 0.0 0.0 0.0
null null 77.17 7.99 5990723.8645 3557632.8147 0.3 0.0 0.0 0.0
null null 77.57 7.89 5990723.9656 3557632.7576 0.12 0.0 0.0 0.0
null null 77.77 7.86 5990724.0161 3557632.7291 0.12 0.0 0.0 0.0
null null 78.57 7.87 5990724.2184 3557632.615 0.12 0.0 0.0 0.0
null null 81.77 7.63 5990725.0273 3557632.1585 0.12 0.0 0.0 0.0
null null 82.87 7.58 5990725.3053 3557632.0016 0.12 0.0 0.0 0.0
null null 83.67 7.86 5990725.5076 3557631.8875 0.2 0.0 0.0 0.0
null null 84.06 8.03 5990725.6061 3557631.8319 0.2 0.0 0.0 0.0
null null 85.06 8.97 5990725.8589 3557631.6893 0.2 0.0 0.0 0.0
low null 87.06 9.66 5990726.3645 3557631.404 0.35 0.0 0.0 0.0
null null 93.46 9.57 5990727.9823 3557630.4911 0.35 0.0 0.0 0.0
null null 96.65 9.58 5990728.7887 3557630.0361 0.35 0.0 0.0 0.0
null null 104.65 9.64 5990730.811 3557628.895 0.35 0.0 0.0 0.0
null null 106.24 9.71 5990731.2129 3557628.6682 0.35 0.0 0.0 0.0
null null 120.64 10.53 5990734.853 3557626.6142 0.35 0.0 0.0 0.0
null true 165.54 10.83 5990746.2031 3557620.2098 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>66.2000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557643.822 5990704.357 9.87,3557641.313 5990708.8035 9.77,3557641.0834 5990709.2105 9.69,3557640.0578 5990711.028 9.52,3557639.8866 5990711.3314 9.45,3557639.4302 5990712.1403 9.54,3557637.6044 5990715.3759 9.82,3557634.8672 5990720.2269 10.19,3557634.639 5990720.6313 10.13,3557634.4108 5990721.0358 10.05,3557633.9543 5990721.8447 9.81,3557633.2711 5990723.0555 9.96,3557633.0999 5990723.3589 9.33,3557632.9288 5990723.6622 8.96,3557632.8289 5990723.8392 8.04,3557632.8147 5990723.8645 7.99,3557632.7576 5990723.9656 7.89,3557632.7291 5990724.0161 7.86,3557632.615 5990724.2184 7.87,3557632.1585 5990725.0273 7.63,3557632.0016 5990725.3053 7.58,3557631.8875 5990725.5076 7.86,3557631.8319 5990725.6061 8.03,3557631.6893 5990725.8589 8.97,3557631.404 5990726.3645 9.66,3557630.4911 5990727.9823 9.57,3557630.0361 5990728.7887 9.58,3557628.895 5990730.811 9.64,3557628.6682 5990731.2129 9.71,3557626.6142 5990734.853 10.53,3557620.2098 5990746.2031 10.83</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952169514">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859521695217">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 9.96 5990771.195 3557721.56 0.35 0.0 0.0 0.0
null null 35.17 10.1 5990775.873 3557719.1399 0.35 0.0 0.0 0.0
null null 43.97 9.68 5990777.0435 3557718.5343 0.35 0.0 0.0 0.0
null null 47.17 9.7 5990777.4691 3557718.3141 0.35 0.0 0.0 0.0
null null 66.38 9.66 5990780.0243 3557716.9922 0.35 0.0 0.0 0.0
null null 79.19 9.63 5990781.7281 3557716.1107 0.35 0.0 0.0 0.0
null null 86.39 9.56 5990782.6858 3557715.6153 0.35 0.0 0.0 0.0
null null 91.97 9.67 5990783.428 3557715.2313 0.35 0.0 0.0 0.0
null null 94.36 9.61 5990783.7459 3557715.0668 0.35 0.0 0.0 0.0
null null 98.74 10.01 5990784.3285 3557714.7654 0.35 0.0 0.0 0.0
null null 99.44 9.65 5990784.4216 3557714.7173 0.35 8.0 8.0 0.15
low null 100.03 9.47 5990784.5001 3557714.6767 0.2 0.0 0.0 0.0
null null 101.02 8.81 5990784.6318 3557714.6085 0.2 0.0 0.0 0.0
null null 101.51 8.24 5990784.6969 3557714.5748 0.12 0.0 0.0 0.0
null null 101.61 8.18 5990784.7102 3557714.5679 0.12 0.0 0.0 0.0
null null 103.19 7.97 5990784.9204 3557714.4592 0.12 0.0 0.0 0.0
null null 103.58 7.94 5990784.9723 3557714.4324 0.12 0.0 0.0 0.0
null null 107.74 7.95 5990785.5256 3557714.1461 0.12 0.0 0.0 0.0
null null 109.13 8.24 5990785.7105 3557714.0505 0.2 0.0 0.0 0.0
null null 110.12 8.95 5990785.8422 3557713.9823 0.2 0.0 0.0 0.0
low null 112.3 9.84 5990786.1321 3557713.8323 0.35 0.0 0.0 0.0
null null 118.68 9.96 5990786.9807 3557713.3933 0.35 0.0 0.0 0.0
null null 131.44 10.12 5990788.678 3557712.5153 0.35 0.0 0.0 0.0
null null 144.22 10.24 5990790.3778 3557711.6358 0.35 0.0 0.0 0.0
null null 150.61 10.25 5990791.2278 3557711.1961 0.35 0.0 0.0 0.0
null true 163.4 10.32 5990792.929 3557710.316 0.35 0.0 0.0 0.0
null null 186.13 10.32 5990795.9523 3557708.7519 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>66.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557721.56 5990771.195 9.96,3557719.1399 5990775.873 10.1,3557718.5343 5990777.0435 9.68,3557718.3141 5990777.4691 9.7,3557716.9922 5990780.0243 9.66,3557716.1107 5990781.7281 9.63,3557715.6153 5990782.6858 9.56,3557715.2313 5990783.428 9.67,3557715.0668 5990783.7459 9.61,3557714.7654 5990784.3285 10.01,3557714.7173 5990784.4216 9.65,3557714.6767 5990784.5001 9.47,3557714.6085 5990784.6318 8.81,3557714.5748 5990784.6969 8.24,3557714.5679 5990784.7102 8.18,3557714.4592 5990784.9204 7.97,3557714.4324 5990784.9723 7.94,3557714.1461 5990785.5256 7.95,3557714.0505 5990785.7105 8.24,3557713.9823 5990785.8422 8.95,3557713.8323 5990786.1321 9.84,3557713.3933 5990786.9807 9.96,3557712.5153 5990788.678 10.12,3557711.6358 5990790.3778 10.24,3557711.1961 5990791.2278 10.25,3557710.316 5990792.929 10.32,3557708.7519 5990795.9523 10.32</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859521820143">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859521820195">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.24 5990836.924 3557796.145 0.35 0.0 0.0 0.0
null null 6.41 10.27 5990837.9896 3557795.2989 0.35 0.0 0.0 0.0
null null 33.62 10.13 5990842.5129 3557791.7074 0.35 0.0 0.0 0.0
null null 35.22 10.09 5990842.7789 3557791.4962 0.35 0.0 0.0 0.0
null null 67.17 10.21 5990848.0902 3557787.279 0.35 0.0 0.0 0.0
null null 79.93 10.39 5990850.2114 3557785.5948 0.35 0.0 0.0 0.0
null null 83.14 10.39 5990850.745 3557785.1711 0.35 0.0 0.0 0.0
null null 83.94 10.35 5990850.878 3557785.0655 0.35 0.0 0.0 0.0
null null 92.25 10.49 5990852.2594 3557783.9686 0.35 8.0 8.0 0.2
low null 93.34 9.87 5990852.4406 3557783.8247 0.35 0.0 0.0 0.0
null null 93.44 9.77 5990852.4573 3557783.8115 0.35 0.0 0.0 0.0
null null 93.83 9.61 5990852.5221 3557783.7601 0.2 0.0 0.0 0.0
null null 95.02 8.98 5990852.7199 3557783.603 0.2 0.0 0.0 0.0
null null 95.61 8.04 5990852.818 3557783.5251 0.12 0.0 0.0 0.0
null null 95.71 7.97 5990852.8346 3557783.5119 0.12 0.0 0.0 0.0
null null 97.3 7.82 5990853.0989 3557783.3021 0.12 0.0 0.0 0.0
null null 98.88 7.72 5990853.3616 3557783.0935 0.12 0.0 0.0 0.0
null null 100.06 7.69 5990853.5578 3557782.9378 0.12 0.0 0.0 0.0
null null 100.85 7.77 5990853.6891 3557782.8335 0.12 0.0 0.0 0.0
null null 102.43 8.11 5990853.9517 3557782.6249 0.12 0.0 0.0 0.0
null null 102.93 8.28 5990854.0349 3557782.5589 0.12 0.0 0.0 0.0
null null 103.32 8.57 5990854.0997 3557782.5075 0.25 0.0 0.0 0.0
null null 104.11 8.98 5990854.231 3557782.4032 0.25 0.0 0.0 0.0
low null 106.11 9.67 5990854.5635 3557782.1392 0.25 0.0 0.0 0.0
null null 114.1 9.95 5990855.8917 3557781.0846 0.35 0.0 0.0 0.0
null null 127.66 9.8 5990858.1459 3557779.2947 0.35 0.0 0.0 0.0
null true 149.25 10.47 5990861.735 3557776.445 0.35 0.0 0.0 0.0
null null 188.99 10.3 5990868.3413 3557771.1996 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>66.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557796.145 5990836.924 10.24,3557795.2989 5990837.9896 10.27,3557791.7074 5990842.5129 10.13,3557791.4962 5990842.7789 10.09,3557787.279 5990848.0902 10.21,3557785.5948 5990850.2114 10.39,3557785.1711 5990850.745 10.39,3557785.0655 5990850.878 10.35,3557783.9686 5990852.2594 10.49,3557783.8247 5990852.4406 9.87,3557783.8115 5990852.4573 9.77,3557783.7601 5990852.5221 9.61,3557783.603 5990852.7199 8.98,3557783.5251 5990852.818 8.04,3557783.5119 5990852.8346 7.97,3557783.3021 5990853.0989 7.82,3557783.0935 5990853.3616 7.72,3557782.9378 5990853.5578 7.69,3557782.8335 5990853.6891 7.77,3557782.6249 5990853.9517 8.11,3557782.5589 5990854.0349 8.28,3557782.5075 5990854.0997 8.57,3557782.4032 5990854.231 8.98,3557782.1392 5990854.5635 9.67,3557781.0846 5990855.8917 9.95,3557779.2947 5990858.1459 9.8,3557776.445 5990861.735 10.47,3557771.1996 5990868.3413 10.3</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859521929165">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859521929128">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.16 5990908.332 3557867.277 0.35 0.0 0.0 0.0
null null 35.17 10.32 5990913.6838 3557863.6145 0.35 0.0 0.0 0.0
null null 47.96 10.27 5990915.63 3557862.2825 0.35 0.0 0.0 0.0
null null 51.16 10.24 5990916.1169 3557861.9493 0.35 0.0 0.0 0.0
null null 66.74 9.88 5990918.4877 3557860.3268 0.35 0.0 0.0 0.0
null null 71.83 10.35 5990919.2622 3557859.7968 0.35 5.0 5.0 0.2
low null 72.03 10.24 5990919.2927 3557859.7759 0.35 0.0 0.0 0.0
null null 72.42 9.92 5990919.352 3557859.7353 0.2 0.0 0.0 0.0
null null 74.01 9.2 5990919.594 3557859.5697 0.2 0.0 0.0 0.0
null null 74.6 8.91 5990919.6838 3557859.5083 0.2 0.0 0.0 0.0
null null 75.09 8.38 5990919.7583 3557859.4573 0.12 0.0 0.0 0.0
null null 75.19 8.34 5990919.7735 3557859.4469 0.12 0.0 0.0 0.0
null null 75.98 8.3 5990919.8937 3557859.3646 0.12 0.0 0.0 0.0
null null 76.78 8.28 5990920.0155 3557859.2813 0.12 0.0 0.0 0.0
null null 79.97 8.01 5990920.5009 3557858.9491 0.12 0.0 0.0 0.0
null null 81.47 7.78 5990920.7291 3557858.7929 0.12 0.0 0.0 0.0
null null 82.87 8.02 5990920.9422 3557858.6471 0.25 0.0 0.0 0.0
null null 83.88 9.1 5990921.0959 3557858.5419 0.25 0.0 0.0 0.0
low null 85.47 9.67 5990921.3378 3557858.3763 0.35 0.0 0.0 0.0
null null 91.88 9.56 5990922.3132 3557857.7088 0.35 0.0 0.0 0.0
null null 95.07 9.54 5990922.7986 3557857.3766 0.35 0.0 0.0 0.0
null null 111.03 9.54 5990925.2272 3557855.7145 0.35 0.0 0.0 0.0
null null 133.4 10.39 5990928.6312 3557853.385 0.35 0.0 0.0 0.0
null null 146.2 10.37 5990930.579 3557852.052 0.35 0.0 0.0 0.0
null true 160.36 10.41 5990932.7337 3557850.5774 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>66.5000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557867.277 5990908.332 10.16,3557863.6145 5990913.6838 10.32,3557862.2825 5990915.63 10.27,3557861.9493 5990916.1169 10.24,3557860.3268 5990918.4877 9.88,3557859.7968 5990919.2622 10.35,3557859.7759 5990919.2927 10.24,3557859.7353 5990919.352 9.92,3557859.5697 5990919.594 9.2,3557859.5083 5990919.6838 8.91,3557859.4573 5990919.7583 8.38,3557859.4469 5990919.7735 8.34,3557859.3646 5990919.8937 8.3,3557859.2813 5990920.0155 8.28,3557858.9491 5990920.5009 8.01,3557858.7929 5990920.7291 7.78,3557858.6471 5990920.9422 8.02,3557858.5419 5990921.0959 9.1,3557858.3763 5990921.3378 9.67,3557857.7088 5990922.3132 9.56,3557857.3766 5990922.7986 9.54,3557855.7145 5990925.2272 9.54,3557853.385 5990928.6312 10.39,3557852.052 5990930.579 10.37,3557850.5774 5990932.7337 10.41</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859521960114">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952196052">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.38 5990969.466 3557941.996 0.35 0.0 0.0 0.0
null null 23.99 10.57 5990973.7724 3557939.3146 0.35 0.0 0.0 0.0
null null 55.18 10.25 5990979.3712 3557935.8284 0.35 0.0 0.0 0.0
null null 55.58 10.23 5990979.443 3557935.7837 0.35 0.0 0.0 0.0
null null 60.49 10.32 5990980.3244 3557935.2349 0.35 5.0 5.0 0.2
low null 62.28 9.51 5990980.6457 3557935.0348 0.2 0.0 0.0 0.0
null null 63.17 8.92 5990980.8055 3557934.9354 0.2 0.0 0.0 0.0
null null 63.57 8.48 5990980.8773 3557934.8906 0.12 0.0 0.0 0.0
null null 65.95 7.91 5990981.3045 3557934.6246 0.12 0.0 0.0 0.0
null null 67.54 7.8 5990981.5899 3557934.4469 0.12 0.0 0.0 0.0
null null 69.12 7.75 5990981.8736 3557934.2703 0.12 0.0 0.0 0.0
null null 69.92 7.75 5990982.0172 3557934.1809 0.12 0.0 0.0 0.0
null null 71.42 7.98 5990982.2864 3557934.0132 0.12 0.0 0.0 0.0
null null 71.92 8.55 5990982.3762 3557933.9574 0.25 0.0 0.0 0.0
null null 72.32 8.89 5990982.448 3557933.9126 0.25 0.0 0.0 0.0
null null 72.72 9.13 5990982.5198 3557933.8679 0.25 0.0 0.0 0.0
low null 74.13 9.84 5990982.7729 3557933.7103 0.35 0.0 0.0 0.0
null null 80.53 9.87 5990983.9217 3557932.995 0.35 0.0 0.0 0.0
null null 96.49 9.83 5990986.7867 3557931.2111 0.35 0.0 0.0 0.0
null null 98.09 9.84 5990987.0739 3557931.0323 0.35 0.0 0.0 0.0
null null 110.87 10.16 5990989.368 3557929.6038 0.35 0.0 0.0 0.0
null null 123.66 10.52 5990991.6639 3557928.1743 0.35 0.0 0.0 0.0
null true 135.66 10.81 5990993.818 3557926.833 0.35 0.0 0.0 0.0
null null 142.96 10.66 5990995.1284 3557926.0171 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>66.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3557941.996 5990969.466 10.38,3557939.3146 5990973.7724 10.57,3557935.8284 5990979.3712 10.25,3557935.7837 5990979.443 10.23,3557935.2349 5990980.3244 10.32,3557935.0348 5990980.6457 9.51,3557934.9354 5990980.8055 8.92,3557934.8906 5990980.8773 8.48,3557934.6246 5990981.3045 7.91,3557934.4469 5990981.5899 7.8,3557934.2703 5990981.8736 7.75,3557934.1809 5990982.0172 7.75,3557934.0132 5990982.2864 7.98,3557933.9574 5990982.3762 8.55,3557933.9126 5990982.448 8.89,3557933.8679 5990982.5198 9.13,3557933.7103 5990982.7729 9.84,3557932.995 5990983.9217 9.87,3557931.2111 5990986.7867 9.83,3557931.0323 5990987.0739 9.84,3557929.6038 5990989.368 10.16,3557928.1743 5990991.6639 10.52,3557926.833 5990993.818 10.81,3557926.0171 5990995.1284 10.66</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859522085192">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952208541">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.24 5991017.162 3558028.454 0.35 0.0 0.0 0.0
null null 25.59 10.37 5991022.9944 3558026.1605 0.35 0.0 0.0 0.0
null null 38.39 10.37 5991025.9117 3558025.0133 0.35 0.0 0.0 0.0
null null 54.39 10.33 5991029.5584 3558023.5793 0.35 0.0 0.0 0.0
null null 55.99 10.29 5991029.923 3558023.4359 0.35 0.0 0.0 0.0
null null 62.37 10.83 5991031.3771 3558022.8641 0.35 5.0 5.0 0.2
low null 63.77 9.79 5991031.6962 3558022.7386 0.2 0.0 0.0 0.0
null null 64.97 9.18 5991031.9697 3558022.6311 0.2 0.0 0.0 0.0
null null 65.67 8.6 5991032.1293 3558022.5683 0.12 0.0 0.0 0.0
null null 67.27 8.1 5991032.4939 3558022.4249 0.12 0.0 0.0 0.0
null null 68.07 7.93 5991032.6763 3558022.3532 0.12 0.0 0.0 0.0
null null 68.47 7.87 5991032.7674 3558022.3174 0.12 0.0 0.0 0.0
null null 69.88 7.79 5991033.0888 3558022.191 0.12 0.0 0.0 0.0
null null 72.58 7.93 5991033.7042 3558021.949 0.12 0.0 0.0 0.0
null null 73.76 8.59 5991033.9731 3558021.8432 0.25 0.0 0.0 0.0
null null 75.43 9.75 5991034.3537 3558021.6936 0.35 0.0 0.0 0.0
low null 76.12 10.1 5991034.511 3558021.6317 0.35 0.0 0.0 0.0
null null 82.5 10.41 5991035.9651 3558021.0599 0.35 0.0 0.0 0.0
null null 85.69 10.55 5991036.6921 3558020.774 0.35 0.0 0.0 0.0
null null 111.26 10.57 5991042.52 3558018.4823 0.35 0.0 0.0 0.0
null null 124.05 10.56 5991045.435 3558017.336 0.35 0.0 0.0 0.0
null true 141.23 10.36 5991049.3506 3558015.7962 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>66.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558028.454 5991017.162 10.24,3558026.1605 5991022.9944 10.37,3558025.0133 5991025.9117 10.37,3558023.5793 5991029.5584 10.33,3558023.4359 5991029.923 10.29,3558022.8641 5991031.3771 10.83,3558022.7386 5991031.6962 9.79,3558022.6311 5991031.9697 9.18,3558022.5683 5991032.1293 8.6,3558022.4249 5991032.4939 8.1,3558022.3532 5991032.6763 7.93,3558022.3174 5991032.7674 7.87,3558022.191 5991033.0888 7.79,3558021.949 5991033.7042 7.93,3558021.8432 5991033.9731 8.59,3558021.6936 5991034.3537 9.75,3558021.6317 5991034.511 10.1,3558021.0599 5991035.9651 10.41,3558020.774 5991036.6921 10.55,3558018.4823 5991042.52 10.57,3558017.336 5991045.435 10.56,3558015.7962 5991049.3506 10.36</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859522195179">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859522195122">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.13 5991057.516 3558118.692 0.35
null null 12.79 10.15 5991059.7551 3558118.05 0.35
null null 35.18 10.08 5991063.675 3558116.9261 0.35
null null 38.38 10.1 5991064.2352 3558116.7655 0.35
null null 52.77 10.03 5991066.7544 3558116.0432 0.35
null null 65.12 10.37 5991068.9165 3558115.4233 0.35
null null 65.72 10.34 5991069.0216 3558115.3931 0.35
null null 68.5 10.75 5991069.5083 3558115.2536 0.35
low null 69.69 10.86 5991069.7166 3558115.1939 0.35
null null 71.28 9.76 5991069.995 3558115.1141 0.2
null null 72.47 9.31 5991070.2033 3558115.0543 0.15
null null 72.57 9.21 5991070.2208 3558115.0493 0.15
null null 72.97 8.67 5991070.2908 3558115.0292 0.15
null null 74.76 7.79 5991070.6042 3558114.9394 0.15
null null 76.96 7.8 5991070.9894 3558114.8289 0.15
null null 80.04 8.38 5991071.5286 3558114.6743 0.15
null null 80.84 8.78 5991071.6686 3558114.6342 0.2
null null 82.42 9.73 5991071.9453 3558114.5549 0.35
low null 84.02 10.26 5991072.2254 3558114.4746 0.35
null null 90.41 10.28 5991073.3441 3558114.1538 0.35
null null 115.98 10.26 5991077.8206 3558112.8703 0.35
null null 122.38 10.21 5991078.941 3558112.5491 0.35
null null 135.16 10.06 5991081.1784 3558111.9076 0.35
null null 136.76 10.05 5991081.4586 3558111.8273 0.35
null null 143.16 10.18 5991082.579 3558111.506 0.35
null true 157.58 10.51 5991085.1035 3558110.7822 0.35
]]></om:result>
       <prof:station>66.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558118.692 5991057.516 10.13,3558118.05 5991059.7551 10.15,3558116.9261 5991063.675 10.08,3558116.7655 5991064.2352 10.1,3558116.0432 5991066.7544 10.03,3558115.4233 5991068.9165 10.37,3558115.3931 5991069.0216 10.34,3558115.2536 5991069.5083 10.75,3558115.1939 5991069.7166 10.86,3558115.1141 5991069.995 9.76,3558115.0543 5991070.2033 9.31,3558115.0493 5991070.2208 9.21,3558115.0292 5991070.2908 8.67,3558114.9394 5991070.6042 7.79,3558114.8289 5991070.9894 7.8,3558114.6743 5991071.5286 8.38,3558114.6342 5991071.6686 8.78,3558114.5549 5991071.9453 9.73,3558114.4746 5991072.2254 10.26,3558114.1538 5991073.3441 10.28,3558112.8703 5991077.8206 10.26,3558112.5491 5991078.941 10.21,3558111.9076 5991081.1784 10.06,3558111.8273 5991081.4586 10.05,3558111.506 5991082.579 10.18,3558110.7822 5991085.1035 10.51</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952221019">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859522210204">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.23 5991088.932 3558212.332 0.35
null null 2.4 10.17 5991089.3406 3558212.2032 0.35
null null 12.01 10.16 5991090.9768 3558211.6874 0.35
null null 24.81 10.03 5991093.1562 3558211.0004 0.35
null null 34.42 10.19 5991094.7924 3558210.4846 0.35
null null 60.04 10.1 5991099.1545 3558209.1096 0.35
low null 69.02 10.1 5991100.6834 3558208.6276 0.35
null null 70.22 9.72 5991100.8878 3558208.5632 0.2
null null 71.61 8.71 5991101.1244 3558208.4886 0.15
null null 73.22 8.46 5991101.3985 3558208.4022 0.15
null null 73.62 8.43 5991101.4667 3558208.3807 0.15
null null 76.03 8.37 5991101.877 3558208.2514 0.15
null null 78.4 8.5 5991102.2805 3558208.1242 0.15
null null 79.79 8.89 5991102.5172 3558208.0496 0.2
null null 81.37 9.76 5991102.7862 3558207.9648 0.35
low null 82.77 10.4 5991103.0245 3558207.8896 0.35
null null 90.77 10.64 5991104.3866 3558207.4602 0.35
null null 92.37 10.66 5991104.6591 3558207.3744 0.35
null null 110.77 10.53 5991107.7919 3558206.3868 0.35
null null 111.57 10.57 5991107.9281 3558206.3439 0.35
null null 132.38 10.79 5991111.4712 3558205.227 0.35
null true 136.17 11.17 5991112.1165 3558205.0236 0.35
null null 163.29 10.9 5991116.734 3558203.568 0.35
null null 165.88 10.81 5991117.175 3558203.429 0.35
]]></om:result>
       <prof:station>66.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558212.332 5991088.932 10.23,3558212.2032 5991089.3406 10.17,3558211.6874 5991090.9768 10.16,3558211.0004 5991093.1562 10.03,3558210.4846 5991094.7924 10.19,3558209.1096 5991099.1545 10.1,3558208.6276 5991100.6834 10.1,3558208.5632 5991100.8878 9.72,3558208.4886 5991101.1244 8.71,3558208.4022 5991101.3985 8.46,3558208.3807 5991101.4667 8.43,3558208.2514 5991101.877 8.37,3558208.1242 5991102.2805 8.5,3558208.0496 5991102.5172 8.89,3558207.9648 5991102.7862 9.76,3558207.8896 5991103.0245 10.4,3558207.4602 5991104.3866 10.64,3558207.3744 5991104.6591 10.66,3558206.3868 5991107.7919 10.53,3558206.3439 5991107.9281 10.57,3558205.227 5991111.4712 10.79,3558205.0236 5991112.1165 11.17,3558203.568 5991116.734 10.9,3558203.429 5991117.175 10.81</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952232024">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952232023">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#OBERKANTEWEHR"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.25 5991105.507 3558259.355 0.35 10.25
null null 4.8 10.12 5991106.5677 3558259.0098 0.35 10.12
null null 20.79 10.02 5991110.1013 3558257.8599 0.35 10.02
null null 23.99 10.03 5991110.8084 3558257.6298 0.35 10.03
null null 51.17 10.67 5991116.8148 3558255.6752 0.35 10.67
null null 57.06 10.57 5991118.1164 3558255.2517 0.2 10.57
null null 58.36 9.79 5991118.4037 3558255.1582 0.15 9.79
low null 59.5 9.13 5991118.6556 3558255.0762 0.15 9.13
null null 60.06 8.48 5991118.7793 3558255.0359 0.15 9.13
null null 60.16 8.45 5991118.8014 3558255.0287 0.15 9.13
null null 63.35 8.28 5991119.5064 3558254.7993 0.15 9.13
null null 65.75 8.32 5991120.0367 3558254.6267 0.15 9.13
null null 66.15 8.34 5991120.1251 3558254.598 0.15 9.13
null null 66.55 8.4 5991120.2135 3558254.5692 0.15 9.13
null null 67.75 8.43 5991120.4787 3558254.4829 0.15 9.13
null null 68.75 8.55 5991120.6997 3558254.411 0.15 9.13
null null 69.74 8.92 5991120.9185 3558254.3398 0.15 9.13
low null 70.0 9.13 5991120.9759 3558254.3211 0.15 9.13
null null 71.14 9.88 5991121.2278 3558254.2391 0.2 9.88
null null 72.64 10.57 5991121.5593 3558254.1313 0.35 10.57
null null 83.82 10.56 5991124.0299 3558253.3273 0.35 10.56
null null 96.61 10.35 5991126.8563 3558252.4075 0.35 10.35
null null 102.2 10.32 5991128.0916 3558252.0055 0.35 10.32
null null 122.97 10.9 5991132.6815 3558250.5119 0.35 10.9
null null 126.17 10.94 5991133.3886 3558250.2818 0.35 10.94
null null 138.96 10.83 5991136.215 3558249.362 0.35 10.83
null true 147.85 10.77 5991138.1796 3558248.7227 0.35 10.77
]]></om:result>
       <prof:station>66.9500</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558259.355 5991105.507 10.25,3558259.0098 5991106.5677 10.12,3558257.8599 5991110.1013 10.02,3558257.6298 5991110.8084 10.03,3558255.6752 5991116.8148 10.67,3558255.2517 5991118.1164 10.57,3558255.1582 5991118.4037 9.79,3558255.0762 5991118.6556 9.13,3558255.0359 5991118.7793 8.48,3558255.0287 5991118.8014 8.45,3558254.7993 5991119.5064 8.28,3558254.6267 5991120.0367 8.32,3558254.598 5991120.1251 8.34,3558254.5692 5991120.2135 8.4,3558254.4829 5991120.4787 8.43,3558254.411 5991120.6997 8.55,3558254.3398 5991120.9185 8.92,3558254.3211 5991120.9759 9.13,3558254.2391 5991121.2278 9.88,3558254.1313 5991121.5593 10.57,3558253.3273 5991124.0299 10.56,3558252.4075 5991126.8563 10.35,3558252.0055 5991128.0916 10.32,3558250.5119 5991132.6815 10.9,3558250.2818 5991133.3886 10.94,3558249.362 5991136.215 10.83,3558248.7227 5991138.1796 10.77</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
       <prof:member>
        <om:Observation gml:id="Observation118285952235150">
         <gml:description>Bauwerk-Observation</gml:description>
         <gml:name>urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR</gml:name>
         <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR"/>
         <om:resultDefinition>
          <swe:RecordDefinition gml:id="RecordDefinition1182859522351169">
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#WEHRART"/>
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#FORMBEIWERT"/>
          </swe:RecordDefinition>
         </om:resultDefinition>
         <om:result><![CDATA[org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_BREITKRONIG 5.0
]]></om:result>
        </om:Observation>
       </prof:member>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952235186">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595223511">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.49 5991111.681 3558284.927 0.35
null null 25.57 10.29 5991115.9908 3558283.5201 0.35
null null 51.15 10.39 5991120.3024 3558282.1127 0.35
null null 67.13 10.26 5991122.9958 3558281.2335 0.35
low null 76.27 10.69 5991124.5363 3558280.7306 0.35
null null 77.88 10.13 5991124.8077 3558280.642 0.25
null null 81.46 8.53 5991125.4111 3558280.445 0.12
null null 86.13 8.67 5991126.1983 3558280.1881 0.12
null null 86.86 8.99 5991126.3213 3558280.1479 0.25
null null 87.42 9.69 5991126.4157 3558280.1171 0.25
null null 88.98 10.27 5991126.6786 3558280.0313 0.35
low null 90.54 10.62 5991126.9416 3558279.9454 0.35
null null 103.33 10.54 5991129.0973 3558279.2417 0.35
null null 141.7 10.9 5991135.5646 3558277.1306 0.35
null true 185.44 10.7 5991142.937 3558274.724 0.35
]]></om:result>
       <prof:station>66.9770</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558284.927 5991111.681 10.49,3558283.5201 5991115.9908 10.29,3558282.1127 5991120.3024 10.39,3558281.2335 5991122.9958 10.26,3558280.7306 5991124.5363 10.69,3558280.642 5991124.8077 10.13,3558280.445 5991125.4111 8.53,3558280.1881 5991126.1983 8.67,3558280.1479 5991126.3213 8.99,3558280.1171 5991126.4157 9.69,3558280.0313 5991126.6786 10.27,3558279.9454 5991126.9416 10.62,3558279.2417 5991129.0973 10.54,3558277.1306 5991135.5646 10.9,3558274.724 5991142.937 10.7</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859522445141">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859522460145">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.44 5991120.451 3558306.924 0.35 0.0 0.0 0.0
null null 6.4 10.4 5991121.8513 3558306.4395 0.35 0.0 0.0 0.0
null null 51.18 10.22 5991131.6487 3558303.0495 0.35 0.0 0.0 0.0
null null 58.38 10.16 5991133.224 3558302.5044 0.35 0.0 0.0 0.0
null null 66.87 10.85 5991135.0815 3558301.8617 0.35 0.0 0.0 0.0
null null 66.97 10.46 5991135.1034 3558301.8541 0.35 20.0 20.0 0.15
low null 68.37 10.09 5991135.4097 3558301.7482 0.25 0.0 0.0 0.0
null null 69.17 9.49 5991135.5847 3558301.6876 0.25 0.0 0.0 0.0
null null 70.96 8.58 5991135.9763 3558301.5521 0.12 0.0 0.0 0.0
null null 73.37 8.41 5991136.5036 3558301.3696 0.12 0.0 0.0 0.0
null null 75.88 8.4 5991137.0528 3558301.1796 0.12 0.0 0.0 0.0
null null 76.98 8.88 5991137.2935 3558301.0964 0.25 0.0 0.0 0.0
null null 77.87 9.91 5991137.4882 3558301.029 0.25 0.0 0.0 0.0
null null 79.07 10.13 5991137.7507 3558300.9381 0.35 0.0 0.0 0.0
low null 79.87 10.33 5991137.9258 3558300.8776 0.35 0.0 0.0 0.0
null null 86.25 10.51 5991139.3216 3558300.3946 0.35 0.0 0.0 0.0
null null 87.85 10.58 5991139.6717 3558300.2735 0.35 0.0 0.0 0.0
null null 91.03 10.66 5991140.3675 3558300.0327 0.35 0.0 0.0 0.0
null true 106.98 10.94 5991143.8572 3558298.8253 0.35 0.0 0.0 0.0
null null 113.39 10.93 5991145.2596 3558298.34 0.35 0.0 0.0 0.0
null null 129.38 10.87 5991148.7581 3558297.1295 0.35 0.0 0.0 0.0
null null 132.57 10.9 5991149.456 3558296.888 0.35 0.0 0.0 0.0
null null 162.17 10.77 5991155.9322 3558294.6472 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>67.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558306.924 5991120.451 10.44,3558306.4395 5991121.8513 10.4,3558303.0495 5991131.6487 10.22,3558302.5044 5991133.224 10.16,3558301.8617 5991135.0815 10.85,3558301.8541 5991135.1034 10.46,3558301.7482 5991135.4097 10.09,3558301.6876 5991135.5847 9.49,3558301.5521 5991135.9763 8.58,3558301.3696 5991136.5036 8.41,3558301.1796 5991137.0528 8.4,3558301.0964 5991137.2935 8.88,3558301.029 5991137.4882 9.91,3558300.9381 5991137.7507 10.13,3558300.8776 5991137.9258 10.33,3558300.3946 5991139.3216 10.51,3558300.2735 5991139.6717 10.58,3558300.0327 5991140.3675 10.66,3558298.8253 5991143.8572 10.94,3558298.34 5991145.2596 10.93,3558297.1295 5991148.7581 10.87,3558296.888 5991149.456 10.9,3558294.6472 5991155.9322 10.77</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859522570158">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859522570241">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.8 5991143.525 3558400.984 0.35 0.0 0.0 0.0
null null 47.93 10.75 5991151.4329 3558400.3671 0.35 0.0 0.0 0.0
null null 60.68 10.56 5991153.5365 3558400.2031 0.35 0.0 0.0 0.0
null null 65.97 10.62 5991154.4093 3558400.135 0.35 30.0 30.0 0.2
low null 67.17 10.07 5991154.6073 3558400.1195 0.25 0.0 0.0 0.0
null null 68.17 9.19 5991154.7723 3558400.1067 0.25 0.0 0.0 0.0
null null 70.47 8.56 5991155.1518 3558400.0771 0.12 0.0 0.0 0.0
null null 72.07 8.61 5991155.4158 3558400.0565 0.12 0.0 0.0 0.0
null null 73.67 8.67 5991155.6798 3558400.0359 0.12 0.0 0.0 0.0
null null 75.77 8.79 5991156.0262 3558400.0088 0.12 0.0 0.0 0.0
null null 76.37 9.22 5991156.1252 3558400.0011 0.25 0.0 0.0 0.0
low null 77.97 10.13 5991156.3892 3558399.9805 0.35 0.0 0.0 0.0
null null 89.11 10.57 5991158.2272 3558399.8372 0.35 0.0 0.0 0.0
null null 114.71 10.71 5991162.4509 3558399.5077 0.35 0.0 0.0 0.0
null null 133.93 10.77 5991165.622 3558399.2603 0.35 0.0 0.0 0.0
null null 165.87 10.71 5991170.8917 3558398.8493 0.35 0.0 0.0 0.0
null true 175.06 10.74 5991172.408 3558398.731 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>67.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558400.984 5991143.525 10.8,3558400.3671 5991151.4329 10.75,3558400.2031 5991153.5365 10.56,3558400.135 5991154.4093 10.62,3558400.1195 5991154.6073 10.07,3558400.1067 5991154.7723 9.19,3558400.0771 5991155.1518 8.56,3558400.0565 5991155.4158 8.61,3558400.0359 5991155.6798 8.67,3558400.0088 5991156.0262 8.79,3558400.0011 5991156.1252 9.22,3558399.9805 5991156.3892 10.13,3558399.8372 5991158.2272 10.57,3558399.5077 5991162.4509 10.71,3558399.2603 5991165.622 10.77,3558398.8493 5991170.8917 10.71,3558398.731 5991172.408 10.74</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859522601211">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859522601106">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null null 0.0 10.9 5991138.115 3558499.418 0.35 0.0 0.0 0.0
null null 38.37 10.9 5991144.6562 3558500.2311 0.35 0.0 0.0 0.0
null null 51.12 10.87 5991146.8298 3558500.5012 0.35 0.0 0.0 0.0
null true 62.32 10.94 5991148.7391 3558500.7385 0.35 0.0 0.0 0.0
null null 68.62 10.86 5991149.8131 3558500.872 0.35 6.0 6.0 0.2
low null 70.62 10.23 5991150.154 3558500.9144 0.25 0.0 0.0 0.0
null null 72.06 9.36 5991150.3995 3558500.9449 0.25 0.0 0.0 0.0
null null 74.24 8.58 5991150.7712 3558500.9911 0.12 0.0 0.0 0.0
null null 76.63 8.65 5991151.1786 3558501.0418 0.12 0.0 0.0 0.0
null null 79.32 8.91 5991151.6372 3558501.0988 0.25 0.0 0.0 0.0
null null 80.11 9.35 5991151.7719 3558501.1155 0.25 0.0 0.0 0.0
null null 81.51 10.3 5991152.0105 3558501.1452 0.25 0.0 0.0 0.0
low null 82.71 10.6 5991152.2151 3558501.1706 0.35 0.0 0.0 0.0
null null 92.28 10.7 5991153.8466 3558501.3734 0.35 0.0 0.0 0.0
null null 108.26 10.52 5991156.5708 3558501.712 0.35 0.0 0.0 0.0
null null 121.07 10.54 5991158.7546 3558501.9834 0.35 0.0 0.0 0.0
null null 172.25 10.76 5991167.4796 3558503.0679 0.35 0.0 0.0 0.0
null true 177.35 10.8 5991168.349 3558503.176 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>67.2000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558499.418 5991138.115 10.9,3558500.2311 5991144.6562 10.9,3558500.5012 5991146.8298 10.87,3558500.7385 5991148.7391 10.94,3558500.872 5991149.8131 10.86,3558500.9144 5991150.154 10.23,3558500.9449 5991150.3995 9.36,3558500.9911 5991150.7712 8.58,3558501.0418 5991151.1786 8.65,3558501.0988 5991151.6372 8.91,3558501.1155 5991151.7719 9.35,3558501.1452 5991152.0105 10.3,3558501.1706 5991152.2151 10.6,3558501.3734 5991153.8466 10.7,3558501.712 5991156.5708 10.52,3558501.9834 5991158.7546 10.54,3558503.0679 5991167.4796 10.76,3558503.176 5991168.349 10.8</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859522710221">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859522710108">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null null 0.0 11.02 5991141.691 3558597.239 0.35 0.0 0.0 0.0
null true 6.4 11.06 5991143.691 3558596.8503 0.35 0.0 0.0 0.0
null null 32.02 10.95 5991151.6974 3558595.2942 0.35 0.0 0.0 0.0
null null 47.12 10.77 5991156.4162 3558594.3771 0.35 12.0 12.0 0.2
low null 48.66 10.16 5991156.8974 3558594.2835 0.25 0.0 0.0 0.0
null null 50.23 9.41 5991157.388 3558594.1882 0.25 0.0 0.0 0.0
null null 51.8 8.47 5991157.8787 3558594.0928 0.12 0.0 0.0 0.0
null null 54.18 8.57 5991158.6224 3558593.9483 0.12 0.0 0.0 0.0
null null 57.55 8.9 5991159.6756 3558593.7436 0.25 0.0 0.0 0.0
null null 58.34 9.79 5991159.9224 3558593.6956 0.25 0.0 0.0 0.0
null null 59.54 10.08 5991160.2974 3558593.6227 0.25 0.0 0.0 0.0
low null 61.24 10.67 5991160.8287 3558593.5195 0.35 0.0 0.0 0.0
null null 67.63 10.56 5991162.8256 3558593.1313 0.35 0.0 0.0 0.0
null null 68.43 10.61 5991163.0756 3558593.0828 0.35 0.0 0.0 0.0
null null 70.03 10.63 5991163.5756 3558592.9856 0.35 0.0 0.0 0.0
null null 81.22 10.51 5991167.0725 3558592.3059 0.35 0.0 0.0 0.0
null null 100.36 10.69 5991173.0538 3558591.1434 0.35 0.0 0.0 0.0
null true 157.11 10.81 5991190.7884 3558587.6966 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>67.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558597.239 5991141.691 11.02,3558596.8503 5991143.691 11.06,3558595.2942 5991151.6974 10.95,3558594.3771 5991156.4162 10.77,3558594.2835 5991156.8974 10.16,3558594.1882 5991157.388 9.41,3558594.0928 5991157.8787 8.47,3558593.9483 5991158.6224 8.57,3558593.7436 5991159.6756 8.9,3558593.6956 5991159.9224 9.79,3558593.6227 5991160.2974 10.08,3558593.5195 5991160.8287 10.67,3558593.1313 5991162.8256 10.56,3558593.0828 5991163.0756 10.61,3558592.9856 5991163.5756 10.63,3558592.3059 5991167.0725 10.51,3558591.1434 5991173.0538 10.69,3558587.6966 5991190.7884 10.81</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952282083">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859522820209">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 11.05 5991188.422 3558687.821 0.35
null null 44.8 10.62 5991193.5915 3558683.1437 0.35
null null 51.2 10.58 5991194.33 3558682.4756 0.35
null null 62.4 10.39 5991195.6223 3558681.3062 0.35
null null 63.98 10.4 5991195.8046 3558681.1413 0.35
null null 65.77 10.64 5991196.0112 3558680.9544 0.35
null null 67.65 10.59 5991196.2281 3558680.7581 0.35
null null 67.75 10.68 5991196.2397 3558680.7477 0.35
low null 67.86 10.43 5991196.2524 3558680.7362 0.25
null null 69.07 10.19 5991196.392 3558680.6099 0.25
null null 70.24 9.45 5991196.527 3558680.4877 0.12
null null 72.1 8.56 5991196.7416 3558680.2935 0.12
null null 73.68 8.72 5991196.9239 3558680.1286 0.12
null null 74.47 8.77 5991197.0151 3558680.0461 0.12
null null 77.63 8.83 5991197.3797 3558679.7162 0.12
null null 78.12 8.86 5991197.4363 3558679.665 0.25
null null 78.51 9.39 5991197.4813 3558679.6243 0.25
null null 79.0 10.06 5991197.5378 3558679.5731 0.25
null null 80.17 10.39 5991197.6728 3558679.451 0.35
low null 80.96 10.54 5991197.764 3558679.3685 0.35
null null 83.36 10.49 5991198.0409 3558679.1179 0.35
null null 86.55 10.58 5991198.409 3558678.7849 0.35
null null 105.69 10.8 5991200.6175 3558676.7866 0.35
null null 118.5 11.05 5991202.0957 3558675.4492 0.35
null null 121.7 11.1 5991202.4649 3558675.1151 0.35
null true 134.5 11.18 5991203.9419 3558673.7788 0.35
null null 160.1 11.06 5991206.8959 3558671.106 0.35
null null 163.29 11.06 5991207.264 3558670.773 0.35
null null 172.87 10.97 5991208.3694 3558669.7728 0.35
]]></om:result>
       <prof:station>67.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558687.821 5991188.422 11.05,3558683.1437 5991193.5915 10.62,3558682.4756 5991194.33 10.58,3558681.3062 5991195.6223 10.39,3558681.1413 5991195.8046 10.4,3558680.9544 5991196.0112 10.64,3558680.7581 5991196.2281 10.59,3558680.7477 5991196.2397 10.68,3558680.7362 5991196.2524 10.43,3558680.6099 5991196.392 10.19,3558680.4877 5991196.527 9.45,3558680.2935 5991196.7416 8.56,3558680.1286 5991196.9239 8.72,3558680.0461 5991197.0151 8.77,3558679.7162 5991197.3797 8.83,3558679.665 5991197.4363 8.86,3558679.6243 5991197.4813 9.39,3558679.5731 5991197.5378 10.06,3558679.451 5991197.6728 10.39,3558679.3685 5991197.764 10.54,3558679.1179 5991198.0409 10.49,3558678.7849 5991198.409 10.58,3558676.7866 5991200.6175 10.8,3558675.4492 5991202.0957 11.05,3558675.1151 5991202.4649 11.1,3558673.7788 5991203.9419 11.18,3558671.106 5991206.8959 11.06,3558670.773 5991207.264 11.06,3558669.7728 5991208.3694 10.97</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859522835147">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859522835168">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 10.88 5991257.684 3558758.869 0.35
null null 25.2 10.98 5991260.4395 3558756.3265 0.35
null null 28.4 10.75 5991260.7894 3558756.0036 0.35
null null 30.8 10.66 5991261.0518 3558755.7615 0.35
null null 69.21 10.77 5991265.2517 3558751.8861 0.35
null null 70.81 10.79 5991265.4266 3558751.7247 0.35
null null 73.31 10.88 5991265.7 3558751.4725 0.35
null null 73.4 10.9 5991265.7098 3558751.4634 0.35
null null 73.6 11.08 5991265.7317 3558751.4432 0.35
low null 76.0 11.18 5991265.9941 3558751.201 0.35
null null 76.1 10.95 5991266.005 3558751.191 0.35
null null 77.69 10.36 5991266.1789 3558751.0305 0.25
null null 79.18 9.42 5991266.3418 3558750.8802 0.25
null null 80.74 8.99 5991266.5124 3558750.7228 0.12
null null 81.13 8.9 5991266.555 3558750.6835 0.12
null null 83.92 8.88 5991266.8601 3558750.402 0.12
null null 85.61 8.79 5991267.0449 3558750.2315 0.12
null null 86.3 9.04 5991267.1203 3558750.1618 0.12
null null 86.69 9.38 5991267.163 3558750.1225 0.25
null null 87.37 10.16 5991267.2373 3558750.0539 0.25
null null 88.76 10.39 5991267.3893 3558749.9136 0.35
low null 89.95 10.8 5991267.5194 3558749.7936 0.35
null null 96.32 11.02 5991268.216 3558749.1509 0.35
null null 99.5 11.09 5991268.5637 3558748.83 0.35
null true 115.49 11.31 5991270.3121 3558747.2167 0.35
null null 131.47 11.31 5991272.0594 3558745.6044 0.35
null null 157.05 11.09 5991274.8564 3558743.0236 0.35
null null 189.01 10.94 5991278.351 3558739.799 0.35
null null 191.21 10.94 5991278.5916 3558739.577 0.35
]]></om:result>
       <prof:station>67.5000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558758.869 5991257.684 10.88,3558756.3265 5991260.4395 10.98,3558756.0036 5991260.7894 10.75,3558755.7615 5991261.0518 10.66,3558751.8861 5991265.2517 10.77,3558751.7247 5991265.4266 10.79,3558751.4725 5991265.7 10.88,3558751.4634 5991265.7098 10.9,3558751.4432 5991265.7317 11.08,3558751.201 5991265.9941 11.18,3558751.191 5991266.005 10.95,3558751.0305 5991266.1789 10.36,3558750.8802 5991266.3418 9.42,3558750.7228 5991266.5124 8.99,3558750.6835 5991266.555 8.9,3558750.402 5991266.8601 8.88,3558750.2315 5991267.0449 8.79,3558750.1618 5991267.1203 9.04,3558750.1225 5991267.163 9.38,3558750.0539 5991267.2373 10.16,3558749.9136 5991267.3893 10.39,3558749.7936 5991267.5194 10.8,3558749.1509 5991268.216 11.02,3558748.83 5991268.5637 11.09,3558747.2167 5991270.3121 11.31,3558745.6044 5991272.0594 11.31,3558743.0236 5991274.8564 11.09,3558739.799 5991278.351 10.94,3558739.577 5991278.5916 10.94</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859522945248">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952294564">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null null 0.0 12.39 5991307.653 3558843.295 0.35 0.0 0.0 0.0
null null 6.39 13.01 5991309.5165 3558842.7425 0.35 0.0 0.0 0.0
null null 11.97 13.53 5991311.1438 3558842.2601 0.35 0.0 0.0 0.0
null null 15.17 13.63 5991312.0771 3558841.9835 0.35 0.0 0.0 0.0
null true 15.96 13.64 5991312.3074 3558841.9152 0.35 0.0 0.0 0.0
null null 19.16 13.58 5991313.2407 3558841.6385 0.35 0.0 0.0 0.0
null null 25.54 13.12 5991315.1013 3558841.0869 0.35 0.0 0.0 0.0
null null 35.13 12.28 5991317.898 3558840.2578 0.35 0.0 0.0 0.0
null null 36.72 12.19 5991318.3617 3558840.1204 0.35 0.0 0.0 0.0
null null 43.11 12.01 5991320.2253 3558839.5679 0.35 0.0 0.0 0.0
null null 44.71 11.95 5991320.6919 3558839.4296 0.35 0.0 0.0 0.0
null null 58.24 11.21 5991324.6377 3558838.2598 0.35 0.0 0.0 0.0
null null 59.04 11.2 5991324.871 3558838.1907 0.35 0.0 0.0 0.0
null null 63.04 11.39 5991326.0375 3558837.8448 0.35 0.0 0.0 0.0
low null 63.24 11.38 5991326.0958 3558837.8276 0.35 0.0 0.0 0.0
null null 63.64 11.0 5991326.2125 3558837.793 0.25 0.0 0.0 0.0
null null 65.04 10.4 5991326.6208 3558837.6719 0.25 0.0 0.0 0.0
null null 66.23 9.61 5991326.9678 3558837.569 0.25 0.0 0.0 0.0
null null 68.22 9.31 5991327.5481 3558837.397 0.12 0.0 0.0 0.0
null null 69.81 8.76 5991328.0118 3558837.2595 0.12 0.0 0.0 0.0
null null 72.1 8.82 5991328.6797 3558837.0616 0.12 0.0 0.0 0.0
null null 73.79 9.58 5991329.1725 3558836.9154 0.25 0.0 0.0 0.0
null null 74.68 10.44 5991329.4321 3558836.8385 0.25 0.0 0.0 0.0
low null 74.78 10.49 5991329.4613 3558836.8299 0.35 12.0 12.0 0.2
null null 75.98 10.63 5991329.8112 3558836.7261 0.35 0.0 0.0 0.0
null null 76.89 11.05 5991330.0766 3558836.6474 0.35 0.0 0.0 0.0
null null 78.49 11.16 5991330.5432 3558836.5091 0.35 0.0 0.0 0.0
null null 81.7 11.32 5991331.4793 3558836.2316 0.35 0.0 0.0 0.0
null null 82.1 11.37 5991331.596 3558836.197 0.35 0.0 0.0 0.0
null null 133.24 11.13 5991346.5101 3558831.7757 0.35 0.0 0.0 0.0
null true 139.43 11.13 5991348.3153 3558831.2405 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>67.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558843.295 5991307.653 12.39,3558842.7425 5991309.5165 13.01,3558842.2601 5991311.1438 13.53,3558841.9835 5991312.0771 13.63,3558841.9152 5991312.3074 13.64,3558841.6385 5991313.2407 13.58,3558841.0869 5991315.1013 13.12,3558840.2578 5991317.898 12.28,3558840.1204 5991318.3617 12.19,3558839.5679 5991320.2253 12.01,3558839.4296 5991320.6919 11.95,3558838.2598 5991324.6377 11.21,3558838.1907 5991324.871 11.2,3558837.8448 5991326.0375 11.39,3558837.8276 5991326.0958 11.38,3558837.793 5991326.2125 11.0,3558837.6719 5991326.6208 10.4,3558837.569 5991326.9678 9.61,3558837.397 5991327.5481 9.31,3558837.2595 5991328.0118 8.76,3558837.0616 5991328.6797 8.82,3558836.9154 5991329.1725 9.58,3558836.8385 5991329.4321 10.44,3558836.8299 5991329.4613 10.49,3558836.7261 5991329.8112 10.63,3558836.6474 5991330.0766 11.05,3558836.5091 5991330.5432 11.16,3558836.2316 5991331.4793 11.32,3558836.197 5991331.596 11.37,3558831.7757 5991346.5101 11.13,3558831.2405 5991348.3153 11.13</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952296015">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859522960127">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 11.45 5991324.003 3558938.685 0.35 0.0 0.0 0.0
null null 19.17 11.36 5991327.1376 3558939.0025 0.35 0.0 0.0 0.0
null null 31.95 11.24 5991329.2273 3558939.2142 0.35 0.0 0.0 0.0
null null 38.35 11.2 5991330.2738 3558939.3202 0.35 0.0 0.0 0.0
null null 47.95 11.25 5991331.8435 3558939.4791 0.35 0.0 0.0 0.0
null null 60.72 11.17 5991333.9316 3558939.6906 0.35 0.0 0.0 0.0
null null 63.91 11.13 5991334.4532 3558939.7435 0.35 0.0 0.0 0.0
null null 68.68 11.06 5991335.2331 3558939.8225 0.35 0.0 0.0 0.0
low null 74.04 10.99 5991336.1096 3558939.9113 0.35 0.0 0.0 0.0
null null 74.24 10.63 5991336.1423 3558939.9146 0.25 0.0 0.0 0.0
null null 75.81 10.05 5991336.399 3558939.9406 0.25 0.0 0.0 0.0
null null 78.26 9.1 5991336.7996 3558939.9811 0.12 0.0 0.0 0.0
null null 79.87 9.07 5991337.0628 3558940.0078 0.12 0.0 0.0 0.0
null null 82.22 8.96 5991337.4471 3558940.0467 0.12 0.0 0.0 0.0
null null 82.61 8.96 5991337.5109 3558940.0532 0.12 0.0 0.0 0.0
null null 84.19 9.06 5991337.7692 3558940.0794 0.12 0.0 0.0 0.0
null null 84.39 9.09 5991337.8019 3558940.0827 0.3 0.0 0.0 0.0
null null 85.19 9.99 5991337.9327 3558940.0959 0.3 0.0 0.0 0.0
low null 87.28 10.99 5991338.2745 3558940.1305 0.35 10.0 10.0 0.2
null null 93.88 10.91 5991339.3537 3558940.2398 0.35 0.0 0.0 0.0
null null 94.67 10.91 5991339.4828 3558940.2529 0.35 0.0 0.0 0.0
null null 101.05 11.13 5991340.5261 3558940.3586 0.35 0.0 0.0 0.0
null null 102.64 11.2 5991340.786 3558940.3849 0.35 0.0 0.0 0.0
null null 109.02 11.67 5991341.8293 3558940.4906 0.35 0.0 0.0 0.0
null null 114.6 12.16 5991342.7417 3558940.583 0.35 0.0 0.0 0.0
null null 117.79 12.31 5991343.2633 3558940.6358 0.35 0.0 0.0 0.0
null null 136.94 12.85 5991346.3946 3558940.953 0.35 0.0 0.0 0.0
null true 149.7 13.02 5991348.481 3558941.1643 0.35 0.0 0.0 0.0
null null 172.02 12.99 5991352.1306 3558941.534 0.35 0.0 0.0 0.0
null null 175.01 12.77 5991352.6195 3558941.5835 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>67.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3558938.685 5991324.003 11.45,3558939.0025 5991327.1376 11.36,3558939.2142 5991329.2273 11.24,3558939.3202 5991330.2738 11.2,3558939.4791 5991331.8435 11.25,3558939.6906 5991333.9316 11.17,3558939.7435 5991334.4532 11.13,3558939.8225 5991335.2331 11.06,3558939.9113 5991336.1096 10.99,3558939.9146 5991336.1423 10.63,3558939.9406 5991336.399 10.05,3558939.9811 5991336.7996 9.1,3558940.0078 5991337.0628 9.07,3558940.0467 5991337.4471 8.96,3558940.0532 5991337.5109 8.96,3558940.0794 5991337.7692 9.06,3558940.0827 5991337.8019 9.09,3558940.0959 5991337.9327 9.99,3558940.1305 5991338.2745 10.99,3558940.2398 5991339.3537 10.91,3558940.2529 5991339.4828 10.91,3558940.3586 5991340.5261 11.13,3558940.3849 5991340.786 11.2,3558940.4906 5991341.8293 11.67,3558940.583 5991342.7417 12.16,3558940.6358 5991343.2633 12.31,3558940.953 5991346.3946 12.85,3558941.1643 5991348.481 13.02,3558941.534 5991352.1306 12.99,3558941.5835 5991352.6195 12.77</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952307075">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859523070205">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 11.65 5991317.952 3559038.878 0.35 0.0 0.0 0.0
null null 19.13 11.61 5991321.5565 3559038.6398 0.35 0.0 0.0 0.0
null null 31.93 11.43 5991323.9682 3559038.4805 0.35 0.0 0.0 0.0
null null 38.33 11.32 5991325.1741 3559038.4008 0.35 0.0 0.0 0.0
null null 44.74 11.4 5991326.3819 3559038.321 0.35 12.0 12.0 0.15
low null 52.63 11.35 5991327.8685 3559038.2227 0.25 0.0 0.0 0.0
null null 54.23 10.43 5991328.17 3559038.2028 0.25 0.0 0.0 0.0
null null 56.82 8.86 5991328.658 3559038.1706 0.12 0.0 0.0 0.0
null null 57.72 8.84 5991328.8276 3559038.1594 0.12 0.0 0.0 0.0
null null 58.52 8.94 5991328.9783 3559038.1494 0.12 0.0 0.0 0.0
null null 60.0 9.2238 5991329.2572 3559038.131 0.12 0.0 0.0 0.0
null null 61.91 9.59 5991329.6171 3559038.1072 0.3 0.0 0.0 0.0
null null 62.91 9.89 5991329.8055 3559038.0948 0.3 0.0 0.0 0.0
null null 63.71 10.49 5991329.9562 3559038.0848 0.3 0.0 0.0 0.0
low null 64.71 10.77 5991330.1446 3559038.0723 0.35 10.0 10.0 0.2
null null 72.69 10.93 5991331.6482 3559037.973 0.35 0.0 0.0 0.0
null null 95.03 11.06 5991335.8575 3559037.6948 0.35 0.0 0.0 0.0
null null 133.33 12.1 5991343.074 3559037.218 0.35 0.0 0.0 0.0
null true 148.3 12.4 5991345.8946 3559037.0316 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>67.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559038.878 5991317.952 11.65,3559038.6398 5991321.5565 11.61,3559038.4805 5991323.9682 11.43,3559038.4008 5991325.1741 11.32,3559038.321 5991326.3819 11.4,3559038.2227 5991327.8685 11.35,3559038.2028 5991328.17 10.43,3559038.1706 5991328.658 8.86,3559038.1594 5991328.8276 8.84,3559038.1494 5991328.9783 8.94,3559038.131 5991329.2572 9.2238,3559038.1072 5991329.6171 9.59,3559038.0948 5991329.8055 9.89,3559038.0848 5991329.9562 10.49,3559038.0723 5991330.1446 10.77,3559037.973 5991331.6482 10.93,3559037.6948 5991335.8575 11.06,3559037.218 5991343.074 12.1,3559037.0316 5991345.8946 12.4</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952317993">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859523179212">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null null 0.0 12.01 5991328.311 3559140.778 0.35 0.0 0.0 0.0
null null 3.2 12.01 5991328.6914 3559140.685 0.35 0.0 0.0 0.0
null null 4.8 12.08 5991328.8816 3559140.6386 0.35 0.0 0.0 0.0
null null 20.8 11.86 5991330.7835 3559140.1737 0.35 0.0 0.0 0.0
null null 33.6 11.97 5991332.3051 3559139.8019 0.35 0.0 0.0 0.0
null true 47.2 12.13 5991333.9217 3559139.4068 0.35 0.0 0.0 0.0
null null 59.98 11.89 5991335.4409 3559139.0355 0.35 0.0 0.0 0.0
null null 75.95 11.65 5991337.3393 3559138.5716 0.35 0.0 0.0 0.0
null null 101.55 11.57 5991340.3824 3559137.8279 0.35 0.0 0.0 0.0
null null 123.94 11.54 5991343.0439 3559137.1775 0.35 0.0 0.0 0.0
null null 132.34 11.16 5991344.0425 3559136.9334 0.35 0.0 0.0 0.0
null null 132.54 11.52 5991344.0662 3559136.9276 0.35 0.0 0.0 0.0
null null 132.64 11.55 5991344.0781 3559136.9247 0.35 12.0 12.0 0.15
low null 133.44 10.85 5991344.1732 3559136.9015 0.25 0.0 0.0 0.0
null null 134.94 9.5 5991344.3515 3559136.8579 0.12 0.0 0.0 0.0
null null 136.24 9.15 5991344.5061 3559136.8201 0.12 0.0 0.0 0.0
null null 137.8 9.22 5991344.6915 3559136.7748 0.12 0.0 0.0 0.0
null null 140.16 8.95 5991344.972 3559136.7063 0.12 0.0 0.0 0.0
null null 140.56 9.16 5991345.0196 3559136.6946 0.12 0.0 0.0 0.0
null null 141.15 9.5 5991345.0897 3559136.6775 0.12 0.0 0.0 0.0
null null 141.55 9.46 5991345.1373 3559136.6659 0.3 0.0 0.0 0.0
null null 142.25 10.18 5991345.2205 3559136.6455 0.3 0.0 0.0 0.0
low null 143.95 11.04 5991345.4226 3559136.5962 0.35 0.0 0.0 0.0
null null 152.69 11.31 5991346.4615 3559136.3423 0.35 0.0 0.0 0.0
null null 165.42 11.38 5991347.9747 3559135.9724 0.35 0.0 0.0 0.0
null null 193.79 11.59 5991351.3471 3559135.1483 0.35 0.0 0.0 0.0
null null 194.99 11.28 5991351.4898 3559135.1134 0.35 0.0 0.0 0.0
null true 233.42 11.78 5991356.058 3559133.997 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>67.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559140.778 5991328.311 12.01,3559140.685 5991328.6914 12.01,3559140.6386 5991328.8816 12.08,3559140.1737 5991330.7835 11.86,3559139.8019 5991332.3051 11.97,3559139.4068 5991333.9217 12.13,3559139.0355 5991335.4409 11.89,3559138.5716 5991337.3393 11.65,3559137.8279 5991340.3824 11.57,3559137.1775 5991343.0439 11.54,3559136.9334 5991344.0425 11.16,3559136.9276 5991344.0662 11.52,3559136.9247 5991344.0781 11.55,3559136.9015 5991344.1732 10.85,3559136.8579 5991344.3515 9.5,3559136.8201 5991344.5061 9.15,3559136.7748 5991344.6915 9.22,3559136.7063 5991344.972 8.95,3559136.6946 5991345.0196 9.16,3559136.6775 5991345.0897 9.5,3559136.6659 5991345.1373 9.46,3559136.6455 5991345.2205 10.18,3559136.5962 5991345.4226 11.04,3559136.3423 5991346.4615 11.31,3559135.9724 5991347.9747 11.38,3559135.1483 5991351.3471 11.59,3559135.1134 5991351.4898 11.28,3559133.997 5991356.058 11.78</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952321018">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952321091">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 12.85 5991352.902 3559237.597 0.35 0.0 0.0 0.0
null null 16.77 12.52 5991355.3155 3559236.7486 0.35 0.0 0.0 0.0
null null 17.17 12.5 5991355.3731 3559236.7284 0.35 0.0 0.0 0.0
null null 20.77 11.81 5991355.8912 3559236.5462 0.35 0.0 0.0 0.0
null null 33.55 11.45 5991357.7305 3559235.8997 0.35 0.0 0.0 0.0
null null 39.94 11.37 5991358.6501 3559235.5764 0.35 0.0 0.0 0.0
null null 64.69 11.23 5991362.2121 3559234.3243 0.35 0.0 0.0 0.0
null null 66.29 11.26 5991362.4424 3559234.2434 0.35 0.0 0.0 0.0
null null 91.86 11.25 5991366.1224 3559232.9498 0.35 0.0 0.0 0.0
null null 100.47 11.04 5991367.3615 3559232.5142 0.35 0.0 0.0 0.0
null null 100.67 11.36 5991367.3903 3559232.5041 0.35 8.0 8.0 0.25
low null 101.58 10.68 5991367.5213 3559232.4581 0.25 0.0 0.0 0.0
null null 101.78 10.6 5991367.55 3559232.4479 0.25 0.0 0.0 0.0
null null 102.76 9.92 5991367.6911 3559232.3984 0.12 0.0 0.0 0.0
null null 104.42 9.03 5991367.93 3559232.3144 0.12 0.0 0.0 0.0
null null 106.0 9.02 5991368.1574 3559232.2345 0.12 0.0 0.0 0.0
null null 107.81 8.87 5991368.4179 3559232.1429 0.12 0.0 0.0 0.0
null null 109.01 9.28 5991368.5906 3559232.0822 0.12 0.0 0.0 0.0
null null 109.92 9.74 5991368.7215 3559232.0361 0.3 0.0 0.0 0.0
null null 110.83 10.51 5991368.8525 3559231.9901 0.3 0.0 0.0 0.0
low null 112.04 10.82 5991369.0266 3559231.9289 0.35 0.0 0.0 0.0
null null 116.83 11.18 5991369.716 3559231.6866 0.35 0.0 0.0 0.0
null null 129.59 11.45 5991371.5524 3559231.041 0.35 0.0 0.0 0.0
null null 134.37 11.62 5991372.2404 3559230.7992 0.35 0.0 0.0 0.0
null null 150.39 11.69 5991374.5459 3559229.9888 0.35 0.0 0.0 0.0
null null 151.99 11.74 5991374.7762 3559229.9078 0.35 0.0 0.0 0.0
null null 171.18 11.89 5991377.538 3559228.937 0.35 0.0 0.0 0.0
null true 197.35 12.35 5991381.3044 3559227.6131 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559237.597 5991352.902 12.85,3559236.7486 5991355.3155 12.52,3559236.7284 5991355.3731 12.5,3559236.5462 5991355.8912 11.81,3559235.8997 5991357.7305 11.45,3559235.5764 5991358.6501 11.37,3559234.3243 5991362.2121 11.23,3559234.2434 5991362.4424 11.26,3559232.9498 5991366.1224 11.25,3559232.5142 5991367.3615 11.04,3559232.5041 5991367.3903 11.36,3559232.4581 5991367.5213 10.68,3559232.4479 5991367.55 10.6,3559232.3984 5991367.6911 9.92,3559232.3144 5991367.93 9.03,3559232.2345 5991368.1574 9.02,3559232.1429 5991368.4179 8.87,3559232.0822 5991368.5906 9.28,3559232.0361 5991368.7215 9.74,3559231.9901 5991368.8525 10.51,3559231.9289 5991369.0266 10.82,3559231.6866 5991369.716 11.18,3559231.041 5991371.5524 11.45,3559230.7992 5991372.2404 11.62,3559229.9888 5991374.5459 11.69,3559229.9078 5991374.7762 11.74,3559228.937 5991377.538 11.89,3559227.6131 5991381.3044 12.35</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859523320152">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952332028">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.6 5991369.694 3559331.893 0.35 0.0 0.0 0.0
null null 12.0 13.53 5991371.6509 3559336.5901 0.35 0.0 0.0 0.0
null null 17.61 13.16 5991372.5657 3559338.7859 0.35 0.0 0.0 0.0
null null 25.21 12.32 5991373.8051 3559341.7607 0.35 0.0 0.0 0.0
null null 47.6 11.9 5991377.4562 3559350.5247 0.35 0.0 0.0 0.0
null null 57.2 11.88 5991379.0217 3559354.2823 0.35 0.0 0.0 0.0
null null 79.6 11.4 5991382.6746 3559363.0502 0.35 0.0 0.0 0.0
null null 86.31 11.1 5991383.7688 3559365.6766 0.35 5.0 5.0 0.25
low null 87.52 10.65 5991383.9661 3559366.1503 0.25 0.0 0.0 0.0
null null 88.32 10.11 5991384.0966 3559366.4634 0.25 0.0 0.0 0.0
null null 89.32 9.56 5991384.2596 3559366.8548 0.12 0.0 0.0 0.0
null null 90.93 9.42 5991384.5222 3559367.485 0.12 0.0 0.0 0.0
null null 92.13 9.21 5991384.7179 3559367.9547 0.12 0.0 0.0 0.0
null null 94.31 9.17 5991385.0734 3559368.808 0.12 0.0 0.0 0.0
null null 96.32 9.88 5991385.4011 3559369.5948 0.3 0.0 0.0 0.0
null null 98.03 10.79 5991385.68 3559370.2641 0.35 0.0 0.0 0.0
low null 99.17 10.95 5991385.8659 3559370.7103 0.35 0.0 0.0 0.0
null null 106.36 11.33 5991387.0384 3559373.5246 0.35 0.0 0.0 0.0
null null 119.16 11.52 5991389.1257 3559378.5348 0.35 0.0 0.0 0.0
null null 122.36 11.58 5991389.6475 3559379.7874 0.35 0.0 0.0 0.0
null null 131.96 11.49 5991391.213 3559383.545 0.35 0.0 0.0 0.0
null null 133.56 11.49 5991391.4739 3559384.1713 0.35 0.0 0.0 0.0
null null 144.35 11.67 5991393.2335 3559388.3948 0.35 0.0 0.0 0.0
null null 144.46 11.7 5991393.2514 3559388.4378 0.35 0.0 0.0 0.0
null null 144.66 11.83 5991393.284 3559388.5161 0.35 0.0 0.0 0.0
null null 144.75 11.87 5991393.2987 3559388.5513 0.35 0.0 0.0 0.0
null null 149.55 12.6 5991394.0815 3559390.4302 0.35 0.0 0.0 0.0
null null 162.34 13.0 5991396.1672 3559395.4364 0.35 0.0 0.0 0.0
null null 165.54 13.06 5991396.689 3559396.689 0.35 0.0 0.0 0.0
null true 180.82 13.03 5991399.1807 3559402.6699 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.1000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559331.893 5991369.694 13.6,3559336.5901 5991371.6509 13.53,3559338.7859 5991372.5657 13.16,3559341.7607 5991373.8051 12.32,3559350.5247 5991377.4562 11.9,3559354.2823 5991379.0217 11.88,3559363.0502 5991382.6746 11.4,3559365.6766 5991383.7688 11.1,3559366.1503 5991383.9661 10.65,3559366.4634 5991384.0966 10.11,3559366.8548 5991384.2596 9.56,3559367.485 5991384.5222 9.42,3559367.9547 5991384.7179 9.21,3559368.808 5991385.0734 9.17,3559369.5948 5991385.4011 9.88,3559370.2641 5991385.68 10.79,3559370.7103 5991385.8659 10.95,3559373.5246 5991387.0384 11.33,3559378.5348 5991389.1257 11.52,3559379.7874 5991389.6475 11.58,3559383.545 5991391.213 11.49,3559384.1713 5991391.4739 11.49,3559388.3948 5991393.2335 11.67,3559388.4378 5991393.2514 11.7,3559388.5161 5991393.284 11.83,3559388.5513 5991393.2987 11.87,3559390.4302 5991394.0815 12.6,3559395.4364 5991396.1672 13.0,3559396.689 5991396.689 13.06,3559402.6699 5991399.1807 13.03</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859523429114">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952342924">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.97 5991366.46 3559429.257 0.35 0.0 0.0 0.0
null null 6.39 13.67 5991367.5859 3559429.4528 0.35 0.0 0.0 0.0
null null 9.59 13.56 5991368.1498 3559429.5509 0.35 0.0 0.0 0.0
null null 20.78 13.3 5991370.1215 3559429.8938 0.35 0.0 0.0 0.0
null null 27.17 12.87 5991371.2474 3559430.0897 0.35 0.0 0.0 0.0
null null 46.35 11.78 5991374.6269 3559430.6775 0.35 0.0 0.0 0.0
null null 59.14 11.75 5991376.8805 3559431.0695 0.35 0.0 0.0 0.0
null null 65.53 11.76 5991378.0065 3559431.2653 0.35 0.0 0.0 0.0
null null 68.73 11.79 5991378.5703 3559431.3634 0.35 0.0 0.0 0.0
null null 84.2 11.52 5991381.2961 3559431.8375 0.35 8.0 8.0 0.2
low null 85.79 10.83 5991381.5763 3559431.8862 0.25 0.0 0.0 0.0
null null 86.59 10.3 5991381.7173 3559431.9107 0.25 0.0 0.0 0.0
null null 87.58 9.69 5991381.8917 3559431.9411 0.15 0.0 0.0 0.0
null null 89.55 8.84 5991382.2388 3559432.0015 0.15 0.0 0.0 0.0
null null 90.53 8.58 5991382.4115 3559432.0315 0.15 0.0 0.0 0.0
null null 91.24 8.65 5991382.5366 3559432.0532 0.15 0.0 0.0 0.0
null null 92.04 9.04 5991382.6776 3559432.0778 0.15 0.0 0.0 0.0
null null 92.85 9.35 5991382.8203 3559432.1026 0.15 0.0 0.0 0.0
null null 93.85 9.67 5991382.9965 3559432.1332 0.25 0.0 0.0 0.0
null null 94.22 9.73 5991383.0617 3559432.1446 0.25 0.0 0.0 0.0
null null 95.65 10.87 5991383.3136 3559432.1884 0.35 0.0 0.0 0.0
low null 96.82 11.36 5991383.5198 3559432.2243 0.35 0.0 0.0 0.0
null null 102.78 11.82 5991384.57 3559432.4069 0.35 0.0 0.0 0.0
null null 115.58 11.76 5991386.8253 3559432.7992 0.35 0.0 0.0 0.0
null null 118.79 11.76 5991387.3909 3559432.8976 0.35 0.0 0.0 0.0
null null 137.99 11.62 5991390.774 3559433.486 0.35 0.0 0.0 0.0
null true 167.72 12.2 5991396.0125 3559434.3971 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.2000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559429.257 5991366.46 13.97,3559429.4528 5991367.5859 13.67,3559429.5509 5991368.1498 13.56,3559429.8938 5991370.1215 13.3,3559430.0897 5991371.2474 12.87,3559430.6775 5991374.6269 11.78,3559431.0695 5991376.8805 11.75,3559431.2653 5991378.0065 11.76,3559431.3634 5991378.5703 11.79,3559431.8375 5991381.2961 11.52,3559431.8862 5991381.5763 10.83,3559431.9107 5991381.7173 10.3,3559431.9411 5991381.8917 9.69,3559432.0015 5991382.2388 8.84,3559432.0315 5991382.4115 8.58,3559432.0532 5991382.5366 8.65,3559432.0778 5991382.6776 9.04,3559432.1026 5991382.8203 9.35,3559432.1332 5991382.9965 9.67,3559432.1446 5991383.0617 9.73,3559432.1884 5991383.3136 10.87,3559432.2243 5991383.5198 11.36,3559432.4069 5991384.57 11.82,3559432.7992 5991386.8253 11.76,3559432.8976 5991387.3909 11.76,3559433.486 5991390.774 11.62,3559434.3971 5991396.0125 12.2</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859523460252">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition11828595234601">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#OBERKANTEWEHR"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.72 5991361.676 3559464.821 0.35 13.72
null null 10.0 14.02 5991363.0624 3559465.1075 0.35 14.02
null null 16.39 13.78 5991363.9483 3559465.2905 0.35 13.78
null null 29.19 13.36 5991365.7228 3559465.6572 0.35 13.36
null null 35.59 13.09 5991366.6101 3559465.8406 0.35 13.09
null null 51.58 12.33 5991368.8269 3559466.2987 0.35 12.33
null null 65.98 11.94 5991370.8232 3559466.7112 0.35 11.94
null null 69.17 11.91 5991371.2655 3559466.8026 0.35 11.91
null null 81.17 11.69 5991372.9291 3559467.1464 0.35 11.69
null null 85.47 11.77 5991373.5252 3559467.2696 0.35 11.77
null null 85.57 11.74 5991373.5391 3559467.2725 0.25 11.74
low null 87.27 10.08 5991373.7748 3559467.3212 0.15 10.08
null null 88.47 9.61 5991373.9412 3559467.3555 0.15 10.08
null null 88.87 9.53 5991373.9966 3559467.367 0.15 10.08
null null 90.46 9.49 5991374.217 3559467.4125 0.15 10.08
null null 92.06 9.49 5991374.4389 3559467.4584 0.15 10.08
null null 93.66 9.52 5991374.6607 3559467.5042 0.15 10.08
null null 96.36 9.53 5991375.035 3559467.5816 0.15 10.08
low null 96.8 10.08 5991375.096 3559467.5942 0.25 10.08
null null 98.66 11.58 5991375.3539 3559467.6475 0.35 11.58
null null 104.25 11.79 5991376.1288 3559467.8076 0.35 11.79
null null 110.65 11.8 5991377.0161 3559467.991 0.35 11.8
null null 123.44 11.7 5991378.7893 3559468.3574 0.35 11.7
null null 126.63 11.7 5991379.2315 3559468.4488 0.35 11.7
null null 142.62 11.51 5991381.4483 3559468.9068 0.35 11.51
null null 152.21 11.5 5991382.7778 3559469.1816 0.35 11.5
null null 165.0 11.7 5991384.551 3559469.548 0.35 11.7
null true 172.79 11.77 5991385.631 3559469.7712 0.35 11.77
]]></om:result>
       <prof:station>68.2360</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559464.821 5991361.676 13.72,3559465.1075 5991363.0624 14.02,3559465.2905 5991363.9483 13.78,3559465.6572 5991365.7228 13.36,3559465.8406 5991366.6101 13.09,3559466.2987 5991368.8269 12.33,3559466.7112 5991370.8232 11.94,3559466.8026 5991371.2655 11.91,3559467.1464 5991372.9291 11.69,3559467.2696 5991373.5252 11.77,3559467.2725 5991373.5391 11.74,3559467.3212 5991373.7748 10.08,3559467.3555 5991373.9412 9.61,3559467.367 5991373.9966 9.53,3559467.4125 5991374.217 9.49,3559467.4584 5991374.4389 9.49,3559467.5042 5991374.6607 9.52,3559467.5816 5991375.035 9.53,3559467.5942 5991375.096 10.08,3559467.6475 5991375.3539 11.58,3559467.8076 5991376.1288 11.79,3559467.991 5991377.0161 11.8,3559468.3574 5991378.7893 11.7,3559468.4488 5991379.2315 11.7,3559468.9068 5991381.4483 11.51,3559469.1816 5991382.7778 11.5,3559469.548 5991384.551 11.7,3559469.7712 5991385.631 11.77</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
       <prof:member>
        <om:Observation gml:id="Observation118285952355474">
         <gml:description>Bauwerk-Observation</gml:description>
         <gml:name>urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR</gml:name>
         <om:observedProperty xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#WEHR"/>
         <om:resultDefinition>
          <swe:RecordDefinition gml:id="RecordDefinition118285952355414">
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#WEHRART"/>
           <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#FORMBEIWERT"/>
          </swe:RecordDefinition>
         </om:resultDefinition>
         <om:result><![CDATA[org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_BREITKRONIG 5.0
]]></om:result>
        </om:Observation>
       </prof:member>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859523554167">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952355493">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.4 5991359.132 3559478.077 0.35 0.0 0.0 0.0
null null 31.93 14.16 5991362.5203 3559478.2965 0.35 0.0 0.0 0.0
null null 95.91 11.47 5991369.3097 3559478.7363 0.35 8.0 8.0 0.2
low null 97.46 11.12 5991369.4742 3559478.747 0.25 0.0 0.0 0.0
null null 99.8 9.78 5991369.7225 3559478.7631 0.12 0.0 0.0 0.0
null null 101.39 9.46 5991369.8913 3559478.774 0.12 0.0 0.0 0.0
null null 103.78 9.51 5991370.1449 3559478.7904 0.25 0.0 0.0 0.0
null null 105.34 10.12 5991370.3104 3559478.8012 0.25 0.0 0.0 0.0
null null 106.87 11.14 5991370.4728 3559478.8117 0.35 0.0 0.0 0.0
low null 108.44 11.47 5991370.6394 3559478.8225 0.35 0.0 0.0 0.0
null null 114.86 11.77 5991371.3207 3559478.8666 0.35 0.0 0.0 0.0
null null 166.0 11.49 5991376.7475 3559479.2182 0.35 0.0 0.0 0.0
null true 223.29 12.31 5991382.827 3559479.612 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.2480</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559478.077 5991359.132 13.4,3559478.2965 5991362.5203 14.16,3559478.7363 5991369.3097 11.47,3559478.747 5991369.4742 11.12,3559478.7631 5991369.7225 9.78,3559478.774 5991369.8913 9.46,3559478.7904 5991370.1449 9.51,3559478.8012 5991370.3104 10.12,3559478.8117 5991370.4728 11.14,3559478.8225 5991370.6394 11.47,3559478.8666 5991371.3207 11.77,3559479.2182 5991376.7475 11.49,3559479.612 5991382.827 12.31</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859523663248">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859523679165">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 12.92 5991358.753 3559532.298 0.35 0.0 0.0 0.0
null null 19.18 12.19 5991360.9494 3559531.8832 0.35 0.0 0.0 0.0
null null 19.98 12.17 5991361.041 3559531.8659 0.35 0.0 0.0 0.0
null null 21.98 12.26 5991361.27 3559531.8226 0.35 0.0 0.0 0.0
null null 22.78 12.1 5991361.3616 3559531.8053 0.35 0.0 0.0 0.0
null null 41.97 11.78 5991363.5592 3559531.3902 0.35 0.0 0.0 0.0
null null 54.77 11.76 5991365.025 3559531.1134 0.35 0.0 0.0 0.0
null null 59.57 11.76 5991365.5746 3559531.0096 0.35 0.0 0.0 0.0
null null 60.37 11.74 5991365.6663 3559530.9923 0.35 0.0 0.0 0.0
null null 73.17 11.73 5991367.1321 3559530.7154 0.35 0.0 0.0 0.0
null null 87.54 11.87 5991368.7776 3559530.4046 0.35 0.0 0.0 0.0
null null 89.93 11.86 5991369.0513 3559530.3529 0.35 13.0 13.0 0.2
low null 95.21 12.15 5991369.656 3559530.2387 0.35 0.0 0.0 0.0
null null 95.41 11.62 5991369.6789 3559530.2344 0.35 0.0 0.0 0.0
null null 96.5 11.03 5991369.8037 3559530.2108 0.3 0.0 0.0 0.0
null null 97.2 10.34 5991369.8838 3559530.1957 0.3 0.0 0.0 0.0
null null 98.88 9.32 5991370.0762 3559530.1594 0.12 0.0 0.0 0.0
null null 100.04 9.27 5991370.2091 3559530.1343 0.12 0.0 0.0 0.0
null null 102.56 9.38 5991370.4976 3559530.0798 0.12 0.0 0.0 0.0
null null 103.26 9.79 5991370.5778 3559530.0646 0.12 0.0 0.0 0.0
null null 103.36 9.81 5991370.5893 3559530.0625 0.12 0.0 0.0 0.0
null null 103.46 9.79 5991370.6007 3559530.0603 0.12 0.0 0.0 0.0
null null 104.46 10.22 5991370.7152 3559530.0387 0.25 0.0 0.0 0.0
null null 105.45 11.12 5991370.8286 3559530.0173 0.25 0.0 0.0 0.0
low null 106.84 11.6 5991370.9878 3559529.9872 0.35 0.0 0.0 0.0
null null 112.39 11.96 5991371.6233 3559529.8671 0.35 0.0 0.0 0.0
null null 138.0 11.6 5991374.556 3559529.3132 0.35 0.0 0.0 0.0
null null 157.21 11.41 5991376.7559 3559528.8978 0.35 0.0 0.0 0.0
null null 163.61 11.42 5991377.4888 3559528.7593 0.35 0.0 0.0 0.0
null null 192.4 11.69 5991380.7857 3559528.1366 0.35 0.0 0.0 0.0
null null 195.6 11.76 5991381.1521 3559528.0674 0.35 0.0 0.0 0.0
null null 202.0 11.86 5991381.885 3559527.929 0.35 0.0 0.0 0.0
null true 211.01 12.04 5991382.9168 3559527.7341 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.3000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559532.298 5991358.753 12.92,3559531.8832 5991360.9494 12.19,3559531.8659 5991361.041 12.17,3559531.8226 5991361.27 12.26,3559531.8053 5991361.3616 12.1,3559531.3902 5991363.5592 11.78,3559531.1134 5991365.025 11.76,3559531.0096 5991365.5746 11.76,3559530.9923 5991365.6663 11.74,3559530.7154 5991367.1321 11.73,3559530.4046 5991368.7776 11.87,3559530.3529 5991369.0513 11.86,3559530.2387 5991369.656 12.15,3559530.2344 5991369.6789 11.62,3559530.2108 5991369.8037 11.03,3559530.1957 5991369.8838 10.34,3559530.1594 5991370.0762 9.32,3559530.1343 5991370.2091 9.27,3559530.0798 5991370.4976 9.38,3559530.0646 5991370.5778 9.79,3559530.0625 5991370.5893 9.81,3559530.0603 5991370.6007 9.79,3559530.0387 5991370.7152 10.22,3559530.0173 5991370.8286 11.12,3559529.9872 5991370.9878 11.6,3559529.8671 5991371.6233 11.96,3559529.3132 5991374.556 11.6,3559528.8978 5991376.7559 11.41,3559528.7593 5991377.4888 11.42,3559528.1366 5991380.7857 11.69,3559528.0674 5991381.1521 11.76,3559527.929 5991381.885 11.86,3559527.7341 5991382.9168 12.04</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859523804107">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859523804203">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 12.94 5991369.15 3559576.659 0.35 0.0 0.0 0.0
null null 51.16 12.39 5991373.9267 3559575.0169 0.35 0.0 0.0 0.0
null null 89.57 11.47 5991377.5129 3559573.784 0.35 0.0 0.0 0.0
null null 127.97 11.97 5991381.0983 3559572.5515 0.35 0.0 0.0 0.0
null null 137.91 11.88 5991382.0263 3559572.2324 0.35 8.0 8.0 0.2
low null 138.5 11.4174 5991382.0814 3559572.2135 0.3 0.0 0.0 0.0
null null 140.18 10.1 5991382.2383 3559572.1595 0.12 0.0 0.0 0.0
null null 141.73 9.48 5991382.383 3559572.1098 0.12 0.0 0.0 0.0
null null 144.92 9.61 5991382.6808 3559572.0074 0.12 0.0 0.0 0.0
null null 147.73 10.24 5991382.9432 3559571.9172 0.25 0.0 0.0 0.0
low null 149.25 11.62 5991383.0851 3559571.8684 0.35 0.0 0.0 0.0
null null 150.02 11.99 5991383.157 3559571.8437 0.35 0.0 0.0 0.0
null null 201.15 12.09 5991387.9309 3559570.2026 0.35 0.0 0.0 0.0
null null 213.94 11.88 5991389.1251 3559569.792 0.35 0.0 0.0 0.0
null true 258.43 12.11 5991393.279 3559568.364 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.3440</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559576.659 5991369.15 12.94,3559575.0169 5991373.9267 12.39,3559573.784 5991377.5129 11.47,3559572.5515 5991381.0983 11.97,3559572.2324 5991382.0263 11.88,3559572.2135 5991382.0814 11.4174,3559572.1595 5991382.2383 10.1,3559572.1098 5991382.383 9.48,3559572.0074 5991382.6808 9.61,3559571.9172 5991382.9432 10.24,3559571.8684 5991383.0851 11.62,3559571.8437 5991383.157 11.99,3559570.2026 5991387.9309 12.09,3559569.792 5991389.1251 11.88,3559568.364 5991393.279 12.11</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859523820240">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859523820270">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.72 5991399.021 3559628.579 0.35 0.0 0.0 0.0
null null 3.2 13.7 5991399.4205 3559628.3192 0.35 0.0 0.0 0.0
null null 28.79 13.13 5991402.6152 3559626.2416 0.35 0.0 0.0 0.0
null null 41.59 12.6 5991404.2131 3559625.2024 0.35 0.0 0.0 0.0
null null 51.18 12.24 5991405.4104 3559624.4238 0.35 0.0 0.0 0.0
null null 62.38 11.66 5991406.8086 3559623.5145 0.35 0.0 0.0 0.0
null null 68.78 11.62 5991407.6076 3559622.9949 0.35 0.0 0.0 0.0
null null 107.19 11.78 5991412.4027 3559619.8765 0.35 6.0 6.0 0.2
low null 115.9 11.84 5991413.4901 3559619.1693 0.35 0.0 0.0 0.0
null null 117.47 11.03 5991413.6861 3559619.0419 0.3 0.0 0.0 0.0
null null 118.18 10.42 5991413.7747 3559618.9842 0.3 0.0 0.0 0.0
null null 119.89 9.46 5991413.9882 3559618.8454 0.12 0.0 0.0 0.0
null null 121.44 9.54 5991414.1817 3559618.7196 0.12 0.0 0.0 0.0
null null 122.64 9.57 5991414.3315 3559618.6221 0.12 0.0 0.0 0.0
null null 124.23 9.97 5991414.53 3559618.493 0.25 0.0 0.0 0.0
null null 125.62 10.28 5991414.7035 3559618.3802 0.25 0.0 0.0 0.0
low null 127.6 11.76 5991414.9507 3559618.2194 0.35 0.0 0.0 0.0
null null 146.74 12.2 5991417.3402 3559616.6655 0.35 0.0 0.0 0.0
null null 166.7 12.82 5991419.832 3559615.045 0.35 0.0 0.0 0.0
null true 206.13 12.34 5991424.7545 3559611.8438 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.4000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559628.579 5991399.021 13.72,3559628.3192 5991399.4205 13.7,3559626.2416 5991402.6152 13.13,3559625.2024 5991404.2131 12.6,3559624.4238 5991405.4104 12.24,3559623.5145 5991406.8086 11.66,3559622.9949 5991407.6076 11.62,3559619.8765 5991412.4027 11.78,3559619.1693 5991413.4901 11.84,3559619.0419 5991413.6861 11.03,3559618.9842 5991413.7747 10.42,3559618.8454 5991413.9882 9.46,3559618.7196 5991414.1817 9.54,3559618.6221 5991414.3315 9.57,3559618.493 5991414.53 9.97,3559618.3802 5991414.7035 10.28,3559618.2194 5991414.9507 11.76,3559616.6655 5991417.3402 12.2,3559615.045 5991419.832 12.82,3559611.8438 5991424.7545 12.34</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859523929266">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952392989">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.16 5991439.594 3559664.531 0.35 0.0 0.0 0.0
null null 25.59 13.14 5991441.198 3559662.4365 0.35 0.0 0.0 0.0
null null 51.19 11.56 5991442.8026 3559660.3412 0.35 0.0 0.0 0.0
null null 76.78 11.36 5991444.4065 3559658.2468 0.35 0.0 0.0 0.0
null null 121.56 11.6 5991447.2133 3559654.5816 0.35 5.0 5.0 0.2
low null 123.15 11.46 5991447.313 3559654.4515 0.3 0.0 0.0 0.0
null null 124.72 10.53 5991447.4114 3559654.323 0.3 0.0 0.0 0.0
null null 126.26 9.88 5991447.5079 3559654.1969 0.12 0.0 0.0 0.0
null null 129.37 9.62 5991447.7028 3559653.9424 0.12 0.0 0.0 0.0
null null 130.97 9.7 5991447.8031 3559653.8114 0.12 0.0 0.0 0.0
null null 133.36 10.38 5991447.9529 3559653.6158 0.25 0.0 0.0 0.0
low null 134.11 11.47 5991447.9999 3559653.5544 0.35 0.0 0.0 0.0
null null 134.88 11.75 5991448.0482 3559653.4914 0.35 0.0 0.0 0.0
null null 141.24 12.11 5991448.4469 3559652.9709 0.35 0.0 0.0 0.0
null null 192.32 12.24 5991451.6485 3559648.7901 0.35 0.0 0.0 0.0
null true 223.55 12.04 5991453.606 3559646.234 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.4500</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559664.531 5991439.594 13.16,3559662.4365 5991441.198 13.14,3559660.3412 5991442.8026 11.56,3559658.2468 5991444.4065 11.36,3559654.5816 5991447.2133 11.6,3559654.4515 5991447.313 11.46,3559654.323 5991447.4114 10.53,3559654.1969 5991447.5079 9.88,3559653.9424 5991447.7028 9.62,3559653.8114 5991447.8031 9.7,3559653.6158 5991447.9529 10.38,3559653.5544 5991447.9999 11.47,3559653.4914 5991448.0482 11.75,3559652.9709 5991448.4469 12.11,3559648.7901 5991451.6485 12.24,3559646.234 5991453.606 12.04</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524038230">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952403828">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.73 5991484.169 3559690.641 0.35 0.0 0.0 0.0
null null 19.21 13.76 5991485.0707 3559688.4502 0.35 0.0 0.0 0.0
null null 30.41 13.49 5991485.5964 3559687.1729 0.35 0.0 0.0 0.0
null null 36.81 13.23 5991485.8969 3559686.443 0.35 0.0 0.0 0.0
null null 40.01 13.08 5991486.0471 3559686.0781 0.35 0.0 0.0 0.0
null null 67.21 11.59 5991487.3238 3559682.976 0.35 0.0 0.0 0.0
null null 86.39 11.83 5991488.2241 3559680.7886 0.35 0.0 0.0 0.0
null null 106.37 11.78 5991489.162 3559678.51 0.35 5.0 5.0 0.2
low null 112.69 11.48 5991489.4587 3559677.7893 0.35 0.0 0.0 0.0
null null 113.88 10.96 5991489.5145 3559677.6536 0.3 0.0 0.0 0.0
null null 114.67 10.34 5991489.5516 3559677.5635 0.12 0.0 0.0 0.0
null null 116.28 9.47 5991489.6272 3559677.3798 0.12 0.0 0.0 0.0
null null 117.49 9.35 5991489.684 3559677.2418 0.12 0.0 0.0 0.0
null null 119.75 9.41 5991489.79 3559676.9841 0.25 0.0 0.0 0.0
null null 122.0 10.57 5991489.8957 3559676.7275 0.25 0.0 0.0 0.0
null null 122.97 11.15 5991489.9412 3559676.6169 0.35 0.0 0.0 0.0
low null 124.37 11.7 5991490.0069 3559676.4572 0.35 0.0 0.0 0.0
null null 130.75 11.85 5991490.3064 3559675.7296 0.35 0.0 0.0 0.0
null null 148.35 12.06 5991491.1325 3559673.7224 0.35 0.0 0.0 0.0
null null 169.95 11.94 5991492.1464 3559671.259 0.35 0.0 0.0 0.0
null null 177.14 12.35 5991492.4839 3559670.4391 0.35 0.0 0.0 0.0
null null 177.94 12.37 5991492.5215 3559670.3478 0.35 0.0 0.0 0.0
null null 192.31 12.45 5991493.196 3559668.709 0.35 0.0 0.0 0.0
null true 197.4 12.36 5991493.4349 3559668.1285 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.5000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559690.641 5991484.169 13.73,3559688.4502 5991485.0707 13.76,3559687.1729 5991485.5964 13.49,3559686.443 5991485.8969 13.23,3559686.0781 5991486.0471 13.08,3559682.976 5991487.3238 11.59,3559680.7886 5991488.2241 11.83,3559678.51 5991489.162 11.78,3559677.7893 5991489.4587 11.48,3559677.6536 5991489.5145 10.96,3559677.5635 5991489.5516 10.34,3559677.3798 5991489.6272 9.47,3559677.2418 5991489.684 9.35,3559676.9841 5991489.79 9.41,3559676.7275 5991489.8957 10.57,3559676.6169 5991489.9412 11.15,3559676.4572 5991490.0069 11.7,3559675.7296 5991490.3064 11.85,3559673.7224 5991491.1325 12.06,3559671.259 5991492.1464 11.94,3559670.4391 5991492.4839 12.35,3559670.3478 5991492.5215 12.37,3559668.709 5991493.196 12.45,3559668.1285 5991493.4349 12.36</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524054116">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952405457">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.53 5991523.282 3559708.164 0.35 0.0 0.0 0.0
null null 25.59 13.62 5991524.2377 3559705.8357 0.35 0.0 0.0 0.0
null null 38.39 13.36 5991524.7157 3559704.6711 0.35 0.0 0.0 0.0
null null 76.79 11.79 5991526.1498 3559701.1773 0.35 0.0 0.0 0.0
null null 127.94 11.81 5991528.06 3559696.5234 0.35 5.0 5.0 0.2
low null 132.69 11.49 5991528.2374 3559696.0912 0.3 0.0 0.0 0.0
null null 135.82 9.69 5991528.3543 3559695.8064 0.12 0.0 0.0 0.0
null null 137.42 9.38 5991528.414 3559695.6608 0.12 0.0 0.0 0.0
null null 139.77 9.43 5991528.5018 3559695.447 0.12 0.0 0.0 0.0
null null 141.31 10.03 5991528.5593 3559695.3069 0.25 0.0 0.0 0.0
null null 142.85 11.16 5991528.6168 3559695.1668 0.35 0.0 0.0 0.0
low null 144.4 11.88 5991528.6747 3559695.0258 0.35 0.0 0.0 0.0
null true 226.4 12.28 5991531.737 3559687.565 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.5490</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559708.164 5991523.282 13.53,3559705.8357 5991524.2377 13.62,3559704.6711 5991524.7157 13.36,3559701.1773 5991526.1498 11.79,3559696.5234 5991528.06 11.81,3559696.0912 5991528.2374 11.49,3559695.8064 5991528.3543 9.69,3559695.6608 5991528.414 9.38,3559695.447 5991528.5018 9.43,3559695.3069 5991528.5593 10.03,3559695.1668 5991528.6168 11.16,3559695.0258 5991528.6747 11.88,3559687.565 5991531.737 12.28</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524179240">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952417942">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.31 5991568.782 3559729.658 0.35 0.0 0.0 0.0
null null 11.19 13.04 5991569.465 3559728.1806 0.35 0.0 0.0 0.0
null null 20.78 12.58 5991570.0504 3559726.9144 0.35 0.0 0.0 0.0
null null 36.77 11.98 5991571.0265 3559724.8032 0.35 0.0 0.0 0.0
null null 43.56 11.56 5991571.4409 3559723.9067 0.35 0.0 0.0 0.0
null null 69.16 11.64 5991573.0036 3559720.5266 0.35 0.0 0.0 0.0
null null 88.36 11.65 5991574.1755 3559717.9916 0.35 0.0 0.0 0.0
null null 91.56 11.67 5991574.3709 3559717.5691 0.35 0.0 0.0 0.0
null null 104.35 11.85 5991575.1516 3559715.8804 0.35 0.0 0.0 0.0
null null 120.73 12.02 5991576.1514 3559713.7177 0.35 0.0 0.0 0.0
null null 126.3 11.75 5991576.4914 3559712.9823 0.35 0.0 0.0 0.0
null null 126.4 12.09 5991576.4975 3559712.9691 0.35 0.0 0.0 0.0
null null 126.5 12.16 5991576.5036 3559712.9559 0.35 0.0 0.0 0.0
null null 126.9 11.98 5991576.528 3559712.9031 0.35 4.0 4.0 0.25
low null 127.69 11.28 5991576.5763 3559712.7988 0.35 0.0 0.0 0.0
null null 127.88 11.19 5991576.5879 3559712.7737 0.3 0.0 0.0 0.0
null null 129.06 10.29 5991576.6599 3559712.6179 0.3 0.0 0.0 0.0
null null 130.63 9.61 5991576.7557 3559712.4106 0.12 0.0 0.0 0.0
null null 131.84 9.4 5991576.8296 3559712.2508 0.12 0.0 0.0 0.0
null null 133.14 9.31 5991576.9089 3559712.0792 0.12 0.0 0.0 0.0
null null 134.56 9.9 5991576.9956 3559711.8917 0.25 0.0 0.0 0.0
null null 136.33 11.09 5991577.1037 3559711.658 0.35 0.0 0.0 0.0
low null 137.91 11.69 5991577.2001 3559711.4494 0.35 5.0 5.0 0.25
null null 139.5 11.8 5991577.2972 3559711.2395 0.35 0.0 0.0 0.0
null null 143.06 11.96 5991577.5145 3559710.7694 0.35 0.0 0.0 0.0
null null 143.86 12.06 5991577.5633 3559710.6638 0.35 0.0 0.0 0.0
null null 144.66 12.09 5991577.6121 3559710.5582 0.35 0.0 0.0 0.0
null null 160.68 12.06 5991578.59 3559708.443 0.35 0.0 0.0 0.0
null null 186.24 12.29 5991580.1502 3559705.0682 0.35 0.0 0.0 0.0
null true 212.27 12.6 5991581.7391 3559701.6314 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.6000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559729.658 5991568.782 13.31,3559728.1806 5991569.465 13.04,3559726.9144 5991570.0504 12.58,3559724.8032 5991571.0265 11.98,3559723.9067 5991571.4409 11.56,3559720.5266 5991573.0036 11.64,3559717.9916 5991574.1755 11.65,3559717.5691 5991574.3709 11.67,3559715.8804 5991575.1516 11.85,3559713.7177 5991576.1514 12.02,3559712.9823 5991576.4914 11.75,3559712.9691 5991576.4975 12.09,3559712.9559 5991576.5036 12.16,3559712.9031 5991576.528 11.98,3559712.7988 5991576.5763 11.28,3559712.7737 5991576.5879 11.19,3559712.6179 5991576.6599 10.29,3559712.4106 5991576.7557 9.61,3559712.2508 5991576.8296 9.4,3559712.0792 5991576.9089 9.31,3559711.8917 5991576.9956 9.9,3559711.658 5991577.1037 11.09,3559711.4494 5991577.2001 11.69,3559711.2395 5991577.2972 11.8,3559710.7694 5991577.5145 11.96,3559710.6638 5991577.5633 12.06,3559710.5582 5991577.6121 12.09,3559708.443 5991578.59 12.06,3559705.0682 5991580.1502 12.29,3559701.6314 5991581.7391 12.6</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524288275">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952428847">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.23 5991609.746 3559759.187 0.35 0.0 0.0 0.0
null null 6.39 12.79 5991610.201 3559758.8052 0.35 0.0 0.0 0.0
null null 31.97 12.41 5991612.0223 3559757.2767 0.35 0.0 0.0 0.0
null null 44.76 12.02 5991612.933 3559756.5125 0.35 0.0 0.0 0.0
null null 70.33 11.86 5991614.7536 3559754.9846 0.35 0.0 0.0 0.0
null null 121.5 12.04 5991618.397 3559751.9271 0.35 4.0 4.0 0.25
low null 130.23 11.62 5991619.0186 3559751.4055 0.3 0.0 0.0 0.0
null null 133.45 10.05 5991619.2478 3559751.2131 0.12 0.0 0.0 0.0
null null 135.02 9.47 5991619.3596 3559751.1193 0.12 0.0 0.0 0.0
null null 138.13 9.56 5991619.5811 3559750.9335 0.3 0.0 0.0 0.0
low null 142.03 11.75 5991619.8587 3559750.7004 0.35 5.0 5.0 0.25
null null 148.4 12.13 5991620.3123 3559750.3198 0.35 0.0 0.0 0.0
null null 161.21 12.14 5991621.2244 3559749.5544 0.35 0.0 0.0 0.0
null null 174.0 11.69 5991622.1351 3559748.7902 0.35 0.0 0.0 0.0
null null 186.8 12.37 5991623.0464 3559748.0253 0.35 0.0 0.0 0.0
null true 245.95 13.36 5991627.258 3559744.491 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.6510</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559759.187 5991609.746 13.23,3559758.8052 5991610.201 12.79,3559757.2767 5991612.0223 12.41,3559756.5125 5991612.933 12.02,3559754.9846 5991614.7536 11.86,3559751.9271 5991618.397 12.04,3559751.4055 5991619.0186 11.62,3559751.2131 5991619.2478 10.05,3559751.1193 5991619.3596 9.47,3559750.9335 5991619.5811 9.56,3559750.7004 5991619.8587 11.75,3559750.3198 5991620.3123 12.13,3559749.5544 5991621.2244 12.14,3559748.7902 5991622.1351 11.69,3559748.0253 5991623.0464 12.37,3559744.491 5991627.258 13.36</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524304281">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859524304251">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 12.66 5991635.097 3559796.942 0.35 0.0 0.0 0.0
null null 12.8 12.48 5991636.4267 3559796.1763 0.35 0.0 0.0 0.0
null null 38.41 12.36 5991639.0872 3559794.6443 0.35 0.0 0.0 0.0
null null 64.02 12.44 5991641.7476 3559793.1124 0.35 0.0 0.0 0.0
null null 70.42 12.43 5991642.4125 3559792.7295 0.35 0.0 0.0 0.0
null null 89.62 12.26 5991644.4071 3559791.581 0.35 0.0 0.0 0.0
null null 108.83 11.98 5991646.4027 3559790.4318 0.35 0.0 0.0 0.0
null null 110.43 11.93 5991646.5689 3559790.3361 0.35 0.0 0.0 0.0
null null 115.13 11.72 5991647.0571 3559790.055 0.35 0.0 0.0 0.0
null null 115.22 11.74 5991647.0665 3559790.0496 0.35 4.0 4.0 0.25
low null 115.42 12.0 5991647.0873 3559790.0376 0.3 0.0 0.0 0.0
null null 117.01 10.57 5991647.2524 3559789.9425 0.3 0.0 0.0 0.0
null null 117.91 10.04 5991647.3459 3559789.8887 0.12 0.0 0.0 0.0
null null 118.7 10.04 5991647.428 3559789.8414 0.12 0.0 0.0 0.0
null null 120.29 9.89 5991647.5932 3559789.7463 0.12 0.0 0.0 0.0
null null 121.94 9.69 5991647.7646 3559789.6476 0.12 0.0 0.0 0.0
null null 122.73 9.84 5991647.8467 3559789.6004 0.12 0.0 0.0 0.0
null null 123.52 10.06 5991647.9287 3559789.5531 0.3 0.0 0.0 0.0
null null 125.64 11.23 5991648.149 3559789.4263 0.35 0.0 0.0 0.0
low null 127.04 11.84 5991648.2944 3559789.3425 0.35 0.0 0.0 0.0
null null 129.84 11.84 5991648.5853 3559789.175 0.35 0.0 0.0 0.0
null null 132.23 12.09 5991648.8335 3559789.0321 0.35 0.0 0.0 0.0
null null 148.21 11.78 5991650.4936 3559788.0762 0.35 0.0 0.0 0.0
null null 151.41 11.75 5991650.826 3559787.8847 0.35 0.0 0.0 0.0
null null 170.58 11.87 5991652.8175 3559786.738 0.35 0.0 0.0 0.0
null null 181.36 12.01 5991653.9374 3559786.0931 0.35 0.0 0.0 0.0
null null 184.56 12.25 5991654.2698 3559785.9017 0.35 0.0 0.0 0.0
null null 186.16 12.36 5991654.436 3559785.806 0.35 0.0 0.0 0.0
null true 217.6 13.13 5991657.7021 3559783.9253 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.7000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559796.942 5991635.097 12.66,3559796.1763 5991636.4267 12.48,3559794.6443 5991639.0872 12.36,3559793.1124 5991641.7476 12.44,3559792.7295 5991642.4125 12.43,3559791.581 5991644.4071 12.26,3559790.4318 5991646.4027 11.98,3559790.3361 5991646.5689 11.93,3559790.055 5991647.0571 11.72,3559790.0496 5991647.0665 11.74,3559790.0376 5991647.0873 12.0,3559789.9425 5991647.2524 10.57,3559789.8887 5991647.3459 10.04,3559789.8414 5991647.428 10.04,3559789.7463 5991647.5932 9.89,3559789.6476 5991647.7646 9.69,3559789.6004 5991647.8467 9.84,3559789.5531 5991647.9287 10.06,3559789.4263 5991648.149 11.23,3559789.3425 5991648.2944 11.84,3559789.175 5991648.5853 11.84,3559789.0321 5991648.8335 12.09,3559788.0762 5991650.4936 11.78,3559787.8847 5991650.826 11.75,3559786.738 5991652.8175 11.87,3559786.0931 5991653.9374 12.01,3559785.9017 5991654.2698 12.25,3559785.806 5991654.436 12.36,3559783.9253 5991657.7021 13.13</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524413200">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952441383">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 12.68 5991645.493 3559839.283 0.35 0.0 0.0 0.0
null null 76.78 12.54 5991653.4101 3559839.8311 0.35 0.0 0.0 0.0
null null 108.75 12.17 5991656.7067 3559840.0593 0.35 0.0 0.0 0.0
null null 113.54 11.78 5991657.2006 3559840.0935 0.35 5.0 5.0 0.25
low null 114.31 11.54 5991657.28 3559840.099 0.3 0.0 0.0 0.0
null null 115.81 10.46 5991657.4347 3559840.1097 0.12 0.0 0.0 0.0
null null 118.13 9.59 5991657.6739 3559840.1262 0.12 0.0 0.0 0.0
null null 120.49 9.4 5991657.9172 3559840.1431 0.12 0.0 0.0 0.0
null null 122.03 10.12 5991658.076 3559840.1541 0.3 0.0 0.0 0.0
null null 123.61 11.11 5991658.239 3559840.1654 0.3 0.0 0.0 0.0
low null 125.0 11.7091 5991658.3823 3559840.1753 0.35 5.0 5.0 0.25
null null 125.93 12.11 5991658.4782 3559840.1819 0.35 0.0 0.0 0.0
null null 164.27 11.61 5991662.4316 3559840.4556 0.35 0.0 0.0 0.0
null null 215.46 13.46 5991667.71 3559840.821 0.35 0.0 0.0 0.0
null true 287.99 14.31 5991675.1889 3559841.3387 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.7480</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559839.283 5991645.493 12.68,3559839.8311 5991653.4101 12.54,3559840.0593 5991656.7067 12.17,3559840.0935 5991657.2006 11.78,3559840.099 5991657.28 11.54,3559840.1097 5991657.4347 10.46,3559840.1262 5991657.6739 9.59,3559840.1431 5991657.9172 9.4,3559840.1541 5991658.076 10.12,3559840.1654 5991658.239 11.11,3559840.1753 5991658.3823 11.7091,3559840.1819 5991658.4782 12.11,3559840.4556 5991662.4316 11.61,3559840.821 5991667.71 13.46,3559841.3387 5991675.1889 14.31</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524523273">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952452344">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 12.66 5991636.46 3559887.48 0.35 0.0 0.0 0.0
null null 6.4 12.57 5991636.9476 3559887.6506 0.35 0.0 0.0 0.0
null null 28.78 12.5 5991638.6527 3559888.2471 0.35 0.0 0.0 0.0
null null 35.18 12.42 5991639.1403 3559888.4176 0.35 0.0 0.0 0.0
null null 49.57 12.07 5991640.2366 3559888.8012 0.35 0.0 0.0 0.0
null null 71.91 12.06 5991641.9387 3559889.3966 0.35 0.0 0.0 0.0
null null 85.46 12.3 5991642.971 3559889.7578 0.35 0.0 0.0 0.0
null null 87.06 12.22 5991643.0929 3559889.8004 0.35 5.0 5.0 0.25
low null 93.04 12.38 5991643.5485 3559889.9598 0.35 0.0 0.0 0.0
null null 93.24 11.87 5991643.5637 3559889.9651 0.35 0.0 0.0 0.0
null null 94.42 11.26 5991643.6536 3559889.9966 0.25 0.0 0.0 0.0
null null 95.2 10.62 5991643.7131 3559890.0174 0.25 0.0 0.0 0.0
null null 97.9 9.56 5991643.9188 3559890.0893 0.12 0.0 0.0 0.0
null null 98.56 9.31 5991643.9691 3559890.1069 0.12 0.0 0.0 0.0
null null 98.94 9.4 5991643.998 3559890.117 0.12 0.0 0.0 0.0
null null 99.93 9.41 5991644.0734 3559890.1434 0.12 0.0 0.0 0.0
null null 102.25 10.5 5991644.2502 3559890.2053 0.3 0.0 0.0 0.0
null null 104.26 11.6 5991644.4033 3559890.2588 0.35 0.0 0.0 0.0
low null 105.26 12.38 5991644.4795 3559890.2855 0.35 7.5 7.5 0.25
null null 110.07 12.51 5991644.846 3559890.4137 0.35 0.0 0.0 0.0
null null 119.67 12.23 5991645.5774 3559890.6696 0.35 0.0 0.0 0.0
null null 122.87 12.18 5991645.8212 3559890.7548 0.35 0.0 0.0 0.0
null null 129.27 12.13 5991646.3088 3559890.9254 0.35 0.0 0.0 0.0
null null 135.67 12.16 5991646.7964 3559891.096 0.35 0.0 0.0 0.0
null null 148.46 12.27 5991647.7708 3559891.4369 0.35 0.0 0.0 0.0
null null 154.86 12.34 5991648.2584 3559891.6075 0.35 0.0 0.0 0.0
null null 167.66 12.6 5991649.2336 3559891.9486 0.35 0.0 0.0 0.0
null null 172.46 12.72 5991649.5993 3559892.0766 0.35 0.0 0.0 0.0
null null 175.65 12.89 5991649.8424 3559892.1616 0.35 0.0 0.0 0.0
null null 198.05 13.42 5991651.549 3559892.7586 0.35 0.0 0.0 0.0
null null 204.44 13.47 5991652.0358 3559892.9289 0.35 0.0 0.0 0.0
null null 210.84 13.49 5991652.5234 3559893.0995 0.35 0.0 0.0 0.0
null null 218.84 13.49 5991653.1329 3559893.3127 0.35 0.0 0.0 0.0
null null 241.24 13.84 5991654.8395 3559893.9097 0.35 0.0 0.0 0.0
null null 254.04 13.88 5991655.8147 3559894.2509 0.35 0.0 0.0 0.0
null null 274.84 13.88 5991657.3994 3559894.8053 0.35 0.0 0.0 0.0
null null 287.64 14.07 5991658.3746 3559895.1464 0.35 0.0 0.0 0.0
null null 297.24 14.23 5991659.106 3559895.4023 0.35 0.0 0.0 0.0
null null 305.22 14.31 5991659.714 3559895.615 0.35 0.0 0.0 0.0
null true 307.13 14.39 5991659.8595 3559895.6659 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.8000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559887.48 5991636.46 12.66,3559887.6506 5991636.9476 12.57,3559888.2471 5991638.6527 12.5,3559888.4176 5991639.1403 12.42,3559888.8012 5991640.2366 12.07,3559889.3966 5991641.9387 12.06,3559889.7578 5991642.971 12.3,3559889.8004 5991643.0929 12.22,3559889.9598 5991643.5485 12.38,3559889.9651 5991643.5637 11.87,3559889.9966 5991643.6536 11.26,3559890.0174 5991643.7131 10.62,3559890.0893 5991643.9188 9.56,3559890.1069 5991643.9691 9.31,3559890.117 5991643.998 9.4,3559890.1434 5991644.0734 9.41,3559890.2053 5991644.2502 10.5,3559890.2588 5991644.4033 11.6,3559890.2855 5991644.4795 12.38,3559890.4137 5991644.846 12.51,3559890.6696 5991645.5774 12.23,3559890.7548 5991645.8212 12.18,3559890.9254 5991646.3088 12.13,3559891.096 5991646.7964 12.16,3559891.4369 5991647.7708 12.27,3559891.6075 5991648.2584 12.34,3559891.9486 5991649.2336 12.6,3559892.0766 5991649.5993 12.72,3559892.1616 5991649.8424 12.89,3559892.7586 5991651.549 13.42,3559892.9289 5991652.0358 13.47,3559893.0995 5991652.5234 13.49,3559893.3127 5991653.1329 13.49,3559893.9097 5991654.8395 13.84,3559894.2509 5991655.8147 13.88,3559894.8053 5991657.3994 13.88,3559895.1464 5991658.3746 14.07,3559895.4023 5991659.106 14.23,3559895.615 5991659.714 14.31,3559895.6659 5991659.8595 14.39</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile118285952453887">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859524538116">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.55 5991618.404 3559925.943 0.35 0.0 0.0 0.0
null null 51.1 12.08 5991622.3033 3559927.7833 0.35 0.0 0.0 0.0
null null 83.08 12.42 5991624.7436 3559928.935 0.35 0.0 0.0 0.0
null null 89.45 12.13 5991625.2297 3559929.1644 0.35 5.0 5.0 0.25
low null 92.62 11.77 5991625.4716 3559929.2785 0.35 0.0 0.0 0.0
null null 93.41 11.39 5991625.5319 3559929.307 0.25 0.0 0.0 0.0
null null 94.96 10.24 5991625.6502 3559929.3628 0.12 0.0 0.0 0.0
null null 96.51 9.77 5991625.7685 3559929.4186 0.12 0.0 0.0 0.0
null null 99.61 9.64 5991626.005 3559929.5303 0.12 0.0 0.0 0.0
null null 101.18 10.28 5991626.1248 3559929.5868 0.3 0.0 0.0 0.0
low null 102.78 11.32 5991626.2469 3559929.6444 0.35 5.0 5.0 0.25
null null 105.13 12.41 5991626.4262 3559929.7291 0.35 0.0 0.0 0.0
null null 111.51 12.45 5991626.9131 3559929.9588 0.35 0.0 0.0 0.0
null null 130.68 11.88 5991628.3759 3559930.6492 0.35 0.0 0.0 0.0
null null 156.25 11.98 5991630.3271 3559931.57 0.35 0.0 0.0 0.0
null null 181.79 12.67 5991632.276 3559932.4898 0.35 0.0 0.0 0.0
null null 207.39 13.58 5991634.2295 3559933.4117 0.35 0.0 0.0 0.0
null null 258.56 13.73 5991638.1341 3559935.2545 0.35 0.0 0.0 0.0
null null 284.13 14.03 5991640.0853 3559936.1754 0.35 0.0 0.0 0.0
null true 303.39 14.03 5991641.555 3559936.869 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.8460</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559925.943 5991618.404 13.55,3559927.7833 5991622.3033 12.08,3559928.935 5991624.7436 12.42,3559929.1644 5991625.2297 12.13,3559929.2785 5991625.4716 11.77,3559929.307 5991625.5319 11.39,3559929.3628 5991625.6502 10.24,3559929.4186 5991625.7685 9.77,3559929.5303 5991626.005 9.64,3559929.5868 5991626.1248 10.28,3559929.6444 5991626.2469 11.32,3559929.7291 5991626.4262 12.41,3559929.9588 5991626.9131 12.45,3559930.6492 5991628.3759 11.88,3559931.57 5991630.3271 11.98,3559932.4898 5991632.276 12.67,3559933.4117 5991634.2295 13.58,3559935.2545 5991638.1341 13.73,3559936.1754 5991640.0853 14.03,3559936.869 5991641.555 14.03</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524648223">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952464857">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.36 5991594.343 3559975.033 0.35 0.0 0.0 0.0
null null 33.59 13.15 5991598.2652 3559976.8404 0.35 0.0 0.0 0.0
null null 49.57 12.41 5991600.1311 3559977.7003 0.35 0.0 0.0 0.0
null null 52.77 12.43 5991600.5047 3559977.8724 0.35 0.0 0.0 0.0
null null 59.16 12.38 5991601.2508 3559978.2163 0.35 0.0 0.0 0.0
null null 65.55 12.36 5991601.997 3559978.5601 0.35 0.0 0.0 0.0
null null 68.74 12.34 5991602.3695 3559978.7318 0.35 0.0 0.0 0.0
null null 76.72 12.13 5991603.3012 3559979.1611 0.35 0.0 0.0 0.0
null null 84.47 11.72 5991604.2062 3559979.5781 0.35 0.0 0.0 0.0
null null 84.67 11.97 5991604.2295 3559979.5889 0.35 0.0 0.0 0.0
null null 84.76 12.0 5991604.24 3559979.5938 0.35 5.0 5.0 0.25
low null 85.26 11.62 5991604.2984 3559979.6207 0.25 0.0 0.0 0.0
null null 86.43 10.49 5991604.435 3559979.6836 0.25 0.0 0.0 0.0
null null 87.39 10.05 5991604.5471 3559979.7353 0.12 0.0 0.0 0.0
null null 87.59 10.0 5991604.5705 3559979.746 0.12 0.0 0.0 0.0
null null 88.37 9.9 5991604.6616 3559979.788 0.12 0.0 0.0 0.0
null null 89.7 9.71 5991604.8169 3559979.8596 0.12 0.0 0.0 0.0
null null 91.85 9.78 5991605.0679 3559979.9753 0.12 0.0 0.0 0.0
null null 92.05 9.81 5991605.0913 3559979.986 0.12 0.0 0.0 0.0
null null 92.97 10.09 5991605.1987 3559980.0355 0.3 0.0 0.0 0.0
null null 93.67 10.58 5991605.2804 3559980.0732 0.3 0.0 0.0 0.0
null null 94.52 11.04 5991605.3797 3559980.1189 0.3 0.0 0.0 0.0
low null 96.92 11.88 5991605.6599 3559980.2481 0.35 5.0 5.0 0.25
null null 103.25 12.16 5991606.399 3559980.5887 0.35 0.0 0.0 0.0
null null 122.45 11.88 5991608.6409 3559981.6218 0.35 0.0 0.0 0.0
null null 130.45 11.8 5991609.5751 3559982.0522 0.35 0.0 0.0 0.0
null null 156.04 12.41 5991612.5631 3559983.4292 0.35 0.0 0.0 0.0
null null 168.02 12.72 5991613.9619 3559984.0738 0.35 0.0 0.0 0.0
null null 190.39 14.26 5991616.574 3559985.2775 0.35 0.0 0.0 0.0
null null 194.38 14.46 5991617.0399 3559985.4922 0.35 0.0 0.0 0.0
null null 200.77 14.51 5991617.786 3559985.836 0.35 0.0 0.0 0.0
null true 213.34 14.22 5991619.2537 3559986.5124 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.9000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3559975.033 5991594.343 13.36,3559976.8404 5991598.2652 13.15,3559977.7003 5991600.1311 12.41,3559977.8724 5991600.5047 12.43,3559978.2163 5991601.2508 12.38,3559978.5601 5991601.997 12.36,3559978.7318 5991602.3695 12.34,3559979.1611 5991603.3012 12.13,3559979.5781 5991604.2062 11.72,3559979.5889 5991604.2295 11.97,3559979.5938 5991604.24 12.0,3559979.6207 5991604.2984 11.62,3559979.6836 5991604.435 10.49,3559979.7353 5991604.5471 10.05,3559979.746 5991604.5705 10.0,3559979.788 5991604.6616 9.9,3559979.8596 5991604.8169 9.71,3559979.9753 5991605.0679 9.78,3559979.986 5991605.0913 9.81,3559980.0355 5991605.1987 10.09,3559980.0732 5991605.2804 10.58,3559980.1189 5991605.3797 11.04,3559980.2481 5991605.6599 11.88,3559980.5887 5991606.399 12.16,3559981.6218 5991608.6409 11.88,3559982.0522 5991609.5751 11.8,3559983.4292 5991612.5631 12.41,3559984.0738 5991613.9619 12.72,3559985.2775 5991616.574 14.26,3559985.4922 5991617.0399 14.46,3559985.836 5991617.786 14.51,3559986.5124 5991619.2537 14.22</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524757107">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859524757285">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.56 5991573.008 3560024.0 0.35 0.0 0.0 0.0
null null 51.17 12.87 5991577.4656 3560026.2538 0.35 0.0 0.0 0.0
null null 63.97 12.53 5991578.5807 3560026.8176 0.35 0.0 0.0 0.0
null null 76.76 12.4 5991579.6949 3560027.3809 0.35 0.0 0.0 0.0
null null 95.93 12.66 5991581.3648 3560028.2253 0.35 0.0 0.0 0.0
null null 102.28 12.29 5991581.918 3560028.5049 0.35 5.0 5.0 0.25
low null 105.46 11.9 5991582.195 3560028.645 0.25 0.0 0.0 0.0
null null 109.36 9.96 5991582.5348 3560028.8168 0.12 0.0 0.0 0.0
null null 112.48 9.63 5991582.8066 3560028.9542 0.12 0.0 0.0 0.0
null null 114.07 10.18 5991582.9451 3560029.0242 0.3 0.0 0.0 0.0
low null 118.03 12.17 5991583.2901 3560029.1987 0.35 5.0 5.0 0.25
null null 124.37 12.53 5991583.8424 3560029.4779 0.35 0.0 0.0 0.0
null null 149.96 11.95 5991586.0716 3560030.605 0.35 0.0 0.0 0.0
null null 162.76 11.89 5991587.1867 3560031.1688 0.35 0.0 0.0 0.0
null null 175.56 12.21 5991588.3017 3560031.7326 0.35 0.0 0.0 0.0
null null 194.76 13.92 5991589.9743 3560032.5783 0.35 0.0 0.0 0.0
null true 251.9 14.01 5991594.952 3560035.095 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>68.9510</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3560024.0 5991573.008 13.56,3560026.2538 5991577.4656 12.87,3560026.8176 5991578.5807 12.53,3560027.3809 5991579.6949 12.4,3560028.2253 5991581.3648 12.66,3560028.5049 5991581.918 12.29,3560028.645 5991582.195 11.9,3560028.8168 5991582.5348 9.96,3560028.9542 5991582.8066 9.63,3560029.0242 5991582.9451 10.18,3560029.1987 5991583.2901 12.17,3560029.4779 5991583.8424 12.53,3560030.605 5991586.0716 11.95,3560031.1688 5991587.1867 11.89,3560031.7326 5991588.3017 12.21,3560032.5783 5991589.9743 13.92,3560035.095 5991594.952 14.01</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524866145">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859524866172">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AX"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_AY"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BEWUCHS_DP"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.62 5991544.961 3560065.514 0.35 0.0 0.0 0.0
null null 38.38 13.02 5991550.9457 3560068.1906 0.35 0.0 0.0 0.0
null null 79.96 12.82 5991557.4294 3560071.0905 0.35 0.0 0.0 0.0
null null 86.36 12.71 5991558.4274 3560071.5368 0.35 0.0 0.0 0.0
null null 90.36 12.6 5991559.0511 3560071.8158 0.35 0.0 0.0 0.0
null null 90.57 12.88 5991559.0839 3560071.8304 0.35 5.0 5.0 0.25
low null 92.18 12.09 5991559.3349 3560071.9427 0.35 0.0 0.0 0.0
null null 92.58 11.82 5991559.3973 3560071.9706 0.35 0.0 0.0 0.0
null null 92.78 11.73 5991559.4285 3560071.9845 0.3 0.0 0.0 0.0
null null 93.97 10.97 5991559.614 3560072.0675 0.3 0.0 0.0 0.0
null null 94.17 10.86 5991559.6452 3560072.0815 0.15 0.0 0.0 0.0
null null 95.57 10.32 5991559.8635 3560072.1791 0.15 0.0 0.0 0.0
null null 97.16 10.09 5991560.1115 3560072.29 0.15 0.0 0.0 0.0
null null 98.25 9.96 5991560.2814 3560072.366 0.15 0.0 0.0 0.0
null null 99.84 10.32 5991560.5294 3560072.4769 0.15 0.0 0.0 0.0
null null 101.42 10.8 5991560.7757 3560072.5871 0.3 0.0 0.0 0.0
null null 102.22 11.1 5991560.9005 3560072.6429 0.3 0.0 0.0 0.0
low null 104.82 12.38 5991561.3059 3560072.8242 0.35 5.0 5.0 0.25
null null 109.63 12.38 5991562.0559 3560073.1597 0.35 0.0 0.0 0.0
null null 116.03 12.26 5991563.0539 3560073.606 0.35 0.0 0.0 0.0
null null 128.83 12.1 5991565.0499 3560074.4987 0.35 0.0 0.0 0.0
null null 133.63 12.14 5991565.7983 3560074.8334 0.35 0.0 0.0 0.0
null null 164.44 12.04 5991570.6026 3560076.9821 0.35 0.0 0.0 0.0
null null 169.25 12.57 5991571.3527 3560077.3176 0.35 0.0 0.0 0.0
null null 179.25 13.89 5991572.912 3560078.015 0.35 0.0 0.0 0.0
null true 188.15 13.95 5991574.2998 3560078.6357 0.35 0.0 0.0 0.0
]]></om:result>
       <prof:station>69.0000</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3560065.514 5991544.961 13.62,3560068.1906 5991550.9457 13.02,3560071.0905 5991557.4294 12.82,3560071.5368 5991558.4274 12.71,3560071.8158 5991559.0511 12.6,3560071.8304 5991559.0839 12.88,3560071.9427 5991559.3349 12.09,3560071.9706 5991559.3973 11.82,3560071.9845 5991559.4285 11.73,3560072.0675 5991559.614 10.97,3560072.0815 5991559.6452 10.86,3560072.1791 5991559.8635 10.32,3560072.29 5991560.1115 10.09,3560072.366 5991560.2814 9.96,3560072.4769 5991560.5294 10.32,3560072.5871 5991560.7757 10.8,3560072.6429 5991560.9005 11.1,3560072.8242 5991561.3059 12.38,3560073.1597 5991562.0559 12.38,3560073.606 5991563.0539 12.26,3560074.4987 5991565.0499 12.1,3560074.8334 5991565.7983 12.14,3560076.9821 5991570.6026 12.04,3560077.3176 5991571.3527 12.57,3560078.015 5991572.912 13.89,3560078.6357 5991574.2998 13.95</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524882211">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition118285952488284">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.2 13.36 5991534.364 3560102.909 0.35
null null 25.79 13.8 5991536.3565 3560104.2453 0.35
null null 28.98 13.77 5991536.6048 3560104.4119 0.35
null null 32.18 13.56 5991536.854 3560104.579 0.35
null null 35.38 13.01 5991537.1031 3560104.7461 0.35
null null 41.78 12.85 5991537.6014 3560105.0803 0.35
null null 67.36 12.59 5991539.5931 3560106.4162 0.35
null null 86.53 12.81 5991541.0857 3560107.4172 0.35
low null 92.92 12.77 5991541.5832 3560107.7509 0.35
null null 95.25 11.55 5991541.7647 3560107.8726 0.25
null null 96.41 10.79 5991541.855 3560107.9332 0.12
null null 97.94 10.38 5991541.9741 3560108.0131 0.12
null null 101.88 10.5 5991542.2809 3560108.2188 0.12
null null 103.44 10.74 5991542.4023 3560108.3003 0.25
null null 104.23 10.95 5991542.4639 3560108.3415 0.25
null null 105.8 11.76 5991542.5861 3560108.4235 0.35
low null 108.64 12.91 5991542.8072 3560108.5718 0.35
null null 108.73 12.81 5991542.8142 3560108.5765 0.35
null null 110.29 12.87 5991542.9357 3560108.658 0.35
null null 123.04 12.38 5991543.9284 3560109.3238 0.35
null null 164.6 12.27 5991547.1643 3560111.4941 0.35
null null 190.1 13.71 5991549.1498 3560112.8258 0.35
null null 215.7 14.21 5991551.143 3560114.1626 0.35
null null 241.31 14.56 5991553.137 3560115.5 0.35
null true 262.11 14.74 5991554.7565 3560116.5862 0.35
]]></om:result>
       <prof:station>69.0420</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3560102.909 5991534.364 13.36,3560104.2453 5991536.3565 13.8,3560104.4119 5991536.6048 13.77,3560104.579 5991536.854 13.56,3560104.7461 5991537.1031 13.01,3560105.0803 5991537.6014 12.85,3560106.4162 5991539.5931 12.59,3560107.4172 5991541.0857 12.81,3560107.7509 5991541.5832 12.77,3560107.8726 5991541.7647 11.55,3560107.9332 5991541.855 10.79,3560108.0131 5991541.9741 10.38,3560108.2188 5991542.2809 10.5,3560108.3003 5991542.4023 10.74,3560108.3415 5991542.4639 10.95,3560108.4235 5991542.5861 11.76,3560108.5718 5991542.8072 12.91,3560108.5765 5991542.8142 12.81,3560108.658 5991542.9357 12.87,3560109.3238 5991543.9284 12.38,3560111.4941 5991547.1643 12.27,3560112.8258 5991549.1498 13.71,3560114.1626 5991551.143 14.21,3560115.5 5991553.137 14.56,3560116.5862 5991554.7565 14.74</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
     <simBase:riverProfile>
      <prof:Profile xmlns:prof="org.kalypso.model.wspmprofile" gml:id="Profile1182859524991293">
       <gml:name>Ist</gml:name>
       <om:resultDefinition>
        <swe:RecordDefinition gml:id="RecordDefinition1182859525007123">
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#DURCHSTROEMTE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOCHWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RECHTSWERT"/>
         <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#RAUHEIT"/>
        </swe:RecordDefinition>
       </om:resultDefinition>
       <om:result><![CDATA[null true 0.0 13.09 5991527.276 3560126.414 0.35
null null 33.6 13.4 5991530.1856 3560127.6792 0.35
null null 36.8 13.06 5991530.4627 3560127.7997 0.35
null null 48.0 12.5 5991531.4325 3560128.2214 0.35
null null 68.81 12.98 5991533.2346 3560129.005 0.35
null null 78.41 12.45 5991534.0659 3560129.3665 0.35
low null 83.98 12.44 5991534.5482 3560129.5763 0.35
null null 85.56 11.71 5991534.685 3560129.6358 0.25
null null 87.15 10.83 5991534.8227 3560129.6956 0.12
null null 88.72 10.23 5991534.9587 3560129.7548 0.12
null null 90.28 10.0 5991535.0938 3560129.8135 0.12
null null 91.83 10.27 5991535.228 3560129.8719 0.12
null null 93.22 10.63 5991535.3484 3560129.9242 0.25
null null 93.62 11.03 5991535.383 3560129.9393 0.25
null null 95.21 11.71 5991535.5207 3560129.9991 0.35
low null 97.23 12.76 5991535.6956 3560130.0752 0.35
null null 98.02 12.91 5991535.764 3560130.1049 0.35
null null 102.78 12.56 5991536.1762 3560130.2842 0.35
null null 113.99 12.71 5991537.1469 3560130.7063 0.35
null null 119.6 13.8 5991537.6327 3560130.9175 0.35
null null 135.61 12.77 5991539.0191 3560131.5204 0.35
null null 161.22 13.06 5991541.2368 3560132.4847 0.35
null null 174.02 13.04 5991542.3452 3560132.9667 0.35
null null 177.22 13.11 5991542.6223 3560133.0872 0.35
null null 183.62 13.55 5991543.1765 3560133.3282 0.35
null null 196.42 14.05 5991544.2849 3560133.8102 0.35
null null 247.59 14.82 5991548.716 3560135.737 0.35
null true 276.45 14.38 5991551.2151 3560136.8237 0.35
]]></om:result>
       <prof:station>69.0660</prof:station>
       <prof:type>org.kalypso.model.wspm.tuhh.profiletype</prof:type>
       <prof:profileLocation>
        <gml:LineString xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
         <gml:coordinates ts="," decimal="." cs=" ">3560126.414 5991527.276 13.09,3560127.6792 5991530.1856 13.4,3560127.7997 5991530.4627 13.06,3560128.2214 5991531.4325 12.5,3560129.005 5991533.2346 12.98,3560129.3665 5991534.0659 12.45,3560129.5763 5991534.5482 12.44,3560129.6358 5991534.685 11.71,3560129.6956 5991534.8227 10.83,3560129.7548 5991534.9587 10.23,3560129.8135 5991535.0938 10.0,3560129.8719 5991535.228 10.27,3560129.9242 5991535.3484 10.63,3560129.9393 5991535.383 11.03,3560129.9991 5991535.5207 11.71,3560130.0752 5991535.6956 12.76,3560130.1049 5991535.764 12.91,3560130.2842 5991536.1762 12.56,3560130.7063 5991537.1469 12.71,3560130.9175 5991537.6327 13.8,3560131.5204 5991539.0191 12.77,3560132.4847 5991541.2368 13.06,3560132.9667 5991542.3452 13.04,3560133.0872 5991542.6223 13.11,3560133.3282 5991543.1765 13.55,3560133.8102 5991544.2849 14.05,3560135.737 5991548.716 14.82,3560136.8237 5991551.2151 14.38</gml:coordinates>
        </gml:LineString>
       </prof:profileLocation>
      </prof:Profile>
     </simBase:riverProfile>
    </simBase:RiverProfileNetwork>
   </simBase:riverProfileNetwork>
  </simBase:RiverProfileNetworkCollection>
 </simBase:riverProfileNetworkCollectionMember>
</simBase:TerrainModel>
