<?xml version="1.0" encoding="WINDOWS-1252"?>
<simBase:TerrainModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase" xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel" xmlns:swe="http://www.opengis.net/swe" xmlns:ns0="http://www.tu-harburg.de/wb/kalypso/schemata/observation" xmlns:sweExt="org.kalypso.swe.ext" xmlns:om="http://www.opengis.net/om" gml:id="root">
 <simBase:terrainElevationModelSystem>
  <simBase:TerrainElevationModelSystem gml:id="Scenario_Terrain_Elevation_ModelSystem">
   <simBase:terrainElevationModel>
    <simBase:NativeTerrainElevationModelWrapper gml:id="NativeTerrainElevationModelWrapper1188548756437137">
     <gml:description>Keine Information:</gml:description>
     <gml:name>h1</gml:name>
     <simBase:fileName>native_tem/hoehenmodell_stoer_10m.asc</simBase:fileName>
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
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481170463">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556143.094 5990062.489,3556157.107 5990082.202,3556173.963 5990072.244,3556163.403 5990056.596,3556143.094 5990062.489</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171405">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556122.38 5990110.247,3556133.144 5990095.615,3556124.208 5990090.331,3556119.537 5990096.834,3556108.57 5990101.305,3556122.38 5990110.247</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171567">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556115.272 5990137.276,3556141.469 5990152.518,3556147.359 5990133.212,3556119.537 5990124.88,3556115.272 5990137.276</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171565">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556058.852 5990188.307,3556066.493 5990204.99,3556080.386 5990197.344,3556070.661 5990181.356,3556058.852 5990188.307</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171563">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556095.668 5990235.808,3556106.783 5990242.991,3556116.045 5990233.722,3556100.762 5990223.527,3556095.668 5990235.808</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171560">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556122.297 5990245.308,3556135.727 5990242.527,3556132.717 5990229.783,3556128.549 5990215.649,3556115.582 5990217.966,3556119.518 5990232.1,3556122.297 5990245.308</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715610">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556160.734 5990262.222,3556147.304 5990258.747,3556149.157 5990265.466,3556152.861 5990286.783,3556166.986 5990280.527,3556160.734 5990262.222</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171566">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556324.455 5990472.147,3556331.404 5990454.064,3556334.472 5990446.006,3556328.974 5990443.6,3556324.978 5990451.707,3556317.831 5990469.065,3556308.038 5990493.674,3556298.755 5990516.187,3556308.04 5990511.144,3556314.248 5990496.073,3556324.455 5990472.147</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171568">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559965.513 5991576.245,3559976.828 5991577.017,3559979.142 5991567.753,3559967.056 5991563.893,3559965.513 5991576.245</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171569">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559933.883 5991553.085,3559961.913 5991550.254,3559982.742 5991542.535,3559977.599 5991527.61,3559955.741 5991537.131,3559931.311 5991546.652,3559933.883 5991553.085</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715614">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560040.602 5991518.088,3560048.831 5991515.515,3560047.031 5991505.994,3560037.773 5991510.111,3560040.602 5991518.088</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171561">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560072.746 5991506.509,3560070.432 5991497.759,3560056.803 5991502.906,3560059.374 5991511.912,3560072.746 5991506.509</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715615">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558817.364 5991393.866,3558852.429 5991417.593,3558868.068 5991426.519,3558862.995 5991405.866,3558854.266 5991403.724,3558817.364 5991393.866</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171562">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558893.843 5991432.128,3558915.772 5991425.667,3558921.395 5991416.412,3558898.181 5991412.079,3558877.281 5991408.619,3558883.69 5991430.66,3558893.843 5991432.128</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715613">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553527.384 5989130.853,3553532.59 5989129.941,3553527.946 5989110.085,3553522.569 5989111.06,3553519.597 5989111.494,3553522.7515916 5989131.8073919,3553527.384 5989130.853</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715616">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553514.4380483 5989133.6852967,3553510.357 5989113.187,3553508.194 5989113.656,3553511.637 5989133.614,3553514.4380483 5989133.6852967</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715617">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556336.852 5990425.541,3556342.218 5990427.868,3556345.707 5990419.085,3556355.247 5990395.754,3556368.4 5990365.285,3556374.49 5990346.027,3556368.644 5990344.443,3556362.432 5990362.603,3556350.01 5990393.073,3556340.404 5990416.771,3556336.852 5990425.541</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715611">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553041.245 5988702.871,3553028.371 5988708.921,3553028.991 5988721.493,3553043.586 5988708.726,3553062.702 5988698.577,3553077.916 5988691.551,3553081.495 5988690.278,3553078.739 5988684.646,3553075.5142349 5988686.1667352,3553060.419 5988693.1464348,3553041.245 5988702.871</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715619">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556121.364 5990173.451,3556124.41 5990167.76,3556105.727 5990159.225,3556102.883 5990164.915,3556121.364 5990173.451</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715621">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556108.172 5990197.344,3556123.145 5990199.912,3556121.364 5990173.451,3556108.172 5990197.344</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715618">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553469.857 5988420.35,3553433.062 5988400.173,3553382.448 5988387.001,3553347.341 5988409.248,3553323.621 5988422.965,3553300.312 5988436.236,3553281.587 5988448.921,3553255.548 5988459.655,3553249.348 5988481.305,3553247.983 5988502.578,3553235.653 5988523.082,3553217.708 5988539.866,3553202.843 5988556.371,3553188.492 5988568.735,3553183.03 5988574.981,3553176.203 5988580.055,3553158.259 5988589.813,3553153.382 5988594.302,3553147.968 5988601.543,3553134.308 5988620.263,3553125.292 5988646.667,3553125.379 5988657.683,3553125.465 5988665.576,3553122.369 5988666.553,3553116.028914 5988669.2147999,3553109.283384 5988672.0468201,3553102.6128715 5988674.8473452,3553104.7386196 5988679.7411746,3553110.5009041 5988677.5215827,3553118.0335779 5988674.6200492,3553125.295 5988671.823,3553128.221 5988671.237,3553133.224 5988696.921,3553140.831 5988715.949,3553151.949 5988739.075,3553156.045 5988750.2,3553161.311 5988755.762,3553170.381 5988761.617,3553191.18 5988769.619,3553206.784 5988777.035,3553224.729 5988783.865,3553248.851 5988788.804,3553280.578 5988792.447,3553299.043 5988793.878,3553321.214 5988804.416,3553341.369 5988810.531,3553359.191 5988810.588,3553377.16 5988807.799,3553406.613 5988803.115,3553444.565 5988792.869,3553460.761 5988777.425,3553467.198 5988751.663,3553470.416 5988727.659,3553475.646 5988722.334,3553491.967 5988726.028,3553493.591 5988713.185,3553490.08 5988687.424,3553486.338 5988664.966,3553485.753 5988634.814,3553483.923 5988610.63,3553488.311 5988590.724,3553492.992 5988564.671,3553486.557 5988543.037,3553488.605 5988518.155,3553501.944 5988489.55,3553484.876 5988461.214,3553469.857 5988420.35</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715612">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553021.776 5986902.205,3553036.103 5986900.742,3553034.349 5986892.55,3553019.437 5986894.306,3552994.584 5986896.646,3552976.661 5986900.137,3552971.197 5986900.612,3552967.328 5986901.287,3552966.675 5986904.563,3552965.757 5986909.153,3552968.419 5986910.53,3552973.182 5986910.131,3552996.046 5986905.715,3553021.776 5986902.205</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715622">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552917.464 5986921.669,3552929.928 5986916.517,3552933.717 5986915.205,3552937.506 5986914.331,3552938.089 5986910.395,3552939.442 5986904.668,3552934.685 5986904.502,3552926.066 5986904.855,3552918.011 5986902.633,3552912.499 5986901.947,3552911.027 5986908.101,3552906.435 5986915.989,3552899.337 5986935.906,3552917.464 5986921.669</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117813102775819"/>
     <simBase:roughnessStyle>deaktivierte Elemente</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715627">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557652.9024586 5990735.9991590995,3557675.3186688 5990754.2702067,3557696.9521737 5990774.4249857,3557708.3741734 5990785.3513305,3557723.7947615 5990799.0152741,3557747.5690862 5990820.6195665,3557769.3180782 5990840.8364862,3557782.8540715 5990853.6631486,3557800.2766799 5990869.9616108,3557823.1187026 5990890.8079891,3557844.408829 5990910.2836936,3557857.5488536 5990922.5469145,3557874.852295 5990938.6295815,3557890.670879 5990952.7414356,3557904.7535329 5990964.1840184,3557921.4790985 5990976.6415311,3557932.6429653 5990984.4871107,3557956.4936647 5990999.3275598,3557977.084894 5991011.1957491,3557999.2683001 5991024.4552551,3558021.110303 5991035.8369726,3558042.1386798 5991044.6415034,3558061.2806391 5991052.8441721,3558091.6083487 5991064.4085233,3558114.1846322 5991073.23659,3558114.7256596 5991071.3496194,3558115.3694906 5991069.1040954,3558116.0907686 5991066.588455,3558116.528717 5991065.0609989,3558116.7605072 5991064.2525718,3558094.7641242 5991055.4466409,3558064.9312348 5991043.2442759,3558045.6619901 5991034.8161156,3558025.0608292 5991025.7908108,3558003.0400373 5991015.5559413005,3557981.1616065 5991003.3000913,3557960.4166736 5990990.5965246,3557938.1400866 5990975.6586534,3557926.5853381 5990967.8551653,3557910.9399596 5990956.3986739,3557896.7254123 5990945.468816,3557881.0640802 5990931.4397917,3557862.934491 5990914.6773398,3557851.0370239 5990903.1334468,3557829.8849153 5990883.6498027,3557806.6592912 5990862.2745924,3557789.0443584 5990845.8668435,3557776.0195472 5990833.5236882,3557754.5124825 5990813.7758396,3557730.6739204 5990792.0314976,3557715.2117736 5990778.010469,3557703.5127082 5990767.4642889,3557681.4336737 5990747.2926199,3557658.2208466 5990728.6612211,3557637.7720441 5990715.078882,3557617.3358072 5990702.6297377,3557595.926634 5990692.7984445,3557568.3559871 5990682.7359191,3557543.9728925 5990675.6783518,3557518.5651411 5990670.6107182,3557495.5521983 5990668.914327,3557469.5703479 5990667.8455169,3557444.24194 5990668.9490361,3557423.4928029 5990672.1172791,3557394.5181424 5990677.4879217,3557367.9179042 5990682.5529144,3557346.0910722 5990686.9376532,3557326.3402269 5990690.174651,3557299.6914121 5990694.9436677,3557269.3527023 5990700.7419953,3557247.1345017 5990704.8505645,3557223.1227569 5990708.8894373,3557198.5880742 5990712.5373693,3557172.9262041 5990714.1158494,3557149.2417926 5990713.482414,3557133.5076746 5990711.7657127,3557114.4232368 5990708.6010843,3557100.2827747 5990705.5898414,3557094.0420832 5990703.6837095,3557085.8214301 5990701.422653,3557082.2809406 5990700.4310146,3557080.3187506 5990699.5640163,3557077.3445859 5990699.0100862,3557075.4210037002 5990697.9977081,3557075.1558224 5990698.7768152,3557074.3870221 5990701.0355632,3557073.5122137 5990703.6057646,3557072.7434135 5990705.8645125,3557072.3725463 5990706.9541264,3557074.8959328 5990707.6617344,3557077.3480338 5990708.7181773,3557079.8594048 5990709.3345921,3557083.2623653 5990710.2683641,3557091.3107312 5990712.2423084,3557098.311616 5990714.2985482,3557111.4765633 5990717.0685607,3557131.6336399 5990721.0160189,3557148.819316 5990723.2853144,3557173.2386118 5990723.6805965,3557199.5728915 5990722.0706369,3557225.2305132 5990718.2520338,3557248.6577125 5990713.9611077,3557271.3755462 5990710.0647545,3557301.2762343 5990704.1707655,3557327.4186067 5990699.089435,3557347.6679 5990695.5271179,3557369.5627367 5990691.3545253,3557395.9461246 5990686.2728575,3557424.7041394 5990680.9572617,3557445.1487211 5990677.9906796,3557469.9343964 5990676.7393761,3557494.7403501 5990677.0559501,3557517.2751857 5990678.9645873,3557541.465485 5990684.4292099,3557565.859088 5990691.2133099,3557592.644726 5990701.1271529,3557613.5967576 5990710.4967641,3557633.5767331 5990722.5139163,3557652.9024586 5990735.9991590995</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715620">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556276.7874348 5990414.2400942,3556304.9173239 5990427.4104744,3556321.3852359 5990435.0884503,3556327.9162211 5990438.4367321,3556328.3471732 5990437.4214049,3556329.7728703 5990434.0624484,3556330.4691137 5990432.422092,3556331.4638588 5990430.0784627,3556331.79518 5990429.2978666,3556325.7951316 5990426.3494835,3556308.3528324 5990418.4589869,3556280.5526994 5990405.5550058,3556260.4734994 5990395.9198099,3556243.0381812 5990387.7535886,3556218.9577455 5990376.8085402,3556190.0489962 5990362.9749647,3556171.340819 5990354.0371962,3556154.8227395 5990345.9168209,3556136.4959673 5990336.202472,3556124.5599326 5990329.2218498,3556106.6616748 5990314.7697688,3556090.1566913 5990299.1733341,3556074.3570814 5990281.5473956,3556057.9921788 5990260.5663442,3556047.6481188 5990241.5025663,3556036.532628 5990218.4946979,3556035.2134474 5990219.1318695,3556033.2792495 5990220.0660978,3556030.7291339 5990221.2978178,3556028.707591 5990222.2742343,3556027.0363287 5990223.0814633,3556038.1344275 5990245.5930985,3556049.4942775 5990265.7151081,3556065.5343883 5990287.6765087,3556082.4939988 5990307.026227,3556100.5024376 5990323.4122447,3556119.2130415 5990337.4847621,3556131.791877 5990345.6128455,3556152.227184 5990356.2235015,3556167.5218501 5990363.6590141,3556186.5010508 5990372.1234588,3556215.3340457 5990385.4766241,3556239.3381871 5990396.5723605,3556256.7277742 5990405.0438612,3556276.7874348 5990414.2400942</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171564">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556013.8908075 5990182.2865739,3556019.4868136 5990202.8512352,3556020.6956766 5990202.4874397,3556022.1936697 5990202.0366334,3556024.9304357 5990201.2130304,3556027.3730866 5990200.4779386,3556028.9553262 5990200.0017791,3556022.1293709 5990180.4171414,3556018.4749185 5990161.9605823,3556016.3589265 5990142.595905,3556015.4852577 5990123.8916227,3556014.0352355 5990123.9845983,3556012.7788156 5990124.0651602,3556010.6521829 5990124.2015202,3556008.1383453 5990124.3627078,3556006.0107145 5990124.4991318,3556007.223899 5990143.338607,3556009.7930804 5990163.3962716,3556013.8908075 5990182.2865739</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715623">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556017.9716157 5990090.7668014,3556020.4555183 5990072.5926927,3556024.1564294 5990057.4669687,3556022.4473899 5990057.216834,3556021.0249256 5990057.0086424,3556018.1413243 5990056.5865991,3556016.3772505 5990056.3284096,3556015.0960906 5990056.1408992,3556011.7339429 5990071.0213344,3556008.9354388 5990089.9063079,3556007.1408379 5990108.6067344,3556008.9532263 5990108.6804841,3556011.3385 5990108.7775456,3556013.4694493 5990108.8642581,3556014.7413986 5990108.9160163,3556016.2683277 5990108.9781501,3556017.9716157 5990090.7668014</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715630">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557027.6209761 5990692.864998,3557046.5524386 5990698.5111094,3557056.1052492 5990701.7394436,3557056.5629367 5990700.602079,3557057.6317453 5990697.9460637,3557058.2424931 5990696.4283407,3557059.0820846 5990694.3419354,3557059.8835977 5990692.3501558,3557049.071263 5990688.4917221,3557030.0726136 5990682.7584862,3557013.4179139 5990677.4862548,3556999.1517761 5990672.6251331,3556975.559844 5990666.1380989,3556957.6975608 5990661.8996696,3556934.342159 5990657.0226373,3556908.2349657 5990654.8372274,3556892.45 5990654.5537123,3556873.461672 5990655.1075958,3556856.8446154 5990656.9581302,3556836.9636609 5990659.6483537,3556809.2688393 5990663.381942,3556779.6188573 5990667.6860638,3556756.6093173 5990671.0602178,3556731.9111842 5990675.2710505,3556707.4088541 5990677.5270423,3556684.0258721 5990676.2893107,3556659.622452 5990673.7732614,3556635.379368 5990668.893549,3556610.9074494 5990660.952736,3556589.6462197 5990652.5598224,3556568.036577 5990641.4744725,3556550.1274552 5990629.4447824,3556530.451973 5990613.2052009,3556509.9911345 5990593.3661401,3556494.1967249 5990575.8083557,3556477.5396693 5990555.5678732,3556448.9336695 5990521.3336004,3556430.7346738 5990500.4218321,3556415.4534255 5990483.7003449,3556397.0393949 5990467.7239181,3556376.7828433 5990452.8127608,3556358.8470495 5990442.4866198,3556351.7935082 5990438.4839295,3556348.192801 5990436.5493182,3556344.3754022 5990434.6547756,3556340.1490971 5990432.9903478,3556339.836847 5990433.8712343,3556339.1718616 5990435.7472197,3556338.3565667 5990438.0472421,3556337.278342 5990441.089014,3556336.7747381 5990442.5097274,3556341.1346725 5990444.2439651,3556344.8167621 5990445.9738988,3556347.7729422 5990447.9668114,3556354.7820056 5990451.3263335,3556372.2259084 5990460.9830229,3556391.6018086 5990475.3289464,3556408.6406958 5990490.6442746,3556423.2697056 5990507.2532602,3556440.6302873 5990527.5380378,3556469.0903328 5990561.2022273,3556487.1151836 5990582.5392303,3556504.173962 5990601.2571861,3556522.9462367 5990619.7507628,3556545.2386882 5990638.0975582,3556563.2671068 5990650.5268743,3556585.504707 5990661.729038,3556607.6328097 5990670.5705494,3556632.5684562 5990679.0145389,3556657.7964398 5990684.2645378,3556683.2683872 5990686.6792255,3556707.6306541 5990687.6576145,3556732.8937896 5990685.4015534,3556757.5507244 5990681.9245069,3556781.3199504 5990678.5179509,3556810.1224408 5990674.4028535,3556838.209319 5990670.3930528,3556858.0565371 5990667.9293967,3556874.2519487 5990666.3492091,3556892.45 5990665.7423173,3556906.7484364 5990666.1449352,3556932.9172269 5990667.8951969,3556955.1954106 5990671.8302951995,3556972.4915117 5990676.1351129,3556995.3483616 5990682.7123211,3557010.2729419 5990687.6790974,3557027.6209761 5990692.864998</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715628">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558845.417 5991336.891,3558858.107 5991374.318,3558867.457 5991407.067,3558872.47 5991428.953,3558873.553 5991434.115,3558875.64 5991434.115,3558879.517 5991434.214,3558881.703 5991434.214,3558879.984 5991430.123,3558874.136 5991408.069,3558864.786 5991374.819,3558852.263 5991338.061,3558851.466 5991334.387,3558849.903 5991332.162,3558850.158 5991330.097,3558850.5739562 5991328.2434596,3558855.1520942 5991329.6606948,3558861.7231579 5991331.4101125,3558873.4198272 5991333.8549702,3558884.7726709 5991335.6410442,3558895.8933255 5991337.145702,3558909.9111801 5991338.9270491,3558926.458487 5991340.0840116,3558940.1725915 5991340.0582312,3558964.9454317 5991338.6861681,3558988.4716265 5991336.8288978,3559012.4982244 5991335.5197994,3559037.7598953 5991334.8730994,3559053.5173483 5991334.8101148,3559071.600955 5991335.1590301,3559087.8401794 5991336.2764319,3559104.3746915 5991338.4401214,3559122.6981908 5991342.3864152,3559136.4625386 5991345.969326,3559161.1892168 5991352.0637235,3559182.0600414 5991357.519721,3559207.9704908 5991364.0738562,3559231.387966 5991370.5654828,3559254.025676 5991376.4316901,3559277.034234 5991381.3279601,3559304.5011066 5991385.1843977,3559331.3463691 5991387.5156187,3559354.8924305 5991387.8090794,3559378.5587669 5991387.4118117,3559397.6226998 5991386.9223582,3559417.7434122 5991385.4144415,3559432.2376356 5991383.5967164,3559448.1400124 5991381.8691858,3559447.784516 5991380.2647297,3559447.3347337 5991378.2347348,3559446.922168 5991376.3727088,3559446.3710386 5991373.8853055,3559445.9736952 5991372.0919823,3559430.7408061 5991374.9909203,3559417.2638469 5991376.3853357,3559397.2185593 5991377.9555799,3559378.601311 5991378.4599128,3559355.7118962 5991378.9565313,3559331.6244059 5991378.4508818,3559305.6275453 5991376.3350267,3559278.4976113 5991372.4244183,3559256.9287731 5991367.9605798,3559234.1165849 5991362.8030977,3559210.5262445 5991356.1643672,3559183.9658911 5991349.325436,3559163.7026512 5991344.2852032,3559138.4030481 5991338.0290049,3559124.5829036 5991334.5089496,3559106.2216226 5991330.364741,3559088.9477643 5991327.5403635,3559072.5570822 5991326.4815295,3559053.7884693 5991325.9957499,3559038.3453212 5991326.0134201,3559012.5648854002 5991326.5350514,3558987.7000531 5991328.0788503,3558964.1131671 5991329.8082028,3558939.3167928 5991330.8217933,3558925.8992987 5991330.783639,3558910.8378894 5991330.0150846,3558897.3477585 5991328.4168586,3558886.0909686 5991326.6966732,3558875.4475004 5991325.0042782,3558863.9891273 5991322.8870535,3558857.4706419 5991321.1867303,3558852.5043606 5991319.6413947,3558847.8580934 5991317.8821443,3558844.177636 5991316.688951,3558840.9595035 5991315.5311054,3558832.2427727 5991312.627189,3558818.9689473 5991306.6825515,3558802.1642527 5991298.0707458,3558786.3585796 5991288.9742347,3558768.164349 5991276.2356874,3558753.7158046 5991263.2687451,3558736.4227376 5991246.9163545,3558716.1942528 5991226.7976733,3558707.9598783 5991217.978417,3558694.4759077 5991204.7779756,3558682.7293606 5991194.0494443,3558668.2002161 5991182.0016742,3558651.5593121 5991170.6682541,3558624.6155882 5991156.8608371,3558595.8424386 5991148.8765422,3558573.059675 5991144.9587009,3558547.0502603 5991143.4971104,3558523.6651108 5991144.6497554,3558500.5808011 5991147.4700106,3558476.7045353 5991150.262662,3558452.7881255 5991152.4896687,3558421.514 5991152.2515069,3558400.4174592 5991150.7879372,3558386.6686924 5991149.5049807,3558367.6943094 5991146.8173265,3558352.1522189 5991143.9766208,3558337.1798626 5991140.2979632,3558318.4891394 5991135.1091336,3558303.4389557 5991130.5231115,3558293.4643276 5991126.7593017,3558281.5097759 5991122.1493678,3558275.4437074 5991120.0974188,3558268.3331202 5991117.9576041,3558263.1738019 5991116.2673739,3558260.6021453 5991114.6573302,3558257.182989 5991112.1814834,3558256.7488344 5991113.5156194,3558255.7564367 5991116.5652089,3558254.8571827 5991119.3285724,3558254.0510724 5991121.8057101,3558253.3377962 5991123.9975728,3558257.7100711 5991124.0760492,3558260.8587406 5991124.432523,3558265.9335523 5991126.2046489,3558272.9979402 5991129.2849364,3558278.6306436 5991130.9693383,3558290.0253129 5991135.0972075,3558300.508811 5991138.99151,3558315.7690447 5991143.9980146,3558334.4605788 5991149.3218149,3558350.3386384 5991152.8267102,3558366.2870931 5991155.7145178,3558385.6324946 5991158.962329,3558399.6436664 5991160.7078033,3558421.514 5991162.0919603,3558453.3446096 5991162.3985828,3558477.5816278 5991160.1294151,3558501.8045362 5991157.3152487,3558524.6425447 5991154.7553555,3558547.2736743 5991153.9747287,3558571.4851181 5991154.4094773,3558594.0650662 5991158.0214207,3558621.1094284 5991166.4174371,3558646.1138344 5991180.0955306,3558662.2393098 5991189.7181995995,3558676.8185234 5991200.5822923,3558687.897673 5991211.5268881,3558700.6231733 5991224.9679348,3558708.972423 5991233.9184414,3558729.1161864 5991254.0972575,3558747.356874 5991270.1601986,3558762.8978531 5991283.2601804,3558781.3012592 5991296.4788008,3558797.8857684 5991306.3270145,3558815.6698169 5991315.6519113,3558829.0440731 5991321.299768,3558838.3633705 5991324.2883909,3558842.3912044 5991325.6218023,3558841.723 5991328.108,3558841.949 5991330.81,3558843.773 5991333.573,3558845.417 5991336.891</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715625">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554525.9914198 5989014.6757568,3554546.9280357 5989021.1347929,3554563.491098 5989028.8882433,3554577.7214737 5989036.0282598,3554590.4245631 5989044.6036344,3554599.6210963 5989052.9197793,3554613.0284788 5989067.9123584,3554628.4298087 5989086.6477443,3554640.1991522 5989101.0774654,3554653.0128341 5989116.1364202,3554663.3613946 5989128.8466204,3554679.790726 5989148.4718066,3554695.6171012 5989165.2006439,3554707.7949038 5989176.115643,3554723.6821935 5989189.2282122,3554735.6437587 5989197.8755951,3554736.401699 5989196.7551763,3554738.0432417 5989194.3285796,3554739.774011 5989191.7700845,3554740.3990425 5989190.8461369,3554729.7594081 5989182.3673178,3554715.2013499 5989168.8099218,3554703.2305771 5989157.0431973,3554687.091892 5989140.9394393,3554670.8577304 5989122.0603599,3554661.5361537 5989109.9275103,3554648.1137997 5989093.9757346,3554635.9626492 5989079.6584599,3554621.3394885 5989059.8110836,3554607.1086542 5989044.7017624,3554598.1876945 5989035.9451104,3554584.0438768 5989025.918636,3554568.8526194 5989017.9134262,3554550.8121946 5989010.018599,3554527.0997777 5989004.110261,3554526.9175588 5989005.847275,3554526.4913443 5989009.9101927,3554526.1391626 5989013.2673883,3554525.9914198 5989014.6757568</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715633">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554107.9909637 5988959.7958464,3554128.0509901 5988960.3170254,3554155.2059657 5988963.5535831,3554178.1909923 5988968.5454842,3554201.8174827 5988976.0252638,3554221.2217596 5988984.1334195,3554251.3698298 5988997.1972774,3554284.4555143 5989010.890897,3554315.4049454 5989020.5569778,3554341.8415175 5989025.7991679,3554341.9451248 5989024.3681351,3554342.229419 5989020.4414403,3554342.5148854 5989016.4985559,3554342.7140405 5989013.7478103,3554317.3014175 5989009.4671383,3554288.5073292 5988999.4342704,3554255.7932021 5988985.1545146,3554225.8416443 5988972.4950071,3554205.2091011 5988964.2309671,3554180.6974335 5988956.9728399,3554156.6463247 5988951.2950475,3554128.521311 5988948.6667217,3554106.7186728 5988947.7936487,3554083.9927629 5988949.2835079,3554084.5069572 5988952.8273992,3554084.9288234 5988955.7349536,3554085.4957151 5988959.6420418,3554085.6671611 5988960.8236687,3554107.9909637 5988959.7958464</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715626">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554365.3926456 5989027.523282,3554391.1014413 5989026.931966,3554416.083438 5989024.856591,3554440.2494331 5989020.0599224,3554460.1091665 5989016.4857512005,3554483.9023415 5989012.9065027,3554483.7924097 5989011.5424074,3554483.5232574 5989008.2026162,3554483.2033462 5989004.2329799,3554483.0542425 5989002.3828175,3554458.7207522 5989006.1355588,3554438.0224704 5989009.4344368,3554414.3771111 5989012.7332969,3554390.0636732 5989014.6484467,3554366.1015735 5989015.9727849,3554365.9457522 5989018.5115664,3554365.7080919 5989022.3837441,3554365.4780699 5989026.1314722,3554365.3926456 5989027.523282</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715629">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553859.0978672 5988993.0573162995,3553885.6602539 5988991.3376648,3553910.1562134 5988987.8877705,3553909.9665466 5988986.6321089,3553909.3817556 5988982.7605855,3553908.9467621 5988979.8807745,3553908.5172438 5988977.0372114,3553883.7334991 5988980.7922385,3553857.8611916 5988982.6587102,3553858.1647887 5988985.2115112,3553858.4991351 5988988.0228678,3553858.9486877 5988991.8029379,3553859.0978672 5988993.0573162995</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715634">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553931.104576 5988985.2174534,3553966.9098056 5988979.3414433,3554003.1411877 5988974.425391,3554029.9841956 5988969.4302142,3554029.7528033 5988968.2301972,3554029.0054861 5988964.3545573,3554028.4494329 5988961.4708261,3554027.8139744 5988958.1752933,3554001.06761 5988963.0147508,3553964.5977254 5988967.6466021,3553928.9877241 5988974.3667574,3553929.5593597 5988977.2968837,3553930.1161417 5988980.1508728,3553930.8646049 5988983.9873936,3553931.104576 5988985.2174534</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715635">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553611.5706483 5988986.6799774,3553623.8778581 5988979.2033491,3553623.1499102 5988977.7472428,3553621.7749971002 5988974.9970192,3553620.3383371 5988972.1232838,3553619.1908556 5988969.8279893,3553605.2955903998 5988977.9894473,3553606.905433 5988980.2189704,3553608.8477028 5988982.9088824,3553610.5786892 5988985.3061813,3553611.5706483 5988986.6799774</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715631">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553705.2687145 5988971.1720519,3553729.0202372 5988979.9308127,3553756.6251333 5988985.7473698,3553784.0172677 5988990.3488543,3553809.8435921 5988993.1571379,3553831.6002289 5988993.5399171,3553831.5281858 5988992.2769919,3553831.3135376 5988988.514183,3553831.1539093 5988985.7158791,3553831.0153664 5988983.287207,3553810.6934024 5988982.3510108,3553784.9082947 5988980.3444859,3553758.7664192 5988975.8722956,3553731.3091526 5988969.2311648,3553707.7142454 5988961.3859114,3553684.8340257 5988956.8138275,3553669.2521738 5988956.4726322,3553652.7287199 5988957.581567,3553643.7170993 5988959.1493467,3553633.5007373 5988962.8759165,3553634.3742844 5988965.0338053,3553635.5132465 5988967.8473386,3553636.6822212 5988970.7350106,3553637.2544947 5988972.1486753,3553645.9212193 5988968.3848916,3553654.202881 5988966.1544323,3553669.7246457 5988965.624612,3553683.9975334 5988966.3763105,3553705.2687145 5988971.1720519</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715624">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556024.2995741 5990015.3423951,3556024.5374709 5990018.4650408,3556026.6067471 5990019.010656,3556028.1190593 5990019.4094140995,3556029.8702084 5990019.8711473,3556031.9394847 5990020.4167626,3556033.6906338 5990020.8784958,3556034.9218929 5990018.1117767,3556036.0808342 5990014.3470537,3556038.9268518 5990003.4176926,3556042.3676059 5989990.2044071,3556046.0848505 5989967.6118016,3556046.4552321 5989947.7486805,3556044.403145 5989927.5845454,3556038.9786075 5989906.9447131,3556030.4859309 5989886.1077852,3556023.7799572 5989871.1318417,3556011.3538035 5989852.2355864,3555998.7788832 5989838.4256491,3555980.5772293 5989821.6117324,3555959.2221963 5989808.4560922,3555933.8707574 5989797.6588857,3555909.4897927 5989791.8110186,3555889.1537515 5989786.6482494,3555863.439172 5989780.7051528,3555835.5619164 5989774.0258545,3555811.8100463 5989769.0677256,3555792.4667063 5989764.0934908,3555765.6894039 5989757.5567533,3555735.8502255 5989749.8240003,3555715.2326207 5989744.2771501,3555695.4142288 5989738.0461317,3555669.9834471 5989728.5361169,3555647.7068842 5989719.0257362,3555624.2551526 5989707.1573078,3555603.5848979 5989695.2281406,3555583.6255087 5989682.067623,3555563.9267431 5989669.1752639,3555541.2170509 5989652.0151747,3555522.9017372 5989636.3154375,3555503.7803804 5989618.2013895,3555488.2716346 5989600.8783195,3555472.5412291 5989582.3154186,3555459.6384365 5989565.3315207,3555444.866275 5989543.3458879,3555432.1591001 5989523.1455406,3555417.07833 5989500.2949524,3555401.9673731 5989474.9507085,3555382.2489442 5989442.4617304,3555365.3523283 5989415.1951042,3555351.9095093 5989393.5901831,3555336.6484402 5989374.2857025,3555320.7138225 5989354.5921245,3555301.9350889 5989335.9291937,3555286.0187218 5989321.3000183,3555264.7832169 5989305.8551616,3555242.385531 5989291.4168653,3555219.4399219 5989278.6917513,3555197.5466778 5989268.5282998,3555173.9828471 5989258.894376,3555149.8011585 5989251.5783945,3555124.8276911 5989246.6688068,3555099.7153981 5989242.4763565,3555075.0168361 5989240.6760726,3555050.064 5989240.233,3555023.8950937 5989241.4089796,3555003.6265187 5989243.0829932,3554977.6766722 5989245.6391482,3554948.4012288 5989248.3113486,3554925.1314177 5989248.8816474,3554909.2589321 5989248.3545434,3554890.5170919 5989246.9328685,3554876.4696435 5989245.1698414,3554868.5531306 5989243.7316463,3554857.2507896 5989241.122737,3554846.2991498 5989238.5569911,3554839.3128222 5989236.7197224,3554832.223898 5989234.8960207,3554831.6699471 5989237.1211021,3554831.0717865 5989239.5237631,3554830.5180772 5989241.7478742,3554829.9641263 5989243.9729556,3554837.6493325 5989245.8867832,3554844.6446922002 5989247.5399041,3554855.2830087 5989249.9289108,3554866.8740114 5989252.2719936,3554875.6576696 5989253.3697376,3554889.8523793 5989255.5385824,3554909.2978169 5989257.2798474,3554925.2156895 5989257.7752482,3554948.4851668 5989257.3814519,3554977.998734 5989255.4697734,3555004.1745017 5989253.0188492,3555024.6462943 5989251.2674007,3555050.297 5989250.225,3555074.6128058 5989250.0663847,3555098.084708 5989252.0055685,3555123.0917033 5989255.792113,3555147.3086132 5989261.4381555,3555170.6911056 5989268.5010645,3555193.4354186 5989277.667152,3555215.3198563 5989287.3593616,3555236.9424641 5989299.8117295,3555258.6999237 5989314.3224528,3555279.0994714 5989329.6297899,3555294.895576 5989343.9608597,3555313.2036986 5989362.3287465,3555329.0433301 5989381.010686,3555342.9406255 5989399.4605682,3555356.6214262 5989421.0745621,3555373.4698757 5989447.4639005,3555393.0700762 5989479.6182469,3555408.816389 5989505.6378218,3555422.6725458 5989528.6442206,3555435.6125919 5989549.173876,3555450.7148821 5989571.1924979,3555464.7604952 5989588.7551531,3555481.6494653 5989608.0052396,3555497.7220072 5989624.7932271,3555516.9429987 5989643.3232906,3555535.4101849 5989659.1574665,3555558.2338485 5989678.1119231,3555579.3066183 5989692.3970761,3555599.0801651 5989704.5064188,3555620.0920425 5989715.4040743,3555644.3642741 5989727.9960715,3555666.516515 5989737.9507911,3555691.9884826 5989747.908403,3555712.2735695 5989754.2785958,3555734.0904755 5989759.7392035,3555763.2892404 5989767.0670333,3555790.3517986 5989773.6608308,3555809.7476439 5989778.336033,3555833.5493165 5989783.9083519,3555861.6126054 5989790.389768,3555887.3299026 5989796.3177633,3555907.716363 5989801.3414217,3555932.0548424 5989807.1050894,3555954.863251 5989816.5999098,3555975.1712298 5989829.8173053,3555992.1192261 5989845.1344625,3556004.5478427 5989859.0429623,3556015.8154468 5989876.2924598,3556022.2864849 5989889.6503418,3556029.7240244 5989908.7412641,3556034.1154193 5989929.1289252,3556035.7390767 5989948.9564532,3556034.9579536 5989966.7272619,3556031.1725913 5989987.2828762,3556027.7925276 5990000.4142687,3556024.8498691 5990011.8463618,3556024.2995741 5990015.3423951</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715640">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553496.1483446 5989064.9751709,3553498.973 5989070.126,3553502.7038176 5989083.0618008,3553504.8528174 5989091.1853522,3553508.911 5989102.814,3553512.0548117 5989112.8759183,3553516.0299411 5989133.325712,3553519.0957004 5989132.6332029,3553521.9849079 5989131.9805742,3553518.7045143 5989111.6575258,3553514.81 5989100.962,3553510.8835512 5989089.5330028,3553509.4094345 5989080.086712,3553505.7947299 5989067.1188673,3553503.5354251 5989061.980845,3553501.391 5989059.022,3553502.895 5989058.596,3553499.9435322 5989053.6024341,3553498.572 5989054.735,3553495.934 5989056.347,3553493.5182772 5989057.3779747,3553491.9392119 5989057.992707,3553493.99 5989061.623,3553496.1483446 5989064.9751709</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715641">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559474.4049714 5991375.8396222,3559476.8428206 5991375.1428761,3559479.0887114 5991374.7492642,3559484.6010359 5991374.3448061,3559496.4462815 5991373.5863572,3559513.2478408 5991373.2808518,3559529.3389326 5991374.4200053,3559543.4178327 5991376.7970648,3559559.317852 5991381.3726892,3559570.9085822 5991385.8771647,3559588.4679863 5991393.6990265,3559608.0403512 5991405.0374973,3559619.182882 5991413.467839,3559631.0510226 5991423.7089149,3559643.8558695 5991438.3221105,3559651.7534071 5991449.3791906,3559659.7951174 5991460.557831,3559669.7394697 5991477.4645251,3559675.5169616 5991490.3939086,3559685.0474182 5991509.398695,3559694.1792104 5991529.0221523,3559706.2022566 5991553.657063,3559715.6359861 5991575.264579,3559725.727199 5991592.8891165,3559738.137901 5991610.7014272,3559748.6465014 5991622.3062348,3559759.9751952 5991631.6549624,3559775.8986483 5991641.592636,3559789.1888447 5991648.5612843,3559805.3081665 5991654.1055054,3559824.0271519 5991658.3333721,3559840.2856965 5991659.977335,3559858.8478264 5991660.562729,3559877.090143 5991658.8116429,3559893.6450093 5991654.0827569,3559906.5238244 5991648.1152991,3559922.2552191 5991640.4283917,3559933.679628 5991634.7970692,3559949.0249711 5991626.339894,3559967.7165997 5991616.8022956,3559982.182997 5991609.8588177,3559998.167075 5991602.2105231,3560014.9135175 5991594.2378689,3560030.9594761 5991586.7726456,3560053.3794255 5991576.24042,3560075.067604 5991566.3219141,3560074.02324 5991563.9868193,3560073.2197574 5991562.1903116,3560072.5771346 5991560.7534707,3560071.9345118 5991559.3166297,3560071.0505993 5991557.3402887,3560049.1205545 5991568.0866017,3560026.9982961 5991578.9381135,3560010.8569743 5991586.6106694,3559994.406658 5991594.6178814,3559978.7126285 5991602.3279607,3559964.0377151 5991609.158888,3559945.7059561 5991618.4003723,3559929.7918945 5991626.5593869,3559917.774728 5991632.4191477,3559902.9138519 5991639.5271911,3559890.5446734 5991645.2204074,3559874.7064155 5991650.0361570995,3559857.4860497 5991652.0259996,3559839.7380428 5991652.0662684,3559825.1008695 5991650.2756785,3559807.8152954 5991645.9323795,3559793.6115914 5991640.8806541,3559780.6361591 5991634.2953704,3559764.5475577 5991624.0940809,3559754.1986321 5991615.6902229,3559744.8172696 5991604.7521863,3559733.2857141998 5991589.2458939,3559722.7840544 5991571.959924,3559713.2548787 5991549.8088991,3559701.6993767 5991525.9354487,3559693.0153431 5991506.5312103,3559683.5861979 5991487.0726886,3559677.1024493 5991473.7053146,3559667.166011 5991455.9295771,3559659.2703854 5991443.6226239,3559651.5263023 5991433.0227271,3559637.5269763 5991417.3863638,3559624.0522369 5991405.9786554,3559612.2717867 5991397.5721746,3559592.159815 5991385.7748933,3559573.9271614 5991377.096538,3559562.3352833 5991373.0719009,3559545.3408297 5991368.5115498,3559530.8876919 5991366.2199828,3559513.8474638 5991365.0476221,3559496.6085974 5991365.3671389,3559484.226393 5991366.1396987,3559478.5655952 5991366.6741906,3559476.0383516 5991366.773976,3559473.1071928 5991367.1834531,3559470.7551816 5991367.5058238,3559470.9889229 5991368.8151232,3559471.2896232 5991370.4994926,3559471.6736267 5991372.6504846,3559471.9908471 5991374.4273909,3559472.2913716 5991376.1107759,3559474.4049714 5991375.8396222</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715639">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553089.0470673 5988686.7282977,3553086.8941255 5988681.4890116,3553085.191 5988682.239,3553087.297 5988687.518,3553089.0470673 5988686.7282977</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715644">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553098.412 5988676.417,3553096.6453827 5988677.1949476,3553099.0341792 5988682.2217019,3553100.833 5988681.41,3553098.412 5988676.417</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715645">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552951.7265618 5986890.7912252,3552948.7323796 5986901.2556775,3552945.2215508 5986912.7600118,3552938.8604077 5986923.1018229,3552927.9872944 5986940.0866072,3552912.2083414 5986965.4256857,3552897.8402215 5986987.8791931,3552884.5732866 5987006.7366824,3552873.8931804 5987024.6789812,3552864.6324823 5987043.9141627,3552858.7075645 5987064.5988236,3552856.537712 5987079.8155593,3552854.7566319 5987104.2306878,3552854.6679099 5987128.8664943,3552856.7291185 5987155.1745108,3552861.3584911 5987182.4858464,3552869.7861712 5987208.3834462,3552876.4897515 5987232.9142234,3552880.3364618 5987256.1930361,3552880.7912453 5987277.4108379,3552882.1978784 5987277.4976169,3552877.9221695 5987301.2490856,3552871.0414492 5987325.1551477,3552862.6486996 5987346.152225,3552854.6918444 5987371.7184176,3552851.0894294 5987396.8524581,3552849.0059533 5987422.678252,3552846.996539 5987447.1912616,3552845.1998341 5987474.0211865,3552844.4187967 5987488.3802244,3552845.2011779 5987499.7347808,3552848.5950867 5987511.7789093,3552853.150355 5987521.8226216,3552862.0169657 5987534.4209521,3552876.3389839 5987547.9116484,3552878.9136556 5987545.0978785,3552881.2281931 5987542.5684002,3552882.8767033 5987540.7668003,3552869.4301314 5987528.2222733,3552861.1486832 5987516.5192676,3552857.2888802 5987508.1898413,3552854.6791194 5987497.5658705,3552853.967433 5987487.9271455,3552854.0857521 5987473.5178335,3552856.5530465 5987447.8286681,3552859.6267214 5987423.315094,3552861.4654532 5987397.440427,3552865.9342203 5987374.0661573,3552873.5639897 5987349.1107285,3552881.6646451 5987328.4433502,3552888.9688104 5987303.4074604,3552892.7670616 5987278.1496586,3552892.4276342 5987255.7941283,3552888.7368878 5987230.6482409,3552881.9400321 5987205.7859497,3552873.4542459 5987179.0966513,3552868.7271059 5987153.279874,3552866.3881964 5987128.7936242,3552867.022388 5987104.6575233,3552869.0956975 5987081.8991807,3552871.1366388 5987067.300827,3552876.0569726 5987048.3757853,3552884.8604542 5987030.9990975,3552896.3912792 5987012.3246639,3552908.7863427 5986994.62464,3552923.1943481 5986971.0240753,3552937.7581836 5986945.8894498,3552948.8541174 5986926.7999676,3552955.3808794 5986915.3627565,3552959.7109699 5986903.0852319,3552962.8420048 5986893.3403704,3552961.8556116 5986893.1141572,3552959.2560951 5986892.5180006,3552953.8777178 5986891.2845577,3552951.7265618 5986890.7912252</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715642">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553124.1882763 5988027.2270657,3553136.3543173 5988049.8564837,3553141.712798 5988059.8235141,3553154.0347636 5988083.6211517,3553166.8609454 5988107.5122829,3553177.8501935 5988128.8688536,3553192.3167588 5988153.1886074,3553206.7492591 5988172.8065846,3553230.6072577 5988197.7576177,3553259.2171806 5988229.2318583,3553257.0596297 5988230.6336634,3553268.6371664 5988247.5205799,3553273.6831402 5988260.3473469,3553274.7760111 5988274.8542436,3553273.7648212 5988287.2487402,3553271.4979967 5988297.5051894,3553263.2910923 5988314.4254992,3553251.4833152 5988330.9836394,3553235.7009529 5988350.7909937,3553218.7076239 5988371.1120816,3553202.0983673 5988391.4918777,3553187.6510431 5988408.6585145,3553171.1580601 5988428.3722479,3553156.2878763 5988445.5596201,3553137.9055041 5988462.7028531,3553119.3234369 5988478.5015873,3553098.3318651 5988497.3273008,3553083.2160967 5988513.5149395,3553071.8853437 5988530.1212023,3553064.183111 5988547.4388228,3553059.4636618 5988561.0934506,3553056.654014 5988578.2172142,3553057.0122896 5988597.8251861,3553059.8413535 5988613.8708265,3553061.6934717 5988613.464149,3553066.1318094 5988612.4896041,3553070.0606733 5988611.6269265,3553071.1175002 5988611.3948745,3553069.3794175 5988597.8730237,3553069.1570981 5988579.590751,3553071.4855537 5988564.0329197,3553075.9498278 5988551.5960628,3553082.7834321 5988535.9024067,3553093.0344199 5988521.2722391,3553107.0269411 5988507.0289821,3553128.5114273 5988489.0117645,3553148.3432334 5988472.478959,3553165.8837942 5988455.6285556,3553182.771307 5988436.4502377,3553198.1788226 5988417.5251975,3553213.0056857 5988398.9899798,3553228.6355905 5988379.4027437,3553246.3466894 5988359.2482515,3553261.5386936 5988339.2084751,3553276.4095616 5988319.4614959,3553284.0552237 5988301.5175025,3553287.2692691 5988288.5609365,3553287.8243053 5988274.9557589,3553285.7247541 5988257.2049395,3553279.4729661 5988241.652227,3553267.8677755 5988223.6113896,3553238.1735991 5988190.1929728,3553214.0275843 5988165.4765483,3553200.8102262 5988147.5851424,3553186.8723669 5988124.196736,3553175.5037264 5988102.563932,3553162.593499 5988078.8167502,3553150.6516287 5988054.8874072,3553144.9377203 5988044.3750397,3553133.2344201 5988022.8434742,3553115.8318642 5987990.8768906,3553105.5065394 5987971.2195312,3553093.4872615 5987948.2383724,3553082.3855184 5987927.2911835,3553069.3058028 5987903.3365756,3553059.5695488 5987884.030601,3553046.4244632 5987859.7589347,3553033.2287395 5987837.3406307995,3553020.465207 5987815.5620463,3553007.7037788 5987794.3484463,3552995.773976 5987770.6112486,3552992.6175927 5987753.1562032,3552991.7165384 5987726.4844368,3552992.0612251 5987703.9690958,3552992.2464958 5987678.1491361,3552991.5818387 5987662.5215363,3552988.3122734 5987644.2167552,3552982.935774 5987630.6823439,3552973.4484364 5987616.3716922,3552963.0373523 5987606.6380273,3552949.0455517 5987595.1148756,3552940.362799 5987587.6843688,3552919.9732505 5987570.3994621,3552918.2166799 5987572.336475,3552915.8207074 5987574.9785718,3552913.2535978 5987577.8093856,3552934.1800843 5987595.730689,3552942.1214902 5987602.8696931,3552955.426591 5987614.174738,3552964.6875097 5987622.8674781,3552973.513499 5987635.2413848,3552978.4954942 5987646.7024353,3552981.494756 5987663.5314627,3552982.1185264 5987677.8949378,3552982.6263932 5987703.9696522,3552982.898861 5987726.647851,3552983.5292934002 5987753.9400725,3552987.4445702 5987774.0438978,3552998.7888763 5987797.9487599,3553012.5882528 5987820.4449313,3553025.6677991 5987843.0082698,3553038.5771897 5987864.6275836,3553051.2817337 5987887.8892103,3553061.2082909 5987907.7888482995,3553074.4071718 5987931.7942314,3553085.1824799 5987952.6859804,3553096.8999375 5987974.7735561,3553107.4378318 5987995.8421107,3553124.1882763 5988027.2270657</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715637">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553464.52835 5989053.5546422005,3553480.8518846 5989053.834456,3553480.7993252 5989052.5434947,3553480.6149176 5989048.0140821,3553480.3981542 5989042.68995,3553480.3235199 5989040.8567849,3553465.0822772 5989041.0484348,3553453.7667024 5989039.9246598,3553441.9553521 5989035.1445507,3553433.0667572 5989026.108948,3553427.076404 5989013.9077181,3553422.3948955 5988990.2590949,3553415.8791546 5988960.3325802,3553411.5670124 5988937.6925037,3553405.4817787 5988913.542813,3553397.9615304 5988885.3698632,3553386.7571488 5988862.7312073,3553372.370733 5988844.6825042,3553357.5040622 5988834.0859651,3553337.3981013 5988825.5210455,3553316.8548644 5988819.4593192,3553297.0236359 5988817.4766106,3553272.5735893 5988815.4100087,3553247.7787811 5988814.2930118,3553222.2502212 5988811.6897411,3553197.8919061 5988807.9618842,3553180.9370612 5988801.9372111,3553165.9585982 5988793.1881008,3553153.8233174 5988783.2652276,3553142.3518158 5988770.3236282,3553136.128565 5988760.9312258,3553129.2224216 5988748.8054424,3553121.3446883 5988731.8557193,3553111.5452412 5988709.2615944,3553103.5886362 5988691.2759273,3553099.0341792 5988682.2217019,3553098.1665812 5988682.6131978,3553094.8104075 5988684.1276415,3553090.8923419 5988685.8956338,3553089.0470673 5988686.7282977,3553092.4008596 5988696.2513067,3553100.0427258 5988714.3428134,3553109.630644 5988736.1085231,3553117.9158806 5988754.9511529,3553125.339958 5988768.5062366,3553131.5247903 5988778.364787,3553144.5842489 5988792.6115379,3553157.98484 5988803.9226042,3553176.0251998 5988814.5715235,3553195.2818724 5988820.5182623,3553220.8420103 5988823.8064546,3553246.9395985 5988826.7033746,3553272.0076198 5988828.2779745,3553297.0273762 5988830.4134975,3553315.0096887 5988832.8019929,3553333.6970193 5988837.6890376,3553350.8085312 5988845.4841662,3553363.0823192 5988854.8731521,3553375.1195643 5988869.8164648,3553385.2087948 5988888.6651144,3553392.5624276 5988915.7413998,3553397.4707556 5988940.1384781,3553402.3183261 5988962.9873577,3553409.3631156 5988993.1021705,3553415.0670213 5989016.8023186,3553422.5163828 5989033.6409919,3553435.1277487 5989046.1888649,3553451.8229839 5989052.6116288,3553464.52835 5989053.5546422005</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230887268"/>
     <simBase:roughnessStyle>Fluss</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715632">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556014.0352355 5990123.9845983,3556015.4852577 5990123.8916227,3556016.2683277 5990108.9781501,3556014.7413986 5990108.9160163,3556013.4694493 5990108.8642581,3556011.3385 5990108.7775456,3556008.9532263 5990108.6804841,3556007.1408379 5990108.6067344,3556006.0107145 5990124.4991318,3556008.1383453 5990124.3627078,3556010.6521829 5990124.2015202,3556012.7788156 5990124.0651602,3556014.0352355 5990123.9845983</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715646">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556035.2134474 5990219.1318695,3556036.532628 5990218.4946979,3556028.9553262 5990200.0017791,3556027.3730866 5990200.4779386,3556024.9304357 5990201.2130304,3556022.1936697 5990202.0366334,3556020.6956766 5990202.4874397,3556019.4868136 5990202.8512352,3556027.0363287 5990223.0814633,3556028.707591 5990222.2742343,3556030.7291339 5990221.2978178,3556033.2792495 5990220.0660978,3556035.2134474 5990219.1318695</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715647">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556339.836847 5990433.8712343,3556340.1490971 5990432.9903478,3556335.7681663 5990431.0705726,3556331.79518 5990429.2978666,3556331.4638588 5990430.0784627,3556330.4691137 5990432.422092,3556329.7728703 5990434.0624484,3556328.3471732 5990437.4214049,3556327.9162211 5990438.4367321,3556331.9556421 5990440.3446513,3556336.7747381 5990442.5097274,3556337.278342 5990441.089014,3556338.3565667 5990438.0472421,3556339.1718616 5990435.7472197,3556339.836847 5990433.8712343</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811715651">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558238.6067891 5991118.1338306,3558247.3952394 5991121.6379736,3558253.3377962 5991123.9975728,3558254.0510724 5991121.8057101,3558254.8571827 5991119.3285724,3558255.7564367 5991116.5652089,3558256.7488344 5991113.5156194,3558257.182989 5991112.1814834,3558251.1061311 5991110.4320522,3558242.1117493 5991107.5486791,3558234.4200458 5991105.1665253,3558225.0123825 5991102.026094,3558209.8739235 5991096.7297456,3558192.4665487 5991090.8672386,3558167.2043696 5991082.1471342,3558138.9708551 5991072.2322339,3558116.7605072 5991064.2525718,3558116.528717 5991065.0609989,3558116.0907686 5991066.588455,3558115.3694906 5991069.1040954,3558114.7256596 5991071.3496194,3558114.1846322 5991073.23659,3558135.9355162 5991081.3417109,3558163.9443215 5991091.1436612,3558188.9730048 5991099.9519395,3558206.6729524 5991106.8841728,3558220.7762821 5991111.7587743,3558230.7426942 5991115.0004505,3558238.6067891 5991118.1338306</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717114">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553910.1562134 5988987.8877705,3553931.104576 5988985.2174534,3553930.8646049 5988983.9873936,3553930.1161417 5988980.1508728,3553929.5593597 5988977.2968837,3553928.9877241 5988974.3667574,3553908.5172438 5988977.0372114,3553908.9467621 5988979.8807745,3553909.3817556 5988982.7605855,3553909.9665466 5988986.6321089,3553910.1562134 5988987.8877705</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717148">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554029.9841956 5988969.4302142,3554058.0883288 5988964.4396009,3554085.6671611 5988960.8236687,3554085.4957151 5988959.6420418,3554084.9288234 5988955.7349536,3554084.5069572 5988952.8273992,3554083.9927629 5988949.2835079,3554055.8452479998 5988952.827467,3554027.8139744 5988958.1752933,3554028.4494329 5988961.4708261,3554029.0054861 5988964.3545573,3554029.7528033 5988968.2301972,3554029.9841956 5988969.4302142</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717152">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553623.8778581 5988979.2033491,3553637.2544947 5988972.1486753,3553636.6822212 5988970.7350106,3553635.5132465 5988967.8473386,3553634.3742844 5988965.0338053,3553633.5007373 5988962.8759165,3553619.1908556 5988969.8279893,3553620.3383371 5988972.1232838,3553621.7749971002 5988974.9970192,3553623.1499102 5988977.7472428,3553623.8778581 5988979.2033491</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171715">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554365.9457522 5989018.5115664,3554366.1015735 5989015.9727849,3554342.7140405 5989013.7478103,3554342.5148854 5989016.4985559,3554342.229419 5989020.4414403,3554341.9451248 5989024.3681351,3554341.8415175 5989025.7991679,3554365.3926456 5989027.523282,3554365.4780699 5989026.1314722,3554365.7080919 5989022.3837441,3554365.9457522 5989018.5115664</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717145">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560117.0456585 5991547.9859281,3560122.7384484 5991545.5649392,3560128.5460961 5991543.3776192,3560132.7983312 5991541.9579759,3560131.976862 5991540.0688522,3560131.3420178 5991538.608908,3560130.7071736 5991537.1489639,3560129.9231889 5991535.3460429,3560129.1392042 5991533.543122,3560123.9104795 5991534.7268957,3560117.1231259 5991536.3232237,3560112.2225391 5991537.7858597,3560106.5318288 5991539.7655857,3560100.6425486 5991542.957053,3560089.2560593 5991548.5327239,3560071.0505993 5991557.3402887,3560071.9345118 5991559.3166297,3560072.5771346 5991560.7534707,3560073.2197574 5991562.1903116,3560074.02324 5991563.9868193,3560075.067604 5991566.3219141,3560094.3030437 5991557.7129093,3560106.3945585 5991552.5735674,3560113.1764667 5991549.6726452,3560117.0456585 5991547.9859281</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171716">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554505.749111 5989001.9483896,3554483.0542425 5989002.3828175,3554483.2033462 5989004.2329799,3554483.5232574 5989008.2026162,3554483.7924097 5989011.5424074,3554483.9023415 5989012.9065027,3554505.2938858 5989012.3854668,3554525.9914198 5989014.6757568,3554526.1391626 5989013.2673883,3554526.4913443 5989009.9101927,3554526.9175588 5989005.847275,3554527.0997777 5989004.110261,3554505.749111 5989001.9483896</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717122">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559470.138519 5991376.9060981,3559472.2913716 5991376.1107759,3559471.9908471 5991374.4273909,3559471.6736267 5991372.6504846,3559471.2896232 5991370.4994926,3559470.9889229 5991368.8151232,3559470.7551816 5991367.5058238,3559468.4737994 5991367.4524176,3559465.9670167 5991367.2218286,3559462.5203973 5991368.0348678,3559454.5264065 5991369.8886819,3559445.9736952 5991372.0919823,3559446.3710386 5991373.8853055,3559446.922168 5991376.3727088,3559447.3347337 5991378.2347348,3559447.784516 5991380.2647297,3559448.1400124 5991381.8691858,3559456.6294542 5991380.7570149,3559464.1727757 5991379.4226179,3559468.238206 5991378.2126169,3559470.138519 5991376.9060981</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717118">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552895.7363472 5987551.0291772,3552882.8767033 5987540.7668003,3552881.2281931 5987542.5684002,3552878.9136556 5987545.0978785,3552876.3389839 5987547.9116484,3552889.8723103 5987558.4333042,3552913.2535978 5987577.8093856,3552915.8207074 5987574.9785718,3552918.2166799 5987572.336475,3552919.9732505 5987570.3994621,3552895.7363472 5987551.0291772</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717129">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554813.5922214 5989227.2933192,3554808.4302937 5989225.4016065,3554800.0262447 5989222.4979021,3554784.5194248 5989216.3284532,3554769.8428991 5989209.5551113,3554756.010898 5989201.3873253,3554740.3990425 5989190.8461369,3554739.774011 5989191.7700845,3554738.0432417 5989194.3285796,3554736.401699 5989196.7551763,3554735.6437587 5989197.8755951,3554752.4780834 5989209.34724,3554765.6952113 5989216.8439851,3554781.4292817 5989224.6117241,3554796.7165064 5989231.9920504,3554804.6804956 5989235.443927,3554809.9951975 5989237.4395817,3554813.5621475 5989238.8723513,3554816.1995544 5989239.471875,3554818.3734377 5989240.0673813,3554821.9459773 5989241.2960734,3554826.322234 5989242.8389603,3554829.9641263 5989243.9729556,3554830.5180772 5989241.7478742,3554831.0717865 5989239.5237631,3554831.6699471 5989237.1211021,3554832.223898 5989234.8960207,3554829.0977675 5989233.6218103,3554825.1713051 5989231.8718786,3554822.1584016 5989230.5536455,3554819.9608302 5989229.858816,3554816.9701224 5989228.6859881,3554813.5922214 5989227.2933192</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717127">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553610.5786892 5988985.3061813,3553608.8477028 5988982.9088824,3553606.905433 5988980.2189704,3553605.2955903998 5988977.9894473,3553596.2174723 5988984.0439886,3553592.9204014 5988985.9980583,3553588.7020585 5988989.0745029,3553585.2001576 5988991.0701284,3553583.1146757 5988991.8650054,3553581.8925912 5988991.3189034,3553580.4509452 5988990.7783873,3553577.2617082 5988992.5269921,3553572.0309277 5988996.2538667,3553563.2907383 5989001.8790665995,3553551.6697201 5989009.7256162,3553540.2657525 5989017.2597643,3553524.2160005 5989026.8393759,3553516.4625825 5989030.4148111,3553506.1491071 5989035.0230841,3553499.0103799 5989037.2904437,3553496.9135548 5989037.8613897,3553493.929282 5989038.8251885,3553490.6619706 5989039.5946083,3553487.4126995 5989040.220201,3553480.3235199 5989040.8567849,3553480.3981542 5989042.68995,3553480.6149176 5989048.0140821,3553480.7993252 5989052.5434947,3553480.8518846 5989053.834456,3553488.201 5989055.54,3553490.1336758 5989056.0854929,3553491.9392119 5989057.992707,3553493.5182772 5989057.3779747,3553495.934 5989056.347,3553498.572 5989054.735,3553499.9435322 5989053.6024341,3553499.3792019 5989051.4467548,3553500.4420055 5989050.6345006,3553502.8524661 5989049.7659149,3553509.704196 5989046.5784134,3553520.0214895 5989041.5505092,3553530.1615576 5989037.0132248,3553545.9394232 5989027.1805372,3553558.9496161 5989020.8957551,3553569.3135005 5989014.1083782,3553577.7762833 5989008.8976393,3553583.9413948 5989005.6459867,3553587.7384864 5989003.6782167,3553588.4313324 5989002.2421012,3553588.5838549 5989000.2905679,3553590.2030587 5988999.0595258,3553593.7674872 5988997.5485991,3553598.8884575 5988994.8369291,3553602.464528 5988992.8293568,3553611.5706483 5988986.6799774,3553610.5786892 5988985.3061813</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717128">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556032.1522717 5990027.3235729,3556032.9022205 5990024.1683418,3556033.6906338 5990020.8784958,3556031.9394847 5990020.4167626,3556029.8702084 5990019.8711473,3556028.1190593 5990019.4094140995,3556026.6067471 5990019.010656,3556024.5374709 5990018.4650408,3556024.1449594 5990022.2348019,3556023.1301579 5990025.9176486,3556020.5804081 5990034.0918162,3556018.2775963 5990042.0782088,3556015.0960906 5990056.1408992,3556016.3772505 5990056.3284096,3556018.1413243 5990056.5865991,3556021.0249256 5990057.0086424,3556022.4473899 5990057.216834,3556024.1564294 5990057.4669687,3556027.5701563 5990043.5728903,3556029.5570855 5990036.509144,3556032.1522717 5990027.3235729</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717149">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553831.6002289 5988993.5399171,3553859.0978672 5988993.0573162995,3553858.9486877 5988991.8029379,3553858.4991351 5988988.0228678,3553858.1647887 5988985.2115112,3553857.8611916 5988982.6587102,3553831.0153664 5988983.287207,3553831.1539093 5988985.7158791,3553831.3135376 5988988.514183,3553831.5281858 5988992.2769919,3553831.6002289 5988993.5399171</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717158">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553098.1665812 5988682.6131978,3553099.0341792 5988682.2217019,3553096.6453827 5988677.1949476,3553094.2382807 5988669.6095426,3553089.4547981 5988657.1133282,3553084.0422625 5988644.0942222,3553076.3474408 5988626.8925579,3553071.1175002 5988611.3948745,3553070.0606733 5988611.6269265,3553066.1318094 5988612.4896041,3553061.6934717 5988613.464149,3553059.8413535 5988613.8708265,3553065.3875647 5988630.821056,3553073.0658942 5988648.5104091,3553078.4988039 5988662.1288372,3553083.3469392 5988672.4496081,3553086.8941255 5988681.4890116,3553089.0470673 5988686.7282977,3553090.8923419 5988685.8956338,3553094.8104075 5988684.1276415,3553098.1665812 5988682.6131978</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171710">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557059.8835977 5990692.3501558,3557059.0820846 5990694.3419354,3557058.2424931 5990696.4283407,3557057.6317453 5990697.9460637,3557056.5629367 5990700.602079,3557056.1052492 5990701.7394436,3557059.7975559 5990703.6891226,3557063.0018107 5990705.4713119,3557066.1901037 5990706.1899043,3557069.7530367 5990706.3541255,3557072.3725463 5990706.9541264,3557072.7434135 5990705.8645125,3557073.5122137 5990703.6057646,3557074.3870221 5990701.0355632,3557075.1558224 5990698.7768152,3557075.4210037002 5990697.9977081,3557072.7539843 5990697.0688797,3557069.7844747 5990695.078033,3557066.3738392 5990693.595772,3557063.2729135 5990693.0575544,3557059.8835977 5990692.3501558</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308790520"/>
     <simBase:roughnessStyle>Sohlschüttung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717139">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555732.1278211 5989689.2951016,3555751.8595174 5989694.9300494,3555770.091 5989664.001,3555751.323 5989655.856,3555732.1278211 5989689.2951016</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308713428"/>
     <simBase:roughnessStyle>Graben</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171711">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556170.459 5990299.527,3556183.657 5990299.991,3556188.751 5990283.771,3556166.986 5990280.527,3556170.459 5990299.527</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308713428"/>
     <simBase:roughnessStyle>Graben</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717120">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554740.847 5989034.539,3554746.68 5989031.037,3554741.722 5989016.447,3554729.475 5988999.813,3554723.059 5988982.888,3554699.438 5989011.486,3554711.394 5989020.24,3554728.6 5989028.411,3554740.847 5989034.539</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308713428"/>
     <simBase:roughnessStyle>Graben</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171718">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554476.937 5988944.078,3554467.897 5988953.416,3554467.605 5988961.295,3554477.5203676 5988962.7542416,3554506.682 5988944.078,3554534.094 5988939.409,3554549.549 5988937.075,3554554.215 5988924.527,3554536.135 5988920.441,3554508.14 5988923.068,3554476.937 5988944.078</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308713428"/>
     <simBase:roughnessStyle>Graben</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717150">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556028.684 5989868.244,3556033.46 5989867.724,3556039.276 5989866.374,3556053.215 5989870.542,3556105.57 5989879.463,3556137.711 5989885.996,3556169.0858957 5989907.0595019,3556215.599 5989941.962,3556218.314 5989933.992,3556172.111 5989902.832,3556139.72 5989881.222,3556104.817 5989874.437,3556051.332 5989864.26,3556030.76 5989857.855,3556018.487 5989845.412,3556023.18 5989859.206,3556028.684 5989868.244</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308713428"/>
     <simBase:roughnessStyle>Graben</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717137">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552991.7165384 5987726.4844368,3552992.6175927 5987753.1562032,3552995.773976 5987770.6112486,3553007.7037788 5987794.3484463,3553020.465207 5987815.5620463,3553033.2287395 5987837.3406307995,3553046.4244632 5987859.7589347,3553059.5695488 5987884.030601,3553069.3058028 5987903.3365756,3553070.991 5987902.41,3553061.229 5987883.258,3553047.933 5987858.823,3553034.621 5987836.297,3553021.851 5987814.703,3553009.198 5987793.745,3552997.0998021 5987770.0648596,3552994.104 5987753.028,3552993.197 5987726.457,3552991.7165384 5987726.4844368</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717119">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555647.7068842 5989719.0257362,3555669.9834471 5989728.5361169,3555670.801 5989726.316,3555648.518 5989716.849,3555625.2957048 5989705.0960625,3555604.472 5989693.401,3555584.2870751 5989680.4853604,3555564.885 5989667.671,3555542.3014619 5989650.6813779,3555524.175 5989634.818,3555505.2426997 5989616.6103071,3555489.583 5989599.467,3555473.783065 5989581.2876114,3555461.11 5989564.365,3555446.4376072 5989542.3562597,3555433.887 5989522.144,3555418.6964578 5989499.2485341,3555403.589 5989474.1,3555383.735 5989441.615,3555366.7217677 5989414.2729129,3555353.569 5989392.504,3555338.2777856 5989372.8449185,3555322.171 5989353.091,3555303.1656854 5989334.5251562,3555287.234 5989319.837,3555265.8568078 5989304.3608382,3555264.7832169 5989305.8551616,3555286.0187218 5989321.3000183,3555301.9350889 5989335.9291937,3555320.7138225 5989354.5921245,3555336.6484402 5989374.2857025,3555351.9095093 5989393.5901831,3555365.3523283 5989415.1951042,3555382.2489442 5989442.4617304,3555401.9673731 5989474.9507085,3555417.07833 5989500.2949524,3555432.1591001 5989523.1455406,3555444.866275 5989543.3458879,3555459.6384365 5989565.3315207,3555472.5412291 5989582.3154186,3555488.2716346 5989600.8783195,3555503.7803804 5989618.2013895,3555522.9017372 5989636.3154375,3555541.2170509 5989652.0151747,3555563.9267431 5989669.1752639,3555583.6255087 5989682.067623,3555603.5848979 5989695.2281406,3555624.2551526 5989707.1573078,3555647.7068842 5989719.0257362</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717138">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558886.3153752 5991325.1741218995,3558875.799 5991323.47,3558864.389 5991321.383,3558857.887 5991319.665,3558852.857 5991318.07,3558848.195 5991316.229,3558844.515 5991315.002,3558841.4577541 5991313.8504046,3558832.861 5991310.951,3558819.611 5991304.937,3558803.0025577 5991296.4530534,3558787.346 5991287.509,3558769.189 5991274.869,3558754.9486654 5991261.9326395,3558737.813 5991245.55,3558717.5443405 5991225.4664786,3558709.454 5991216.555,3558695.936 5991203.28,3558684.1604407 5991192.4677683,3558669.395 5991180.455,3558652.4671418 5991169.0966084,3558625.244 5991155.148,3558596.1870001 5991147.1037158,3558573.404 5991142.892,3558546.9964746 5991140.9746837,3558547.0502603 5991143.4971104,3558573.059675 5991144.9587009,3558595.8424386 5991148.8765422,3558624.6155882 5991156.8608371,3558651.5593121 5991170.6682541,3558668.2002161 5991182.0016742,3558682.7293606 5991194.0494443,3558694.4759077 5991204.7779756,3558707.9598783 5991217.978417,3558716.1942528 5991226.7976733,3558736.4227376 5991246.9163545,3558753.7158046 5991263.2687451,3558768.164349 5991276.2356874,3558786.3585796 5991288.9742347,3558802.1642527 5991298.0707458,3558818.9689473 5991306.6825515,3558832.2427727 5991312.627189,3558840.9595035 5991315.5311054,3558844.177636 5991316.688951,3558847.8580934 5991317.8821443,3558852.5043606 5991319.6413947,3558857.4706419 5991321.1867303,3558863.9891273 5991322.8870535,3558875.4475004 5991325.0042782,3558886.0909686 5991326.6966732,3558897.3477585 5991328.4168586,3558910.8378894 5991330.0150846,3558925.8992987 5991330.783639,3558939.3167928 5991330.8217933,3558964.1131671 5991329.8082028,3558987.7000531 5991328.0788503,3558987.587532 5991326.8028017,3558963.961 5991328.185,3558939.1303364 5991328.8094129,3558925.784 5991328.866,3558911.018 5991328.283,3558897.613 5991326.825,3558886.3153752 5991325.1741218995</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717160">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552948.7683568 5986890.1128092,3552945.998 5986900.8,3552948.7323796 5986901.2556775,3552951.7265618 5986890.7912252,3552948.7683568 5986890.1128092</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717174">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552942.86 5986912.155,3552936.699 5986922.302,3552926.028 5986938.923,3552910.174 5986964.389,3552895.977 5986986.731,3552897.8402215 5986987.8791931,3552912.2083414 5986965.4256857,3552927.9872944 5986940.0866072,3552938.8604077 5986923.1018229,3552945.2215508 5986912.7600118,3552942.86 5986912.155</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717131">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558873.1 5991335.251,3558884.5902586 5991336.8786737,3558884.7726709 5991335.6410442,3558873.4198272 5991333.8549702,3558861.7231579 5991331.4101125,3558855.1520942 5991329.6606948,3558850.5739562 5991328.2434596,3558850.158 5991330.097,3558849.903 5991332.162,3558851.466 5991334.387,3558852.263 5991338.061,3558864.786 5991374.819,3558874.136 5991408.069,3558879.984 5991430.123,3558883.69 5991430.66,3558877.281 5991408.619,3558874.477 5991397.398,3558867.935 5991375.356,3558856.853 5991339.554,3558855.658 5991336.333,3558854.327 5991333.423,3558854.697 5991331.324,3558861.322 5991332.919,3558873.1 5991335.251</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717146">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552892.4276342 5987255.7941283,3552892.7670616 5987278.1496586,3552888.9688104 5987303.4074604,3552881.6646451 5987328.4433502,3552873.5639897 5987349.1107285,3552865.9342203 5987374.0661573,3552861.4654532 5987397.440427,3552859.6267214 5987423.315094,3552856.5530465 5987447.8286681,3552854.0857521 5987473.5178335,3552853.967433 5987487.9271455,3552854.6791194 5987497.5658705,3552857.2888802 5987508.1898413,3552861.1486832 5987516.5192676,3552862.888 5987515.366,3552859.19 5987507.405,3552856.763 5987497.089,3552856.078 5987487.827,3552856.06 5987473.406,3552858.687 5987447.971,3552862.01 5987423.458,3552863.805 5987397.573,3552868.481 5987374.598,3552876.048 5987349.784,3552884.093 5987329.195,3552891.505 5987303.903,3552895.204 5987278.3,3552894.917 5987255.712,3552891.2875961 5987230.1763052,3552884.532 5987205.232,3552876.094 5987178.357,3552871.405 5987152.857,3552869.062 5987128.777,3552869.881 5987104.757,3552872.084 5987082.395,3552874.155 5987067.957,3552878.887 5987049.481,3552887.6304316 5987032.5953537,3552899.503 5987013.796,3552911.789 5986996.475,3552926.332 5986972.623,3552940.662 5986947.614,3552951.943 5986927.943,3552958.645 5986916.199,3552955.3808794 5986915.3627565,3552948.8541174 5986926.7999676,3552937.7581836 5986945.8894498,3552923.1943481 5986971.0240753,3552908.7863427 5986994.62464,3552896.3912792 5987012.3246639,3552884.8604542 5987030.9990975,3552876.0569726 5987048.3757853,3552871.1366388 5987067.300827,3552869.0956975 5987081.8991807,3552867.022388 5987104.6575233,3552866.3881964 5987128.7936242,3552868.7271059 5987153.279874,3552873.4542459 5987179.0966513,3552881.9400321 5987205.7859497,3552888.7368878 5987230.6482409,3552892.4276342 5987255.7941283</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717141">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552880.7912453 5987277.4108379,3552876.3563403 5987300.9431423005,3552869.4419329 5987324.6600487,3552860.9070961 5987345.680177,3552852.7950757 5987371.3223164,3552849.2419547 5987396.7477689,3552847.0138124 5987422.5587994,3552845.1112744 5987447.0655169,3552843.3589089 5987474.1254678,3552845.1998341 5987474.0211865,3552846.996539 5987447.1912616,3552849.0059533 5987422.678252,3552851.0894294 5987396.8524581,3552854.6918444 5987371.7184176,3552862.6486996 5987346.152225,3552871.0414492 5987325.1551477,3552877.9221695 5987301.2490856,3552882.1978784 5987277.4976169,3552880.7912453 5987277.4108379</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717134">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552862.855 5987043.22,3552856.597 5987064.14,3552854.226 5987079.432,3552852.323 5987104.146,3552852.174 5987128.882,3552854.003 5987155.605,3552858.435 5987183.305,3552866.672 5987209.049,3552873.1730441 5987233.5278853,3552877.064 5987256.301,3552877.552 5987277.211,3552880.7912453 5987277.4108379,3552880.3364618 5987256.1930361,3552876.4897515 5987232.9142234,3552869.7861712 5987208.3834462,3552861.3584911 5987182.4858464,3552856.7291185 5987155.1745108,3552854.6679099 5987128.8664943,3552854.7566319 5987104.2306878,3552856.537712 5987079.8155593,3552858.7075645 5987064.5988236,3552864.6324823 5987043.9141627,3552873.8931804 5987024.6789812,3552884.5732866 5987006.7366824,3552882.734 5987005.867,3552872.3422703 5987023.7852375,3552862.855 5987043.22</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717116">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557027.187 5990694.654,3557046.063 5990700.458,3557055.3033628 5990703.7321508,3557056.1052492 5990701.7394436,3557046.5524386 5990698.5111094,3557027.6209761 5990692.864998,3557010.2729419 5990687.6790974,3557009.7687794 5990689.3130864,3557027.187 5990694.654</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717117">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557269.3527023 5990700.7419953,3557299.6914121 5990694.9436677,3557299.327 5990692.822,3557268.899 5990698.651,3557246.8012323 5990702.8572325,3557222.658 5990706.825,3557198.3693059 5990710.4196391,3557172.852 5990711.844,3557149.3484882 5990711.006712,3557133.991 5990709.38,3557115.2 5990706.369,3557100.8143628 5990703.2412507,3557094.821 5990701.243,3557086.59 5990698.766,3557083.0441168 5990697.624945,3557081.034 5990697.36,3557077.755 5990697.56,3557075.7126065 5990697.1409743,3557073.071 5990696.088,3557070.194 5990693.812,3557066.7816511 5990692.1595483,3557063.836 5990691.335,3557060.6851108 5990690.3583763,3557059.8835977 5990692.3501558,3557063.2729135 5990693.0575544,3557066.3738392 5990693.595772,3557069.7844747 5990695.078033,3557072.7539843 5990697.0688797,3557075.4210037002 5990697.9977081,3557077.3445859 5990699.0100862,3557080.3187506 5990699.5640163,3557082.2809406 5990700.4310146,3557085.8214301 5990701.422653,3557094.0420832 5990703.6837095,3557100.2827747 5990705.5898414,3557114.4232368 5990708.6010843,3557133.5076746 5990711.7657127,3557149.2417926 5990713.482414,3557172.9262041 5990714.1158494,3557198.5880742 5990712.5373693,3557223.1227569 5990708.8894373,3557247.1345017 5990704.8505645,3557269.3527023 5990700.7419953</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171717">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556874.399 5990668.441,3556892.45 5990667.485,3556906.5610072 5990667.5706681,3556906.7484364 5990666.1449352,3556892.45 5990665.7423173,3556874.2519487 5990666.3492091,3556858.0565371 5990667.9293967,3556838.209319 5990670.3930528,3556810.1224408 5990674.4028535,3556781.3199504 5990678.5179509,3556757.5507244 5990681.9245069,3556732.8937896 5990685.4015534,3556733.095 5990687.476,3556757.7712062 5990684.4689722,3556781.711 5990681.008,3556810.315 5990676.889,3556838.485 5990672.771,3556858.3196069 5990670.310911,3556874.399 5990668.441</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717140">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556530.451973 5990613.2052009,3556550.1274552 5990629.4447824,3556568.036577 5990641.4744725,3556589.6462197 5990652.5598224,3556610.9074494 5990660.952736,3556635.379368 5990668.893549,3556659.622452 5990673.7732614,3556684.0258721 5990676.2893107,3556707.4088541 5990677.5270423,3556731.9111842 5990675.2710505,3556756.6093173 5990671.0602178,3556779.6188573 5990667.6860638,3556809.2688393 5990663.381942,3556836.9636609 5990659.6483537,3556856.8446154 5990656.9581302,3556873.461672 5990655.1075958,3556892.45 5990654.5537123,3556908.2349657 5990654.8372274,3556908.509853 5990652.7462186,3556892.45 5990652.537,3556873.323 5990653.135,3556856.6378706 5990655.0865144,3556836.751 5990657.814,3556809.123 5990661.499,3556779.328 5990665.834,3556756.4482292 5990669.201184,3556731.733 5990673.434,3556707.3661926 5990675.5785093,3556684.158 5990674.477,3556659.9098402 5990672.1220849,3556635.83 5990667.271,3556611.4424791 5990659.3813216,3556590.498 5990650.674,3556569.2177574 5990639.2326052,3556551.298 5990627.373,3556532.186 5990611.693,3556530.451973 5990613.2052009</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717182">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556397.0393949 5990467.7239181,3556415.4534255 5990483.7003449,3556417.207 5990481.913,3556398.6161728 5990465.5186307,3556377.988 5990450.652,3556359.825 5990440.36,3556352.6706161 5990436.4151882,3556348.698 5990435.139,3556344.6344172 5990433.8883605,3556340.425 5990432.212,3556340.1490971 5990432.9903478,3556344.3754022 5990434.6547756,3556348.192801 5990436.5493182,3556351.7935082 5990438.4839295,3556358.8470495 5990442.4866198,3556376.7828433 5990452.8127608,3556397.0393949 5990467.7239181</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717163">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556503.173 5990602.615,3556521.461 5990621.046,3556522.9462367 5990619.7507628,3556504.173962 5990601.2571861,3556487.1151836 5990582.5392303,3556469.0903328 5990561.2022273,3556440.6302873 5990527.5380378,3556423.2697056 5990507.2532602,3556408.6406958 5990490.6442746,3556391.6018086 5990475.3289464,3556372.2259084 5990460.9830229,3556354.7820056 5990451.3263335,3556347.7729422 5990447.9668114,3556344.8167621 5990445.9738988,3556341.1346725 5990444.2439651,3556336.7747381 5990442.5097274,3556336.49 5990443.313,3556340.8494038 5990445.0880639,3556344.36 5990447.249,3556347.0422141 5990449.6903021,3556353.982 5990453.066,3556371.256 5990462.722,3556390.3513207 5990477.0778833,3556407.341 5990491.969,3556422.1159147 5990508.3091307,3556439.371 5990528.479,3556467.834 5990562.04,3556486.0837575 5990583.5195818,3556503.173 5990602.615</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717165">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556276.259 5990415.459,3556304.508 5990428.477,3556320.953 5990435.945,3556327.6177195 5990439.140005,3556327.9162211 5990438.4367321,3556321.3852359 5990435.0884503,3556304.9173239 5990427.4104744,3556276.7874348 5990414.2400942,3556256.7277742 5990405.0438612,3556256.1224121 5990406.5184366,3556276.259 5990415.459</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717161">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556280.5526994 5990405.5550058,3556308.3528324 5990418.4589869,3556325.7951316 5990426.3494835,3556331.79518 5990429.2978666,3556332.154 5990428.403,3556326.3 5990425.349,3556308.846 5990417.174,3556281.203 5990404.055,3556261.2300121 5990394.0770531,3556243.75 5990386.057,3556219.621 5990375.222,3556190.666 5990361.384,3556171.9709194 5990352.4496706,3556155.277 5990344.113,3556137.368 5990334.458,3556125.6084491 5990327.6015063,3556107.769 5990313.216,3556091.422167 5990297.8764473,3556076.026 5990280.388,3556059.8250092 5990259.455857,3556049.324 5990240.782,3556047.6481188 5990241.5025663,3556057.9921788 5990260.5663442,3556074.3570814 5990281.5473956,3556090.1566913 5990299.1733341,3556106.6616748 5990314.7697688,3556124.5599326 5990329.2218498,3556136.4959673 5990336.202472,3556154.8227395 5990345.9168209,3556171.340819 5990354.0371962,3556190.0489962 5990362.9749647,3556218.9577455 5990376.8085402,3556243.0381812 5990387.7535886,3556260.4734994 5990395.9198099,3556280.5526994 5990405.5550058</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717156">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553082.7834321 5988535.9024067,3553075.9498278 5988551.5960628,3553071.4855537 5988564.0329197,3553069.1570981 5988579.590751,3553069.3794175 5988597.8730237,3553071.1175002 5988611.3948745,3553076.3474408 5988626.8925579,3553084.0422625 5988644.0942222,3553089.4547981 5988657.1133282,3553094.2382807 5988669.6095426,3553096.6453827 5988677.1949476,3553098.412 5988676.417,3553096.223 5988669.092,3553091.463 5988656.194,3553086.066 5988643.28,3553078.38 5988626.164,3553073.221 5988610.933,3553071.7 5988597.882,3553071.517 5988579.85,3553073.768 5988564.591,3553078.197 5988552.39,3553084.877 5988537.013,3553094.9317032 5988522.7712522,3553108.711 5988508.908,3553130.295 5988491.052,3553128.5114273 5988489.0117645,3553107.0269411 5988507.0289821,3553093.0344199 5988521.2722391,3553082.7834321 5988535.9024067</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717186">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553099.0341792 5988682.2217019,3553103.5886362 5988691.2759273,3553111.5452412 5988709.2615944,3553121.3446883 5988731.8557193,3553129.2224216 5988748.8054424,3553136.128565 5988760.9312258,3553142.3518158 5988770.3236282,3553153.8233174 5988783.2652276,3553165.9585982 5988793.1881008,3553180.9370612 5988801.9372111,3553197.8919061 5988807.9618842,3553222.2502212 5988811.6897411,3553247.7787811 5988814.2930118,3553272.5735893 5988815.4100087,3553297.0236359 5988817.4766106,3553297.023 5988815.277,3553272.67 5988813.218,3553247.922 5988812.175,3553222.491 5988809.618,3553198.339 5988805.811,3553181.78 5988799.769,3553167.3294978 5988791.3425561,3553155.421 5988781.649,3553144.235 5988768.925,3553138.016 5988759.606,3553131.212 5988747.724,3553123.418 5988731.103,3553113.593 5988708.357,3553105.592 5988690.385,3553100.833 5988681.41,3553099.0341792 5988682.2217019</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717130">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558188.39 5991101.468,3558206.2072527 5991108.3615099,3558220.112 5991113.285,3558230.1237895 5991116.6555173,3558238.068 5991119.761,3558246.873 5991123.215,3558252.8414426 5991125.522843,3558257.232 5991125.633,3558260.3532842 5991126.2152521,3558265.346 5991128.224,3558272.338 5991131.764,3558277.7859574 5991133.5569607,3558278.6306436 5991130.9693383,3558272.9979402 5991129.2849364,3558265.9335523 5991126.2046489,3558260.8587406 5991124.432523,3558257.7100711 5991124.0760492,3558253.3377962 5991123.9975728,3558247.3952394 5991121.6379736,3558238.6067891 5991118.1338306,3558230.7426942 5991115.0004505,3558220.7762821 5991111.7587743,3558206.6729524 5991106.8841728,3558188.9730048 5991099.9519395,3558163.9443215 5991091.1436612,3558135.9355162 5991081.3417109,3558114.1846322 5991073.23659,3558091.6083487 5991064.4085233,3558061.2806391 5991052.8441721,3558060.554 5991054.755,3558090.918 5991066.369,3558113.5664332 5991075.392716,3558135.286 5991083.291,3558163.326 5991092.85,3558188.39 5991101.468</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717112">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553220.8420103 5988823.8064546,3553195.2818724 5988820.5182623,3553176.0251998 5988814.5715235,3553157.98484 5988803.9226042,3553144.5842489 5988792.6115379,3553131.5247903 5988778.364787,3553125.339958 5988768.5062366,3553117.9158806 5988754.9511529,3553109.630644 5988736.1085231,3553100.0427258 5988714.3428134,3553092.4008596 5988696.2513067,3553089.0470673 5988686.7282977,3553087.297 5988687.518,3553090.434 5988697.126,3553098.014 5988715.239,3553107.558 5988736.861,3553115.909 5988756.042,3553123.419 5988769.855,3553129.591 5988779.801,3553142.929 5988794.286,3553156.5519249 5988805.8516359,3553175.153 5988816.815,3553194.824 5988822.721,3553220.598 5988825.906,3553246.796 5988828.827,3553271.912 5988830.452,3553297.028 5988832.571,3553314.706 5988834.998,3553333.096 5988839.665,3553349.736 5988847.31,3553361.615 5988856.483,3553373.307 5988870.92,3553383.251 5988889.171,3553390.608 5988916.074,3553395.37 5988940.503,3553400.328 5988963.377,3553407.48 5988993.513,3553413.359 5989017.214,3553421.04 5989034.695,3553434.188 5989047.709,3553451.5599345 5989054.3285953,3553464.451 5989055.301,3553480.928 5989055.704,3553486.773 5989057.079,3553488.714 5989057.739,3553490.766 5989058.801,3553493.001 5989062.283,3553495.009 5989065.437,3553497.872 5989070.559,3553501.342 5989083.666,3553503.4543584 5989091.5685133,3553507.494 5989103.16,3553510.357 5989113.187,3553514.4380483 5989133.6852967,3553516.0299411 5989133.325712,3553512.0548117 5989112.8759183,3553508.911 5989102.814,3553504.8528174 5989091.1853522,3553502.7038176 5989083.0618008,3553498.973 5989070.126,3553496.1483446 5989064.9751709,3553493.99 5989061.623,3553491.9392119 5989057.992707,3553490.1336758 5989056.0854929,3553488.201 5989055.54,3553480.8518846 5989053.834456,3553464.52835 5989053.5546422005,3553451.8229839 5989052.6116288,3553435.1277487 5989046.1888649,3553422.5163828 5989033.6409919,3553415.0670213 5989016.8023186,3553409.3631156 5988993.1021705,3553402.3183261 5988962.9873577,3553397.4707556 5988940.1384781,3553392.5624276 5988915.7413998,3553385.2087948 5988888.6651144,3553375.1195643 5988869.8164648,3553363.0823192 5988854.8731521,3553350.8085312 5988845.4841662,3553333.6970193 5988837.6890376,3553315.0096887 5988832.8019929,3553297.0273762 5988830.4134975,3553272.0076198 5988828.2779745,3553246.9395985 5988826.7033746,3553220.8420103 5988823.8064546</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717171">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553453.7667024 5989039.9246598,3553465.0822772 5989041.0484348,3553480.3235199 5989040.8567849,3553487.4126995 5989040.220201,3553486.946 5989037.499,3553480.234 5989038.658,3553465.175 5989038.955,3553454.0882072 5989037.8261452,3553443.087 5989033.314,3553434.819 5989024.858,3553429.075 5989013.426,3553424.568 5988989.785,3553418.145 5988959.889,3553413.927 5988937.283,3553407.649 5988913.174,3553400.105 5988884.816,3553388.717 5988861.538,3553386.7571488 5988862.7312073,3553397.9615304 5988885.3698632,3553405.4817787 5988913.542813,3553411.5670124 5988937.6925037,3553415.8791546 5988960.3325802,3553422.3948955 5988990.2590949,3553427.076404 5989013.9077181,3553433.0667572 5989026.108948,3553441.9553521 5989035.1445507,3553453.7667024 5989039.9246598</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717111">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558367.6943094 5991146.8173265,3558386.6686924 5991149.5049807,3558400.4174592 5991150.7879372,3558421.514 5991152.2515069,3558452.7881255 5991152.4896687,3558452.687 5991150.689,3558421.514 5991150.689,3558400.5226017 5991149.4400318,3558386.82 5991148.124,3558367.915 5991145.422,3558352.4575605 5991142.4865847,3558337.684 5991138.625,3558319.04 5991133.309,3558304.0827962 5991128.6623508,3558294.178 5991125.029,3558282.0733138 5991120.4230187,3558275.878 5991118.466,3558268.713 5991116.652,3558263.4929514 5991115.1417436,3558260.944 5991113.544,3558257.5858895 5991110.9433901,3558251.534 5991109.14,3558242.557 5991106.204,3558234.9356246 5991103.7877713,3558225.637 5991100.591,3558210.3687858 5991095.1598961,3558193.09 5991089.246,3558167.869 5991080.313,3558139.672 5991070.128,3558117.4302457 5991061.9166886,3558095.532 5991053.266,3558065.763 5991041.057,3558046.414 5991032.719,3558025.8509344 5991023.7815784,3558003.972 5991013.357,3557982.4021457 5991000.8974514,3557961.378 5990988.457,3557939.2088538 5990973.9421976,3557927.635 5990966.049,3557912.285 5990954.706,3557898.1182813 5990943.7957216,3557882.486 5990929.794,3557864.1611693 5990912.8848989,3557852.465 5990901.593,3557831.261 5990882.194,3557807.882 5990860.802,3557790.1586723 5990844.4634303,3557777.217 5990832.217,3557755.744 5990812.562,3557731.885 5990790.802,3557716.4065815 5990776.7277207,3557704.939 5990765.951,3557683.045 5990745.454,3557659.888 5990726.361,3557658.2208466 5990728.6612211,3557681.4336737 5990747.2926199,3557703.5127082 5990767.4642889,3557715.2117736 5990778.010469,3557730.6739204 5990792.0314976,3557754.5124825 5990813.7758396,3557776.0195472 5990833.5236882,3557789.0443584 5990845.8668435,3557806.6592912 5990862.2745924,3557829.8849153 5990883.6498027,3557851.0370239 5990903.1334468,3557862.934491 5990914.6773398,3557881.0640802 5990931.4397917,3557896.7254123 5990945.468816,3557910.9399596 5990956.3986739,3557926.5853381 5990967.8551653,3557938.1400866 5990975.6586534,3557960.4166736 5990990.5965246,3557981.1616065 5991003.3000913,3558003.0400373 5991015.5559413005,3558025.0608292 5991025.7908108,3558045.6619901 5991034.8161156,3558064.9312348 5991043.2442759,3558094.7641242 5991055.4466409,3558116.7605072 5991064.2525718,3558138.9708551 5991072.2322339,3558167.2043696 5991082.1471342,3558192.4665487 5991090.8672386,3558209.8739235 5991096.7297456,3558225.0123825 5991102.026094,3558234.4200458 5991105.1665253,3558242.1117493 5991107.5486791,3558251.1061311 5991110.4320522,3558257.182989 5991112.1814834,3558260.6021453 5991114.6573302,3558263.1738019 5991116.2673739,3558268.3331202 5991117.9576041,3558275.4437074 5991120.0974188,3558281.5097759 5991122.1493678,3558293.4643276 5991126.7593017,3558303.4389557 5991130.5231115,3558318.4891394 5991135.1091336,3558337.1798626 5991140.2979632,3558352.1522189 5991143.9766208,3558367.6943094 5991146.8173265</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717115">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554889.768 5989256.631,3554909.302 5989258.24,3554925.2232033 5989258.5682126,3554948.495 5989258.444,3554978.045 5989256.882,3555004.267 5989254.696,3555024.7910335 5989253.1668942,3555050.297 5989252.259,3555074.5557197 5989251.3931572,3555097.862 5989253.307,3555122.8629051 5989256.9945386,3555147.024 5989262.564,3555170.3682524 5989269.4432862,3555192.888 5989278.884,3555214.6183676 5989288.8351223,3555236.137 5989301.054,3555257.9244877 5989315.4017766,3555278.154 5989330.768,3555293.8706282 5989345.1302643,3555311.859 5989363.714,3555327.4139847 5989382.4514701,3555341.102 5989400.664,3555354.9094196 5989422.2274408,3555371.576 5989448.543,3555390.968 5989480.721,3555406.6868757 5989507.0149451,3555420.525 5989529.889,3555433.7789479 5989550.3287087,3555448.747 5989572.485,3555462.8561413 5989590.331294,3555479.717 5989610.085,3555495.6323996 5989627.0668331,3555514.934 5989645.686,3555533.4962217 5989661.5115907,3555556.768 5989680.413,3555578.4506324 5989694.4443302,3555598.104 5989706.517,3555619.0965553 5989717.3760496,3555643.606 5989730.031,3555665.77 5989739.978,3555691.288 5989749.925,3555711.6987816 5989756.2213503,3555733.773 5989761.528,3555762.891 5989768.645,3555790.033 5989775.103,3555809.4696159 5989779.585473,3555833.249 5989785.383,3555861.314 5989791.973,3555887.006 5989798.035,3555907.3766417 5989803.1670828,3555931.67 5989809.107,3555953.8463054 5989818.4998694,3555974.228 5989831.249,3555991.340047 5989845.919393,3556003.61 5989859.981,3556014.546 5989877.115,3556020.7956765 5989890.2944431,3556028.01 5989909.074,3556032.1801046 5989929.4194521,3556034.217 5989949.128,3556033.8554319 5989966.6396162,3556030.012 5989986.98,3556026.6117979 5990000.0957732,3556023.6356052 5990011.5759932,3556023.086 5990015.026,3556023.4235428 5990018.1713265,3556022.769 5990021.931,3556021.3812649 5990025.645117,3556023.1301579 5990025.9176486,3556024.1449594 5990022.2348019,3556024.5374709 5990018.4650408,3556024.2995741 5990015.3423951,3556024.8498691 5990011.8463618,3556027.7925276 5990000.4142687,3556031.1725913 5989987.2828762,3556034.9579536 5989966.7272619,3556035.7390767 5989948.9564532,3556034.1154193 5989929.1289252,3556029.7240244 5989908.7412641,3556022.2864849 5989889.6503418,3556015.8154468 5989876.2924598,3556004.5478427 5989859.0429623,3555992.1192261 5989845.1344625,3555975.1712298 5989829.8173053,3555954.863251 5989816.5999098,3555932.0548424 5989807.1050894,3555907.716363 5989801.3414217,3555887.3299026 5989796.3177633,3555861.6126054 5989790.389768,3555833.5493165 5989783.9083519,3555809.7476439 5989778.336033,3555790.3517986 5989773.6608308,3555763.2892404 5989767.0670333,3555734.0904755 5989759.7392035,3555712.2735695 5989754.2785958,3555691.9884826 5989747.908403,3555666.516515 5989737.9507911,3555644.3642741 5989727.9960715,3555620.0920425 5989715.4040743,3555599.0801651 5989704.5064188,3555579.3066183 5989692.3970761,3555558.2338485 5989678.1119231,3555535.4101849 5989659.1574665,3555516.9429987 5989643.3232906,3555497.7220072 5989624.7932271,3555481.6494653 5989608.0052396,3555464.7604952 5989588.7551531,3555450.7148821 5989571.1924979,3555435.6125919 5989549.173876,3555422.6725458 5989528.6442206,3555408.816389 5989505.6378218,3555393.0700762 5989479.6182469,3555373.4698757 5989447.4639005,3555356.6214262 5989421.0745621,3555342.9406255 5989399.4605682,3555329.0433301 5989381.010686,3555313.2036986 5989362.3287465,3555294.895576 5989343.9608597,3555279.0994714 5989329.6297899,3555258.6999237 5989314.3224528,3555236.9424641 5989299.8117295,3555215.3198563 5989287.3593616,3555193.4354186 5989277.667152,3555170.6911056 5989268.5010645,3555147.3086132 5989261.4381555,3555123.0917033 5989255.792113,3555098.084708 5989252.0055685,3555074.6128058 5989250.0663847,3555050.297 5989250.225,3555024.6462943 5989251.2674007,3555004.1745017 5989253.0188492,3554977.998734 5989255.4697734,3554948.4851668 5989257.3814519,3554925.2156895 5989257.7752482,3554909.2978169 5989257.2798474,3554889.8523793 5989255.5385824,3554875.6576696 5989253.3697376,3554866.8740114 5989252.2719936,3554855.2830087 5989249.9289108,3554844.6446922002 5989247.5399041,3554844.4130246 5989248.797748,3554855.003 5989251.182,3554866.631 5989253.508,3554875.5380414 5989254.5778292,3554889.768 5989256.631</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717192">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557369.999 5990693.689,3557396.327 5990688.616,3557395.9461246 5990686.2728575,3557369.5627367 5990691.3545253,3557347.6679 5990695.5271179,3557327.4186067 5990699.089435,3557301.2762343 5990704.1707655,3557301.7 5990706.638,3557327.705 5990701.457,3557348.0837289 5990697.7922658,3557369.999 5990693.689</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717154">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557425.029 5990683.328,3557445.3932037 5990680.4284507,3557470.023 5990678.904,3557494.5664109 5990678.8002993,3557516.991 5990680.805,3557540.8994384 5990686.4047139,3557565.195 5990693.468,3557591.6376436 5990703.6828904,3557612.59 5990712.615,3557632.6125536 5990724.2226591,3557651.748 5990737.592,3557674.063 5990755.703,3557695.676 5990775.779,3557707.1125708 5990786.7057897,3557722.482 5990800.348,3557746.2 5990821.969,3557747.5690862 5990820.6195665,3557723.7947615 5990799.0152741,3557708.3741734 5990785.3513305,3557696.9521737 5990774.4249857,3557675.3186688 5990754.2702067,3557652.9024586 5990735.9991590995,3557633.5767331 5990722.5139163,3557613.5967576 5990710.4967641,3557592.644726 5990701.1271529,3557565.859088 5990691.2133099,3557541.465485 5990684.4292099,3557517.2751857 5990678.9645873,3557494.7403501 5990677.0559501,3557469.9343964 5990676.7393761,3557445.1487211 5990677.9906796,3557424.7041394 5990680.9572617,3557425.029 5990683.328</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171714">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553505.072 5989061.358,3553502.895 5989058.596,3553501.391 5989059.022,3553503.5354251 5989061.980845,3553505.7947299 5989067.1188673,3553509.4094345 5989080.086712,3553510.8835512 5989089.5330028,3553514.81 5989100.962,3553518.7045143 5989111.6575258,3553521.9849079 5989131.9805742,3553522.7515916 5989131.8073919,3553519.597 5989111.494,3553516.199 5989100.615,3553512.555 5989089.213,3553510.538 5989079.586,3553507.068 5989066.566,3553505.072 5989061.358</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717188">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558862.995 5991405.866,3558868.068 5991426.519,3558872.47 5991428.953,3558867.457 5991407.067,3558858.107 5991374.318,3558845.417 5991336.891,3558843.773 5991333.573,3558841.949 5991330.81,3558841.723 5991328.108,3558842.3912044 5991325.6218023,3558838.3633705 5991324.2883909,3558829.0440731 5991321.299768,3558815.6698169 5991315.6519113,3558797.8857684 5991306.3270145,3558781.3012592 5991296.4788008,3558762.8978531 5991283.2601804,3558747.356874 5991270.1601986,3558729.1161864 5991254.0972575,3558708.972423 5991233.9184414,3558700.6231733 5991224.9679348,3558687.897673 5991211.5268881,3558676.8185234 5991200.5822923,3558662.2393098 5991189.7181995995,3558646.1138344 5991180.0955306,3558621.1094284 5991166.4174371,3558594.0650662 5991158.0214207,3558571.4851181 5991154.4094773,3558547.2736743 5991153.9747287,3558524.6425447 5991154.7553555,3558501.8045362 5991157.3152487,3558477.5816278 5991160.1294151,3558453.3446096 5991162.3985828,3558453.451 5991164.293,3558477.748 5991162.001,3558502.0348268 5991159.1679913,3558524.811 5991156.497,3558547.3088279 5991155.623354,3558571.112 5991156.649,3558593.4846919 5991161.0075436,3558620.283 5991168.67,3558645.2535219 5991181.5849138,3558660.961 5991191.373,3558675.1385306 5991202.4390746,3558686.262 5991213.205,3558699.036 5991226.48,3558707.6230474 5991235.2489339,3558727.404 5991255.78,3558745.5401336 5991272.1290803,3558761.46 5991285.178,3558779.985 5991298.432,3558796.8270747 5991308.3699948,3558814.826 5991317.946,3558828.199 5991323.591,3558837.6553601 5991326.6766544,3558839.188 5991329.873,3558838.596 5991332.487,3558841.498 5991336.402,3558853.381 5991373.539,3558858.321 5991390.771,3558862.995 5991405.866</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308631332"/>
     <simBase:roughnessStyle>Böschung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717124">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557326.3402269 5990690.174651,3557346.0910722 5990686.9376532,3557367.9179042 5990682.5529144,3557394.5181424 5990677.4879217,3557423.4928029 5990672.1172791,3557423.195 5990669.944,3557394.169 5990675.34,3557367.518 5990680.413,3557345.7099107 5990684.8613496,3557326.086 5990688.073,3557299.327 5990692.822,3557299.6914121 5990694.9436677,3557326.3402269 5990690.174651</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717155">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557056.1052492 5990701.7394436,3557055.3033628 5990703.7321508,3557059.219 5990705.459,3557062.566957 5990707.0027708,3557065.777 5990707.467,3557069.458 5990707.267,3557072.1337864 5990707.6556068,3557074.61 5990708.672,3557076.885 5990710.145,3557079.3857002 5990711.0763237,3557082.775 5990711.953,3557090.805 5990713.827,3557097.9572976 5990715.8639502,3557110.913 5990718.688,3557131.255 5990722.885,3557148.729671 5990725.3653836,3557173.308 5990725.805,3557199.8016272 5990724.2848537,3557225.76 5990720.604,3557249.0703002 5990716.4288547,3557271.92 5990712.574,3557301.7 5990706.638,3557301.2762343 5990704.1707655,3557271.3755462 5990710.0647545,3557248.6577125 5990713.9611077,3557225.2305132 5990718.2520338,3557199.5728915 5990722.0706369,3557173.2386118 5990723.6805965,3557148.819316 5990723.2853144,3557131.6336399 5990721.0160189,3557111.4765633 5990717.0685607,3557098.311616 5990714.2985482,3557091.3107312 5990712.2423084,3557083.2623653 5990710.2683641,3557079.8594048 5990709.3345921,3557077.3480338 5990708.7181773,3557074.8959328 5990707.6617344,3557072.3725463 5990706.9541264,3557069.7530367 5990706.3541255,3557066.1901037 5990706.1899043,3557063.0018107 5990705.4713119,3557059.7975559 5990703.6891226,3557056.1052492 5990701.7394436</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717164">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557396.327 5990688.616,3557425.029 5990683.328,3557424.7041394 5990680.9572617,3557395.9461246 5990686.2728575,3557396.327 5990688.616</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717198">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558385.426 5991160.847,3558399.4934184 5991162.6339522,3558421.514 5991163.988,3558453.451 5991164.293,3558453.3446096 5991162.3985828,3558421.514 5991162.0919603,3558399.6436664 5991160.7078033,3558385.6324946 5991158.962329,3558366.2870931 5991155.7145178,3558350.3386384 5991152.8267102,3558334.4605788 5991149.3218149,3558315.7690447 5991143.9980146,3558300.508811 5991138.99151,3558290.0253129 5991135.0972075,3558278.6306436 5991130.9693383,3558277.7859574 5991133.5569607,3558289.085 5991137.377,3558299.7681818 5991141.1319991,3558315.12 5991146.119,3558333.85 5991151.348,3558349.9568109 5991154.68999,3558365.999 5991157.536,3558385.426 5991160.847</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717197">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558476.7045353 5991150.262662,3558500.5808011 5991147.4700106,3558523.6651108 5991144.6497554,3558547.0502603 5991143.4971104,3558546.9964746 5991140.9746837,3558523.436 5991142.281,3558500.3021581 5991145.2282614,3558476.525 5991148.243,3558452.687 5991150.689,3558452.7881255 5991152.4896687,3558476.7045353 5991150.262662</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717168">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552919.9732505 5987570.3994621,3552940.362799 5987587.6843688,3552949.0455517 5987595.1148756,3552963.0373523 5987606.6380273,3552973.4484364 5987616.3716922,3552982.935774 5987630.6823439,3552988.3122734 5987644.2167552,3552991.5818387 5987662.5215363,3552992.2464958 5987678.1491361,3552992.0612251 5987703.9690958,3552991.7165384 5987726.4844368,3552993.197 5987726.457,3552993.686 5987703.969,3552994.034 5987678.194,3552993.405 5987662.339,3552990.128 5987643.757,3552984.718 5987629.82,3552975.142 5987615.116,3552964.54 5987605.15,3552950.441 5987593.552,3552941.634 5987586.03,3552921.382 5987568.846,3552896.9892457 5987549.4472261,3552884.282 5987539.231,3552871.033 5987526.882,3552862.888 5987515.366,3552861.1486832 5987516.5192676,3552869.4301314 5987528.2222733,3552882.8767033 5987540.7668003,3552895.7363472 5987551.0291772,3552919.9732505 5987570.3994621</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171712">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558987.587532 5991326.8028017,3558987.7000531 5991328.0788503,3559012.5648854002 5991326.5350514,3559038.3453212 5991326.0134201,3559053.7884693 5991325.9957499,3559072.5570822 5991326.4815295,3559088.9477643 5991327.5403635,3559106.2216226 5991330.364741,3559124.5829036 5991334.5089496,3559138.4030481 5991338.0290049,3559163.7026512 5991344.2852032,3559183.9658911 5991349.325436,3559210.5262445 5991356.1643672,3559234.1165849 5991362.8030977,3559256.9287731 5991367.9605798,3559278.4976113 5991372.4244183,3559305.6275453 5991376.3350267,3559331.6244059 5991378.4508818,3559355.7118962 5991378.9565313,3559378.601311 5991378.4599128,3559397.2185593 5991377.9555799,3559417.2638469 5991376.3853357,3559430.7408061 5991374.9909203,3559445.9736952 5991372.0919823,3559454.5264065 5991369.8886819,3559462.5203973 5991368.0348678,3559465.9670167 5991367.2218286,3559465.765457 5991366.2464365,3559462.318 5991366.64,3559454.189 5991368.145,3559445.533 5991370.103,3559430.362786 5991372.8175503,3559417.155 5991374.336,3559397.137 5991376.146,3559378.6088436 5991376.8749307,3559355.857 5991377.389,3559331.6736425 5991376.8456367,3559305.868 5991374.446,3559278.8634961 5991370.1982862,3559257.576 5991366.072,3559234.6438703 5991361.3030743,3559210.981 5991354.757,3559184.2764723 5991347.9900783,3559164.158 5991342.876,3559138.79096 5991336.4417178,3559124.92 5991333.1,3559106.515 5991329.082,3559089.1025948 5991326.3191393,3559072.706 5991325.13,3559053.835 5991324.483,3559038.4550349 5991324.353041,3559012.576 5991325.037,3558987.587532 5991326.8028017</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717185">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559468.238206 5991378.2126169,3559468.4395634 5991379.1870297,3559470.372 5991378.232,3559472.6085919 5991377.8876823,3559474.662 5991377.554,3559476.995 5991376.726,3559479.1827714 5991376.2012207,3559484.672 5991375.899,3559496.414 5991375.221,3559513.123 5991374.995,3559529.0022701 5991376.2024908,3559543.053 5991378.369,3559558.823 5991382.734,3559570.4859486 5991387.106547,3559587.983 5991394.74,3559607.522 5991405.952,3559618.6306983 5991414.3171106,3559630.336 5991424.407,3559643.031 5991438.892,3559650.9658234 5991449.982329,3559658.903 5991461.118,3559668.722 5991477.984,3559674.2556232 5991490.9130633,3559683.685 5991509.889,3559692.7749033 5991529.5985597,3559694.1792104 5991529.0221523,3559685.0474182 5991509.398695,3559675.5169616 5991490.3939086,3559669.7394697 5991477.4645251,3559659.7951174 5991460.557831,3559651.7534071 5991449.3791906,3559643.8558695 5991438.3221105,3559631.0510226 5991423.7089149,3559619.182882 5991413.467839,3559608.0403512 5991405.0374973,3559588.4679863 5991393.6990265,3559570.9085822 5991385.8771647,3559559.317852 5991381.3726892,3559543.4178327 5991376.7970648,3559529.3389326 5991374.4200053,3559513.2478408 5991373.2808518,3559496.4462815 5991373.5863572,3559484.6010359 5991374.3448061,3559479.0887114 5991374.7492642,3559476.8428206 5991375.1428761,3559474.4049714 5991375.8396222,3559472.2913716 5991376.1107759,3559470.138519 5991376.9060981,3559468.238206 5991378.2126169</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717180">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559839.6633879 5991650.9878493,3559839.7380428 5991652.0662684,3559857.4860497 5991652.0259996,3559874.7064155 5991650.0361570995,3559890.5446734 5991645.2204074,3559902.9138519 5991639.5271911,3559917.774728 5991632.4191477,3559929.7918945 5991626.5593869,3559945.7059561 5991618.4003723,3559964.0377151 5991609.158888,3559978.7126285 5991602.3279607,3559994.406658 5991594.6178814,3560010.8569743 5991586.6106694,3560026.9982961 5991578.9381135,3560049.1205545 5991568.0866017,3560048.155 5991566.238,3560026.2479324 5991577.4540234,3560010.145 5991585.272,3559993.801 5991593.395,3559978.2058007 5991601.2281214,3559963.478 5991607.996,3559945.182 5991617.147,3559929.1572409 5991625.2146253,3559917.053 5991631.129,3559902.34 5991638.162,3559890.0582749 5991643.8300314,3559874.347 5991648.713,3559857.29 5991650.797,3559839.6633879 5991650.9878493</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717113">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552911.3834485 5987579.8716444,3552932.4846594 5987597.9371521,3552940.2512977 5987604.9642733,3552953.4024998 5987616.1791353,3552962.3941453 5987624.5678923,3552971.0866765 5987636.4156217,3552976.0086883 5987647.3321127,3552978.9825611 5987663.7829855,3552979.6397255 5987677.8327233,3552980.3581298 5987703.9697859,3552980.817471 5987726.6864245,3552981.4240006 5987754.1216548,3552985.5519857 5987774.8238548,3552996.723124 5987798.783021,3553010.728034 5987821.5980717,3553023.8490873 5987844.3715661,3553036.6556866 5987865.8197332,3553049.2170144 5987888.8504945,3553059.1569053 5987908.9167661,3553072.3528413 5987932.9537134,3553083.0100568 5987953.8494169,3553094.6137429 5987975.7176212,3553105.1745806 5987997.1808641,3553121.7135349 5988028.426279,3553133.981551 5988051.3717552,3553139.2326212 5988061.1930911,3553151.6271561002 5988084.9726497,3553164.476559 5988108.877443,3553175.4096691 5988130.1326752,3553190.0645727 5988154.6744606,3553204.8578178 5988174.7114641,3553228.6807065 5988199.6837369,3553257.0596297 5988230.6336634,3553259.2171806 5988229.2318583,3553230.6072577 5988197.7576177,3553206.7492591 5988172.8065846,3553192.3167588 5988153.1886074,3553177.8501935 5988128.8688536,3553166.8609454 5988107.5122829,3553154.0347636 5988083.6211517,3553141.712798 5988059.8235141,3553136.3543173 5988049.8564837,3553124.1882763 5988027.2270657,3553107.4378318 5987995.8421107,3553096.8999375 5987974.7735561,3553085.1824799 5987952.6859804,3553074.4071718 5987931.7942314,3553061.2082909 5987907.7888482995,3553051.2817337 5987887.8892103,3553038.5771897 5987864.6275836,3553025.6677991 5987843.0082698,3553012.5882528 5987820.4449313,3552998.7888763 5987797.9487599,3552987.4445702 5987774.0438978,3552983.5292934002 5987753.9400725,3552982.898861 5987726.647851,3552982.6263932 5987703.9696522,3552982.1185264 5987677.8949378,3552981.494756 5987663.5314627,3552978.4954942 5987646.7024353,3552973.513499 5987635.2413848,3552964.6875097 5987622.8674781,3552955.426591 5987614.174738,3552942.1214902 5987602.8696931,3552934.1800843 5987595.730689,3552913.2535978 5987577.8093856,3552889.8723103 5987558.4333042,3552876.3389839 5987547.9116484,3552862.0169657 5987534.4209521,3552853.150355 5987521.8226216,3552848.5950867 5987511.7789093,3552845.2011779 5987499.7347808,3552844.4187967 5987488.3802244,3552845.1998341 5987474.0211865,3552843.3589089 5987474.1254678,3552842.3441677 5987488.4786646,3552843.044278 5987500.2283609,3552846.5252496 5987512.6334023,3552851.1602819 5987523.1421551,3552860.0912903 5987536.0311473,3552874.5675978 5987549.8475351,3552888.2164727 5987560.5240196,3552911.3834485 5987579.8716444</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717175">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553502.3966062 5989052.2411174,3553503.498 5989051.862,3553510.32 5989048.58,3553520.657 5989043.539,3553531.256 5989038.886,3553547.016 5989029.063,3553559.6452275 5989021.9630886,3553569.891 5989015.281,3553578.329 5989010.114,3553584.586 5989006.912,3553588.4438275 5989004.9267558,3553589.41 5989003.877,3553589.8290528 5989002.2088617,3553591.395 5989000.963,3553595.026 5988999.654,3553600.43 5988997.12,3553604.1375568 5988995.182173,3553613.261 5988989.021,3553625.148 5988981.744,3553638.278 5988974.677,3553646.526 5988970.919,3553654.61 5988968.522,3553669.856 5988968.169,3553683.7633783 5988969.0530885,3553704.605 5988973.828,3553728.418 5988982.746,3553756.079 5988988.266,3553783.797 5988992.822,3553809.64 5988995.746,3553831.736 5988995.92,3553859.376 5988995.396,3553886.08 5988993.635,3553910.502 5988990.177,3553931.537 5988987.434,3553967.367 5988981.654,3554003.538 5988976.609,3554030.386 5988971.514,3554058.49 5988966.519,3554085.9570685 5988962.8217465,3554108.212 5988961.881,3554127.969 5988962.348,3554154.954 5988965.698,3554177.751 5988970.577,3554201.22 5988978.103,3554220.405 5988986.191,3554250.585 5988999.334,3554283.734 5989012.931,3554315.066 5989022.539,3554341.685 5989027.961,3554365.265 5989029.603,3554391.289 5989029.152,3554416.393 5989027.056,3554440.655 5989021.995,3554460.363 5989018.378,3554484.058 5989014.838,3554505.2100045 5989014.3086383,3554525.784 5989016.653,3554546.19 5989023.247,3554562.457 5989031.005,3554576.484 5989038.007,3554588.883 5989046.323,3554598.113 5989054.575,3554611.331 5989069.567,3554626.87 5989088.095,3554638.538 5989102.568,3554651.2 5989117.457,3554661.746 5989130.309,3554678.197 5989150.116,3554693.934 5989167.004,3554706.137 5989177.751,3554722.305 5989190.783,3554734.553 5989199.488,3554735.6437587 5989197.8755951,3554723.6821935 5989189.2282122,3554707.7949038 5989176.115643,3554695.6171012 5989165.2006439,3554679.790726 5989148.4718066,3554663.3613946 5989128.8466204,3554653.0128341 5989116.1364202,3554640.1991522 5989101.0774654,3554628.4298087 5989086.6477443,3554613.0284788 5989067.9123584,3554599.6210963 5989052.9197793,3554590.4245631 5989044.6036344,3554577.7214737 5989036.0282598,3554563.491098 5989028.8882433,3554546.9280357 5989021.1347929,3554525.9914198 5989014.6757568,3554505.2938858 5989012.3854668,3554483.9023415 5989012.9065027,3554460.1091665 5989016.4857512005,3554440.2494331 5989020.0599224,3554416.083438 5989024.856591,3554391.1014413 5989026.931966,3554365.3926456 5989027.523282,3554341.8415175 5989025.7991679,3554315.4049454 5989020.5569778,3554284.4555143 5989010.890897,3554251.3698298 5988997.1972774,3554221.2217596 5988984.1334195,3554201.8174827 5988976.0252638,3554178.1909923 5988968.5454842,3554155.2059657 5988963.5535831,3554128.0509901 5988960.3170254,3554107.9909637 5988959.7958464,3554085.6671611 5988960.8236687,3554058.0883288 5988964.4396009,3554029.9841956 5988969.4302142,3554003.1411877 5988974.425391,3553966.9098056 5988979.3414433,3553931.104576 5988985.2174534,3553910.1562134 5988987.8877705,3553885.6602539 5988991.3376648,3553859.0978672 5988993.0573162995,3553831.6002289 5988993.5399171,3553809.8435921 5988993.1571379,3553784.0172677 5988990.3488543,3553756.6251333 5988985.7473698,3553729.0202372 5988979.9308127,3553705.2687145 5988971.1720519,3553683.9975334 5988966.3763105,3553669.7246457 5988965.624612,3553654.202881 5988966.1544323,3553645.9212193 5988968.3848916,3553637.2544947 5988972.1486753,3553623.8778581 5988979.2033491,3553611.5706483 5988986.6799774,3553602.464528 5988992.8293568,3553598.8884575 5988994.8369291,3553593.7674872 5988997.5485991,3553590.2030587 5988999.0595258,3553588.5838549 5989000.2905679,3553588.4313324 5989002.2421012,3553587.7384864 5989003.6782167,3553583.9413948 5989005.6459867,3553577.7762833 5989008.8976393,3553569.3135005 5989014.1083782,3553558.9496161 5989020.8957551,3553545.9394232 5989027.1805372,3553530.1615576 5989037.0132248,3553520.0214895 5989041.5505092,3553509.704196 5989046.5784134,3553502.8524661 5989049.7659149,3553500.4420055 5989050.6345006,3553499.3792019 5989051.4467548,3553499.9435322 5989053.6024341,3553502.895 5989058.596,3553505.072 5989061.358,3553505.023 5989058.653,3553502.809 5989055.518,3553502.3966062 5989052.2411174</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117171105">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554846.7293378 5989236.2212764,3554846.2991498 5989238.5569911,3554857.2507896 5989241.122737,3554868.5531306 5989243.7316463,3554876.4696435 5989245.1698414,3554890.5170919 5989246.9328685,3554909.2589321 5989248.3545434,3554925.1314177 5989248.8816474,3554948.4012288 5989248.3113486,3554977.6766722 5989245.6391482,3555003.6265187 5989243.0829932,3555023.8950937 5989241.4089796,3555050.064 5989240.233,3555075.0168361 5989240.6760726,3555099.7153981 5989242.4763565,3555124.8276911 5989246.6688068,3555149.8011585 5989251.5783945,3555173.9828471 5989258.894376,3555197.5466778 5989268.5282998,3555219.4399219 5989278.6917513,3555242.385531 5989291.4168653,3555264.7832169 5989305.8551616,3555265.8568078 5989304.3608382,3555243.524 5989289.661,3555220.4479362 5989276.5711356,3555198.429 5989266.567,3555174.5958144 5989257.1054791,3555150.41 5989249.17,3555125.3619275 5989243.8611823,3555100.152 5989239.925,3555075.1090422 5989238.5330554,3555050.006 5989238.374,3555023.7641066 5989239.689963,3555003.538 5989241.478,3554977.629 5989244.184,3554948.39 5989247.098,3554925.1214119 5989247.8256948,3554909.255 5989247.452,3554890.572 5989246.222,3554876.5208845 5989244.6523722,3554868.777 5989242.593,3554857.642 5989239.372,3554846.7293378 5989236.2212764</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717183">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554729.7594081 5989182.3673178,3554740.3990425 5989190.8461369,3554741.309 5989189.501,3554730.936 5989181.039,3554716.652 5989167.379,3554704.739 5989155.427,3554688.555 5989139.43,3554672.377 5989120.685,3554663.283 5989108.655,3554649.754 5989092.504,3554637.541 5989078.194,3554623.1 5989058.095,3554608.712 5989042.942,3554599.868 5989034.071,3554585.427 5989023.707,3554570.038 5989015.487,3554551.68 5989007.535,3554527.35 5989001.725,3554505.8529495 5988999.5676531,3554482.866 5989000.047,3554458.421 5989003.901,3554437.555 5989007.204,3554414.029 5989010.26,3554389.858 5989012.214,3554366.238 5989013.75,3554342.877 5989011.497,3554317.645 5989007.458,3554289.219 5988997.422,3554256.546 5988983.105,3554226.603 5988970.577,3554205.75 5988962.35,3554181.084 5988955.188,3554156.861 5988949.468,3554128.589 5988946.99,3554106.542 5988946.127,3554083.7686196 5988947.738684,3554055.533 5988951.211,3554027.5 5988956.547,3554000.756 5988961.3,3553964.237 5988965.822,3553928.645 5988972.61,3553908.242 5988975.215,3553883.398 5988978.956,3553857.638 5988980.782,3553830.906 5988981.37,3553810.858 5988980.258,3553785.087 5988978.338,3553759.211 5988973.822,3553731.801 5988966.932,3553708.258 5988959.21,3553685.026439 5988954.6142273,3553669.146 5988954.416,3553652.405 5988955.699,3553643.244 5988957.167,3553632.713 5988960.93,3553618.229 5988967.904,3553604.036 5988976.245,3553594.9906618 5988982.3186988,3553591.732 5988984.238,3553587.679 5988987.363,3553584.175 5988989.433,3553581.9772824 5988990.1127903,3553580.966 5988989.771,3553579.7456041 5988989.5298482,3553576.583 5988991.194,3553571.418 5988994.905,3553562.616 5989000.509,3553550.8130369 5989008.4111372,3553539.185 5989015.37,3553523.099 5989024.928,3553515.803 5989028.351,3553505.499 5989032.91,3553498.317 5989035.039,3553496.285 5989035.586,3553493.363 5989036.323,3553490.056 5989036.957,3553486.946 5989037.499,3553487.4126995 5989040.220201,3553490.6619706 5989039.5946083,3553493.929282 5989038.8251885,3553496.9135548 5989037.8613897,3553499.0103799 5989037.2904437,3553506.1491071 5989035.0230841,3553516.4625825 5989030.4148111,3553524.2160005 5989026.8393759,3553540.2657525 5989017.2597643,3553551.6697201 5989009.7256162,3553563.2907383 5989001.8790665995,3553572.0309277 5988996.2538667,3553577.2617082 5988992.5269921,3553580.4509452 5988990.7783873,3553581.8925912 5988991.3189034,3553583.1146757 5988991.8650054,3553585.2001576 5988991.0701284,3553588.7020585 5988989.0745029,3553592.9204014 5988985.9980583,3553596.2174723 5988984.0439886,3553605.2955903998 5988977.9894473,3553619.1908556 5988969.8279893,3553633.5007373 5988962.8759165,3553643.7170993 5988959.1493467,3553652.7287199 5988957.581567,3553669.2521738 5988956.4726322,3553684.8340257 5988956.8138275,3553707.7142454 5988961.3859114,3553731.3091526 5988969.2311648,3553758.7664192 5988975.8722956,3553784.9082947 5988980.3444859,3553810.6934024 5988982.3510108,3553831.0153664 5988983.287207,3553857.8611916 5988982.6587102,3553883.7334991 5988980.7922385,3553908.5172438 5988977.0372114,3553928.9877241 5988974.3667574,3553964.5977254 5988967.6466021,3554001.06761 5988963.0147508,3554027.8139744 5988958.1752933,3554055.8452479998 5988952.827467,3554083.9927629 5988949.2835079,3554106.7186728 5988947.7936487,3554128.521311 5988948.6667217,3554156.6463247 5988951.2950475,3554180.6974335 5988956.9728399,3554205.2091011 5988964.2309671,3554225.8416443 5988972.4950071,3554255.7932021 5988985.1545146,3554288.5073292 5988999.4342704,3554317.3014175 5989009.4671383,3554342.7140405 5989013.7478103,3554366.1015735 5989015.9727849,3554390.0636732 5989014.6484467,3554414.3771111 5989012.7332969,3554438.0224704 5989009.4344368,3554458.7207522 5989006.1355588,3554483.0542425 5989002.3828175,3554505.749111 5989001.9483896,3554527.0997777 5989004.110261,3554550.8121946 5989010.018599,3554568.8526194 5989017.9134262,3554584.0438768 5989025.918636,3554598.1876945 5989035.9451104,3554607.1086542 5989044.7017624,3554621.3394885 5989059.8110836,3554635.9626492 5989079.6584599,3554648.1137997 5989093.9757346,3554661.5361537 5989109.9275103,3554670.8577304 5989122.0603599,3554687.091892 5989140.9394393,3554703.2305771 5989157.0431973,3554715.2013499 5989168.8099218,3554729.7594081 5989182.3673178</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717196">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555695.4142288 5989738.0461317,3555715.2326207 5989744.2771501,3555735.8502255 5989749.8240003,3555765.6894039 5989757.5567533,3555792.4667063 5989764.0934908,3555811.8100463 5989769.0677256,3555835.5619164 5989774.0258545,3555863.439172 5989780.7051528,3555889.1537515 5989786.6482494,3555909.4897927 5989791.8110186,3555933.8707574 5989797.6588857,3555959.2221963 5989808.4560922,3555980.5772293 5989821.6117324,3555998.7788832 5989838.4256491,3556011.3538035 5989852.2355864,3556023.7799572 5989871.1318417,3556030.4859309 5989886.1077852,3556038.9786075 5989906.9447131,3556044.403145 5989927.5845454,3556046.4552321 5989947.7486805,3556046.0848505 5989967.6118016,3556042.3676059 5989990.2044071,3556038.9268518 5990003.4176926,3556036.0808342 5990014.3470537,3556034.9218929 5990018.1117767,3556033.6906338 5990020.8784958,3556034.883852 5990021.1931169,3556036.177 5990018.439,3556037.2950981 5990014.6174223,3556040.2422435 5990003.7725124,3556043.826 5989990.585,3556047.7884759 5989967.7472324,3556048.431 5989947.526,3556046.6450247 5989927.2479973,3556041.424 5989906.47,3556033.0948456 5989884.980608,3556026.141 5989869.602,3556013.229 5989850.36,3556000.4795509 5989836.7124284,3555981.869 5989819.651,3555960.1905362 5989806.6469427,3555934.305 5989795.4,3555909.9425715 5989789.3777869,3555889.641 5989784.065,3555863.949 5989778.002,3555836.148 5989771.148,3555812.4358264 5989766.2555093,3555793.063 5989761.396,3555766.317 5989755.07,3555736.276 5989747.425,3555715.8933714 5989742.0438455,3555696.2 5989735.784,3555670.801 5989726.316,3555669.9834471 5989728.5361169,3555695.4142288 5989738.0461317</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717159">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553297.0236359 5988817.4766106,3553316.8548644 5988819.4593192,3553337.3981013 5988825.5210455,3553357.5040622 5988834.0859651,3553372.370733 5988844.6825042,3553386.7571488 5988862.7312073,3553388.717 5988861.538,3553373.938 5988842.963,3553358.636 5988832.159,3553338.025 5988823.46,3553317.168 5988817.195,3553297.023 5988815.277,3553297.0236359 5988817.4766106</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717179">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556023.1301579 5990025.9176486,3556021.3812649 5990025.645117,3556019.04 5990033.677,3556016.8835148 5990041.8539748,3556013.334 5990055.883,3556009.6258844 5990070.6415275,3556007.063 5990089.728,3556005.55 5990108.542,3556004.6574935 5990124.5859005,3556005.657 5990143.466,3556008.035 5990163.687,3556011.9462413 5990182.7278201,3556017.763 5990203.37,3556025.7180486 5990223.7181999,3556037.002 5990246.08,3556048.6612505 5990266.2198279,3556064.444 5990288.434,3556081.3695971 5990308.1785382,3556099.512 5990324.802,3556118.2693766 5990338.9430712,3556130.933 5990347.331,3556151.738 5990358.166,3556166.7803385 5990365.527238,3556185.84 5990373.828,3556214.688 5990387.022,3556238.709 5990398.072,3556256.1224121 5990406.5184366,3556256.7277742 5990405.0438612,3556239.3381871 5990396.5723605,3556215.3340457 5990385.4766241,3556186.5010508 5990372.1234588,3556167.5218501 5990363.6590141,3556152.227184 5990356.2235015,3556131.791877 5990345.6128455,3556119.2130415 5990337.4847621,3556100.5024376 5990323.4122447,3556082.4939988 5990307.026227,3556065.5343883 5990287.6765087,3556049.4942775 5990265.7151081,3556038.1344275 5990245.5930985,3556027.0363287 5990223.0814633,3556019.4868136 5990202.8512352,3556013.8908075 5990182.2865739,3556009.7930804 5990163.3962716,3556007.223899 5990143.338607,3556006.0107145 5990124.4991318,3556007.1408379 5990108.6067344,3556008.9354388 5990089.9063079,3556011.7339429 5990071.0213344,3556015.0960906 5990056.1408992,3556018.2775963 5990042.0782088,3556020.5804081 5990034.0918162,3556023.1301579 5990025.9176486</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717166">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556544.148 5990640.028,3556562.0854603 5990652.7696263,3556584.474 5990664.011,3556606.8141498 5990672.9750027,3556631.879 5990681.497,3556657.3567839 5990686.7905621,3556683.111 5990688.838,3556707.6690472 5990689.4111943,3556733.095 5990687.476,3556732.8937896 5990685.4015534,3556707.6306541 5990687.6576145,3556683.2683872 5990686.6792255,3556657.7964398 5990684.2645378,3556632.5684562 5990679.0145389,3556607.6328097 5990670.5705494,3556585.504707 5990661.729038,3556563.2671068 5990650.5268743,3556545.2386882 5990638.0975582,3556522.9462367 5990619.7507628,3556521.461 5990621.046,3556544.148 5990640.028</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717151">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553076.598 5988662.999,3553081.451 5988672.944,3553085.191 5988682.239,3553086.8941255 5988681.4890116,3553083.3469392 5988672.4496081,3553078.4988039 5988662.1288372,3553073.0658942 5988648.5104091,3553065.3875647 5988630.821056,3553059.8413535 5988613.8708265,3553057.0122896 5988597.8251861,3553056.654014 5988578.2172142,3553059.4636618 5988561.0934506,3553064.183111 5988547.4388228,3553071.8853437 5988530.1212023,3553083.2160967 5988513.5149395,3553098.3318651 5988497.3273008,3553119.3234369 5988478.5015873,3553137.9055041 5988462.7028531,3553156.2878763 5988445.5596201,3553171.1580601 5988428.3722479,3553187.6510431 5988408.6585145,3553202.0983673 5988391.4918777,3553218.7076239 5988371.1120816,3553235.7009529 5988350.7909937,3553251.4833152 5988330.9836394,3553263.2910923 5988314.4254992,3553271.4979967 5988297.5051894,3553273.7648212 5988287.2487402,3553274.7760111 5988274.8542436,3553273.6831402 5988260.3473469,3553268.6371664 5988247.5205799,3553257.0596297 5988230.6336634,3553254.832 5988232.081,3553266.428 5988248.717,3553271.255 5988260.981,3553272.174 5988274.834,3553271.102 5988286.99,3553269.05 5988296.723,3553260.763 5988313.455,3553249.568 5988329.417,3553233.697 5988349.199,3553216.861 5988369.57,3553200.094 5988390.114,3553185.74 5988407.049,3553169.076 5988426.924,3553154.589 5988443.777,3553136.081 5988460.994,3553117.738 5988476.688,3553096.851 5988495.675,3553081.5659781 5988512.2112073,3553070.047 5988529.146,3553062.191 5988546.735,3553057.421 5988560.594,3553054.522 5988577.983,3553054.896 5988597.817,3553057.905 5988614.296,3553063.499 5988631.498,3553071.168 5988649.274,3553076.598 5988662.999</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717133">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556999.1517761 5990672.6251331,3557013.4179139 5990677.4862548,3557030.0726136 5990682.7584862,3557049.071263 5990688.4917221,3557059.8835977 5990692.3501558,3557060.6851108 5990690.3583763,3557049.577 5990686.48,3557030.538 5990680.84,3557013.9816325 5990675.6592449,3556999.918 5990670.593,3556976.248 5990663.896,3556958.3174177 5990659.4395585,3556934.648 5990654.689,3556908.509853 5990652.7462186,3556908.2349657 5990654.8372274,3556934.342159 5990657.0226373,3556957.6975608 5990661.8996696,3556975.559844 5990666.1380989,3556999.1517761 5990672.6251331</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717123">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557767.954 5990842.325,3557781.5544532 5990855.2999419,3557799.001 5990871.498,3557821.836 5990892.165,3557843.222 5990911.564,3557856.6423993 5990923.8714391,3557873.74 5990939.917,3557889.5211383 5990954.1224878,3557903.711 5990965.496,3557920.721 5990977.946,3557931.9304538 5990985.6314146,3557955.846 5991000.769,3557976.2425753 5991012.8271274,3557998.578 5991026.084,3558020.4640196 5991037.480467,3558041.502 5991046.417,3558060.554 5991054.755,3558061.2806391 5991052.8441721,3558042.1386798 5991044.6415034,3558021.110303 5991035.8369726,3557999.2683001 5991024.4552551,3557977.084894 5991011.1957491,3557956.4936647 5990999.3275598,3557932.6429653 5990984.4871107,3557921.4790985 5990976.6415311,3557904.7535329 5990964.1840184,3557890.670879 5990952.7414356,3557874.852295 5990938.6295815,3557857.5488536 5990922.5469145,3557844.408829 5990910.2836936,3557823.1187026 5990890.8079891,3557800.2766799 5990869.9616108,3557782.8540715 5990853.6631486,3557769.3180782 5990840.8364862,3557747.5690862 5990820.6195665,3557746.2 5990821.969,3557767.954 5990842.325</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717170">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553175.5037264 5988102.563932,3553186.8723669 5988124.196736,3553200.8102262 5988147.5851424,3553214.0275843 5988165.4765483,3553238.1735991 5988190.1929728,3553267.8677755 5988223.6113896,3553279.4729661 5988241.652227,3553285.7247541 5988257.2049395,3553287.8243053 5988274.9557589,3553287.2692691 5988288.5609365,3553284.0552237 5988301.5175025,3553276.4095616 5988319.4614959,3553261.5386936 5988339.2084751,3553246.3466894 5988359.2482515,3553228.6355905 5988379.4027437,3553213.0056857 5988398.9899798,3553198.1788226 5988417.5251975,3553182.771307 5988436.4502377,3553165.8837942 5988455.6285556,3553148.3432334 5988472.478959,3553128.5114273 5988489.0117645,3553130.295 5988491.052,3553150.374 5988474.381,3553167.755 5988457.592,3553185.041 5988438.029,3553200.241 5988419.262,3553215.147 5988400.462,3553230.589 5988381.034,3553248.446 5988360.916,3553263.526 5988340.834,3553279.008 5988320.459,3553286.548 5988302.314,3553289.956 5988288.822,3553290.426 5988274.976,3553288.131 5988256.577,3553281.643 5988240.477,3553270.037 5988222.202,3553240.083 5988188.284,3553215.876 5988163.615,3553202.981 5988146.153,3553189.193 5988122.995,3553177.741 5988101.283,3553164.8232174 5988077.5651094,3553152.916 5988053.637,3553147.0903906 5988043.0003236,3553135.46 5988021.765,3553117.835 5987989.692,3553107.496 5987970.398,3553095.344 5987947.244,3553084.108 5987926.319,3553070.991 5987902.41,3553069.3058028 5987903.3365756,3553082.3855184 5987927.2911835,3553093.4872615 5987948.2383724,3553105.5065394 5987971.2195312,3553115.8318642 5987990.8768906,3553133.2344201 5988022.8434742,3553144.9377203 5988044.3750397,3553150.6516287 5988054.8874072,3553162.593499 5988078.8167502,3553175.5037264 5988102.563932</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308554222"/>
     <simBase:roughnessStyle>Böschung_rauh</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117171102">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556972.064 5990677.528,3556994.778 5990684.225,3557009.7687794 5990689.3130864,3557010.2729419 5990687.6790974,3556995.3483616 5990682.7123211,3556972.4915117 5990676.1351129,3556955.1954106 5990671.8302951995,3556932.9172269 5990667.8951969,3556906.7484364 5990666.1449352,3556906.5610072 5990667.5706681,3556932.736 5990669.278,3556954.8741209 5990673.1054414,3556972.064 5990677.528</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717169">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554803.769 5989237.885,3554809.1110614 5989239.9334982,3554812.855 5989240.986,3554815.568 5989241.086,3554817.8928778 5989241.2752978,3554821.536 5989242.494,3554825.969 5989244.012,3554829.6761589 5989245.1296486,3554837.427 5989247.112,3554844.4130246 5989248.797748,3554844.6446922002 5989247.5399041,3554837.6493325 5989245.8867832,3554829.9641263 5989243.9729556,3554826.322234 5989242.8389603,3554821.9459773 5989241.2960734,3554818.3734377 5989240.0673813,3554816.1995544 5989239.471875,3554813.5621475 5989238.8723513,3554809.9951975 5989237.4395817,3554804.6804956 5989235.443927,3554796.7165064 5989231.9920504,3554781.4292817 5989224.6117241,3554765.6952113 5989216.8439851,3554752.4780834 5989209.34724,3554735.6437587 5989197.8755951,3554734.553 5989199.488,3554751.658 5989211.195,3554764.721 5989218.556,3554780.695 5989226.58,3554795.921 5989234.274,3554803.769 5989237.885</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171719">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556034.883852 5990021.1931169,3556033.6906338 5990020.8784958,3556032.9022205 5990024.1683418,3556032.1522717 5990027.3235729,3556029.5570855 5990036.509144,3556027.5701563 5990043.5728903,3556024.1564294 5990057.4669687,3556020.4555183 5990072.5926927,3556017.9716157 5990090.7668014,3556016.2683277 5990108.9781501,3556015.4852577 5990123.8916227,3556016.3589265 5990142.595905,3556018.4749185 5990161.9605823,3556022.1293709 5990180.4171414,3556028.9553262 5990200.0017791,3556036.532628 5990218.4946979,3556047.6481188 5990241.5025663,3556049.324 5990240.782,3556037.8518086 5990217.8575263,3556031.058 5990199.369,3556024.8141205 5990179.8079383,3556021.115 5990161.524,3556018.953 5990142.385,3556017.9990953 5990123.730435,3556018.845 5990109.083,3556020.683 5990091.025,3556023.2347701 5990073.0934279,3556026.63 5990057.829,3556029.707682 5990043.9167052,3556031.417 5990037.01,3556033.8092736 5990027.581785,3556034.273 5990024.471,3556034.883852 5990021.1931169</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717135">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560123.9104795 5991534.7268957,3560129.1392042 5991533.543122,3560128.4672743 5991531.9978922,3560123.148 5991533.304,3560123.9104795 5991534.7268957</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117171114">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560106.5318288 5991539.7655857,3560112.2225391 5991537.7858597,3560111.58 5991536.427,3560105.7325116 5991538.5738151,3560099.716 5991541.408,3560088.21 5991546.63,3560070.0062353 5991555.0051939,3560048.155 5991566.238,3560049.1205545 5991568.0866017,3560071.0505993 5991557.3402887,3560089.2560593 5991548.5327239,3560100.6425486 5991542.957053,3560106.5318288 5991539.7655857</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717121">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560118.007 5991550.019,3560123.882 5991547.447,3560129.513 5991545.182,3560133.5823159 5991543.7608968,3560132.7983312 5991541.9579759,3560128.5460961 5991543.3776192,3560122.7384484 5991545.5649392,3560117.0456585 5991547.9859281,3560113.1764667 5991549.6726452,3560106.3945585 5991552.5735674,3560094.3030437 5991557.7129093,3560075.067604 5991566.3219141,3560053.3794255 5991576.24042,3560030.9594761 5991586.7726456,3560014.9135175 5991594.2378689,3559998.167075 5991602.2105231,3559982.182997 5991609.8588177,3559967.7165997 5991616.8022956,3559949.0249711 5991626.339894,3559933.679628 5991634.7970692,3559922.2552191 5991640.4283917,3559906.5238244 5991648.1152991,3559893.6450093 5991654.0827569,3559877.090143 5991658.8116429,3559858.8478264 5991660.562729,3559840.2856965 5991659.977335,3559824.0271519 5991658.3333721,3559805.3081665 5991654.1055054,3559789.1888447 5991648.5612843,3559775.8986483 5991641.592636,3559759.9751952 5991631.6549624,3559748.6465014 5991622.3062348,3559738.137901 5991610.7014272,3559725.727199 5991592.8891165,3559715.6359861 5991575.264579,3559706.2022566 5991553.657063,3559694.1792104 5991529.0221523,3559692.7749033 5991529.5985597,3559704.815 5991554.414,3559714.1564494 5991575.9485901,3559724.215 5991593.618,3559736.845 5991611.853,3559747.6057582 5991623.5464019,3559759.111 5991633.084,3559774.996 5991642.983,3559788.3395256 5991650.0362288,3559804.685 5991656.137,3559823.695 5991660.826,3559840.4910148 5991662.9432368,3559859.243 5991663.04,3559877.602 5991660.696,3559894.1010285 5991655.3862934,3559907.158 5991649.624,3559923.173 5991642.069,3559934.5917024 5991636.7296553,3559949.91 5991628.457,3559968.823 5991619.101,3559983.3527606 5991612.3972578,3559999.352 5991604.603,3560016.106 5991596.48,3560032.0432845 5991588.9162321,3560054.334 5991578.068,3560075.7906567 5991567.9385884,3560095.241 5991559.419,3560107.493 5991554.41,3560114.4754268 5991551.6093761,3560118.007 5991550.019</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811717195">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554800.0262447 5989222.4979021,3554808.4302937 5989225.4016065,3554813.5922214 5989227.2933192,3554816.9701224 5989228.6859881,3554819.9608302 5989229.858816,3554822.1584016 5989230.5536455,3554825.1713051 5989231.8718786,3554829.0977675 5989233.6218103,3554832.223898 5989234.8960207,3554839.3128222 5989236.7197224,3554846.2991498 5989238.5569911,3554846.7293378 5989236.2212764,3554839.708 5989234.542,3554832.7111718 5989232.9387641,3554829.656 5989231.768,3554825.776 5989230.105,3554822.8193564 5989228.8922958,3554820.622 5989228.169,3554817.573 5989226.884,3554814.2324349 5989225.4874447,3554809.106 5989223.592,3554800.63 5989220.766,3554785.09 5989214.799,3554770.618 5989208.193,3554756.679 5989199.882,3554741.309 5989189.501,3554740.3990425 5989190.8461369,3554756.010898 5989201.3873253,3554769.8428991 5989209.5551113,3554784.5194248 5989216.3284532,3554800.0262447 5989222.4979021</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117171108">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559484.226393 5991366.1396987,3559496.6085974 5991365.3671389,3559513.8474638 5991365.0476221,3559530.8876919 5991366.2199828,3559545.3408297 5991368.5115498,3559562.3352833 5991373.0719009,3559573.9271614 5991377.096538,3559592.159815 5991385.7748933,3559612.2717867 5991397.5721746,3559624.0522369 5991405.9786554,3559637.5269763 5991417.3863638,3559651.5263023 5991433.0227271,3559659.2703854 5991443.6226239,3559667.166011 5991455.9295771,3559677.1024493 5991473.7053146,3559683.5861979 5991487.0726886,3559693.0153431 5991506.5312103,3559701.6993767 5991525.9354487,3559713.2548787 5991549.8088991,3559722.7840544 5991571.959924,3559733.2857141998 5991589.2458939,3559744.8172696 5991604.7521863,3559754.1986321 5991615.6902229,3559764.5475577 5991624.0940809,3559780.6361591 5991634.2953704,3559793.6115914 5991640.8806541,3559807.8152954 5991645.9323795,3559825.1008695 5991650.2756785,3559839.7380428 5991652.0662684,3559839.6633879 5991650.9878493,3559825.257 5991649.104,3559808.201 5991644.675,3559794.3266761 5991639.6388237,3559781.507 5991632.954,3559765.491 5991622.534,3559755.4714373 5991614.1735268,3559746.35 5991603.387,3559735.022 5991588.409,3559724.4278832 5991571.1999583,3559714.84 5991548.944,3559703.3516116 5991525.2572776005,3559694.547 5991505.98,3559684.9307623 5991486.5192788,3559678.243 5991473.123,3559668.226 5991455.264,3559660.2723316 5991442.8553249,3559652.652 5991432.245,3559638.568 5991416.37,3559624.9058594 5991404.6657636,3559612.977 5991396.328,3559592.744 5991384.521,3559574.3800296 5991375.7792076,3559562.79 5991371.821,3559545.632 5991367.257,3559531.1233928 5991364.9720465,3559513.951 5991363.626,3559496.64 5991363.777,3559484.146 5991364.379,3559478.4421211 5991364.7681858,3559475.866 5991364.981,3559472.856 5991365.508,3559470.4880487 5991366.0094816,3559468.264 5991366.261,3559465.765457 5991366.2464365,3559465.9670167 5991367.2218286,3559468.4737994 5991367.4524176,3559470.7551816 5991367.5058238,3559473.1071928 5991367.1834531,3559476.0383516 5991366.773976,3559478.5655952 5991366.6741906,3559484.226393 5991366.1396987</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718796">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556415.4534255 5990483.7003449,3556430.7346738 5990500.4218321,3556448.9336695 5990521.3336004,3556477.5396693 5990555.5678732,3556494.1967249 5990575.8083557,3556509.9911345 5990593.3661401,3556530.451973 5990613.2052009,3556532.186 5990611.693,3556511.285 5990591.611,3556495.709435 5990574.3705527,3556479.384 5990554.338,3556450.784 5990519.951,3556432.4314252 5990498.8690813,3556417.207 5990481.913,3556415.4534255 5990483.7003449</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718711">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557444.24194 5990668.9490361,3557469.5703479 5990667.8455169,3557495.5521983 5990668.914327,3557518.5651411 5990670.6107182,3557543.9728925 5990675.6783518,3557568.3559871 5990682.7359191,3557595.926634 5990692.7984445,3557617.3358072 5990702.6297377,3557637.7720441 5990715.078882,3557658.2208466 5990728.6612211,3557659.888 5990726.361,3557639.3151244 5990712.3441968,3557618.464 5990700.256,3557596.7100832 5990690.8102352,3557569.044 5990680.4,3557544.7546153 5990672.9501374,3557518.915 5990668.345,3557495.7357622 5990667.0734565,3557469.484 5990665.736,3557444.0178143 5990666.7142467,3557423.195 5990669.944,3557423.4928029 5990672.1172791,3557444.24194 5990668.9490361</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187120">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559448.469 5991383.354,3559456.899 5991382.15,3559464.35 5991380.644,3559468.4395634 5991379.1870297,3559468.238206 5991378.2126169,3559464.1727757 5991379.4226179,3559456.6294542 5991380.7570149,3559448.1400124 5991381.8691858,3559432.2376356 5991383.5967164,3559417.7434122 5991385.4144415,3559397.6226998 5991386.9223582,3559378.5587669 5991387.4118117,3559354.8924305 5991387.8090794,3559331.3463691 5991387.5156187,3559304.5011066 5991385.1843977,3559277.034234 5991381.3279601,3559254.025676 5991376.4316901,3559231.387966 5991370.5654828,3559207.9704908 5991364.0738562,3559182.0600414 5991357.519721,3559161.1892168 5991352.0637235,3559136.4625386 5991345.969326,3559122.6981908 5991342.3864152,3559104.3746915 5991338.4401214,3559087.8401794 5991336.2764319,3559071.600955 5991335.1590301,3559053.5173483 5991334.8101148,3559037.7598953 5991334.8730994,3559012.4982244 5991335.5197994,3558988.4716265 5991336.8288978,3558964.9454317 5991338.6861681,3558940.1725915 5991340.0582312,3558926.458487 5991340.0840116,3558909.9111801 5991338.9270491,3558895.8933255 5991337.145702,3558884.7726709 5991335.6410442,3558884.5902586 5991336.8786737,3558895.67 5991338.486,3558909.755 5991340.429,3558926.561 5991341.789,3558940.3419798 5991341.8864006,3558965.127 5991340.623,3558988.6564387 5991338.9247653,3559012.484 5991337.437,3559037.6501156 5991336.5344764,3559053.465 5991336.512,3559071.411 5991336.883,3559087.613908 5991338.0611454,3559103.993 5991340.109,3559122.304 5991344.034,3559136.0515988 5991347.65084,3559160.539 5991354.076,3559181.4801085 5991360.0131679,3559207.362 5991365.957,3559230.9230263 5991371.888145,3559253.505 5991377.951,3559276.7598204 5991382.9975592,3559304.284 5991386.89,3559331.2913381 5991389.309775,3559354.726 5991389.607,3559378.5499035 5991389.2767907,3559397.703 5991388.704,3559417.834 5991387.12,3559432.5054712 5991385.1365972,3559448.469 5991383.354</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308473018"/>
     <simBase:roughnessStyle>Böschungssicherung</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718752">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552895.977 5986986.731,3552882.734 5987005.867,3552884.5732866 5987006.7366824,3552897.8402215 5986987.8791931,3552895.977 5986986.731</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308355818"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 1</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187119">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552948.7683568 5986890.1128092,3552944.54 5986889.142,3552941.951 5986900.131,3552945.998 5986900.8,3552948.7683568 5986890.1128092</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308355818"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 1</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187107">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552929.85 5986975.587,3552939.103 5986960.194,3552940.662 5986947.614,3552926.332 5986972.623,3552911.789 5986996.475,3552899.503 5987013.796,3552887.6304316 5987032.5953537,3552878.887 5987049.481,3552874.155 5987067.957,3552878.276 5987068.154,3552883.893 5987051.512,3552890.963 5987034.351,3552903.232 5987017.606,3552915.917 5986999.508,3552922.987 5986987.028,3552929.85 5986975.587</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308355818"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 1</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187133">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553054.522 5988577.983,3553051.371 5988577.862,3553051.963 5988598.326,3553055.224 5988615.626,3553060.955 5988633.42,3553067.478 5988650.127,3553072.815 5988664.264,3553078.08 5988674.519,3553082.453 5988682.968,3553085.191 5988682.239,3553081.451 5988672.944,3553076.598 5988662.999,3553071.168 5988649.274,3553063.499 5988631.498,3553057.905 5988614.296,3553054.896 5988597.817,3553054.522 5988577.983</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308355818"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 1</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718730">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553338.025 5988823.46,3553339.3156082 5988819.8753578,3553317.9026126 5988813.4433485,3553297.3635568 5988811.4878306,3553272.9104589 5988809.4203675,3553248.1926244 5988808.3786388,3553247.922 5988812.175,3553272.67 5988813.218,3553297.023 5988815.277,3553317.168 5988817.195,3553338.025 5988823.46</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308355818"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 1</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187103">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553077.45 5988609.761,3553076.633 5988597.176,3553076.306 5988580.996,3553071.517 5988579.85,3553071.7 5988597.882,3553073.221 5988610.933,3553078.38 5988626.164,3553086.066 5988643.28,3553091.463 5988656.194,3553096.223 5988669.092,3553098.412 5988676.417,3553101.09 5988675.431,3553099.092 5988668.635,3553095.586 5988655.197,3553090.847 5988641.142,3553084.639 5988625.942,3553077.45 5988609.761</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308355818"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 1</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187136">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553464.451 5989055.301,3553451.5599345 5989054.3285953,3553434.188 5989047.709,3553421.04 5989034.695,3553413.359 5989017.214,3553407.48 5988993.513,3553400.328 5988963.377,3553395.37 5988940.503,3553390.608 5988916.074,3553387.505 5988917.115,3553390.951 5988933.358,3553393.602 5988947.147,3553396.916 5988964.582,3553401.622 5988980.427,3553404.803 5988994.481,3553408.779 5989010.194,3553413.551 5989024.845,3553419.316 5989036.579,3553427.203 5989044.667,3553437.475 5989052.423,3553450.995 5989057.263,3553464.449 5989058.389,3553472.932 5989058.257,3553480.928 5989055.704,3553464.451 5989055.301</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308355818"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 1</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718777">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560129.513 5991545.182,3560130.747 5991547.355,3560134.668 5991545.994,3560133.5823159 5991543.7608968,3560129.513 5991545.182</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718748">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560128.4672743 5991531.9978922,3560127.607 5991530.487,3560127.019 5991528.724,3560121.495 5991531.114,3560123.336 5991531.976,3560123.148 5991533.304,3560128.4672743 5991531.9978922</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718769">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552963.376 5986903.696,3552967.328 5986901.287,3552968.819 5986895.144,3552966.6959563 5986894.2242112,3552963.376 5986903.696</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187131">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553532.598 5988690.022,3553541.986 5988668.336,3553561.564 5988662.997,3553581.347 5988662.532,3553614.261 5988661.039,3553637.102 5988659.56,3553636.204 5988652.058,3553612.945 5988654.629,3553582.225 5988655.946,3553561.677 5988656.776,3553535.413 5988663.739,3553527.331 5988689.086,3553532.598 5988690.022</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187127">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552941.951 5986900.131,3552940.637 5986903.013,3552939.442 5986904.668,3552942.195 5986904.8,3552941.951 5986900.131</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187101">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553147.0903906 5988043.0003236,3553152.916 5988053.637,3553164.8232174 5988077.5651094,3553177.741 5988101.283,3553189.193 5988122.995,3553202.981 5988146.153,3553215.876 5988163.615,3553240.083 5988188.284,3553270.037 5988222.202,3553281.643 5988240.477,3553288.131 5988256.577,3553290.426 5988274.976,3553289.956 5988288.822,3553286.548 5988302.314,3553279.008 5988320.459,3553263.526 5988340.834,3553248.446 5988360.916,3553230.589 5988381.034,3553215.147 5988400.462,3553200.241 5988419.262,3553185.041 5988438.029,3553167.755 5988457.592,3553150.374 5988474.381,3553130.295 5988491.052,3553108.711 5988508.908,3553094.9317032 5988522.7712522,3553084.877 5988537.013,3553078.197 5988552.39,3553073.768 5988564.591,3553071.517 5988579.85,3553076.306 5988580.996,3553076.633 5988597.176,3553077.45 5988609.761,3553084.639 5988625.942,3553090.847 5988641.142,3553095.586 5988655.197,3553099.092 5988668.635,3553101.09 5988675.431,3553102.6128715 5988674.8473452,3553109.283384 5988672.0468201,3553116.028914 5988669.2147999,3553122.369 5988666.553,3553125.465 5988665.576,3553125.379 5988657.683,3553125.292 5988646.667,3553134.308 5988620.263,3553147.968 5988601.543,3553153.382 5988594.302,3553158.259 5988589.813,3553176.203 5988580.055,3553183.03 5988574.981,3553188.492 5988568.735,3553202.843 5988556.371,3553217.708 5988539.866,3553235.653 5988523.082,3553247.983 5988502.578,3553249.348 5988481.305,3553255.548 5988459.655,3553281.587 5988448.921,3553300.312 5988436.236,3553323.621 5988422.965,3553347.341 5988409.248,3553382.448 5988387.001,3553433.062 5988400.173,3553469.857 5988420.35,3553484.876 5988461.214,3553501.944 5988489.55,3553488.605 5988518.155,3553486.557 5988543.037,3553492.992 5988564.671,3553488.311 5988590.724,3553483.923 5988610.63,3553485.753 5988634.814,3553486.338 5988664.966,3553490.08 5988687.424,3553493.591 5988713.185,3553491.967 5988726.028,3553475.646 5988722.334,3553470.416 5988727.659,3553467.198 5988751.663,3553460.761 5988777.425,3553444.565 5988792.869,3553406.613 5988803.115,3553377.16 5988807.799,3553359.191 5988810.588,3553341.369 5988810.531,3553321.214 5988804.416,3553299.043 5988793.878,3553280.578 5988792.447,3553248.851 5988788.804,3553224.729 5988783.865,3553206.784 5988777.035,3553191.18 5988769.619,3553170.381 5988761.617,3553161.311 5988755.762,3553156.045 5988750.2,3553151.949 5988739.075,3553140.831 5988715.949,3553133.224 5988696.921,3553128.221 5988671.237,3553125.295 5988671.823,3553118.0335779 5988674.6200492,3553110.5009041 5988677.5215827,3553104.7386196 5988679.7411746,3553102.746 5988680.857,3553108.435 5988688.764,3553117.57 5988706.833,3553127.11 5988729.26,3553134.5134238 5988745.8335028,3553141.2527238 5988757.6025154,3553147.259139 5988766.6029631,3553158.0688108 5988778.8988922,3553169.5011358 5988788.2056573,3553183.4029861 5988796.3114625,3553199.2961332 5988802.110508,3553222.9778628 5988805.8433809,3553248.1926244 5988808.3786388,3553272.9104589 5988809.4203675,3553297.3635568 5988811.4878306,3553317.9026126 5988813.4433485,3553339.3156082 5988819.8753578,3553360.4942643 5988828.8139406,3553376.57649 5988840.1688201,3553391.6940512 5988859.1693411,3553403.819 5988882.793,3553407.846 5988897.197,3553411.141 5988912.334,3553415.41 5988926.739,3553418.095 5988942.364,3553421.268 5988959.21,3553418.145 5988959.889,3553424.568 5988989.785,3553427.247 5988989.24,3553429.931 5989002.546,3553431.518 5989013.044,3553437.252 5989023.054,3553434.819 5989024.858,3553443.087 5989033.314,3553454.0882072 5989037.8261452,3553465.175 5989038.955,3553472.154 5989035.627,3553486.798 5989035.017,3553492.586 5989033.456,3553497.709 5989032.09,3553498.317 5989035.039,3553505.499 5989032.91,3553515.803 5989028.351,3553523.099 5989024.928,3553521.766 5989022.012,3553537.703 5989012.75,3553549.391 5989005.993,3553561.989 5988998.477,3553571.019 5988993.239,3553575.862 5988990.096,3553579.211 5988988.382,3553579.7456041 5988989.5298482,3553580.966 5988989.771,3553581.9772824 5988990.1127903,3553584.175 5988989.433,3553587.679 5988987.363,3553591.732 5988984.238,3553594.9906618 5988982.3186988,3553604.036 5988976.245,3553618.229 5988967.904,3553632.713 5988960.93,3553643.244 5988957.167,3553652.405 5988955.699,3553669.146 5988954.416,3553685.026439 5988954.6142273,3553685.856 5988951.41,3553707.329 5988955.936,3553723.409 5988960.463,3553732.653 5988963.353,3553748.251 5988967.304,3553765.487 5988971.542,3553786.093 5988974.914,3553810.454 5988977.131,3553831.926 5988977.901,3553857.154 5988977.612,3553883.441 5988975.975,3553883.398 5988978.956,3553908.242 5988975.215,3553928.645 5988972.61,3553964.237 5988965.822,3553973.493 5988961.896,3554000.271 5988958.406,3554026.851 5988952.711,3554054.341 5988947.785,3554083.61 5988944.228,3554083.7686196 5988947.738684,3554106.542 5988946.127,3554128.589 5988946.99,3554156.861 5988949.468,3554181.084 5988955.188,3554182.494 5988952.026,3554206.838 5988959.003,3554228.584 5988967.349,3554252.108 5988977.61,3554276.456 5988988.691,3554301.074 5988997.721,3554319.128 5989003.33,3554343.062 5989007.435,3554366.178 5989009.623,3554390.796 5989008.665,3554413.5 5989007.297,3554436.474 5989003.931,3554457.855 5988999.791,3554458.421 5989003.901,3554482.866 5989000.047,3554505.8529495 5988999.5676531,3554527.35 5989001.725,3554551.68 5989007.535,3554570.038 5989015.487,3554572.348 5989011.349,3554589.593 5989020.834,3554611.269 5989038.917,3554626.675 5989056.321,3554641.151 5989075.85,3554652.705 5989090.065,3554666.25 5989105.609,3554675.281 5989117.699,3554692.543 5989136.035,3554707.683 5989152.641,3554723.886 5989168.583,3554742.479 5989185.588,3554759.213 5989196.216,3554772.494 5989204.32,3554795.337 5989214.417,3554806.492 5989218.535,3554815.195 5989222.414,3554821.697 5989225.169,3554826.752 5989227.245,3554833.435 5989229.775,3554840.183 5989231.735,3554847.43 5989233.687,3554858.673 5989236.847,3554869.079 5989240.751,3554877.069 5989242.517,3554901.598 5989244.84,3554925.198 5989244.84,3554948.628 5989243.629,3554976.37 5989240.774,3555001.891 5989237.761,3555023.767 5989236.017,3555050.557 5989234.591,3555075.128 5989234.274,3555100.9198972 5989235.1653848,3555126.2874458 5989239.2263793,3555151.2898861 5989244.8381324,3555176.0240985 5989252.5321503,3555199.9295078 5989261.7214687,3555222.712 5989272.015,3555246.5362243 5989284.170429,3555269.1979645 5989298.3759916,3555265.8568078 5989304.3608382,3555287.234 5989319.837,3555303.1656854 5989334.5251562,3555322.171 5989353.091,3555338.2777856 5989372.8449185,3555353.569 5989392.504,3555366.7217677 5989414.2729129,3555383.735 5989441.615,3555403.589 5989474.1,3555418.6964578 5989499.2485341,3555433.887 5989522.144,3555446.4376072 5989542.3562597,3555461.11 5989564.365,3555473.783065 5989581.2876114,3555489.583 5989599.467,3555505.2426997 5989616.6103071,3555524.175 5989634.818,3555542.3014619 5989650.6813779,3555564.885 5989667.671,3555584.2870751 5989680.4853604,3555604.472 5989693.401,3555625.2957048 5989705.0960625,3555648.518 5989716.849,3555670.801 5989726.316,3555696.2 5989735.784,3555715.8933714 5989742.0438455,3555736.276 5989747.425,3555766.317 5989755.07,3555793.063 5989761.396,3555812.4358264 5989766.2555093,3555836.148 5989771.148,3555863.949 5989778.002,3555864.114 5989772.514,3555889.474 5989778.368,3555911.786 5989784.222,3555936.415 5989790.929,3555962.629 5989801.662,3555985.185 5989814.712,3556004.084 5989831.787,3556018.487 5989845.412,3556030.76 5989857.855,3556051.332 5989864.26,3556104.817 5989874.437,3556139.72 5989881.222,3556172.111 5989902.832,3556218.314 5989933.992,3556244.931 5989856.095,3556248.714 5989803.025,3556230.869 5989755.584,3556194.856831 5989676.6211981,3556177.0400021 5989657.9490136,3556099.255 5989655.527,3556094.8321022 5989676.5272982,3556026.648 5989695.44,3556018.8395421 5989714.1473972,3555949.5457848 5989742.7069046,3555923.3617269 5989735.7457171,3555898.4365421 5989730.0002542,3555872.1964042 5989724.1504087,3555874.895 5989711.204,3555853.636 5989696.579,3555833.373 5989682.285,3555805.635 5989673.809,3555783.379 5989668.156,3555770.091 5989664.001,3555751.8595174 5989694.9300494,3555732.1278211 5989689.2951016,3555751.323 5989655.856,3555743.843836 5989651.1769634,3555723.0906929 5989644.3407429,3555706.0260137 5989637.806953,3555688.7401476 5989630.2252391,3555674.921 5989612.474,3555661.966 5989602.17,3555644.194 5989594.358,3555636.8536626 5989600.4032781,3555584.544 5989635.828,3555557.568 5989630.849,3555538.186 5989616.695,3555526.662 5989595.466,3555532.9481457 5989552.2218145,3555526.401 5989539.642,3555518.483 5989525.607,3555509.164 5989501.886,3555501.465 5989475.326,3555503.897 5989446.332,3555506.531 5989414.095,3555506.125 5989403.147,3555494.576 5989394.226,3555477.76 5989378.412,3555457.298 5989358.541,3555430.8327819 5989342.7631898,3555398.655 5989322.597,3555367.817 5989308.92,3555337.682 5989299.103,3555320.161 5989279.466,3555303.691 5989247.555,3555290.024 5989215.293,3555300.537 5989201.267,3555309.298 5989184.785,3555272.505 5989170.497,3555236.243 5989175.604,3555204.547 5989168.078,3555170.703 5989165.928,3555143.036 5989155.714,3555129.337 5989149.8,3555111.071 5989150.338,3555079.375 5989150.069,3555054.0784987 5989162.1390771,3555020.817 5989178.022,3554994.552 5989193.869,3554969.473 5989213.128,3554947.602 5989225.093,3554924.856 5989225.384,3554912.025 5989212.545,3554903.568 5989204.082,3554883.739 5989191.826,3554876.8222463 5989174.8947433,3554864.886158 5989172.1009799,3554855.5302706 5989169.054877,3554846.0151806 5989165.3939585,3554837.3122594 5989162.3690344,3554820.944814 5989156.0965346,3554802.8744057 5989147.3412094,3554793.6539704 5989142.0124952,3554781.687578 5989133.7036283,3554763.2967236 5989119.8503468,3554777.882 5989092.61,3554763.885 5989073.934,3554759.219 5989047.962,3554752.22 5989028.703,3554752.22 5989008.568,3554741.722 5988987.849,3554731.224 5988972.675,3554700.605 5988962.753,3554693.606 5988968.589,3554662.986 5988977.052,3554653.363 5988987.557,3554642.592018 5989004.4937604,3554620.8732356 5988985.3217184,3554597.082 5988961.003,3554581.044 5988939.992,3554568.796 5988922.192,3554555.673 5988919.274,3554536.718 5988913.438,3554507.265 5988907.018,3554486.852 5988904.975,3554475.771 5988935.032,3554465.273 5988950.79,3554452.151 5988968.591,3554431.154 5988970.634,3554432.7130632 5988978.8991154,3554410.0425494 5988982.1077309,3554390.5469163 5988982.5064989,3554390.0075893 5988956.5011836,3554378.026 5988923.167,3554360.768 5988895.104,3554347.285 5988894.025,3554333.263 5988893.215,3554318.432 5988894.025,3554303.331 5988890.247,3554290.657 5988894.026,3554280.68 5988883.771,3554263.421 5988881.343,3554221.4888222 5988908.9488745,3554188.5670987 5988927.4650439,3554172.4660073 5988923.5534133,3554151.2279374 5988919.8011871,3554130.1658974 5988917.9434336,3554108.4475086 5988917.5327016,3554081.8398553 5988919.5163746,3554049.4843711 5988923.9754299,3554022.7968978 5988928.5949914,3553996.298566 5988933.0113801,3553969.211865 5988937.3631158,3553942.317 5988920.991,3553922.166 5988928.301,3553901.007 5988936.619,3553878.841 5988941.408,3553854.5276988 5988953.1227529,3553832.371 5988955.03,3553809.926 5988954.132,3553788.828 5988947.396,3553773.341 5988939.086,3553759.425 5988927.364,3553745.958 5988914.563,3553737.204 5988905.579,3553720.146 5988891.431,3553711.874 5988882.715,3553703.12 5988858.01,3553695.267 5988831.956,3553716.911 5988807.477,3553724.768 5988801.862,3553726.115 5988785.915,3553715.115 5988788.835,3553691.546 5988791.979,3553676.731 5988793.327,3553663.265 5988778.505,3553646.88 5988752.454,3553637.003 5988722.807,3553610.3263210002 5988722.3550532,3553575.904 5988721.27,3553543.648 5988721.928,3553518.593 5988722.821,3553496.322 5988725.947,3553497.687 5988712.404,3553496.517 5988685.863,3553493.067 5988665.551,3553492.407 5988634.049,3553489.774 5988611.801,3553491.822 5988591.602,3553498.26 5988565.284,3553490.068 5988541.28,3553492.408 5988519.033,3553505.845 5988489.94,3553488.312 5988459.608,3553473.245 5988416.754,3553434.525 5988397.246,3553382.448 5988380.56,3553343.935 5988405.777,3553321.487 5988420.152,3553302.63 5988401.734,3553280.631 5988381.07,3553292.7532075 5988364.4485652,3553306.491 5988341.089,3553319.152 5988316.829,3553349.008 5988330.081,3553365.912 5988297.968,3553397.44 5988286.472,3553387.354 5988266.947,3553359.288 5988270.676,3553351.177 5988255.148,3553342.845 5988241.985,3553344.161 5988217.413,3553334.513 5988198.546,3553304.52 5988172.66,3553286.102 5988148.966,3553278.82 5988126.41,3553275.577 5988105.966,3553269.437 5988084.027,3553265.491 5988065.159,3553265.052 5988037.955,3553262.859 5988019.088,3553256.7200949 5988000.2203077,3553249.265 5987981.792,3553228.216 5987965.557,3553213.919 5987946.776,3553199.009 5987927.031,3553182.784 5987906.409,3553168.312 5987886.664,3553159.103 5987862.97,3553149.455 5987840.153,3553135.422 5987818.653,3553127.002 5987806.894,3553117.574 5987794.828,3553099.813 5987780.787,3553086.657 5987769.597,3553073.721 5987756.654,3553064.512 5987744.368,3553061.222 5987736.251,3553059.03 5987725.501,3553054.642 5987713.874,3553047.845 5987696.103,3553037.32 5987677.016,3553024.428 5987661.353,3553022.455 5987656.307,3553019.166 5987635.904,3553013.684 5987618.352,3553007.332 5987595.576,3553005.797 5987590.969,3553005.359 5987569.029,3553002.289 5987546.651,3553001.609 5987539.587,3553001.609 5987533.663,3553004.459 5987521.376,3553009.284 5987505.446,3553014.566 5987504.92,3553017.197 5987492.151,3553017.855 5987479.865,3553028.819 5987468.061,3553030.134 5987453.581,3553032.327 5987438.442,3553045.922 5987422.207,3553047.896 5987405.972,3553050.089 5987390.833,3553052.282 5987376.573,3553054.255 5987364.286,3553055.882 5987349.546,3553058.471 5987329.333,3553059.056 5987320.557,3553059.348 5987309.441,3553066.437 5987287.356,3553067.997 5987263.835,3553070.968 5987242.82,3553073.307 5987224.878,3553074.121 5987199.038,3553080.7 5987187.629,3553074.56 5987177.975,3553065.35 5987175.342,3553056.579 5987175.342,3553046.492 5987160.862,3553058.772 5987146.381,3553066.828 5987128.641,3553071.774 5987108.574,3553074.114 5987100.383,3553083.762 5987078.323,3553093.996 5987055.037,3553093.411 5987027.537,3553084.932 5987003.371,3553071.975 5986976.46,3553070.898 5986965.689,3553066.253 5986947.3,3553065.083 5986939.498,3553036.981 5986910.104,3553036.103 5986900.742,3553021.776 5986902.205,3552996.046 5986905.715,3552973.182 5986910.131,3552968.419 5986910.53,3552965.757 5986909.153,3552958.645 5986916.199,3552958.55 5986924.73,3552951.118 5986939.403,3552939.103 5986960.194,3552929.85 5986975.587,3552922.987 5986987.028,3552915.917 5986999.508,3552903.232 5987017.606,3552890.963 5987034.351,3552883.893 5987051.512,3552878.276 5987068.154,3552874.155 5987067.957,3552872.084 5987082.395,3552869.881 5987104.757,3552869.062 5987128.777,3552871.405 5987152.857,3552876.094 5987178.357,3552884.532 5987205.232,3552891.2875961 5987230.1763052,3552894.917 5987255.712,3552895.204 5987278.3,3552891.505 5987303.903,3552884.093 5987329.195,3552876.048 5987349.784,3552868.481 5987374.598,3552863.805 5987397.573,3552862.01 5987423.458,3552858.687 5987447.971,3552856.06 5987473.406,3552856.078 5987487.827,3552856.763 5987497.089,3552859.19 5987507.405,3552862.888 5987515.366,3552871.033 5987526.882,3552884.282 5987539.231,3552896.9892457 5987549.4472261,3552921.382 5987568.846,3552941.634 5987586.03,3552950.441 5987593.552,3552964.54 5987605.15,3552975.142 5987615.116,3552984.718 5987629.82,3552990.128 5987643.757,3552993.405 5987662.339,3552994.034 5987678.194,3552993.686 5987703.969,3552993.197 5987726.457,3552994.104 5987753.028,3552997.0998021 5987770.0648596,3553009.198 5987793.745,3553021.851 5987814.703,3553034.621 5987836.297,3553047.933 5987858.823,3553061.229 5987883.258,3553070.991 5987902.41,3553084.108 5987926.319,3553095.344 5987947.244,3553107.496 5987970.398,3553117.835 5987989.692,3553135.46 5988021.765,3553147.0903906 5988043.0003236</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
       <gml:interior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554740.847 5989034.539,3554728.6 5989028.411,3554711.394 5989020.24,3554699.438 5989011.486,3554723.059 5988982.888,3554729.475 5988999.813,3554741.722 5989016.447,3554746.68 5989031.037,3554740.847 5989034.539</gml:coordinates>
        </gml:LinearRing>
       </gml:interior>
       <gml:interior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554476.937 5988944.078,3554508.14 5988923.068,3554536.135 5988920.441,3554554.215 5988924.527,3554549.549 5988937.075,3554534.094 5988939.409,3554506.682 5988944.078,3554477.5203676 5988962.7542416,3554467.605 5988961.295,3554467.897 5988953.416,3554476.937 5988944.078</gml:coordinates>
        </gml:LinearRing>
       </gml:interior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718790">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553046.312 5987890.203,3553033.833 5987867.571,3553021.059 5987846.463,3553007.747 5987823.446,3552993.264 5987800.18,3552996.723124 5987798.783021,3552985.5519857 5987774.8238548,3552981.4240006 5987754.1216548,3552980.817471 5987726.6864245,3552980.3581298 5987703.9697859,3552979.6397255 5987677.8327233,3552978.9825611 5987663.7829855,3552976.0086883 5987647.3321127,3552971.0866765 5987636.4156217,3552962.3941453 5987624.5678923,3552953.4024998 5987616.1791353,3552940.2512977 5987604.9642733,3552932.4846594 5987597.9371521,3552911.3834485 5987579.8716444,3552888.2164727 5987560.5240196,3552874.5675978 5987549.8475351,3552860.0912903 5987536.0311473,3552851.1602819 5987523.1421551,3552846.5252496 5987512.6334023,3552843.044278 5987500.2283609,3552842.3441677 5987488.4786646,3552843.3589089 5987474.1254678,3552845.1112744 5987447.0655169,3552847.0138124 5987422.5587994,3552849.2419547 5987396.7477689,3552852.7950757 5987371.3223164,3552860.9070961 5987345.680177,3552869.4419329 5987324.6600487,3552876.3563403 5987300.9431423005,3552880.7912453 5987277.4108379,3552877.552 5987277.211,3552877.064 5987256.301,3552873.1730441 5987233.5278853,3552866.672 5987209.049,3552858.435 5987183.305,3552852.382 5987170.396,3552850.219 5987154.814,3552848.2504742 5987129.6326229,3552848.1298969 5987105.3344919,3552850.3261134 5987081.0913446,3552853.7695567 5987061.3418866,3552860.1101134 5987041.3862018,3552854.7286679 5987038.9248625,3552864.0993107 5987019.6734688,3552858.209 5987016.531,3552868.15 5986997.614,3552874.3646086 5987001.2255826,3552887.9040322 5986981.3702515,3552899.1640943 5986963.5112509,3552904.709 5986964.926,3552915.615 5986947.502,3552923.885 5986937.766,3552930.252 5986928.688,3552934.832 5986921.719,3552936.699 5986922.302,3552942.86 5986912.155,3552940.275 5986912.363,3552937.506 5986914.331,3552933.717 5986915.205,3552929.928 5986916.517,3552917.464 5986921.669,3552899.337 5986935.906,3552885.693 5986956.268,3552875.362 5986973.82,3552857.624 5986991.763,3552846.904 5987011.266,3552837.426 5987032.305,3552830.02 5987053.108,3552829.046 5987055.382,3552818.229 5987074.815,3552807.485 5987107.284,3552803.605 5987123.72,3552807.046 5987129.837,3552809.678 5987154.629,3552810.554 5987174.727,3552810.554 5987191.489,3552802.661 5987219.001,3552814.172 5987242.894,3552813.843 5987252.328,3552812.965 5987265.163,3552812.088 5987272.622,3552820.859 5987286.664,3552822.175 5987297.677,3552826.78 5987313.694,3552832.131 5987329.182,3552828.403 5987344.101,3552822.921 5987364.944,3552822.483 5987367.577,3552819.632 5987398.512,3552813.25 5987421.26,3552819.773 5987441.555,3552820.267 5987457.134,3552827.504 5987473.48,3552826.79 5987488.069,3552827.448 5987502.769,3552829.365 5987516.328,3552837.04 5987532.125,3552840.001 5987556.347,3552839.672 5987576.268,3552844.715 5987585.703,3552853.377 5987603.737,3552857.543 5987622.496,3552857.433 5987634.936,3552859.736 5987642.396,3552859.078 5987650.294,3552859.625 5987656.876,3552860.613 5987665.543,3552873.221 5987666.969,3552882.4303046 5987667.4076288,3552897.254 5987668.723,3552906.682 5987680.66,3552906.024 5987698.869,3552906.463 5987715.762,3552909.971 5987729.364,3552917.207 5987750.864,3552927.073 5987771.706,3552930.801 5987788.25,3552933.213 5987797.683,3552942.645 5987817.073,3552953.17 5987835.063,3552963.256 5987850.42,3552972.465 5987866.216,3552981.674 5987882.012,3552989.568 5987894.737,3552999.654 5987915.798,3553007.986 5987940.808,3553016.318 5987965.468,3553004.739 5987995.303,3552992.46 5988035.67,3552983.69 5988073.055,3552987.198 5988088.851,3552980.62 5988116.757,3552992.899 5988133.431,3553009.124 5988151.421,3553021.842 5988168.972,3553038.506 5988189.155,3553045.961 5988212.235,3553060.9578907 5988246.3724866,3553064.466 5988284.546,3553068.413 5988306.485,3553077.183 5988332.02,3553099.109 5988366.245,3553118.404 5988394.766,3553113.409 5988411.786,3553103.674 5988429.295,3553096.877 5988455.621,3553089.861 5988475.103,3553082.713 5988487.561,3553069.338 5988502.391,3553052.017 5988518.187,3553039.08 5988530.911,3553030.0903576 5988554.1662943,3553031.46 5988581.695,3553026.746 5988601.91,3553024.726 5988623.697,3553024.052 5988657.704,3553024.95 5988683.983,3553027.42 5988695.438,3553028.176 5988704.823,3553028.371 5988708.921,3553041.245 5988702.871,3553060.419 5988693.1464348,3553075.5142349 5988686.1667352,3553078.739 5988684.646,3553082.453 5988682.968,3553078.08 5988674.519,3553072.815 5988664.264,3553067.478 5988650.127,3553060.955 5988633.42,3553055.224 5988615.626,3553051.963 5988598.326,3553051.371 5988577.862,3553054.237 5988560.068,3553061.155 5988540.395,3553067.384 5988526.555,3553078.947 5988509.353,3553086.964977 5988492.2527565,3553098.1590087 5988483.151657,3553112.6985677 5988470.5209284,3553132.2865106 5988454.8083376,3553150.0095526 5988439.56708,3553163.0139018 5988423.7592431,3553179.6316788 5988402.1882252,3553193.8646014 5988385.5096144,3553210.6163696 5988364.3752734,3553222.3805047 5988350.9243318,3553232.0497956 5988340.4069256,3553244.5724929 5988325.4985752,3553256.7392405 5988309.6719116,3553263.2252299 5988294.6956322,3553265.1159214 5988286.984082,3553269.966 5988274.803,3553269.463 5988267.177,3553267.872 5988257.959,3553264.102 5988249.747,3553255.557 5988237.094,3553254.832 5988232.081,3553226.703 5988201.661,3553202.927 5988176.656,3553187.778 5988156.183,3553172.945 5988131.409,3553162.081 5988110.249,3553149.2204205 5988086.3236581,3553136.645 5988062.622,3553131.4740613 5988052.9730624,3553119.019 5988029.732,3553102.603 5987998.702,3553091.903 5987976.837,3553080.322 5987955.289,3553069.7 5987934.451,3553072.3528413 5987932.9537134,3553059.1569053 5987908.9167661,3553049.2170144 5987888.8504945,3553046.312 5987890.203</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
       <gml:interior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552846.296 5987250.682,3552845.418 5987239.713,3552840.814 5987215.535,3552851.119 5987212.946,3552857.259 5987237.738,3552858.355 5987249.585,3552859.232 5987265.207,3552846.076 5987265.821,3552846.296 5987250.682</gml:coordinates>
        </gml:LinearRing>
       </gml:interior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171877">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553081.495 5988690.278,3553077.916 5988691.551,3553062.702 5988698.577,3553043.586 5988708.726,3553028.991 5988721.493,3553030.7874768 5988752.8923478,3553026.2978521 5988801.8570435,3553027.6447395 5988824.7671304,3553027.645 5988846.958,3553024.951 5988869.419,3553023.604 5988886.938,3553033.482 5988904.907,3553046.502 5988917.036,3553061.317 5988931.772,3553079.275 5988948.393,3553090.948 5988963.217,3553111.151 5988979.838,3553129.0181567 5988998.6154348,3553153.487 5989022.424,3553178.627 5989047.487,3553198.921 5989069.408,3553229.495 5989102.739,3553258.228 5989132.388,3553273.718 5989149.007,3553288.085 5989171.468,3553312.7771523 5989169.8953913,3553346.449 5989168.548,3553362.611 5989168.145,3553398.753 5989167.92,3553432.218 5989166.66,3553463.867 5989165.671,3553495.066 5989164.863,3553518.014 5989164.267,3553515.151 5989147.859,3553511.637 5989133.614,3553508.194 5989113.656,3553510.357 5989113.187,3553507.494 5989103.16,3553503.4543584 5989091.5685133,3553501.342 5989083.666,3553497.872 5989070.559,3553495.009 5989065.437,3553493.001 5989062.283,3553490.766 5989058.801,3553488.714 5989057.739,3553486.773 5989057.079,3553480.928 5989055.704,3553472.932 5989058.257,3553464.449 5989058.389,3553450.995 5989057.263,3553437.475 5989052.423,3553427.203 5989044.667,3553419.316 5989036.579,3553413.551 5989024.845,3553408.779 5989010.194,3553404.803 5988994.481,3553401.622 5988980.427,3553396.916 5988964.582,3553393.602 5988947.147,3553390.951 5988933.358,3553387.505 5988917.115,3553383.662 5988903.458,3553380.149 5988890.132,3553371.269 5988873.028,3553366.828 5988866.332,3553360.069 5988859.173,3553353.574 5988854.002,3553348.073 5988850.223,3553331.703 5988842.864,3553314.472 5988837.693,3553296.776 5988835.107,3553273.448 5988833.715,3553246.807 5988831.659,3553220.43 5988829.273,3553194.25 5988826.289,3553173.241 5988820.124,3553154.353 5988808.985,3553156.5519249 5988805.8516359,3553142.929 5988794.286,3553129.591 5988779.801,3553123.419 5988769.855,3553115.909 5988756.042,3553107.558 5988736.861,3553098.014 5988715.239,3553090.434 5988697.126,3553087.297 5988687.518,3553085.029 5988688.78,3553081.495 5988690.278</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171870">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559432.5054712 5991385.1365972,3559417.834 5991387.12,3559397.703 5991388.704,3559378.5499035 5991389.2767907,3559354.726 5991389.607,3559331.2913381 5991389.309775,3559304.284 5991386.89,3559276.7598204 5991382.9975592,3559253.505 5991377.951,3559230.9230263 5991371.888145,3559207.362 5991365.957,3559181.4801085 5991360.0131679,3559160.539 5991354.076,3559136.0515988 5991347.65084,3559122.304 5991344.034,3559103.993 5991340.109,3559087.613908 5991338.0611454,3559087.236 5991341.856,3559062.938 5991340.335,3559037.881 5991340.183,3559013.431 5991341.434,3558989.324 5991342.603,3558965.129 5991344.493,3558940.754 5991344.493,3558910.532 5991343.953,3558883.913 5991339.49,3558872.719 5991337.369,3558861.033 5991335.051,3558854.327 5991333.423,3558855.658 5991336.333,3558856.853 5991339.554,3558867.935 5991375.356,3558874.477 5991397.398,3558877.281 5991408.619,3558898.181 5991412.079,3558921.395 5991416.412,3558933.003 5991386.648,3558964.593587 5991369.9038927,3558990.1958207 5991368.8251723,3559015.664 5991370.924,3559038.531 5991377.759,3559060.507 5991383.405,3559080.107 5991391.429,3559102.083 5991399.155,3559124.356 5991406.882,3559140.6615102 5991414.0340361,3559162.667 5991429.466,3559192.2778013 5991427.8813401,3559192.958 5991444.919,3559213.746 5991443.73,3559237.504 5991443.73,3559264.1654549 5991444.5632185,3559283.9607061 5991448.1834003,3559305.807 5991456.212,3559329.268 5991472.259,3559328.6738375 5991451.5147353,3559354.217 5991453.535,3559377.975 5991454.129,3559399.06 5991449.672,3559420.442 5991437.785,3559439.448 5991430.95,3559457.86 5991429.761,3559478.648 5991435.705,3559484.885 5991438.082,3559493.794 5991446.106,3559509.533 5991460.37,3559527.055 5991472.851,3559546.995 5991449.311,3559563.263 5991441.674,3559579.9302207 5991445.1610563,3559589.9050227 5991451.6864306,3559593.79 5991459.764,3559596.803 5991471.822,3559599.213 5991485.489,3559603.029 5991496.341,3559607.648 5991508.4,3559615.481 5991522.267,3559627.999 5991537.488,3559639.454 5991554.768,3559658.032495 5991564.8881913,3559656.38 5991582.998,3559661.338 5991598.739,3559674.333 5991618.757,3559688.694 5991644.762,3559697.929 5991659.82,3559712.12 5991666.321,3559730.585 5991700.881,3559744.947 5991708.751,3559758.967 5991708.067,3559774.565 5991713.205,3559784.5046203 5991739.6838862,3559810.6111463 5991746.5202652,3559837.916 5991757.954,3559857.88 5991764.081,3559880.505 5991772.071,3559876.296 5991802.175,3559871.056 5991860.375,3559884.941 5991858.016,3559978.47 5991843.859,3560001.787 5991837.305,3560011.742 5991796.146,3560016.982 5991767.308,3560023.532 5991738.732,3560029.558 5991711.468,3560042.8335619 5991679.1775113,3560066.229 5991661.394,3560086.14 5991646.188,3560108.671 5991639.11,3560130.416 5991632.031,3560153.96 5991622.159,3560162.649 5991613.275,3560150.457 5991594.28,3560124.728 5991561.627,3560121.904 5991557.362,3560121.303 5991554.76,3560121.86 5991550.389,3560118.007 5991550.019,3560114.4754268 5991551.6093761,3560107.493 5991554.41,3560095.241 5991559.419,3560097.686 5991562.069,3560077.366 5991571.005,3560055.673 5991581.118,3560033.588 5991592.115,3560009.539 5991603.703,3559985.196 5991615.879,3559970.08 5991623.046,3559951.36 5991631.119,3559936.1 5991639.008,3559916.823 5991648.064,3559895.43 5991658.143,3559878.052 5991664.352,3559858.922 5991666.032,3559841.037 5991665.742,3559823.441 5991663.522,3559805.156 5991659.39,3559804.685 5991656.137,3559788.3395256 5991650.0362288,3559774.996 5991642.983,3559759.111 5991633.084,3559747.6057582 5991623.5464019,3559745.484 5991626.026,3559733.856 5991613.782,3559721.002 5991594.957,3559711.745 5991577.814,3559705.242 5991562.662,3559704.815 5991554.414,3559692.7749033 5991529.5985597,3559683.685 5991509.889,3559674.2556232 5991490.9130633,3559668.722 5991477.984,3559658.903 5991461.118,3559650.9658234 5991449.982329,3559643.031 5991438.892,3559630.336 5991424.407,3559618.6306983 5991414.3171106,3559607.522 5991405.952,3559587.983 5991394.74,3559570.4859486 5991387.106547,3559558.823 5991382.734,3559543.053 5991378.369,3559529.0022701 5991376.2024908,3559513.123 5991374.995,3559496.414 5991375.221,3559484.672 5991375.899,3559479.1827714 5991376.2012207,3559476.995 5991376.726,3559474.662 5991377.554,3559472.6085919 5991377.8876823,3559470.372 5991378.232,3559468.4395634 5991379.1870297,3559464.35 5991380.644,3559456.899 5991382.15,3559448.469 5991383.354,3559432.5054712 5991385.1365972</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718734">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558041.502 5991046.417,3558020.4640196 5991037.480467,3557998.578 5991026.084,3557976.2425753 5991012.8271274,3557955.846 5991000.769,3557931.9304538 5990985.6314146,3557920.721 5990977.946,3557903.711 5990965.496,3557889.5211383 5990954.1224878,3557873.74 5990939.917,3557856.6423993 5990923.8714391,3557843.222 5990911.564,3557821.836 5990892.165,3557799.001 5990871.498,3557781.5544532 5990855.2999419,3557767.954 5990842.325,3557746.2 5990821.969,3557722.482 5990800.348,3557707.1125708 5990786.7057897,3557695.676 5990775.779,3557674.063 5990755.703,3557651.748 5990737.592,3557632.6125536 5990724.2226591,3557612.59 5990712.615,3557591.6376436 5990703.6828904,3557565.195 5990693.468,3557540.8994384 5990686.4047139,3557516.991 5990680.805,3557494.5664109 5990678.8002993,3557470.023 5990678.904,3557445.3932037 5990680.4284507,3557425.029 5990683.328,3557396.327 5990688.616,3557369.999 5990693.689,3557348.0837289 5990697.7922658,3557327.705 5990701.457,3557301.7 5990706.638,3557271.92 5990712.574,3557249.0703002 5990716.4288547,3557225.76 5990720.604,3557199.8016272 5990724.2848537,3557173.308 5990725.805,3557148.729671 5990725.3653836,3557131.255 5990722.885,3557110.913 5990718.688,3557097.9572976 5990715.8639502,3557090.805 5990713.827,3557082.775 5990711.953,3557079.3857002 5990711.0763237,3557076.885 5990710.145,3557074.61 5990708.672,3557072.1337864 5990707.6556068,3557069.458 5990707.267,3557065.777 5990707.467,3557062.566957 5990707.0027708,3557059.219 5990705.459,3557055.3033628 5990703.7321508,3557046.063 5990700.458,3557027.187 5990694.654,3557009.7687794 5990689.3130864,3557008.499 5990692.964,3556981.571 5990684.475,3556954.027 5990677.341,3556932.386 5990673.651,3556906.195 5990671.929,3556906.5610072 5990667.5706681,3556892.45 5990667.485,3556874.399 5990668.441,3556858.3196069 5990670.310911,3556838.485 5990672.771,3556810.315 5990676.889,3556781.711 5990681.008,3556757.7712062 5990684.4689722,3556733.095 5990687.476,3556707.6690472 5990689.4111943,3556708.105 5990692.47,3556682.406 5990691.978,3556656.83 5990689.395,3556631.161 5990683.915,3556606.039 5990675.575,3556583.332 5990666.685,3556560.734 5990655.272,3556540.768 5990641.445,3556519.267 5990622.9,3556521.461 5990621.046,3556503.173 5990602.615,3556486.0837575 5990583.5195818,3556467.834 5990562.04,3556439.371 5990528.479,3556422.1159147 5990508.3091307,3556407.341 5990491.969,3556390.3513207 5990477.0778833,3556371.256 5990462.722,3556353.982 5990453.066,3556347.0422141 5990449.6903021,3556344.36 5990447.249,3556340.8494038 5990445.0880639,3556336.49 5990443.313,3556335.398 5990444.261,3556329.847 5990441.802,3556327.6177195 5990439.140005,3556320.953 5990435.945,3556304.508 5990428.477,3556276.259 5990415.459,3556276.933 5990419.337,3556254.949 5990409.799,3556233.628 5990399.863,3556212.969 5990389.663,3556186.35 5990377.608,3556165.559 5990368.334,3556140.132 5990355.352,3556116.689 5990341.842,3556097.885 5990327.401,3556079.345 5990309.782,3556062.261 5990289.911,3556046.103 5990267.92,3556034.714 5990246.989,3556023.59 5990224.733,3556015.114 5990204.332,3556009.288 5990183.666,3556005.447 5990164.325,3556003.328 5990144.056,3556002.269 5990124.847,3556002.269 5990107.891,3556004.388 5990088.55,3556007.831 5990070.401,3556011.539 5990055.432,3556015.244 5990041.356,3556017.727 5990033.336,3556019.89 5990025.62,3556021.3812649 5990025.645117,3556022.769 5990021.931,3556023.4235428 5990018.1713265,3556023.086 5990015.026,3556023.6356052 5990011.5759932,3556026.6117979 5990000.0957732,3556030.012 5989986.98,3556033.8554319 5989966.6396162,3556034.217 5989949.128,3556032.1801046 5989929.4194521,3556028.01 5989909.074,3556020.7956765 5989890.2944431,3556014.546 5989877.115,3556003.61 5989859.981,3555991.340047 5989845.919393,3555974.228 5989831.249,3555953.8463054 5989818.4998694,3555931.67 5989809.107,3555907.3766417 5989803.1670828,3555887.006 5989798.035,3555861.314 5989791.973,3555833.249 5989785.383,3555809.4696159 5989779.585473,3555790.033 5989775.103,3555762.891 5989768.645,3555733.773 5989761.528,3555711.6987816 5989756.2213503,3555691.288 5989749.925,3555665.77 5989739.978,3555663.986 5989743.365,3555641.948 5989733.437,3555617.051 5989720.816,3555596.3038259 5989709.4954923,3555576.339 5989697.592,3555553.500242 5989681.8682606,3555531.254 5989664.44,3555512.0035628 5989648.0231561,3555493.067 5989629.773,3555476.5186368 5989611.9709246,3555460.3192472 5989592.9496784,3555446.132 5989574.914,3555430.9511605 5989552.270477,3555417.3662901 5989530.5941193,3555404.329 5989508.318,3555389.44 5989483.539,3555378.254 5989464.313,3555371.576 5989448.543,3555354.9094196 5989422.2274408,3555341.102 5989400.664,3555327.4139847 5989382.4514701,3555311.859 5989363.714,3555293.8706282 5989345.1302643,3555278.154 5989330.768,3555257.9244877 5989315.4017766,3555236.137 5989301.054,3555214.6183676 5989288.8351223,3555192.888 5989278.884,3555170.3682524 5989269.4432862,3555147.024 5989262.564,3555122.8629051 5989256.9945386,3555097.862 5989253.307,3555074.5557197 5989251.3931572,3555050.297 5989252.259,3555024.7910335 5989253.1668942,3555004.267 5989254.696,3554978.045 5989256.882,3554948.495 5989258.444,3554925.2232033 5989258.5682126,3554909.302 5989258.24,3554889.768 5989256.631,3554875.5380414 5989254.5778292,3554866.631 5989253.508,3554855.003 5989251.182,3554844.4130246 5989248.797748,3554837.427 5989247.112,3554829.6761589 5989245.1296486,3554825.969 5989244.012,3554821.536 5989242.494,3554817.8928778 5989241.2752978,3554815.568 5989241.086,3554812.855 5989240.986,3554809.1110614 5989239.9334982,3554803.769 5989237.885,3554795.921 5989234.274,3554780.695 5989226.58,3554764.721 5989218.556,3554751.658 5989211.195,3554734.553 5989199.488,3554722.305 5989190.783,3554706.137 5989177.751,3554693.934 5989167.004,3554678.197 5989150.116,3554661.746 5989130.309,3554651.2 5989117.457,3554638.538 5989102.568,3554626.87 5989088.095,3554611.331 5989069.567,3554598.113 5989054.575,3554588.883 5989046.323,3554576.484 5989038.007,3554562.457 5989031.005,3554546.19 5989023.247,3554525.784 5989016.653,3554505.2100045 5989014.3086383,3554484.058 5989014.838,3554460.363 5989018.378,3554440.655 5989021.995,3554416.393 5989027.056,3554391.289 5989029.152,3554365.265 5989029.603,3554341.685 5989027.961,3554315.066 5989022.539,3554283.734 5989012.931,3554250.585 5988999.334,3554220.405 5988986.191,3554201.22 5988978.103,3554177.751 5988970.577,3554154.954 5988965.698,3554127.969 5988962.348,3554108.212 5988961.881,3554085.9570685 5988962.8217465,3554058.49 5988966.519,3554030.386 5988971.514,3554003.538 5988976.609,3553967.367 5988981.654,3553931.537 5988987.434,3553910.502 5988990.177,3553886.08 5988993.635,3553859.376 5988995.396,3553831.736 5988995.92,3553809.64 5988995.746,3553783.797 5988992.822,3553756.079 5988988.266,3553728.418 5988982.746,3553704.605 5988973.828,3553683.7633783 5988969.0530885,3553669.856 5988968.169,3553654.61 5988968.522,3553646.526 5988970.919,3553638.278 5988974.677,3553625.148 5988981.744,3553613.261 5988989.021,3553604.1375568 5988995.182173,3553600.43 5988997.12,3553595.026 5988999.654,3553591.395 5989000.963,3553589.8290528 5989002.2088617,3553589.41 5989003.877,3553588.4438275 5989004.9267558,3553584.586 5989006.912,3553578.329 5989010.114,3553569.891 5989015.281,3553559.6452275 5989021.9630886,3553547.016 5989029.063,3553531.256 5989038.886,3553520.657 5989043.539,3553510.32 5989048.58,3553503.498 5989051.862,3553502.3966062 5989052.2411174,3553502.809 5989055.518,3553505.023 5989058.653,3553505.072 5989061.358,3553507.068 5989066.566,3553510.538 5989079.586,3553512.555 5989089.213,3553516.199 5989100.615,3553519.597 5989111.494,3553522.569 5989111.06,3553527.946 5989110.085,3553544.422 5989108.738,3553559.685 5989108.289,3553575.396 5989106.941,3553594.924 5989106.268,3553610.187 5989105.819,3553625.452 5989105.819,3553632.186 5989105.145,3553639.368 5989105.145,3553647.897 5989104.921,3553656.427 5989104.471,3553663.834 5989104.471,3553672.587 5989104.471,3553674.6537517 5989092.6376274,3553676.9360964 5989079.7221423,3553678.9512424 5989065.3559573,3553679.836 5989049.942,3553696.221 5989052.188,3553711.932 5989057.576,3553728.677 5989059.636,3553754.264 5989072.212,3553776.347 5989094.717,3553802.607 5989110.213,3553816.074 5989117.848,3553830.128 5989113.085,3553847.959 5989107.631,3553858.2992824 5989087.3224427,3553857.8911965 5989072.8020144,3553893.506 5989055.126,3553917.88 5989051.306,3553941.079 5989047.192,3553959.873 5989044.547,3553983.9416332 5989027.216895,3554011.558 5989030.442,3554041.217 5989038.964,3554054.726 5989038.963,3554066.179 5989033.967,3554093.195 5989028.678,3554110.077 5989031.084,3554126.612 5989035.91,3554138.324 5989036.369,3554151.643 5989034.301,3554161.059 5989032.922,3554178.742 5989034.761,3554194.358 5989045.562,3554220.997 5989046.021,3554253.147 5989039.82,3554285.4011825 5989041.5271241,3554305.966 5989058.434,3554336.049 5989065.328,3554364.755 5989064.639,3554392.083 5989068.316,3554391.9788963 5989055.1558887,3554419.509631 5989052.3803512,3554443.2257687 5989049.4090881,3554464.7748174 5989045.6619099,3554487.1436692 5989067.8841782,3554503.9162963 5989066.8992234,3554516.3735015998 5989068.2674459,3554526.7163868 5989071.2936222,3554540.1667505 5989076.8007661,3554552.9699663 5989084.0898918,3554561.4155614 5989091.4110067,3554571.8585072 5989103.2596824,3554587.4128486 5989121.6782301,3554603.724 5989131.662,3554623.188 5989137.904,3554637.162 5989149.89,3554653.882 5989168.868,3554669.852 5989188.845,3554686.573 5989214.627,3554706.985 5989233.741,3554722.154 5989250.606,3554731.505 5989284.94,3554751.45 5989308.452,3554763.198 5989314.466,3554779.59 5989314.466,3554793.251 5989313.92,3554808.277 5989313.099,3554826.855 5989314.466,3554846.253 5989317.2,3554866.197 5989320.754,3554894.065 5989324.309,3554926.303 5989331.963,3554952.257 5989336.338,3554980.671 5989341.259,3555006.079 5989334.971,3555027.5833425 5989308.4833611,3555052.252 5989327.589,3555073.289 5989333.058,3555085.311 5989337.159,3555105.802 5989345.087,3555120.555 5989355.75,3555135.582 5989365.592,3555155.2525934 5989363.6780872,3555177.929 5989357.663,3555200.332 5989360.944,3555216.998 5989376.801,3555224.246 5989385.705,3555242.111 5989398.324,3555260.851 5989407.613,3555277.84 5989419.531,3555299.7935943 5989429.025209,3555312.5484201 5989448.6198138,3555329.507 5989466.851,3555352.338 5989480.28,3555375.131506 5989495.1814407,3555389.5397637 5989517.8361544,3555387.05 5989548.733,3555382.711 5989580.405,3555369.174 5989632.233,3555371.751 5989664.148,3555389.47 5989687.038,3555403.645 5989674.142,3555448.8808328 5989671.7186105,3555470.1934806 5989693.2845531,3555493.166305 5989711.7761502,3555516.44706 5989730.4114475995,3555543.5457703 5989749.0958551,3555566.1770702 5989762.1847286,3555587.6916464 5989774.0650996,3555614.0092714 5989788.6258617,3555638.3897909 5989799.8882345,3555664.1410223 5989810.0899529,3555700.0200921 5989797.7987727,3555723.9917135 5989804.106717,3555747.6349998 5989809.6902259,3555774.956 5989807.098,3555802.471 5989806.795,3555825.451 5989808.005,3555853.080355 5989808.7979038,3555879.1962439 5989814.9725868,3555903.0583391 5989820.376752,3555926.397898 5989826.5845774,3555945.7421598 5989834.3243433,3555960.304 5989853.088,3555967.259 5989866.704,3555969.375 5989884.556,3555978.45 5989894.264,3555977.5893785 5989906.5398493,3555983.5939299 5989922.4585679,3555986.6104427 5989936.3170398995,3555987.9237543 5989951.544715,3555987.7774687 5989963.0961502,3555986.2201039 5989974.0700214,3555983.0344765 5989986.2145786,3555979.6337585 5990000.6458437,3555977.2487376 5990009.8469853,3555976.0808856 5990020.9777213,3555968.1687983 5990028.4848274,3555958.732 5990041.136,3555943.59 5990056.289,3555919.13 5990056.755,3555912.141 5990053.258,3555899.2607131 5990075.493637,3555893.272 5990100.346,3555889.778 5990126.455,3555886.749 5990149.533,3555891.349 5990185.357,3555897.652 5990208.117,3555906.421 5990238.556,3555935.9672616 5990263.9597961,3555948.8054294 5990290.6107193,3555965.0677039 5990318.5285639,3555986.1637665 5990349.999516,3556008.0237059 5990374.2413801,3556004.15 5990436.227,3555998.069 5990470.528,3556026.817 5990484.358,3556063.858 5990494.87,3556100.07 5990505.38,3556125.501 5990513.126,3556151.487 5990518.103,3556177.747 5990516.443,3556205.943 5990511.464,3556233.862 5990514.507,3556257.911 5990517.273,3556273.9022788 5990518.8697468,3556293.221 5990518.402,3556298.755 5990516.187,3556308.038 5990493.674,3556317.831 5990469.065,3556324.978 5990451.707,3556328.974 5990443.6,3556334.472 5990446.006,3556331.404 5990454.064,3556324.455 5990472.147,3556314.248 5990496.073,3556308.04 5990511.144,3556312.036 5990508.622,3556315.059 5990508.684,3556327.161 5990514.82,3556336.359 5990522.247,3556345.072 5990530.643,3556356.045 5990542.43,3556366.05 5990557.283,3556375.893 5990577.789,3556387.026 5990600.391,3556391.222 5990612.5,3556401.065 5990622.511,3556414.619 5990643.34,3556433.822 5990667.236,3556447.215 5990679.668,3556462.06 5990684.674,3556486.749 5990711.798,3556532.8011817 5990702.0820564,3556559.5061623 5990716.2790893,3556586.5341614 5990727.6754919,3556623.609373 5990710.7481848,3556652.6337679 5990716.8043258,3556679.1481216 5990719.666712,3556711.6295369 5990730.2455495,3556737.0479874 5990727.8063284,3556763.1463291 5990724.5933731,3556788.9286641 5990720.9237501,3556801.28 5990710.669,3556809.641 5990694.635,3556831.935 5990686.095,3556858.41 5990685.746,3556882.446 5990697.249,3556903.341 5990708.92,3556925.9101214 5990717.7030743,3556943.306 5990728.75,3556963.103 5990744.253,3556986.86 5990764.58,3557012.166 5990771.643,3557030.242 5990771.126,3557047.285 5990768.37,3557058.475 5990766.647,3557086.0317408 5990762.1370989,3557115.7278448 5990769.8852327,3557145.757 5990778.533,3557152.47 5990784.218,3557176.399 5990784.218,3557206.182 5990781.289,3557230.627 5990775.777,3557259.111 5990771.993,3557269.781 5990770.081,3557283.157 5990760.68,3557307.2 5990756.699,3557330.767 5990751.919,3557353.7069279 5990732.426179,3557375.673 5990749.848,3557379.495 5990749.529,3557404.712 5990743.89,3557427.336 5990742.355,3557452.615 5990742.494,3557470.353 5990745.01,3557490.744 5990745.151,3557510.716 5990745.571,3557526.358 5990746.828,3557550.799 5990746.968,3557575.798 5990747.806,3557593.72 5990751.747,3557610.922 5990757.832,3557628.6930047 5990767.7727098,3557646.1537976 5990781.5252316,3557663.5180481 5990796.6694528,3557679.903 5990815.902,3557691.371 5990844.242,3557696.757 5990857.109,3557709.094 5990861.108,3557731.161 5990870.495,3557750.275 5990889.099,3557763.306 5990897.445,3557773.732 5990894.141,3557778.945 5990880.58,3557789.544 5990885.448,3557781.915 5990909.526,3557784.567 5990922.793,3557797.605 5990940.041,3557823.903 5990959.279,3557840.698 5990971.22,3557861.028 5990987.14,3557878.265 5991008.589,3557903.899 5991028.489,3557927.324 5991042.641,3557951.853 5991054.582,3557977.708 5991062.321,3558004.1793601 5991068.0219935,3558029.2703741 5991078.4525591,3558051.3986275 5991088.6117209,3558069.486 5991118.111,3558093.721 5991129.95,3558110.631 5991136.614,3558130.963 5991142.585,3558149.528 5991149.44,3558168.092 5991156.075,3558166.324 5991162.488,3558183.563 5991167.132,3558196.16 5991171.776,3558208.094 5991177.305,3558224.448 5991172.44,3558235.941 5991177.747,3558244.118 5991181.507,3558257.157 5991186.151,3558280.805 5991193.891,3558311.523 5991204.063,3558337.602 5991212.909,3558365.669 5991225.735,3558393.737 5991228.831,3558414.29 5991223.745,3558438.38 5991219.543,3558463.7512871 5991211.471398,3558485.5091071 5991209.0984921,3558507.6720933 5991206.4540162,3558526.0827819 5991204.8294825,3558546.9175231 5991203.8174571,3558563.8731555 5991204.9044839,3558582.550554 5991207.8321327,3558595.0546404 5991211.7286633,3558608.4768475 5991217.3299311,3558621.6588754 5991223.8818853,3558634.0738275 5991231.6075441,3558639.756 5991243.975,3558629.782 5991276.873,3558624.795 5991289.625,3558642.157 5991304.78,3558662.74 5991318.345,3558692.463 5991329.539,3558714.1421728 5991342.32651,3558732.706 5991362.288,3558760.091 5991378.16,3558783.3 5991388.352,3558817.364 5991393.866,3558854.266 5991403.724,3558862.995 5991405.866,3558858.321 5991390.771,3558853.381 5991373.539,3558841.498 5991336.402,3558838.596 5991332.487,3558839.188 5991329.873,3558837.6553601 5991326.6766544,3558836.87 5991329.232,3558827.896 5991326.223,3558814.413 5991320.568,3558795.414 5991311.25,3558796.8270747 5991308.3699948,3558779.985 5991298.432,3558761.46 5991285.178,3558745.5401336 5991272.1290803,3558727.404 5991255.78,3558707.6230474 5991235.2489339,3558699.036 5991226.48,3558686.262 5991213.205,3558675.1385306 5991202.4390746,3558660.961 5991191.373,3558645.2535219 5991181.5849138,3558620.283 5991168.67,3558593.4846919 5991161.0075436,3558571.112 5991156.649,3558547.3088279 5991155.623354,3558524.811 5991156.497,3558502.0348268 5991159.1679913,3558477.748 5991162.001,3558453.451 5991164.293,3558421.514 5991163.988,3558399.4934184 5991162.6339522,3558385.426 5991160.847,3558365.999 5991157.536,3558349.9568109 5991154.68999,3558333.85 5991151.348,3558315.12 5991146.119,3558299.7681818 5991141.1319991,3558289.085 5991137.377,3558277.7859574 5991133.5569607,3558272.338 5991131.764,3558265.346 5991128.224,3558260.3532842 5991126.2152521,3558257.232 5991125.633,3558252.8414426 5991125.522843,3558246.873 5991123.215,3558238.068 5991119.761,3558230.1237895 5991116.6555173,3558220.112 5991113.285,3558206.2072527 5991108.3615099,3558188.39 5991101.468,3558163.326 5991092.85,3558135.286 5991083.291,3558113.5664332 5991075.392716,3558090.918 5991066.369,3558060.554 5991054.755,3558041.502 5991046.417</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187123">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556342.218 5990427.868,3556336.852 5990425.541,3556340.404 5990416.771,3556350.01 5990393.073,3556362.432 5990362.603,3556368.644 5990344.443,3556363.163 5990343.102,3556356.342 5990341.883,3556338.438 5990341.395,3556314.5952134 5990340.5843423,3556298.758 5990313.359,3556291.176 5990299.893,3556277.53 5990296.1,3556250.237 5990297.807,3556229.388 5990286.804,3556210.434 5990278.648,3556160.734 5990262.222,3556166.986 5990280.527,3556188.751 5990283.771,3556183.657 5990299.991,3556170.459 5990299.527,3556166.986 5990280.527,3556152.861 5990286.783,3556149.157 5990265.466,3556147.304 5990258.747,3556135.727 5990242.527,3556122.297 5990245.308,3556119.518 5990232.1,3556115.582 5990217.966,3556128.549 5990215.649,3556123.145 5990199.912,3556108.172 5990197.344,3556121.364 5990173.451,3556102.883 5990164.915,3556105.727 5990159.225,3556124.41 5990167.76,3556141.469 5990152.518,3556115.272 5990137.276,3556119.537 5990124.88,3556147.359 5990133.212,3556122.38 5990110.247,3556108.57 5990101.305,3556119.537 5990096.834,3556124.208 5990090.331,3556133.144 5990095.615,3556157.107 5990082.202,3556143.094 5990062.489,3556163.403 5990056.596,3556177.618 5990046.638,3556206.05 5990027.736,3556209.529716 5990011.6787423,3556213.6037905 5989985.6098704,3556215.9000136 5989946.7644625,3556215.599 5989941.962,3556169.0858957 5989907.0595019,3556137.711 5989885.996,3556105.57 5989879.463,3556053.215 5989870.542,3556039.276 5989866.374,3556033.46 5989867.724,3556028.684 5989868.244,3556035.954 5989883.826,3556046.338 5989904.706,3556051.915 5989926.165,3556053.678 5989948.605,3556053.197 5989968.641,3556050.152 5989989.638,3556045.323 5990001.536,3556040.2422435 5990003.7725124,3556037.2950981 5990014.6174223,3556036.177 5990018.439,3556034.883852 5990021.1931169,3556034.273 5990024.471,3556033.8092736 5990027.581785,3556036.421 5990028.254,3556034.46 5990038.115,3556032.807 5990044.73,3556030.541 5990059.125,3556028.093 5990073.918,3556027.113 5990091.191,3556026.012 5990108.829,3556026.18 5990124.241,3556026.51 5990142.204,3556029.815 5990160.498,3556033.341 5990178.021,3556038.96 5990196.535,3556045.24 5990215.27,3556055.045 5990236.54,3556067.055 5990255.054,3556081.708 5990274.891,3556096.471 5990292.523,3556112.227 5990308.503,3556128.973 5990321.838,3556141.642 5990331.095,3556157.837 5990340.794,3556173.812 5990348.729,3556194.195 5990358.097,3556219.974 5990370.44,3556239.805 5990379.918,3556262.281 5990391.16,3556284.315 5990401.078,3556281.203 5990404.055,3556308.846 5990417.174,3556326.3 5990425.349,3556332.154 5990428.403,3556332.769 5990426.088,3556335.689 5990428.424,3556341.055 5990430.803,3556342.218 5990427.868</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
       <gml:interior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556095.668 5990235.808,3556100.762 5990223.527,3556116.045 5990233.722,3556106.783 5990242.991,3556095.668 5990235.808</gml:coordinates>
        </gml:LinearRing>
       </gml:interior>
       <gml:interior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556080.386 5990197.344,3556066.493 5990204.99,3556058.852 5990188.307,3556070.661 5990181.356,3556080.386 5990197.344</gml:coordinates>
        </gml:LinearRing>
       </gml:interior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187112">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556342.218 5990427.868,3556345.868 5990429.516,3556348.145 5990429.921,3556354.382 5990432.269,3556361.282 5990437.834,3556370.935 5990443.259,3556383.476 5990450.725,3556400.99 5990462.795,3556419.938 5990478.603,3556435.873 5990495.847,3556453.256 5990516.424,3556466.294 5990532.509,3556482.373 5990552.651,3556498.823 5990571.807,3556514.246 5990589.204,3556534.481 5990608.789,3556553.184 5990624.107,3556571.341 5990636.033,3556592.342 5990647.303,3556613.124 5990655.071,3556637.078 5990663.058,3556660.704 5990667.982,3556684.22 5990670.608,3556707.197 5990671.909,3556707.3661926 5990675.5785093,3556731.733 5990673.434,3556756.4482292 5990669.201184,3556779.328 5990665.834,3556809.123 5990661.499,3556836.751 5990657.814,3556856.6378706 5990655.0865144,3556873.323 5990653.135,3556892.45 5990652.537,3556908.509853 5990652.7462186,3556908.1516007 5990649.7618382,3556934.926 5990651.463,3556959.256 5990656.033,3556987.198 5990662.834,3557015.566 5990670.805,3557040.427 5990680.052,3557061.571 5990686.852,3557068.29 5990688.631,3557074.894 5990692.049,3557079.817 5990694.135,3557084.046 5990694.889,3557095.249 5990698.411,3557102.096 5990700.368,3557100.8143628 5990703.2412507,3557115.2 5990706.369,3557133.991 5990709.38,3557149.3484882 5990711.006712,3557172.852 5990711.844,3557198.3693059 5990710.4196391,3557222.658 5990706.825,3557246.8012323 5990702.8572325,3557268.899 5990698.651,3557299.327 5990692.822,3557326.086 5990688.073,3557345.7099107 5990684.8613496,3557367.518 5990680.413,3557394.169 5990675.34,3557423.195 5990669.944,3557444.0178143 5990666.7142467,3557469.484 5990665.736,3557495.7357622 5990667.0734565,3557518.915 5990668.345,3557544.7546153 5990672.9501374,3557569.044 5990680.4,3557596.7100832 5990690.8102352,3557598.069 5990687.282,3557620.79 5990697.354,3557641.215 5990709.794,3557662.276 5990723.96,3557684.332 5990742.213,3557704.757 5990761.01,3557720.28 5990774.449,3557736.256 5990789.432,3557757.861 5990809.409,3557777.741 5990828.57,3557792.58 5990841.784,3557810.838 5990859.173,3557833.441 5990879.394,3557854.482 5990898.787,3557866.977 5990910.186,3557884.711 5990926.455,3557900.239 5990940.977,3557913.93 5990952.649,3557928.355 5990963.128,3557941.127 5990971.584,3557963.087 5990985.555,3557984.175 5990997.382,3558005.94 5991009.398,3558027.824 5991019.867,3558049.232 5991028.909,3558068.737 5991038.306,3558065.763 5991041.057,3558095.532 5991053.266,3558117.4302457 5991061.9166886,3558139.672 5991070.128,3558167.869 5991080.313,3558193.09 5991089.246,3558210.3687858 5991095.1598961,3558225.637 5991100.591,3558234.9356246 5991103.7877713,3558242.557 5991106.204,3558251.534 5991109.14,3558257.5858895 5991110.9433901,3558260.944 5991113.544,3558263.4929514 5991115.1417436,3558268.713 5991116.652,3558275.878 5991118.466,3558282.0733138 5991120.4230187,3558282.933 5991118.153,3558294.934 5991122.208,3558304.854 5991125.578,3558329.571 5991133.06,3558353.081 5991139.503,3558377.523 5991143.889,3558401.308 5991146.301,3558420.49 5991147.507,3558438.027 5991147.945,3558459.62 5991146.849,3558478.583 5991144.218,3558499.956 5991142.135,3558523.648 5991139.222,3558546.73 5991137.931,3558572.959 5991139.465,3558596.767 5991143.582,3558617.266 5991149.638,3558637.846 5991158.114,3558653.987 5991166.672,3558652.4671418 5991169.0966084,3558669.395 5991180.455,3558684.1604407 5991192.4677683,3558695.936 5991203.28,3558709.454 5991216.555,3558717.5443405 5991225.4664786,3558737.813 5991245.55,3558754.9486654 5991261.9326395,3558769.189 5991274.869,3558787.346 5991287.509,3558803.0025577 5991296.4530534,3558819.611 5991304.937,3558832.861 5991310.951,3558841.4577541 5991313.8504046,3558844.515 5991315.002,3558848.195 5991316.229,3558852.857 5991318.07,3558857.887 5991319.665,3558864.389 5991321.383,3558875.799 5991323.47,3558886.3153752 5991325.1741218995,3558897.613 5991326.825,3558911.018 5991328.283,3558925.784 5991328.866,3558939.1303364 5991328.8094129,3558963.961 5991328.185,3558987.587532 5991326.8028017,3558987.755 5991324.175,3559012.788 5991321.949,3559038.476 5991321.378,3559063.72 5991321.874,3559088.604 5991322.785,3559115.815 5991327.542,3559139.081 5991333.006,3559164.573 5991338.975,3559185.209 5991344.338,3559211.648 5991351.686,3559235.357 5991358.019,3559257.701 5991363.234,3559279.3 5991367.705,3559295.561 5991370.56,3559313.312 5991372.546,3559331.684 5991373.54,3559355.641 5991373.664,3559378.607 5991373.04,3559396.234 5991372.295,3559415.598 5991370.929,3559429.749 5991369.563,3559430.362786 5991372.8175503,3559445.533 5991370.103,3559454.189 5991368.145,3559462.318 5991366.64,3559465.765457 5991366.2464365,3559468.264 5991366.261,3559470.4880487 5991366.0094816,3559470.231 5991363.693,3559475.67 5991363.089,3559482.367 5991362.434,3559491.381 5991361.779,3559507.898 5991361.125,3559524.113 5991361.981,3559531.969 5991362.384,3559554.393 5991366.952,3559575.35 5991373.519,3559593.951 5991382.077,3559613.83 5991393.635,3559626.962 5991402.464,3559640.595 5991414.454,3559654.445 5991429.888,3559663.198 5991441.089,3559672.527 5991454.441,3559681.64 5991470.521,3559688.742 5991484.951,3559698.502 5991504.333,3559706.481 5991523.534,3559712.781 5991537.213,3559719.526 5991552.144,3559727.634 5991570.02,3559738.108 5991585.884,3559744.638 5991595.862,3559757.555 5991611.942,3559775.853 5991625.868,3559795.445 5991636.851,3559809.723 5991641.591,3559826.229 5991645.181,3559839.864 5991647.119,3559857.227 5991647.407,3559873.876 5991645.325,3559888.588 5991640.159,3559907.39 5991631.042,3559927.122 5991621.712,3559943.483 5991614.175,3559961.424 5991605.776,3559976.422 5991597.809,3559991.922 5991590.846,3560008.859 5991582.52,3560024.862 5991574.48,3560046.677 5991563.644,3560068.563 5991552.375,3560090.303 5991542.758,3560088.21 5991546.63,3560099.716 5991541.408,3560105.7325116 5991538.5738151,3560111.58 5991536.427,3560111.584 5991534.484,3560110.173 5991532.251,3560107.296 5991526.253,3560098.903 5991505.021,3560089.412 5991490.058,3560084.224 5991489.354,3560069.146 5991492.098,3560049.86 5991498.531,3560021.83 5991510.626,3559999.2 5991518.86,3559977.599 5991527.61,3559982.742 5991542.535,3559961.913 5991550.254,3559933.883 5991553.085,3559931.311 5991546.652,3559914.082 5991551.798,3559891.453 5991549.997,3559859.883 5991529.006,3559846.272 5991529.176,3559835.781 5991529.598,3559830.766 5991517.598,3559823.135 5991506.907,3559810.925 5991494.907,3559796.534 5991481.161,3559782.2533305 5991464.3545177,3559772.053 5991439.46,3559770.173 5991421.329,3559760.43 5991406.619,3559745.9 5991384.553,3559726.927 5991364.712,3559717.355 5991352.91,3559704.1346239 5991348.2238236,3559683.4828301 5991330.9825355,3559663.5383423 5991315.9798236,3559665.904 5991312.885,3559639.41 5991295.78,3559613.599 5991278.333,3559583.173 5991258.321,3559567.96 5991255.926,3559553.089 5991260.373,3559543.859 5991270.636,3559506.022 5991306.45,3559491.604 5991303.89,3559475.093 5991300.4,3559466.024 5991298.305,3559458.582 5991296.211,3559437.188 5991288.299,3559414.863 5991279.921,3559405.561 5991288.764,3559390.8557975 5991289.4797635,3559375.9689151 5991290.4435788,3559355.5910571 5991290.3468703,3559333.5760283 5991290.2729815,3559319.7436619 5991289.707769,3559308.7035011 5991288.2436427,3559295.1952954 5991285.7573935,3559274.2503659 5991281.5355372,3559257.431 5991269.117,3559237.574 5991255.593,3559212.479 5991239.864,3559189.315 5991226.066,3559163.944 5991220.823,3559137.47 5991217.235,3559117.383 5991244.922,3559090.0 5991282.808,3559065.753 5991289.07,3559038.182 5991288.874,3559009.633 5991278.112,3558985.776 5991277.721,3558961.138 5991274.59,3558940.345 5991272.496,3558922.806 5991266.423,3558917.748 5991270.471,3558893.577 5991284.758,3558870.3800237 5991288.5992739,3558854.179995 5991280.3701628,3558844.041 5991262.969,3558832.389 5991248.192,3558821.551 5991236.262,3558809.628 5991225.146,3558799.061 5991216.2,3558779.553 5991201.153,3558749.883 5991191.528,3558726.3052769 5991184.5561992,3558707.2409429 5991166.2072643,3558695.663 5991144.83,3558678.104 5991127.812,3558669.256 5991119.373,3558656.814 5991117.022,3558629.717 5991112.18,3558604.554 5991104.848,3558575.383 5991101.804,3558547.595 5991099.868,3558521.881 5991101.666,3558496.67 5991106.87,3558474.512 5991099.1,3558454.437 5991102.321,3558438.339 5991103.648,3558421.483 5991106.87,3558403.87 5991114.639,3558380.6959809 5991121.831876,3558357.8077406 5991118.0349568,3558338.154 5991102.51,3558319.215 5991076.358,3558315.995 5991068.02,3558301.252 5991064.382,3558290.979 5991061.219,3558283.866 5991058.649,3558267.864 5991055.09,3558254.035 5991047.974,3558239.218 5991047.776,3558224.796 5991049.358,3558205.433 5991049.759,3558187.851 5991042.642,3558174.219 5991024.257,3558153.08 5991017.338,3558130.953 5991020.304,3558106.2870105 5991020.405353,3558082.9617472 5991012.0799188,3558066.747 5990998.955,3558057.461 5990989.862,3558043.83 5990987.094,3558023.284 5990976.616,3558002.935 5990964.953,3557979.228 5990956.057,3557958.6654104 5990945.4587572,3557939.1757098 5990930.9544607,3557920.4606672 5990916.486639,3557906.328 5990899.918,3557891.313 5990883.905,3557873.336 5990866.51,3557862.272 5990846.742,3557852.7478056 5990816.519316,3557833.0333612 5990798.4940002,3557814.0973003 5990780.6739921,3557801.162 5990761.334,3557797.0 5990748.994,3557786.364 5990739.585,3557759.6310709 5990730.0894804,3557741.1494385 5990712.4021475,3557720.1293692 5990693.0883072,3557697.2410416 5990675.2509234,3557673.058 5990661.225,3557646.852 5990653.204,3557614.088 5990645.015,3557585.706 5990638.581,3557556.66 5990626.378,3557529.609 5990622.828,3557498.345 5990621.274,3557467.081 5990613.952,3557450.895 5990610.181,3557438.256 5990613.065,3557409.431 5990629.261,3557386.2686906 5990639.8482381,3557372.404 5990613.951,3557356.661 5990612.176,3557332.05 5990612.62,3557310.542 5990610.623,3557283.712 5990609.735,3557262.648 5990609.292,3557266.861 5990622.604,3557278.025 5990658.547,3557283.777 5990675.607,3557256.252 5990661.63,3557233.1853797 5990628.0003788,3557204.9 5990606.752,3557189.289 5990602.23,3557175.116 5990601.408,3557156.218 5990601.613,3557140.402 5990605.93,3557117.601 5990617.44,3557103.429 5990626.482,3557085.0943006 5990640.4044997,3557074.9964986 5990637.9229618,3557054.7826693 5990631.7431003,3557029.7778416 5990624.4999734,3557001.2164872 5990615.1320435,3556969.6125458 5990607.3742886,3556943.033 5990598.015,3556911.49 5990586.358,3556884.786 5990578.467,3556845.5025530998 5990575.0391814,3556818.838 5990572.719,3556792.834 5990567.103,3556766.457 5990558.492,3556752.052 5990557.182,3556740.266 5990563.546,3556720.249 5990578.896,3556698.9443952 5990595.1742707,3556690.619 5990598.57,3556666.889 5990627.917,3556643.6836989 5990637.010547,3556621.4261699 5990629.555119,3556607.8723323 5990618.3755926,3556593.966 5990600.036,3556588.101 5990591.765,3556577.969 5990587.497,3556559.972 5990576.425,3556538.4406575 5990566.5430112,3556523.6966081 5990550.180142,3556507.0140549 5990530.1483911,3556490.7689106 5990511.4174812,3556476.1768176 5990494.0336678,3556463.952 5990469.65,3556451.835 5990446.5,3556438.248 5990414.714,3556417.685 5990400.935,3556401.344 5990384.766,3556389.41 5990363.269,3556389.043 5990350.776,3556380.58 5990347.368,3556374.49 5990346.027,3556368.4 5990365.285,3556355.247 5990395.754,3556345.707 5990419.085,3556342.218 5990427.868</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
       <gml:interior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559965.513 5991576.245,3559967.056 5991563.893,3559979.142 5991567.753,3559976.828 5991577.017,3559965.513 5991576.245</gml:coordinates>
        </gml:LinearRing>
       </gml:interior>
       <gml:interior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560040.602 5991518.088,3560037.773 5991510.111,3560047.031 5991505.994,3560048.831 5991515.515,3560040.602 5991518.088</gml:coordinates>
        </gml:LinearRing>
       </gml:interior>
       <gml:interior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560072.746 5991506.509,3560059.374 5991511.912,3560056.803 5991502.906,3560070.432 5991497.759,3560072.746 5991506.509</gml:coordinates>
        </gml:LinearRing>
       </gml:interior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308269729"/>
     <simBase:roughnessStyle>Vorland ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718779">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553535.413 5988663.739,3553561.677 5988656.776,3553582.225 5988655.946,3553612.945 5988654.629,3553636.204 5988652.058,3553628.305 5988623.013,3553624.98 5988594.559,3553621.388 5988562.665,3553617.796 5988533.466,3553612.858 5988503.369,3553608.819 5988480.772,3553605.923 5988464.761,3553623.504 5988462.241,3553642.36 5988445.62,3553624.402 5988424.956,3553591.627 5988389.917,3553567.249 5988361.302,3553545.27 5988319.907,3553519.397 5988269.448,3553507.557 5988275.152,3553475.106 5988291.387,3553444.409 5988306.305,3553414.589 5988319.468,3553397.44 5988286.472,3553365.912 5988297.968,3553349.008 5988330.081,3553319.152 5988316.829,3553306.491 5988341.089,3553292.7532075 5988364.4485652,3553280.631 5988381.07,3553302.63 5988401.734,3553321.487 5988420.152,3553343.935 5988405.777,3553382.448 5988380.56,3553434.525 5988397.246,3553473.245 5988416.754,3553488.312 5988459.608,3553505.845 5988489.94,3553492.408 5988519.033,3553490.068 5988541.28,3553498.26 5988565.284,3553491.822 5988591.602,3553489.774 5988611.801,3553492.407 5988634.049,3553493.067 5988665.551,3553496.517 5988685.863,3553497.687 5988712.404,3553496.322 5988725.947,3553518.593 5988722.821,3553543.648 5988721.928,3553575.904 5988721.27,3553610.3263210002 5988722.3550532,3553637.003 5988722.807,3553637.901 5988693.159,3553637.102 5988659.56,3553614.261 5988661.039,3553581.347 5988662.532,3553561.564 5988662.997,3553541.986 5988668.336,3553532.598 5988690.022,3553527.331 5988689.086,3553535.413 5988663.739</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308185619"/>
     <simBase:roughnessStyle>Vorland Röhricht ohne Bewuchs</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718747">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553100.833 5988681.41,3553102.746 5988680.857,3553104.7386196 5988679.7411746,3553102.6128715 5988674.8473452,3553101.09 5988675.431,3553098.412 5988676.417,3553100.833 5988681.41</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308098418"/>
     <simBase:roughnessStyle>Strasse</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718712">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560127.737 5991548.116,3560130.747 5991547.355,3560129.513 5991545.182,3560123.882 5991547.447,3560118.007 5991550.019,3560121.86 5991550.389,3560124.739 5991549.079,3560127.737 5991548.116</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308098418"/>
     <simBase:roughnessStyle>Strasse</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171875">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560111.58 5991536.427,3560112.2225391 5991537.7858597,3560117.1231259 5991536.3232237,3560123.9104795 5991534.7268957,3560123.148 5991533.304,3560123.336 5991531.976,3560121.495 5991531.114,3560119.732 5991532.682,3560115.58 5991534.132,3560111.584 5991534.484,3560111.58 5991536.427</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308098418"/>
     <simBase:roughnessStyle>Strasse</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187138">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553081.495 5988690.278,3553085.029 5988688.78,3553087.297 5988687.518,3553085.191 5988682.239,3553082.453 5988682.968,3553078.739 5988684.646,3553081.495 5988690.278</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308098418"/>
     <simBase:roughnessStyle>Strasse</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171878">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556327.9162211 5990438.4367321,3556327.6177195 5990439.140005,3556329.847 5990441.802,3556335.398 5990444.261,3556336.49 5990443.313,3556336.7747381 5990442.5097274,3556331.9556421 5990440.3446513,3556327.9162211 5990438.4367321</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308098418"/>
     <simBase:roughnessStyle>Strasse</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187114">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552958.645 5986916.199,3552965.757 5986909.153,3552966.675 5986904.563,3552967.328 5986901.287,3552963.376 5986903.696,3552959.7109699 5986903.0852319,3552955.3808794 5986915.3627565,3552958.645 5986916.199</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308098418"/>
     <simBase:roughnessStyle>Strasse</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718757">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552945.998 5986900.8,3552941.951 5986900.131,3552942.195 5986904.8,3552939.442 5986904.668,3552938.089 5986910.395,3552937.506 5986914.331,3552940.275 5986912.363,3552942.86 5986912.155,3552945.2215508 5986912.7600118,3552948.7323796 5986901.2556775,3552945.998 5986900.8</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308098418"/>
     <simBase:roughnessStyle>Strasse</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718742">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556335.7681663 5990431.0705726,3556340.1490971 5990432.9903478,3556340.425 5990432.212,3556341.055 5990430.803,3556335.689 5990428.424,3556332.769 5990426.088,3556332.154 5990428.403,3556331.79518 5990429.2978666,3556335.7681663 5990431.0705726</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308098418"/>
     <simBase:roughnessStyle>Strasse</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718724">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553067.384 5988526.555,3553061.155 5988540.395,3553054.237 5988560.068,3553051.371 5988577.862,3553054.522 5988577.983,3553057.421 5988560.594,3553062.191 5988546.735,3553070.047 5988529.146,3553081.5659781 5988512.2112073,3553078.947 5988509.353,3553067.384 5988526.555</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308008315"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 2</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718722">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552962.8420048 5986893.3403704,3552959.7109699 5986903.0852319,3552963.376 5986903.696,3552966.6959563 5986894.2242112,3552962.8420048 5986893.3403704</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308008315"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 2</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718782">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553348.073 5988850.223,3553353.574 5988854.002,3553360.069 5988859.173,3553366.828 5988866.332,3553371.269 5988873.028,3553380.149 5988890.132,3553383.662 5988903.458,3553387.505 5988917.115,3553390.608 5988916.074,3553383.251 5988889.171,3553373.307 5988870.92,3553361.615 5988856.483,3553349.736 5988847.31,3553333.096 5988839.665,3553331.703 5988842.864,3553348.073 5988850.223</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308008315"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 2</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718780">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553424.568 5988989.785,3553429.075 5989013.426,3553434.819 5989024.858,3553437.252 5989023.054,3553431.518 5989013.044,3553429.931 5989002.546,3553427.247 5988989.24,3553424.568 5988989.785</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308008315"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 2</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718754">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553407.846 5988897.197,3553403.819 5988882.793,3553391.6940512 5988859.1693411,3553376.57649 5988840.1688201,3553360.4942643 5988828.8139406,3553339.3156082 5988819.8753578,3553338.025 5988823.46,3553358.636 5988832.159,3553373.938 5988842.963,3553388.717 5988861.538,3553400.105 5988884.816,3553407.649 5988913.174,3553413.927 5988937.283,3553418.145 5988959.889,3553421.268 5988959.21,3553418.095 5988942.364,3553415.41 5988926.739,3553411.141 5988912.334,3553407.846 5988897.197</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812308008315"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 2</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187100">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552940.662 5986947.614,3552939.103 5986960.194,3552951.118 5986939.403,3552958.55 5986924.73,3552958.645 5986916.199,3552951.943 5986927.943,3552940.662 5986947.614</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230781506"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 3</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718786">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553537.703 5989012.75,3553521.766 5989022.012,3553523.099 5989024.928,3553539.185 5989015.37,3553550.8130369 5989008.4111372,3553562.616 5989000.509,3553571.418 5988994.905,3553576.583 5988991.194,3553579.7456041 5988989.5298482,3553579.211 5988988.382,3553575.862 5988990.096,3553571.019 5988993.239,3553561.989 5988998.477,3553549.391 5989005.993,3553537.703 5989012.75</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230781506"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 3</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718798">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553216.861 5988369.57,3553233.697 5988349.199,3553222.3805047 5988350.9243318,3553210.6163696 5988364.3752734,3553193.8646014 5988385.5096144,3553179.6316788 5988402.1882252,3553163.0139018 5988423.7592431,3553150.0095526 5988439.56708,3553154.589 5988443.777,3553169.076 5988426.924,3553185.74 5988407.049,3553200.094 5988390.114,3553216.861 5988369.57</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230781506"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 3</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187145">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552915.615 5986947.502,3552904.709 5986964.926,3552910.174 5986964.389,3552926.028 5986938.923,3552936.699 5986922.302,3552934.832 5986921.719,3552930.252 5986928.688,3552923.885 5986937.766,3552915.615 5986947.502</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230781506"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 3</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187118">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553260.763 5988313.455,3553269.05 5988296.723,3553271.102 5988286.99,3553272.174 5988274.834,3553269.966 5988274.803,3553265.1159214 5988286.984082,3553263.2252299 5988294.6956322,3553256.7392405 5988309.6719116,3553260.763 5988313.455</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307736919"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 4</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187137">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552874.3646086 5987001.2255826,3552868.15 5986997.614,3552858.209 5987016.531,3552864.0993107 5987019.6734688,3552854.7286679 5987038.9248625,3552860.1101134 5987041.3862018,3552853.7695567 5987061.3418866,3552850.3261134 5987081.0913446,3552848.1298969 5987105.3344919,3552848.2504742 5987129.6326229,3552850.219 5987154.814,3552852.382 5987170.396,3552858.435 5987183.305,3552854.003 5987155.605,3552852.174 5987128.882,3552852.323 5987104.146,3552854.226 5987079.432,3552856.597 5987064.14,3552862.855 5987043.22,3552872.3422703 5987023.7852375,3552882.734 5987005.867,3552879.7964742 5987003.3874801,3552874.3646086 5987001.2255826</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307736919"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 4</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187102">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553136.645 5988062.622,3553149.2204205 5988086.3236581,3553162.081 5988110.249,3553172.945 5988131.409,3553187.778 5988156.183,3553190.0645727 5988154.6744606,3553175.4096691 5988130.1326752,3553164.476559 5988108.877443,3553151.6271561002 5988084.9726497,3553139.2326212 5988061.1930911,3553133.981551 5988051.3717552,3553121.7135349 5988028.426279,3553119.019 5988029.732,3553131.4740613 5988052.9730624,3553136.645 5988062.622</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307736919"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 4</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718759">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559575.35 5991373.519,3559554.393 5991366.952,3559562.79 5991371.821,3559574.3800296 5991375.7792076,3559592.744 5991384.521,3559593.951 5991382.077,3559575.35 5991373.519</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307639722"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 5</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187108">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559859.243 5991663.04,3559858.922 5991666.032,3559878.052 5991664.352,3559895.43 5991658.143,3559916.823 5991648.064,3559923.173 5991642.069,3559907.158 5991649.624,3559894.1010285 5991655.3862934,3559877.602 5991660.696,3559859.243 5991663.04</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307639722"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 5</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718751">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552857.259 5987237.738,3552851.119 5987212.946,3552840.814 5987215.535,3552845.418 5987239.713,3552846.296 5987250.682,3552846.076 5987265.821,3552859.232 5987265.207,3552858.355 5987249.585,3552857.259 5987237.738</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307639722"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 5</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718718">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552910.174 5986964.389,3552904.709 5986964.926,3552899.1640943 5986963.5112509,3552887.9040322 5986981.3702515,3552874.3646086 5987001.2255826,3552879.7964742 5987003.3874801,3552882.734 5987005.867,3552895.977 5986986.731,3552910.174 5986964.389</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307639722"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 5</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718715">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3552993.264 5987800.18,3553007.747 5987823.446,3553021.059 5987846.463,3553033.833 5987867.571,3553046.312 5987890.203,3553049.2170144 5987888.8504945,3553036.6556866 5987865.8197332,3553023.8490873 5987844.3715661,3553010.728034 5987821.5980717,3552996.723124 5987798.783021,3552993.264 5987800.18</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230738834"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 7</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718785">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553759.211 5988973.822,3553785.087 5988978.338,3553786.093 5988974.914,3553765.487 5988971.542,3553748.251 5988967.304,3553732.653 5988963.353,3553723.409 5988960.463,3553707.329 5988955.936,3553685.856 5988951.41,3553685.026439 5988954.6142273,3553708.258 5988959.21,3553731.801 5988966.932,3553759.211 5988973.822</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230738834"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 7</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187116">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553086.964977 5988492.2527565,3553078.947 5988509.353,3553081.5659781 5988512.2112073,3553096.851 5988495.675,3553117.738 5988476.688,3553136.081 5988460.994,3553154.589 5988443.777,3553150.0095526 5988439.56708,3553132.2865106 5988454.8083376,3553112.6985677 5988470.5209284,3553098.1590087 5988483.151657,3553086.964977 5988492.2527565</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230738834"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 7</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187170">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553102.603 5987998.702,3553119.019 5988029.732,3553121.7135349 5988028.426279,3553105.1745806 5987997.1808641,3553094.6137429 5987975.7176212,3553083.0100568 5987953.8494169,3553072.3528413 5987932.9537134,3553069.7 5987934.451,3553080.322 5987955.289,3553091.903 5987976.837,3553102.603 5987998.702</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230731726"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 8</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187128">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553190.0645727 5988154.6744606,3553187.778 5988156.183,3553202.927 5988176.656,3553226.703 5988201.661,3553254.832 5988232.081,3553257.0596297 5988230.6336634,3553228.6807065 5988199.6837369,3553204.8578178 5988174.7114641,3553190.0645727 5988154.6744606</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230731726"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 8</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187135">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553269.463 5988267.177,3553269.966 5988274.803,3553272.174 5988274.834,3553271.255 5988260.981,3553266.428 5988248.717,3553254.832 5988232.081,3553255.557 5988237.094,3553264.102 5988249.747,3553267.872 5988257.959,3553269.463 5988267.177</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230731726"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 8</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187130">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553249.568 5988329.417,3553260.763 5988313.455,3553256.7392405 5988309.6719116,3553244.5724929 5988325.4985752,3553232.0497956 5988340.4069256,3553222.3805047 5988350.9243318,3553233.697 5988349.199,3553249.568 5988329.417</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230724212"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 9</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718776">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553247.922 5988812.175,3553248.1926244 5988808.3786388,3553222.9778628 5988805.8433809,3553199.2961332 5988802.110508,3553183.4029861 5988796.3114625,3553169.5011358 5988788.2056573,3553167.3294978 5988791.3425561,3553181.78 5988799.769,3553198.339 5988805.811,3553222.491 5988809.618,3553247.922 5988812.175</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307166011"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 10</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187142">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553105.592 5988690.385,3553113.593 5988708.357,3553123.418 5988731.103,3553127.11 5988729.26,3553117.57 5988706.833,3553108.435 5988688.764,3553102.746 5988680.857,3553100.833 5988681.41,3553105.592 5988690.385</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307166011"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 10</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187132">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553486.946 5989037.499,3553490.056 5989036.957,3553493.363 5989036.323,3553496.285 5989035.586,3553498.317 5989035.039,3553497.709 5989032.09,3553492.586 5989033.456,3553486.798 5989035.017,3553472.154 5989035.627,3553465.175 5989038.955,3553480.234 5989038.658,3553486.946 5989037.499</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812307166011"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 10</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187110">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553141.2527238 5988757.6025154,3553134.5134238 5988745.8335028,3553127.11 5988729.26,3553123.418 5988731.103,3553131.212 5988747.724,3553138.016 5988759.606,3553144.235 5988768.925,3553155.421 5988781.649,3553167.3294978 5988791.3425561,3553169.5011358 5988788.2056573,3553158.0688108 5988778.8988922,3553147.259139 5988766.6029631,3553141.2527238 5988757.6025154</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230709497"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 11</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187104">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553314.472 5988837.693,3553331.703 5988842.864,3553333.096 5988839.665,3553314.706 5988834.998,3553297.028 5988832.571,3553271.912 5988830.452,3553246.796 5988828.827,3553220.598 5988825.906,3553194.824 5988822.721,3553175.153 5988816.815,3553156.5519249 5988805.8516359,3553154.353 5988808.985,3553173.241 5988820.124,3553194.25 5988826.289,3553220.43 5988829.273,3553246.807 5988831.659,3553273.448 5988833.715,3553296.776 5988835.107,3553314.472 5988837.693</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230702084"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 12</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718725">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554608.712 5989042.942,3554623.1 5989058.095,3554637.541 5989078.194,3554649.754 5989092.504,3554663.283 5989108.655,3554672.377 5989120.685,3554688.555 5989139.43,3554704.739 5989155.427,3554707.683 5989152.641,3554692.543 5989136.035,3554675.281 5989117.699,3554666.25 5989105.609,3554652.705 5989090.065,3554641.151 5989075.85,3554626.675 5989056.321,3554611.269 5989038.917,3554589.593 5989020.834,3554572.348 5989011.349,3554570.038 5989015.487,3554585.427 5989023.707,3554599.868 5989034.071,3554608.712 5989042.942</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812306935612"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 13</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718768">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554000.271 5988958.406,3553973.493 5988961.896,3553964.237 5988965.822,3554000.756 5988961.3,3554027.5 5988956.547,3554055.533 5988951.211,3554083.7686196 5988947.738684,3554083.61 5988944.228,3554054.341 5988947.785,3554026.851 5988952.711,3554000.271 5988958.406</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812306935612"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 13</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481171876">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559683.4828301 5991330.9825355,3559665.904 5991312.885,3559663.5383423 5991315.9798236,3559683.4828301 5991330.9825355</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230573989"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 15</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718774">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553810.454 5988977.131,3553786.093 5988974.914,3553785.087 5988978.338,3553810.858 5988980.258,3553830.906 5988981.37,3553857.638 5988980.782,3553883.398 5988978.956,3553883.441 5988975.975,3553857.154 5988977.612,3553831.926 5988977.901,3553810.454 5988977.131</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230573989"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 15</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187168">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559478.4421211 5991364.7681858,3559484.146 5991364.379,3559496.64 5991363.777,3559491.381 5991361.779,3559482.367 5991362.434,3559475.67 5991363.089,3559470.231 5991363.693,3559470.4880487 5991366.0094816,3559472.856 5991365.508,3559475.866 5991364.981,3559478.4421211 5991364.7681858</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230573989"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 15</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718735">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559278.8634961 5991370.1982862,3559279.3 5991367.705,3559257.701 5991363.234,3559235.357 5991358.019,3559211.648 5991351.686,3559185.209 5991344.338,3559184.2764723 5991347.9900783,3559210.981 5991354.757,3559234.6438703 5991361.3030743,3559257.576 5991366.072,3559278.8634961 5991370.1982862</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230573989"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 15</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187126">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555075.1090422 5989238.5330554,3555075.128 5989234.274,3555050.557 5989234.591,3555023.767 5989236.017,3555001.891 5989237.761,3554976.37 5989240.774,3554948.628 5989243.629,3554925.198 5989244.84,3554901.598 5989244.84,3554877.069 5989242.517,3554876.5208845 5989244.6523722,3554890.572 5989246.222,3554909.255 5989247.452,3554925.1214119 5989247.8256948,3554948.39 5989247.098,3554977.629 5989244.184,3555003.538 5989241.478,3555023.7641066 5989239.689963,3555050.006 5989238.374,3555075.1090422 5989238.5330554</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230573989"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 15</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187157">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557810.838 5990859.173,3557792.58 5990841.784,3557777.741 5990828.57,3557757.861 5990809.409,3557736.256 5990789.432,3557720.28 5990774.449,3557704.757 5990761.01,3557684.332 5990742.213,3557683.045 5990745.454,3557704.939 5990765.951,3557716.4065815 5990776.7277207,3557731.885 5990790.802,3557755.744 5990812.562,3557777.217 5990832.217,3557790.1586723 5990844.4634303,3557807.882 5990860.802,3557831.261 5990882.194,3557833.441 5990879.394,3557810.838 5990859.173</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230573989"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 15</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718745">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559430.362786 5991372.8175503,3559429.749 5991369.563,3559415.598 5991370.929,3559396.234 5991372.295,3559378.607 5991373.04,3559378.6088436 5991376.8749307,3559397.137 5991376.146,3559417.155 5991374.336,3559430.362786 5991372.8175503</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230573989"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 15</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187146">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556370.935 5990443.259,3556361.282 5990437.834,3556354.382 5990432.269,3556348.145 5990429.921,3556345.868 5990429.516,3556342.218 5990427.868,3556341.055 5990430.803,3556340.425 5990432.212,3556344.6344172 5990433.8883605,3556348.698 5990435.139,3556352.6706161 5990436.4151882,3556359.825 5990440.36,3556377.988 5990450.652,3556398.6161728 5990465.5186307,3556417.207 5990481.913,3556432.4314252 5990498.8690813,3556450.784 5990519.951,3556479.384 5990554.338,3556495.709435 5990574.3705527,3556511.285 5990591.611,3556532.186 5990611.693,3556551.298 5990627.373,3556569.2177574 5990639.2326052,3556590.498 5990650.674,3556611.4424791 5990659.3813216,3556613.124 5990655.071,3556592.342 5990647.303,3556571.341 5990636.033,3556553.184 5990624.107,3556534.481 5990608.789,3556514.246 5990589.204,3556498.823 5990571.807,3556482.373 5990552.651,3556466.294 5990532.509,3556453.256 5990516.424,3556435.873 5990495.847,3556419.938 5990478.603,3556400.99 5990462.795,3556383.476 5990450.725,3556370.935 5990443.259</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230573989"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 15</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718791">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556034.46 5990038.115,3556036.421 5990028.254,3556033.8092736 5990027.581785,3556031.417 5990037.01,3556029.707682 5990043.9167052,3556026.63 5990057.829,3556023.2347701 5990073.0934279,3556028.093 5990073.918,3556030.541 5990059.125,3556032.807 5990044.73,3556034.46 5990038.115</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230557968"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 17</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718795">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558386.82 5991148.124,3558400.5226017 5991149.4400318,3558421.514 5991150.689,3558452.687 5991150.689,3558459.62 5991146.849,3558438.027 5991147.945,3558420.49 5991147.507,3558401.308 5991146.301,3558377.523 5991143.889,3558353.081 5991139.503,3558352.4575605 5991142.4865847,3558367.915 5991145.422,3558386.82 5991148.124</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230557968"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 17</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187151">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556017.727 5990033.336,3556015.244 5990041.356,3556011.539 5990055.432,3556007.831 5990070.401,3556009.6258844 5990070.6415275,3556013.334 5990055.883,3556016.8835148 5990041.8539748,3556019.04 5990033.677,3556021.3812649 5990025.645117,3556019.89 5990025.62,3556017.727 5990033.336</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230557968"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 17</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187153">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554256.546 5988983.105,3554289.219 5988997.422,3554317.645 5989007.458,3554342.877 5989011.497,3554366.238 5989013.75,3554366.178 5989009.623,3554343.062 5989007.435,3554319.128 5989003.33,3554301.074 5988997.721,3554276.456 5988988.691,3554252.108 5988977.61,3554228.584 5988967.349,3554206.838 5988959.003,3554182.494 5988952.026,3554181.084 5988955.188,3554205.75 5988962.35,3554226.603 5988970.577,3554256.546 5988983.105</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230557968"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 17</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187106">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556013.229 5989850.36,3556026.141 5989869.602,3556033.0948456 5989884.980608,3556041.424 5989906.47,3556046.6450247 5989927.2479973,3556048.431 5989947.526,3556047.7884759 5989967.7472324,3556043.826 5989990.585,3556040.2422435 5990003.7725124,3556045.323 5990001.536,3556050.152 5989989.638,3556053.197 5989968.641,3556053.678 5989948.605,3556051.915 5989926.165,3556046.338 5989904.706,3556035.954 5989883.826,3556028.684 5989868.244,3556023.18 5989859.206,3556018.487 5989845.412,3556004.084 5989831.787,3555985.185 5989814.712,3555962.629 5989801.662,3555960.1905362 5989806.6469427,3555981.869 5989819.651,3556000.4795509 5989836.7124284,3556013.229 5989850.36</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230557968"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 17</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187183">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554458.421 5989003.901,3554457.855 5988999.791,3554436.474 5989003.931,3554413.5 5989007.297,3554390.796 5989008.665,3554366.178 5989009.623,3554366.238 5989013.75,3554389.858 5989012.214,3554414.029 5989010.26,3554437.555 5989007.204,3554458.421 5989003.901</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230548541"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 18</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187109">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557662.276 5990723.96,3557641.215 5990709.794,3557620.79 5990697.354,3557598.069 5990687.282,3557596.7100832 5990690.8102352,3557618.464 5990700.256,3557639.3151244 5990712.3441968,3557659.888 5990726.361,3557683.045 5990745.454,3557684.332 5990742.213,3557662.276 5990723.96</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230548541"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 18</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718758">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554833.435 5989229.775,3554826.752 5989227.245,3554821.697 5989225.169,3554815.195 5989222.414,3554806.492 5989218.535,3554795.337 5989214.417,3554772.494 5989204.32,3554759.213 5989196.216,3554742.479 5989185.588,3554723.886 5989168.583,3554707.683 5989152.641,3554704.739 5989155.427,3554716.652 5989167.379,3554730.936 5989181.039,3554741.309 5989189.501,3554756.679 5989199.882,3554770.618 5989208.193,3554785.09 5989214.799,3554800.63 5989220.766,3554809.106 5989223.592,3554814.2324349 5989225.4874447,3554817.573 5989226.884,3554820.622 5989228.169,3554822.8193564 5989228.8922958,3554825.776 5989230.105,3554829.656 5989231.768,3554832.7111718 5989232.9387641,3554839.708 5989234.542,3554846.7293378 5989236.2212764,3554857.642 5989239.372,3554868.777 5989242.593,3554876.5208845 5989244.6523722,3554877.069 5989242.517,3554869.079 5989240.751,3554858.673 5989236.847,3554847.43 5989233.687,3554840.183 5989231.735,3554833.435 5989229.775</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230538025"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 19</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187191">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555265.8568078 5989304.3608382,3555269.1979645 5989298.3759916,3555246.5362243 5989284.170429,3555222.712 5989272.015,3555199.9295078 5989261.7214687,3555176.0240985 5989252.5321503,3555151.2898861 5989244.8381324,3555126.2874458 5989239.2263793,3555100.9198972 5989235.1653848,3555075.128 5989234.274,3555075.1090422 5989238.5330554,3555100.152 5989239.925,3555125.3619275 5989243.8611823,3555150.41 5989249.17,3555174.5958144 5989257.1054791,3555198.429 5989266.567,3555220.4479362 5989276.5711356,3555243.524 5989289.661,3555265.8568078 5989304.3608382</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230528011"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 20</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718792">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556048.6612505 5990266.2198279,3556037.002 5990246.08,3556025.7180486 5990223.7181999,3556017.763 5990203.37,3556011.9462413 5990182.7278201,3556009.288 5990183.666,3556015.114 5990204.332,3556023.59 5990224.733,3556034.714 5990246.989,3556046.103 5990267.92,3556048.6612505 5990266.2198279</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230528011"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 20</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187192">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556660.704 5990667.982,3556637.078 5990663.058,3556613.124 5990655.071,3556611.4424791 5990659.3813216,3556635.83 5990667.271,3556659.9098402 5990672.1220849,3556684.158 5990674.477,3556707.3661926 5990675.5785093,3556707.197 5990671.909,3556684.22 5990670.608,3556660.704 5990667.982</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230528011"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 20</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718750">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559087.236 5991341.856,3559087.613908 5991338.0611454,3559071.411 5991336.883,3559053.465 5991336.512,3559037.6501156 5991336.5344764,3559012.484 5991337.437,3558988.6564387 5991338.9247653,3558965.127 5991340.623,3558940.3419798 5991341.8864006,3558926.561 5991341.789,3558909.755 5991340.429,3558895.67 5991338.486,3558884.5902586 5991336.8786737,3558883.913 5991339.49,3558910.532 5991343.953,3558940.754 5991344.493,3558965.129 5991344.493,3558989.324 5991342.603,3559013.431 5991341.434,3559037.881 5991340.183,3559062.938 5991340.335,3559087.236 5991341.856</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230528011"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 20</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187174">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555389.44 5989483.539,3555404.329 5989508.318,3555417.3662901 5989530.5941193,3555430.9511605 5989552.270477,3555433.7789479 5989550.3287087,3555420.525 5989529.889,3555406.6868757 5989507.0149451,3555390.968 5989480.721,3555371.576 5989448.543,3555378.254 5989464.313,3555389.44 5989483.539</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230517393"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 21</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187196">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558319.04 5991133.309,3558337.684 5991138.625,3558352.4575605 5991142.4865847,3558353.081 5991139.503,3558329.571 5991133.06,3558304.854 5991125.578,3558294.934 5991122.208,3558282.933 5991118.153,3558282.0733138 5991120.4230187,3558294.178 5991125.029,3558304.0827962 5991128.6623508,3558319.04 5991133.309</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230517393"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 21</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187141">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555448.747 5989572.485,3555433.7789479 5989550.3287087,3555430.9511605 5989552.270477,3555446.132 5989574.914,3555460.3192472 5989592.9496784,3555476.5186368 5989611.9709246,3555493.067 5989629.773,3555495.6323996 5989627.0668331,3555479.717 5989610.085,3555462.8561413 5989590.331294,3555448.747 5989572.485</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230490251"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 22</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718713">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556683.111 5990688.838,3556657.3567839 5990686.7905621,3556631.879 5990681.497,3556606.8141498 5990672.9750027,3556584.474 5990664.011,3556562.0854603 5990652.7696263,3556544.148 5990640.028,3556521.461 5990621.046,3556519.267 5990622.9,3556540.768 5990641.445,3556560.734 5990655.272,3556583.332 5990666.685,3556606.039 5990675.575,3556631.161 5990683.915,3556656.83 5990689.395,3556682.406 5990691.978,3556708.105 5990692.47,3556707.6690472 5990689.4111943,3556683.111 5990688.838</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11781230490251"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 22</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187188">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555598.104 5989706.517,3555578.4506324 5989694.4443302,3555556.768 5989680.413,3555533.4962217 5989661.5115907,3555514.934 5989645.686,3555495.6323996 5989627.0668331,3555493.067 5989629.773,3555512.0035628 5989648.0231561,3555531.254 5989664.44,3555553.500242 5989681.8682606,3555576.339 5989697.592,3555596.3038259 5989709.4954923,3555617.051 5989720.816,3555641.948 5989733.437,3555663.986 5989743.365,3555665.77 5989739.978,3555643.606 5989730.031,3555619.0965553 5989717.3760496,3555598.104 5989706.517</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11768145593596"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 23</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187115">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556994.778 5990684.225,3556972.064 5990677.528,3556954.8741209 5990673.1054414,3556932.736 5990669.278,3556906.5610072 5990667.5706681,3556906.195 5990671.929,3556932.386 5990673.651,3556954.027 5990677.341,3556981.571 5990684.475,3557008.499 5990692.964,3557009.7687794 5990689.3130864,3556994.778 5990684.225</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11768145593596"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 23</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187213">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556011.9462413 5990182.7278201,3556008.035 5990163.687,3556005.657 5990143.466,3556004.6574935 5990124.5859005,3556005.55 5990108.542,3556007.063 5990089.728,3556009.6258844 5990070.6415275,3556007.831 5990070.401,3556004.388 5990088.55,3556002.269 5990107.891,3556002.269 5990124.847,3556003.328 5990144.056,3556005.447 5990164.325,3556009.288 5990183.666,3556011.9462413 5990182.7278201</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11768145593596"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 23</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187147">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558872.719 5991337.369,3558883.913 5991339.49,3558884.5902586 5991336.8786737,3558873.1 5991335.251,3558861.322 5991332.919,3558854.697 5991331.324,3558854.327 5991333.423,3558861.033 5991335.051,3558872.719 5991337.369</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11768145593596"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 23</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187216">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558827.896 5991326.223,3558836.87 5991329.232,3558837.6553601 5991326.6766544,3558828.199 5991323.591,3558814.826 5991317.946,3558796.8270747 5991308.3699948,3558795.414 5991311.25,3558814.413 5991320.568,3558827.896 5991326.223</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11768145593596"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 23</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187217">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558572.959 5991139.465,3558546.73 5991137.931,3558546.9964746 5991140.9746837,3558573.404 5991142.892,3558596.1870001 5991147.1037158,3558625.244 5991155.148,3558652.4671418 5991169.0966084,3558653.987 5991166.672,3558637.846 5991158.114,3558617.266 5991149.638,3558596.767 5991143.582,3558572.959 5991139.465</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11768145593596"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 23</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718721">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559012.788 5991321.949,3558987.755 5991324.175,3558987.587532 5991326.8028017,3559012.576 5991325.037,3559038.4550349 5991324.353041,3559053.835 5991324.483,3559072.706 5991325.13,3559089.1025948 5991326.3191393,3559106.515 5991329.082,3559124.92 5991333.1,3559138.79096 5991336.4417178,3559164.158 5991342.876,3559184.2764723 5991347.9900783,3559185.209 5991344.338,3559164.573 5991338.975,3559139.081 5991333.006,3559115.815 5991327.542,3559088.604 5991322.785,3559063.72 5991321.874,3559038.476 5991321.378,3559012.788 5991321.949</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11768145593596"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 23</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117187173">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559507.898 5991361.125,3559491.381 5991361.779,3559496.64 5991363.777,3559513.951 5991363.626,3559531.1233928 5991364.9720465,3559545.632 5991367.257,3559562.79 5991371.821,3559554.393 5991366.952,3559531.969 5991362.384,3559524.113 5991361.981,3559507.898 5991361.125</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11768145593596"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 23</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811718717">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556934.926 5990651.463,3556908.1516007 5990649.7618382,3556908.509853 5990652.7462186,3556934.648 5990654.689,3556958.3174177 5990659.4395585,3556976.248 5990663.896,3556999.918 5990670.593,3557013.9816325 5990675.6592449,3557030.538 5990680.84,3557049.577 5990686.48,3557060.6851108 5990690.3583763,3557063.836 5990691.335,3557066.7816511 5990692.1595483,3557070.194 5990693.812,3557073.071 5990696.088,3557075.7126065 5990697.1409743,3557077.755 5990697.56,3557081.034 5990697.36,3557083.0441168 5990697.624945,3557086.59 5990698.766,3557094.821 5990701.243,3557100.8143628 5990703.2412507,3557102.096 5990700.368,3557095.249 5990698.411,3557084.046 5990694.889,3557079.817 5990694.135,3557074.894 5990692.049,3557068.29 5990688.631,3557061.571 5990686.852,3557040.427 5990680.052,3557015.566 5990670.805,3556987.198 5990662.834,3556959.256 5990656.033,3556934.926 5990651.463</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11768145593596"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 23</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811720392">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555029.029 5989328.136,3555052.252 5989327.589,3555027.5833425 5989308.4833611,3555006.079 5989334.971,3555029.029 5989328.136</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203208">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556262.281 5990391.16,3556239.805 5990379.918,3556243.75 5990386.057,3556261.2300121 5990394.0770531,3556281.203 5990404.055,3556284.315 5990401.078,3556262.281 5990391.16</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203160">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555889.474 5989778.368,3555864.114 5989772.514,3555863.949 5989778.002,3555889.641 5989784.065,3555909.9425715 5989789.3777869,3555934.305 5989795.4,3555960.1905362 5989806.6469427,3555962.629 5989801.662,3555936.415 5989790.929,3555911.786 5989784.222,3555889.474 5989778.368</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203214">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556254.949 5990409.799,3556276.933 5990419.337,3556276.259 5990415.459,3556256.1224121 5990406.5184366,3556238.709 5990398.072,3556233.628 5990399.863,3556254.949 5990409.799</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203178">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554782.256 5989097.571,3554777.882 5989092.61,3554763.2967236 5989119.8503468,3554781.687578 5989133.7036283,3554793.6539704 5989142.0124952,3554802.8744057 5989147.3412094,3554820.944814 5989156.0965346,3554837.3122594 5989162.3690344,3554846.0151806 5989165.3939585,3554855.5302706 5989169.054877,3554864.886158 5989172.1009799,3554876.8222463 5989174.8947433,3554880.239 5989157.684,3554871.782 5989154.183,3554863.908 5989148.93,3554856.035 5989144.845,3554847.87 5989139.884,3554832.706 5989130.546,3554819.583 5989122.959,3554811.126 5989117.414,3554800.045 5989109.827,3554782.256 5989097.571</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481172039">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554487.1436692 5989067.8841782,3554464.7748174 5989045.6619099,3554443.2257687 5989049.4090881,3554419.509631 5989052.3803512,3554391.9788963 5989055.1558887,3554392.083 5989068.316,3554422.558 5989072.98,3554446.514 5989069.733,3554467.975 5989068.734,3554487.188 5989074.977,3554502.66 5989084.216,3554512.142 5989090.459,3554518.381 5989095.453,3554526.865 5989101.446,3554537.096 5989108.687,3554540.84 5989110.685,3554554.564 5989118.176,3554579.518 5989126.916,3554603.724 5989131.662,3554587.4128486 5989121.6782301,3554571.8585072 5989103.2596824,3554561.4155614 5989091.4110067,3554552.9699663 5989084.0898918,3554540.1667505 5989076.8007661,3554526.7163868 5989071.2936222,3554516.3735015998 5989068.2674459,3554503.9162963 5989066.8992234,3554487.1436692 5989067.8841782</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203113">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555299.7935943 5989429.025209,3555277.84 5989419.531,3555293.077 5989433.376,3555310.066 5989450.026,3555329.507 5989466.851,3555312.5484201 5989448.6198138,3555299.7935943 5989429.025209</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481172034">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3554410.0425494 5988982.1077309,3554432.7130632 5988978.8991154,3554431.154 5988970.634,3554407.242 5988956.043,3554378.026 5988923.167,3554390.0075893 5988956.5011836,3554390.5469163 5988982.5064989,3554410.0425494 5988982.1077309</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203105">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555664.1410223 5989810.0899529,3555697.127 5989809.217,3555722.577 5989809.862,3555747.6349998 5989809.6902259,3555723.9917135 5989804.106717,3555700.0200921 5989797.7987727,3555664.1410223 5989810.0899529</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203135">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555516.44706 5989730.4114475995,3555539.593 5989756.027,3555561.5 5989771.823,3555581.795 5989784.074,3555610.467 5989796.646,3555636.561 5989804.383,3555664.1410223 5989810.0899529,3555638.3897909 5989799.8882345,3555614.0092714 5989788.6258617,3555587.6916464 5989774.0650996,3555566.1770702 5989762.1847286,3555543.5457703 5989749.0958551,3555516.44706 5989730.4114475995</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203185">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555980.429 5989985.745,3555979.6337585 5990000.6458437,3555983.0344765 5989986.2145786,3555986.2201039 5989974.0700214,3555987.7774687 5989963.0961502,3555987.9237543 5989951.544715,3555986.6104427 5989936.3170398995,3555983.5939299 5989922.4585679,3555977.5893785 5989906.5398493,3555971.327 5989925.748,3555974.097 5989938.619,3555976.867 5989952.281,3555979.044 5989962.974,3555980.429 5989973.073,3555980.429 5989985.745</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811720330">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555949.5457848 5989742.7069046,3556018.8395421 5989714.1473972,3556026.648 5989695.44,3555959.769 5989702.063,3555931.035 5989701.897,3555902.135 5989709.542,3555874.895 5989711.204,3555872.1964042 5989724.1504087,3555898.4365421 5989730.0002542,3555923.3617269 5989735.7457171,3555949.5457848 5989742.7069046</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203151">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556532.8011817 5990702.0820564,3556486.749 5990711.798,3556513.212 5990735.048,3556540.483 5990755.23,3556547.1 5990760.719,3556575.821 5990760.397,3556599.867 5990768.468,3556610.515 5990766.854,3556644.08 5990767.499,3556673.932 5990766.692,3556713.498 5990759.297,3556739.973 5990750.757,3556765.575 5990740.821,3556788.9286641 5990720.9237501,3556763.1463291 5990724.5933731,3556737.0479874 5990727.8063284,3556711.6295369 5990730.2455495,3556679.1481216 5990719.666712,3556652.6337679 5990716.8043258,3556623.609373 5990710.7481848,3556586.5341614 5990727.6754919,3556559.5061623 5990716.2790893,3556532.8011817 5990702.0820564</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203127">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553966.499 5988914.185,3553942.317 5988920.991,3553969.211865 5988937.3631158,3553996.298566 5988933.0113801,3554022.7968978 5988928.5949914,3554049.4843711 5988923.9754299,3554081.8398553 5988919.5163746,3554108.4475086 5988917.5327016,3554130.1658974 5988917.9434336,3554151.2279374 5988919.8011871,3554172.4660073 5988923.5534133,3554188.5670987 5988927.4650439,3554221.4888222 5988908.9488745,3554263.421 5988881.343,3554229.714 5988881.343,3554200.321 5988883.232,3554179.827 5988886.201,3554154.7629723 5988893.0585943,3554148.364 5988868.561,3554151.135 5988855.202,3554134.762 5988852.681,3554109.069 5988849.656,3554090.933 5988847.388,3554078.338 5988869.569,3554067.759 5988893.767,3554047.607 5988912.419,3554022.166 5988917.462,3553992.947 5988909.396,3553966.499 5988914.185</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203164">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3555403.645 5989674.142,3555389.47 5989687.038,3555444.558 5989710.572,3555485.472 5989721.533,3555516.44706 5989730.4114475995,3555493.166305 5989711.7761502,3555470.1934806 5989693.2845531,3555448.8808328 5989671.7186105,3555403.645 5989674.142</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls11707777217344"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 24</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203191">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559305.868 5991374.446,3559331.6736425 5991376.8456367,3559355.857 5991377.389,3559378.6088436 5991376.8749307,3559378.607 5991373.04,3559355.641 5991373.664,3559331.684 5991373.54,3559313.312 5991372.546,3559295.561 5991370.56,3559279.3 5991367.705,3559278.8634961 5991370.1982862,3559305.868 5991374.446</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203154">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556243.75 5990386.057,3556239.805 5990379.918,3556219.974 5990370.44,3556194.195 5990358.097,3556173.812 5990348.729,3556157.837 5990340.794,3556141.642 5990331.095,3556128.973 5990321.838,3556112.227 5990308.503,3556096.471 5990292.523,3556081.708 5990274.891,3556067.055 5990255.054,3556055.045 5990236.54,3556045.24 5990215.27,3556038.96 5990196.535,3556033.341 5990178.021,3556029.815 5990160.498,3556026.51 5990142.204,3556026.18 5990124.241,3556026.012 5990108.829,3556027.113 5990091.191,3556028.093 5990073.918,3556023.2347701 5990073.0934279,3556020.683 5990091.025,3556018.845 5990109.083,3556017.9990953 5990123.730435,3556018.953 5990142.385,3556021.115 5990161.524,3556024.8141205 5990179.8079383,3556031.058 5990199.369,3556037.8518086 5990217.8575263,3556049.324 5990240.782,3556059.8250092 5990259.455857,3556076.026 5990280.388,3556091.422167 5990297.8764473,3556107.769 5990313.216,3556125.6084491 5990327.6015063,3556137.368 5990334.458,3556155.277 5990344.113,3556171.9709194 5990352.4496706,3556190.666 5990361.384,3556219.621 5990375.222,3556243.75 5990386.057</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811720326">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553857.8911965 5989072.8020144,3553858.2992824 5989087.3224427,3553893.506 5989055.126,3553857.8911965 5989072.8020144</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811720398">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3558546.9964746 5991140.9746837,3558546.73 5991137.931,3558523.648 5991139.222,3558499.956 5991142.135,3558478.583 5991144.218,3558459.62 5991146.849,3558452.687 5991150.689,3558476.525 5991148.243,3558500.3021581 5991145.2282614,3558523.436 5991142.281,3558546.9964746 5991140.9746837</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203177">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3556212.969 5990389.663,3556233.628 5990399.863,3556238.709 5990398.072,3556214.688 5990387.022,3556185.84 5990373.828,3556166.7803385 5990365.527238,3556151.738 5990358.166,3556130.933 5990347.331,3556118.2693766 5990338.9430712,3556099.512 5990324.802,3556081.3695971 5990308.1785382,3556064.444 5990288.434,3556048.6612505 5990266.2198279,3556046.103 5990267.92,3556062.261 5990289.911,3556079.345 5990309.782,3556097.885 5990327.401,3556116.689 5990341.842,3556140.132 5990355.352,3556165.559 5990368.334,3556186.35 5990377.608,3556212.969 5990389.663</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203217">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559706.481 5991523.534,3559698.502 5991504.333,3559688.742 5991484.951,3559681.64 5991470.521,3559672.527 5991454.441,3559663.198 5991441.089,3559654.445 5991429.888,3559640.595 5991414.454,3559626.962 5991402.464,3559613.83 5991393.635,3559593.951 5991382.077,3559592.744 5991384.521,3559612.977 5991396.328,3559624.9058594 5991404.6657636,3559638.568 5991416.37,3559652.652 5991432.245,3559660.2723316 5991442.8553249,3559668.226 5991455.264,3559678.243 5991473.123,3559684.9307623 5991486.5192788,3559694.547 5991505.98,3559703.3516116 5991525.2572776005,3559714.84 5991548.944,3559724.4278832 5991571.1999583,3559735.022 5991588.409,3559746.35 5991603.387,3559755.4714373 5991614.1735268,3559765.491 5991622.534,3559781.507 5991632.954,3559794.3266761 5991639.6388237,3559808.201 5991644.675,3559825.257 5991649.104,3559839.6633879 5991650.9878493,3559857.29 5991650.797,3559874.347 5991648.713,3559890.0582749 5991643.8300314,3559902.34 5991638.162,3559917.053 5991631.129,3559929.1572409 5991625.2146253,3559945.182 5991617.147,3559963.478 5991607.996,3559978.2058007 5991601.2281214,3559993.801 5991593.395,3560010.145 5991585.272,3560026.2479324 5991577.4540234,3560048.155 5991566.238,3560070.0062353 5991555.0051939,3560088.21 5991546.63,3560090.303 5991542.758,3560068.563 5991552.375,3560046.677 5991563.644,3560024.862 5991574.48,3560008.859 5991582.52,3559991.922 5991590.846,3559976.422 5991597.809,3559961.424 5991605.776,3559943.483 5991614.175,3559927.122 5991621.712,3559907.39 5991631.042,3559888.588 5991640.159,3559873.876 5991645.325,3559857.227 5991647.407,3559839.864 5991647.119,3559826.229 5991645.181,3559809.723 5991641.591,3559795.445 5991636.851,3559775.853 5991625.868,3559757.555 5991611.942,3559744.638 5991595.862,3559738.108 5991585.884,3559727.634 5991570.02,3559719.526 5991552.144,3559712.781 5991537.213,3559706.481 5991523.534</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon11885481172036">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559704.815 5991554.414,3559705.242 5991562.662,3559711.745 5991577.814,3559721.002 5991594.957,3559733.856 5991613.782,3559745.484 5991626.026,3559747.6057582 5991623.5464019,3559736.845 5991611.853,3559724.215 5991593.618,3559714.1564494 5991575.9485901,3559704.815 5991554.414</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811720348">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3553691.666 5989068.314,3553711.932 5989057.576,3553696.221 5989052.188,3553679.836 5989049.942,3553678.9512424 5989065.3559573,3553676.9360964 5989079.7221423,3553674.6537517 5989092.6376274,3553672.587 5989104.471,3553680.443 5989104.247,3553683.586 5989094.814,3553687.401 5989082.912,3553691.666 5989068.314</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811720350">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3560033.588 5991592.115,3560055.673 5991581.118,3560077.366 5991571.005,3560097.686 5991562.069,3560095.241 5991559.419,3560075.7906567 5991567.9385884,3560054.334 5991578.068,3560032.0432845 5991588.9162321,3560016.106 5991596.48,3559999.352 5991604.603,3559983.3527606 5991612.3972578,3559968.823 5991619.101,3559949.91 5991628.457,3559934.5917024 5991636.7296553,3559923.173 5991642.069,3559916.823 5991648.064,3559936.1 5991639.008,3559951.36 5991631.119,3559970.08 5991623.046,3559985.196 5991615.879,3560009.539 5991603.703,3560033.588 5991592.115</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon1188548117203106">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3557961.378 5990988.457,3557982.4021457 5991000.8974514,3558003.972 5991013.357,3558025.8509344 5991023.7815784,3558046.414 5991032.719,3558065.763 5991041.057,3558068.737 5991038.306,3558049.232 5991028.909,3558027.824 5991019.867,3558005.94 5991009.398,3557984.175 5990997.382,3557963.087 5990985.555,3557941.127 5990971.584,3557928.355 5990963.128,3557913.93 5990952.649,3557900.239 5990940.977,3557884.711 5990926.455,3557866.977 5990910.186,3557854.482 5990898.787,3557833.441 5990879.394,3557831.261 5990882.194,3557852.465 5990901.593,3557864.1611693 5990912.8848989,3557882.486 5990929.794,3557898.1182813 5990943.7957216,3557912.285 5990954.706,3557927.635 5990966.049,3557939.2088538 5990973.9421976,3557961.378 5990988.457</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
   <simBase:roughnessLayerMember>
    <simBase:RoughnessPolygon gml:id="RoughnessPolygon118854811720347">
     <simBase:polygonGeometry>
      <gml:Polygon xmlns:ns2="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns3="http://www.isotc211.org/2005/gmd" xmlns:ns4="http://www.isotc211.org/2005/gco" xmlns:ns5="http://www.isotc211.org/2005/gts" xmlns:ns6="http://www.isotc211.org/2005/gss" xmlns:ns7="http://www.isotc211.org/2005/gsr" xmlns:ns8="http://www.w3.org/2001/SMIL20/Language">
       <gml:exterior>
        <gml:LinearRing>
         <gml:coordinates ts="," decimal="." cs=" ">3559804.685 5991656.137,3559805.156 5991659.39,3559823.441 5991663.522,3559841.037 5991665.742,3559858.922 5991666.032,3559859.243 5991663.04,3559840.4910148 5991662.9432368,3559823.695 5991660.826,3559804.685 5991656.137</gml:coordinates>
        </gml:LinearRing>
       </gml:exterior>
      </gml:Polygon>
     </simBase:polygonGeometry>
     <simBase:roughnessClassMember xlink:href="project:/.metadata/roughness.gml#RoughnessCls117812331278327"/>
     <simBase:roughnessStyle>Vorland mit Bewuchs 25</simBase:roughnessStyle>
     <simBase:correction_ks/>
     <simBase:correction_axay/>
     <simBase:correction_dp/>
    </simBase:RoughnessPolygon>
   </simBase:roughnessLayerMember>
  </simBase:RoughnessLayer>
 </simBase:roughnessLayerCollection>
 <simBase:riverProfileNetworkCollectionMember>
  <simBase:RiverProfileNetworkCollection gml:id="RiverProfileNetworkCollection11708754954840"/>
 </simBase:riverProfileNetworkCollectionMember>
</simBase:TerrainModel>
