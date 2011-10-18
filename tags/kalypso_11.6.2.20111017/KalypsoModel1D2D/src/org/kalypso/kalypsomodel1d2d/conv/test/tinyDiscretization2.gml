<?xml version="1.0" encoding="UTF-8"?>
<DiscretisationModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" xmlns:swe="http://www.opengis.net/swe" xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d" gml:id="root">
 <element>
  <PolyElement gml:id="PolyElement1">
   <elementContainer xlink:href="#CalculationUnit2D1"/>
   <roughnessClsID>RoughnessCls11781233116313</roughnessClsID>
   <roughnessStyle>Fluss</roughnessStyle>
   <correction_ks/>
   <correction_axay/>
   <correction_dp/>
   <directedEdge xlink:href="#Edge1"/>
   <directedEdge xlink:href="#Edge2"/>
   <directedEdge xlink:href="#Edge3"/>
   <directedEdge xlink:href="#Edge4"/>
  </PolyElement>
 </element>
 <edge>
  <Edge gml:id="Edge1">
   <directedNode xlink:href="#Node1"/>
   <directedNode xlink:href="#Node2"/>
   <edgeContainer xlink:href="#PolyElement1"/>
  </Edge>
 </edge>
 <edge>
  <Edge gml:id="Edge2">
   <directedNode xlink:href="#Node2"/>
   <directedNode xlink:href="#Node3"/>
   <edgeContainer xlink:href="#PolyElement1"/>
  </Edge>
 </edge>
  <edge>
  <Edge gml:id="Edge3">
   <directedNode xlink:href="#Node3"/>
   <directedNode xlink:href="#Node4"/>
   <edgeContainer xlink:href="#PolyElement1"/>
  </Edge>
 </edge>
 <edge>
  <Edge gml:id="Edge4">
   <directedNode xlink:href="#Node4"/>
   <directedNode xlink:href="#Node1"/>
  </Edge>
 </edge> 
<node>
  <Node gml:id="Node1">
   <gml:pointProperty>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gts" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," cs=" " decimal=".">0.0 0.0 0.0</gml:coordinates>
    </gml:Point>

   </gml:pointProperty>
   <nodeContainer xlink:href="#Edge1"/>
   <nodeContainer xlink:href="#Edge4"/>
  </Node>
 </node>
 <node>
  <Node gml:id="Node2">
   <gml:pointProperty>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gts" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," cs=" " decimal=".">0.0 1.0 1.0</gml:coordinates>
    </gml:Point>

   </gml:pointProperty>
   <nodeContainer xlink:href="#Edge1"/>
   <nodeContainer xlink:href="#Edge2"/>
  </Node>
 </node>
 <node>
  <Node gml:id="Node3">
   <gml:pointProperty>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gts" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," cs=" " decimal=".">0.0 1.0 0.0</gml:coordinates>
    </gml:Point>

   </gml:pointProperty>
   <nodeContainer xlink:href="#Edge3"/>
   <nodeContainer xlink:href="#Edge4"/>
  </Node>
 </node>
 <node>
  <Node gml:id="Node4">
   <gml:pointProperty>
    <gml:Point xmlns:ns1="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns2="http://www.isotc211.org/2005/gmd" xmlns:ns3="http://www.isotc211.org/2005/gco" xmlns:ns4="http://www.isotc211.org/2005/gts" xmlns:ns5="http://www.isotc211.org/2005/gss" xmlns:ns6="http://www.isotc211.org/2005/gsr" xmlns:ns7="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
     <gml:coordinates ts="," cs=" " decimal=".">1.0 0.0 1.0</gml:coordinates>
    </gml:Point>
   </gml:pointProperty>
   <nodeContainer xlink:href="#Edge4"/>
   <nodeContainer xlink:href="#Edge1"/>
  </Node>
 </node>
   <mesh/>
  <complexElement>
  <CalculationUnit2D gml:id="CalculationUnit2D1">
   <gml:description/>
   <gml:name>Teich</gml:name>
   <element xlink:href="#PolyElement1"/>
  </CalculationUnit2D>
 </complexElement>
</DiscretisationModel>
 