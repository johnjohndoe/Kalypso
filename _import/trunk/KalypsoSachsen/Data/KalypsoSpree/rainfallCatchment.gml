<?xml version="1.0" encoding="UTF-8"?>
<gml:FeatureCollection xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:rcm="org.kalypso.model.rcm" gml:id="root">
 <gml:featureMember>
  <rcm:OmbrometerRainfallGenerator gml:id="ombrometerGenerator">
   <gml:description>Übernahme von Niederschlägen aus Ombrometermessungen mit der Thiessen-Methode</gml:description>
   <gml:name>Ombrometermessung</gml:name>
   <rcm:ombrometerCollection xlink:href="ombrometer.gml#root"/>
   <rcm:ombrometerFeaturePath>ombrometerMember</rcm:ombrometerFeaturePath>
   <rcm:timeseriesLinkPath>precipitationLink1</rcm:timeseriesLinkPath>
   <rcm:areaPath>affectedArea</rcm:areaPath>
  </rcm:OmbrometerRainfallGenerator>
 </gml:featureMember>
 <gml:featureMember>
  <rcm:RasterServiceRainfallGenerator/>
 </gml:featureMember>
</gml:FeatureCollection>
