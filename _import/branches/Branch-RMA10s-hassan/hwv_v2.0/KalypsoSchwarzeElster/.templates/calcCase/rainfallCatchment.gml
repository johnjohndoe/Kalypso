<?xml version="1.0" encoding="UTF-8"?>
<gml:FeatureCollection xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:swe="http://www.opengis.net/swe" xmlns:ns7="org.kalypso.raster.database" xmlns:om="http://www.opengis.net/om" xmlns:ras="org.kalypso.model.rcm.rasterservice" xmlns:rcm="org.kalypso.model.rcm" xmlns:commonShp="org.kalypso.gml.common" xmlns:ombr="org.kalypso.model.rcm.ombrometer" gml:id="root">
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
  <ras:RasterServiceRainfallGenerator gml:id="rasterGenerator">
   <gml:description/>
   <gml:name>DWD-Prognose</gml:name>
  </ras:RasterServiceRainfallGenerator>
 </gml:featureMember>
</gml:FeatureCollection>
