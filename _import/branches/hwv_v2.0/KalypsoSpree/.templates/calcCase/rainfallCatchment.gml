<?xml version="1.0" encoding="UTF-8"?>
<gml:FeatureCollection xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:rcm="org.kalypso.model.rcm" xmlns:ras="org.kalypso.model.rcm.rasterservice" gml:id="root">
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
   <gml:description>Gewähltes DWD-Raster: LMK+LM</gml:description>
   <ras:entryMember>
    <ras:RasterServiceEntry gml:id="RasterServiceEntry12187329294290">
     <gml:description>Hallo</gml:description>
     <ras:entryId>id1</ras:entryId>
     <ras:coveredArea/>
    </ras:RasterServiceEntry>
   </ras:entryMember>
   <ras:entryMember>
    <ras:RasterServiceEntry gml:id="RasterServiceEntry12187329294292">
     <gml:description>Hallo</gml:description>
     <ras:entryId>id1</ras:entryId>
     <ras:coveredArea/>
    </ras:RasterServiceEntry>
   </ras:entryMember>
   <ras:entryMember>
    <ras:RasterServiceEntry gml:id="RasterServiceEntry12187329294295">
     <gml:description>Hallo</gml:description>
     <ras:entryId>id1</ras:entryId>
     <ras:coveredArea/>
    </ras:RasterServiceEntry>
   </ras:entryMember>
  </ras:RasterServiceRainfallGenerator>
 </gml:featureMember>
</gml:FeatureCollection>
