<?xml version="1.0" encoding="UTF-8"?>
<rrmControl:NAModellControl xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:rrmControl="org.kalypso.namodell.control" xs:schemaLocation="org.kalypso.namodell.control project:/.model/schema/nacontrol.xsd" gml:fid="ID000000">
 <rrmControl:name>Adorf</rrmControl:name>
 <rrmControl:description>Beschreibung</rrmControl:description>
 <rrmControl:editor/>
 <rrmControl:comment/>
 <rrmControl:calctime/>
 <rrmControl:startsimulation/>
 <rrmControl:startforecast/>
 <rrmControl:endsimulation/>
 <rrmControl:timeStep/>
 <rrmControl:tmp>true</rrmControl:tmp>
 <rrmControl:pre>true</rrmControl:pre>
 <rrmControl:sch>false</rrmControl:sch>
 <rrmControl:bof>true</rrmControl:bof>
 <rrmControl:bsp>false</rrmControl:bsp>
 <rrmControl:gws>false</rrmControl:gws>
 <rrmControl:qgs>true</rrmControl:qgs>
 <rrmControl:qgg>false</rrmControl:qgg>
 <rrmControl:qna>false</rrmControl:qna>
 <rrmControl:qif>false</rrmControl:qif>
 <rrmControl:qvs>false</rrmControl:qvs>
 <rrmControl:qbs>false</rrmControl:qbs>
 <rrmControl:qt1>false</rrmControl:qt1>
 <rrmControl:qtg>false</rrmControl:qtg>
 <rrmControl:qgw>false</rrmControl:qgw>
 <rrmControl:kap/>
 <rrmControl:vet>false</rrmControl:vet>
 <rrmControl:hyd>false</rrmControl:hyd>
 <rrmControl:bil>false</rrmControl:bil>
 <rrmControl:nmq>false</rrmControl:nmq>
 <rrmControl:spi>false</rrmControl:spi>
 <rrmControl:sub/>
 <rrmControl:sph/>
 <rrmControl:spv/>
 <rrmControl:spn/>
 <rrmControl:vep/>
 <rrmControl:sup/>
 <rrmControl:automaticCallibration>false</rrmControl:automaticCallibration>
 <rrmControl:Catchments>//rrm:Catchment[@gml:fid="Catchment1700"]</rrmControl:Catchments>
 <rrmControl:CatchmentsBianf>1.056823185513936</rrmControl:CatchmentsBianf>
 <rrmControl:CatchmentsFaktorRetobTetint>1.2750895615325974</rrmControl:CatchmentsFaktorRetobTetint>
 <rrmControl:CatchmentsFaktn>1.4704225800149069</rrmControl:CatchmentsFaktn>
 <rrmControl:CatchmentsFaktorAigw>1.4677754284403695</rrmControl:CatchmentsFaktorAigw>
 <rrmControl:KMChannels>//rrm:KMChannel[@gml:fid="KMChannel1700"]</rrmControl:KMChannels>
 <rrmControl:KMChannelsFaktorRkf>1.8639867568667658</rrmControl:KMChannelsFaktorRkf>
 <rrmControl:KMChannelsFaktorRnf>0.7925100791849301</rrmControl:KMChannelsFaktorRnf>
 <rrmControl:rootNode>Node1700</rrmControl:rootNode>
 <rrmControl:resultForRootNodeOnly/>
 <pegelZR xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Q" xlink:href="Pegel/Pegel_Adorf.zml" xlink:type="simple"/>
 </pegelZR>

 <qberechnetZR xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Pegel_Adorf.zml" xlink:type="simple"/>
 </qberechnetZR>

 <rrmControl:accuracyPrediction>30.0</rrmControl:accuracyPrediction>
 <rrmControl:useOffsetStartPrediction>false</rrmControl:useOffsetStartPrediction>
 <rrmControl:useOffsetEndPrediction>false</rrmControl:useOffsetEndPrediction>
 <qAblageSpurMittlerer xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Adorf.zml" xlink:type="simple"/>
 </qAblageSpurMittlerer>

 <qAblageSpurOberer xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Adorf.zml" xlink:type="simple"/>
 </qAblageSpurOberer>

 <qAblageSpurUnterer xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Adorf.zml" xlink:type="simple"/>
 </qAblageSpurUnterer>

</rrmControl:NAModellControl>
