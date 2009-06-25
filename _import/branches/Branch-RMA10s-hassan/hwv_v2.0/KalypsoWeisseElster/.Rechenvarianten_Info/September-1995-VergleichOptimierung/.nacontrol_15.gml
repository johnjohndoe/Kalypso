<?xml version="1.0" encoding="UTF-8"?>
<rrmControl:NAModellControl xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:rrmControl="org.kalypso.namodell.control" xs:schemaLocation="org.kalypso.namodell.control project:/.model/schema/nacontrol.xsd" gml:fid="ID000000">
 <rrmControl:name>Oberthau</rrmControl:name>
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
 <rrmControl:Catchments>//rrm:Catchment[@gml:fid="Catchment1007" or @gml:fid="Catchment7008" or @gml:fid="Catchment1008" or @gml:fid="Catchment3004" or @gml:fid="Catchment3003" or @gml:fid="Catchment3002" or @gml:fid="Catchment7007" or @gml:fid="Catchment3001" or @gml:fid="Catchment1013" or @gml:fid="Catchment1011" or @gml:fid="Catchment1014" or @gml:fid="Catchment1005" or @gml:fid="Catchment1004" or @gml:fid="Catchment1010" or @gml:fid="Catchment1003" or @gml:fid="Catchment7006" or @gml:fid="Catchment7005" or @gml:fid="Catchment7004" or @gml:fid="Catchment7003" or @gml:fid="Catchment7002" or @gml:fid="Catchment7001" or @gml:fid="Catchment1002" or @gml:fid="Catchment1009" or @gml:fid="Catchment1001"]</rrmControl:Catchments>
 <rrmControl:CatchmentsBianf>1.0</rrmControl:CatchmentsBianf>
 <rrmControl:CatchmentsFaktorRetobTetint>1.0</rrmControl:CatchmentsFaktorRetobTetint>
 <rrmControl:CatchmentsFaktn>1.0</rrmControl:CatchmentsFaktn>
 <rrmControl:CatchmentsFaktorAigw>1.0</rrmControl:CatchmentsFaktorAigw>
 <rrmControl:KMChannels>//rrm:KMChannel[@gml:fid="KMChannel1001" or @gml:fid="KMChannel1002" or @gml:fid="KMChannel1003" or @gml:fid="KMChannel1004" or @gml:fid="KMChannel1005" or @gml:fid="KMChannel1007" or @gml:fid="KMChannel1008" or @gml:fid="KMChannel1009" or @gml:fid="KMChannel1010" or @gml:fid="KMChannel1011" or @gml:fid="KMChannel1013" or @gml:fid="KMChannel1014" or @gml:fid="KMChannel7001" or @gml:fid="KMChannel7002" or @gml:fid="KMChannel7003" or @gml:fid="KMChannel7004" or @gml:fid="KMChannel7005" or @gml:fid="KMChannel7006" or @gml:fid="KMChannel7007" or @gml:fid="KMChannel7008" or @gml:fid="KMChannel3001" or @gml:fid="KMChannel3002" or @gml:fid="KMChannel3003" or @gml:fid="KMChannel3004"]</rrmControl:KMChannels>
 <rrmControl:KMChannelsFaktorRkf>0.5</rrmControl:KMChannelsFaktorRkf>
 <rrmControl:KMChannelsFaktorRnf>0.5</rrmControl:KMChannelsFaktorRnf>
 <rrmControl:rootNode>Node1001</rrmControl:rootNode>
 <rrmControl:resultForRootNodeOnly/>
 <pegelZR xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Q" xlink:href="Pegel/Pegel_Oberthau.zml" xlink:type="simple"/>
 </pegelZR>

 <qberechnetZR xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Pegel_Oberthau.zml" xlink:type="simple"/>
 </qberechnetZR>

 <rrmControl:accuracyPrediction>30.0</rrmControl:accuracyPrediction>
 <rrmControl:useOffsetStartPrediction>false</rrmControl:useOffsetStartPrediction>
 <rrmControl:useOffsetEndPrediction>false</rrmControl:useOffsetEndPrediction>
 <qAblageSpurMittlerer xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Oberthau.zml" xlink:type="simple"/>
 </qAblageSpurMittlerer>

 <qAblageSpurOberer xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Oberthau.zml" xlink:type="simple"/>
 </qAblageSpurOberer>

 <qAblageSpurUnterer xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Oberthau.zml" xlink:type="simple"/>
 </qAblageSpurUnterer>

</rrmControl:NAModellControl>
