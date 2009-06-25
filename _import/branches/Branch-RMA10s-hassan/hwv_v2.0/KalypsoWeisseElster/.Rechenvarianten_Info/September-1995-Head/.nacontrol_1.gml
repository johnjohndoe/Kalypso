<?xml version="1.0" encoding="UTF-8"?>
<ns5:NAModellControl xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns5="org.kalypso.namodell.control" xs:schemaLocation="org.kalypso.namodell.control project:/.model/schema/nacontrol.xsd" gml:fid="ID000000">
 <ns5:description>kein Pegel verfuegbar</ns5:description>
 <ns5:name>Bad Elster</ns5:name>
 <ns5:tmp>true</ns5:tmp>
 <ns5:pre>true</ns5:pre>
 <ns5:sch>false</ns5:sch>
 <ns5:bof>true</ns5:bof>
 <ns5:bsp>false</ns5:bsp>
 <ns5:gws>false</ns5:gws>
 <ns5:qgs>true</ns5:qgs>
 <ns5:qgg>false</ns5:qgg>
 <ns5:qna>false</ns5:qna>
 <ns5:qif>false</ns5:qif>
 <ns5:qvs>false</ns5:qvs>
 <ns5:qbs>false</ns5:qbs>
 <ns5:qt1>false</ns5:qt1>
 <ns5:qtg>false</ns5:qtg>
 <ns5:qgw>false</ns5:qgw>
 <ns5:vet>false</ns5:vet>
 <ns5:qmr/>
 <ns5:hyd>false</ns5:hyd>
 <ns5:bil>false</ns5:bil>
 <ns5:nmq>false</ns5:nmq>
 <ns5:spi>false</ns5:spi>
 <ns5:sup/>
 <ns5:automaticCallibration>false</ns5:automaticCallibration>
 <ns5:Catchments>//:Catchment[@fid="Catchment1800"]</ns5:Catchments>
 <ns5:CatchmentsBianf>1.0</ns5:CatchmentsBianf>
 <ns5:CatchmentsFaktorRetobTetint>1.0</ns5:CatchmentsFaktorRetobTetint>
 <ns5:CatchmentsFaktn>1.0</ns5:CatchmentsFaktn>
 <ns5:CatchmentsFaktorAigw>1.0</ns5:CatchmentsFaktorAigw>
 <ns5:KMChannels>//:KMChannel[@fid="KMChannel1800"]</ns5:KMChannels>
 <ns5:KMChannelsFaktorRkf>1.0</ns5:KMChannelsFaktorRkf>
 <ns5:KMChannelsFaktorRnf>1.0</ns5:KMChannelsFaktorRnf>
 <ns5:rootNode>Node1800</ns5:rootNode>
 <pegelZR xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Q" xlink:href="Pegel/Pegel_BadElster.zml" xlink:type="simple"/>
 </pegelZR>

 <qberechnetZR xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Pegel_BadElster.zml" xlink:type="simple"/>
 </qberechnetZR>

 <ns5:accuracyPrediction>30.0</ns5:accuracyPrediction>
 <ns5:useOffsetStartPrediction>false</ns5:useOffsetStartPrediction>
 <ns5:useOffsetEndPrediction>false</ns5:useOffsetEndPrediction>
 <qAblageSpurMittlerer xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_BadElster.zml" xlink:type="simple"/>
 </qAblageSpurMittlerer>

 <qAblageSpurOberer xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_BadElster.zml" xlink:type="simple"/>
 </qAblageSpurOberer>

 <qAblageSpurUnterer xmlns="org.kalypso.namodell.control">
  <obslink:TimeseriesLink xmlns:obslink="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" xlink:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_BadElster.zml" xlink:type="simple"/>
 </qAblageSpurUnterer>

</ns5:NAModellControl>
