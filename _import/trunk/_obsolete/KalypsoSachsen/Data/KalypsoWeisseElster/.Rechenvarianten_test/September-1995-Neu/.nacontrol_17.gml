<?xml version="1.0" encoding="UTF-8"?>
<NAModellControl fid="ID000000" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns="org.kalypso.namodell.control" xmlns:obslink="obslink.zml.kalypso.org" xmlns:gml="http://www.opengis.net/gml" xsi:schemaLocation="org.kalypso.namodell.control project:/.model/schema/nacontrol.xsd">
  <description>Beschreibung</description>
  <name>Goessnitz</name>
  <tmp>true</tmp>
  <pre>true</pre>
  <sch>false</sch>
  <bof>true</bof>
  <bsp>false</bsp>
  <gws>false</gws>
  <qgs>true</qgs>
  <qgg>false</qgg>
  <qna>false</qna>
  <qif>false</qif>
  <qvs>false</qvs>
  <qbs>false</qbs>
  <qt1>false</qt1>
  <qtg>false</qtg>
  <qgw>false</qgw>
  <vet>false</vet>
  <hyd>false</hyd>
  <bil>false</bil>
  <nmq>false</nmq>
  <spi>false</spi>
  <sup/>
  <automaticCallibration>false</automaticCallibration>
  <Catchments><![CDATA[//rrm:Catchment[@gml:fid="Catchment7204" or @gml:fid="Catchment7203" or @gml:fid="Catchment7202" or @gml:fid="Catchment7201" or @gml:fid="Catchment7200"]]]></Catchments>
  <CatchmentsBianf>1.0</CatchmentsBianf>
  <CatchmentsFaktorRetobTetint>1.0</CatchmentsFaktorRetobTetint>
  <CatchmentsFaktn>1.0</CatchmentsFaktn>
  <CatchmentsFaktorAigw>1.0</CatchmentsFaktorAigw>
  <KMChannels><![CDATA[//rrm:KMChannel[@gml:fid="KMChannel7200" or @gml:fid="KMChannel7201" or @gml:fid="KMChannel7202" or @gml:fid="KMChannel7203" or @gml:fid="KMChannel7204"]]]></KMChannels>
  <KMChannelsFaktorRkf>1.0</KMChannelsFaktorRkf>
  <KMChannelsFaktorRnf>1.0</KMChannelsFaktorRnf>
  <rootNode>Node7200</rootNode>
  <pegelZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Pegel/Pegel_Goessnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Q"/>
  </pegelZR>
  <qberechnetZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Pegel_Goessnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qberechnetZR>
  <accuracyPrediction>30.0</accuracyPrediction>
  <useOffsetStartPrediction>false</useOffsetStartPrediction>
  <useOffsetEndPrediction>false</useOffsetEndPrediction>
  <qAblageSpurMittlerer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Goessnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurMittlerer>
  <qAblageSpurOberer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Goessnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurOberer>
  <qAblageSpurUnterer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Goessnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurUnterer>
</NAModellControl>
