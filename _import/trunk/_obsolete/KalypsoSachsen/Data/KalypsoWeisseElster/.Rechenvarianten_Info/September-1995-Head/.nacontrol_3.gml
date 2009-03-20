<?xml version="1.0" encoding="UTF-8"?>
<NAModellControl fid="ID000000" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns="org.kalypso.namodell.control" xmlns:obslink="obslink.zml.kalypso.org" xmlns:gml="http://www.opengis.net/gml" xsi:schemaLocation="org.kalypso.namodell.control project:/.model/schema/nacontrol.xsd">
  <description>Beschreibung</description>
  <name>Oelsnitz</name>
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
  <Catchments><![CDATA[//:Catchment[@fid="Catchment1600" or @fid="Catchment1601"]]]></Catchments>
  <CatchmentsBianf>1.056823185513936</CatchmentsBianf>
  <CatchmentsFaktorRetobTetint>1.2750895615325974</CatchmentsFaktorRetobTetint>
  <CatchmentsFaktn>1.4704225800149069</CatchmentsFaktn>
  <CatchmentsFaktorAigw>1.4677754284403695</CatchmentsFaktorAigw>
  <KMChannels><![CDATA[//:KMChannel[@fid="KMChannel1600" or @fid="KMChannel1601"]]]></KMChannels>
  <KMChannelsFaktorRkf>1.8639867568667658</KMChannelsFaktorRkf>
  <KMChannelsFaktorRnf>0.7925100791849301</KMChannelsFaktorRnf>
  <rootNode>Node1600</rootNode>
  <pegelZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Pegel/Pegel_Oelsnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Q"/>
  </pegelZR>
  <qberechnetZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Pegel_Oelsnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qberechnetZR>
  <accuracyPrediction>30.0</accuracyPrediction>
  <useOffsetStartPrediction>false</useOffsetStartPrediction>
  <useOffsetEndPrediction>false</useOffsetEndPrediction>
  <qAblageSpurMittlerer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Oelsnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurMittlerer>
  <qAblageSpurOberer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Oelsnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurOberer>
  <qAblageSpurUnterer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Oelsnitz.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurUnterer>
</NAModellControl>
