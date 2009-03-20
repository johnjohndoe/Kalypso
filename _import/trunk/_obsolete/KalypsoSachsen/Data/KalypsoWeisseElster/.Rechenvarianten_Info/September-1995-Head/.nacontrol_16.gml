<?xml version="1.0" encoding="UTF-8"?>
<NAModellControl fid="ID000000" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns="org.kalypso.namodell.control" xmlns:obslink="obslink.zml.kalypso.org" xmlns:gml="http://www.opengis.net/gml" xsi:schemaLocation="org.kalypso.namodell.control project:/.model/schema/nacontrol.xsd">
  <description>Beschreibung</description>
  <name>Neukirchen</name>
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
  <Catchments><![CDATA[//:Catchment[@fid="Catchment7306" or @fid="Catchment7305" or @fid="Catchment7302" or @fid="Catchment7303" or @fid="Catchment7304" or @fid="Catchment7300"]]]></Catchments>
  <CatchmentsBianf>0.5012313244319353</CatchmentsBianf>
  <CatchmentsFaktorRetobTetint>1.2551059727466938</CatchmentsFaktorRetobTetint>
  <CatchmentsFaktn>1.4610516598506247</CatchmentsFaktn>
  <CatchmentsFaktorAigw>1.583913423302949</CatchmentsFaktorAigw>
  <KMChannels><![CDATA[//:KMChannel[@fid="KMChannel7300" or @fid="KMChannel7302" or @fid="KMChannel7303" or @fid="KMChannel7304" or @fid="KMChannel7305" or @fid="KMChannel7306"]]]></KMChannels>
  <KMChannelsFaktorRkf>1.3007802377439055</KMChannelsFaktorRkf>
  <KMChannelsFaktorRnf>0.7204084957349067</KMChannelsFaktorRnf>
  <rootNode>Node7300</rootNode>
  <pegelZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Pegel/Pegel_Neukirchen.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Q"/>
  </pegelZR>
  <qberechnetZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Pegel_Neukirchen.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Q"/>
  </qberechnetZR>
  <accuracyPrediction>30.0</accuracyPrediction>
  <useOffsetStartPrediction>false</useOffsetStartPrediction>
  <useOffsetEndPrediction>false</useOffsetEndPrediction>
  <qAblageSpurMittlerer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Neukirchen.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurMittlerer>
  <qAblageSpurOberer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Neukirchen.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurOberer>
  <qAblageSpurUnterer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Neukirchen.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurUnterer>
</NAModellControl>
