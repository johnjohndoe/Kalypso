<?xml version="1.0" encoding="UTF-8"?>
<NAModellControl fid="ID000000" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns="org.kalypso.namodell.control" xmlns:obslink="obslink.zml.kalypso.org" xmlns:gml="http://www.opengis.net/gml" xsi:schemaLocation="org.kalypso.namodell.control project:/.model/schema/nacontrol.xsd">
  <description>Beschreibung</description>
  <name>Weida</name>
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
  <Catchments><![CDATA[//:Catchment[@fid="Catchment2004" or @fid="Catchment2005" or @fid="Catchment2003" or @fid="Catchment2006" or @fid="Catchment2007" or @fid="Catchment2001"]]]></Catchments>
  <CatchmentsBianf>1.0</CatchmentsBianf>
  <CatchmentsFaktorRetobTetint>1.0</CatchmentsFaktorRetobTetint>
  <CatchmentsFaktn>1.0</CatchmentsFaktn>
  <CatchmentsFaktorAigw>1.0</CatchmentsFaktorAigw>
  <KMChannels><![CDATA[//:KMChannel[@fid="KMChannel2001" or @fid="KMChannel2003" or @fid="KMChannel2004" or @fid="KMChannel2005" or @fid="KMChannel2006" or @fid="KMChannel2007"]]]></KMChannels>
  <KMChannelsFaktorRkf>1.0</KMChannelsFaktorRkf>
  <KMChannelsFaktorRnf>1.0</KMChannelsFaktorRnf>
  <rootNode>Node2002</rootNode>
  <pegelZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Pegel/Pegel_Weida.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Q"/>
  </pegelZR>
  <qberechnetZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Pegel_Weida.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qberechnetZR>
  <accuracyPrediction>30.0</accuracyPrediction>
  <useOffsetStartPrediction>false</useOffsetStartPrediction>
  <useOffsetEndPrediction>false</useOffsetEndPrediction>
  <qAblageSpurMittlerer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Weida.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurMittlerer>
  <qAblageSpurOberer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Weida.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurOberer>
  <qAblageSpurUnterer>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Weida.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Abfluss"/>
  </qAblageSpurUnterer>
</NAModellControl>
