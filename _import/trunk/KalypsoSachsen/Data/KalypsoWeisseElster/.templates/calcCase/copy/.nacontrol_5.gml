<?xml version="1.0" encoding="UTF-8"?>
<NAModellControl fid="ID000000" xmlns="org.kalypso.namodell.control" xmlns:gml="http://www.opengis.net/gml" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="project:/.model/schema/nacontrol.xsd">
  <description>Beschreibung</description>
  <name>Elsterberg</name>
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
  <kap>false</kap>
  <vet>false</vet>
  <hyd>false</hyd>
  <bil>false</bil>
  <nmq>false</nmq>
  <spi>false</spi>
  <sub>false</sub>
  <sph>false</sph>
  <spv>false</spv>
  <spn>false</spn>
  <vep>false</vep>
  <automaticCallibration>false</automaticCallibration>
  <Catchments><![CDATA[//:Catchment[@fid="Catchment1302" or @fid="Catchment1304" or @fid="Catchment1301" or @fid="Catchment1300" or @fid="Catchment1303"]]]></Catchments>
  <CatchmentsBianf>1.0</CatchmentsBianf>
  <CatchmentsFaktorRetobTetint>1.0</CatchmentsFaktorRetobTetint>
  <CatchmentsFaktn>1.0</CatchmentsFaktn>
  <CatchmentsFaktorAigw>1.0</CatchmentsFaktorAigw>
  <KMChannels><![CDATA[//:KMChannel[@fid="KMChannel1300" or @fid="KMChannel1301" or @fid="KMChannel1302" or @fid="KMChannel1303" or @fid="KMChannel1304"]]]></KMChannels>
  <KMChannelsFaktorRkf>1.0</KMChannelsFaktorRkf>
  <KMChannelsFaktorRnf>1.0</KMChannelsFaktorRnf>
  <rootNode>Node1300</rootNode>
  <resultForRootNodeOnly>true</resultForRootNodeOnly>
  <pegelZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Q" ns1:href="Pegel/Pegel_Elsterberg.zml" ns1:type="simple"/>
  </pegelZR>
  <qberechnetZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" ns1:href="Ergebnisse/Berechnet/Pegel_Elsterberg.zml" ns1:type="simple"/>
  </qberechnetZR>
  <accuracyPrediction>3</accuracyPrediction>
  <qAblageSpurMittlerer>
    <TimeseriesLink linktype="zml" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Elsterberg.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Abfluss" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
  </qAblageSpurMittlerer>
  <qAblageSpurOberer>
    <TimeseriesLink linktype="zml" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Elsterberg.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Abfluss" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
  </qAblageSpurOberer>
  <qAblageSpurUnterer>
    <TimeseriesLink linktype="zml" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Elsterberg.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Abfluss" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
  </qAblageSpurUnterer>  
</NAModellControl>
