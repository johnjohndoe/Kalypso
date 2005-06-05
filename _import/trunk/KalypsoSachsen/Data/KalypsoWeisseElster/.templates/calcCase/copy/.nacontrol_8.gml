<?xml version="1.0" encoding="UTF-8"?>
<NAModellControl fid="ID000000" xmlns="org.kalypso.namodell.control" xmlns:gml="http://www.opengis.net/gml" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="project:/.model/schema/nacontrol.xsd">
  <description>Beschreibung</description>
  <name>Greiz</name>
  <editor>Editor</editor>
  <comment>Kommentar</comment>
  <calctime>2001-12-17T09:30:47</calctime>
  <startsimulation>1995-09-01T01:00:00</startsimulation>
  <startforecast>1995-09-04T12:00:00</startforecast>
  <endsimulation>1995-09-09T01:00:00</endsimulation>
  <timeStep>1.0</timeStep>
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
  <Catchments><![CDATA[//:Catchment[@fid="Catchment4001" or @fid="Catchment4002" or @fid="Catchment4000" or @fid="Catchment1220" or @fid="Catchment1221"]]]></Catchments>
  <CatchmentsBianf>1.0</CatchmentsBianf>
  <CatchmentsFaktorRetobTetint>1.0</CatchmentsFaktorRetobTetint>
  <CatchmentsFaktn>1.0</CatchmentsFaktn>
  <CatchmentsFaktorAigw>1.0</CatchmentsFaktorAigw>
  <KMChannels><![CDATA[//:KMChannel[@fid="KMChannel1220" or @fid="KMChannel1221" or @fid="KMChannel4000" or @fid="KMChannel4001" or @fid="KMChannel4002" or @fid="KMChannel4099"]]]></KMChannels>
  <KMChannelsFaktorRkf>1.0</KMChannelsFaktorRkf>
  <KMChannelsFaktorRnf>1.0</KMChannelsFaktorRnf>
  <rootNode>Node1220</rootNode>
  <resultForRootNodeOnly>true</resultForRootNodeOnly>
  <pegelZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Q" ns1:href="Pegel/Pegel_Node1220.zml" ns1:type="simple"/>
  </pegelZR>
  <qberechnetZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" ns1:href="Ergebnisse/Berechnet/Abfluss_Node1220.zml" ns1:type="simple"/>
  </qberechnetZR>
</NAModellControl>
