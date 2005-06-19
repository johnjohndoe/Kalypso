<?xml version="1.0" encoding="UTF-8"?>
<NAModellControl fid="ID000000" xmlns="org.kalypso.namodell.control" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="org.kalypso.namodell.control project:/.model/schema/nacontrol.xsd">
  <description>Beschreibung</description>
  <name>Wendlohstraße</name>
  <editor>Editor</editor>
  <comment>Kommentar</comment>
  <calctime>2001-12-17T09:30:47</calctime>
  <startsimulation>1995-09-01T01:00:00</startsimulation>
  <startforecast>2000-09-04T12:00:00</startforecast>
  <endsimulation>2000-09-09T01:00:00</endsimulation>
  <timeStep>24</timeStep>
  <tmp>true</tmp>
  <pre>true</pre>
  <sch>true</sch>
  <bof>true</bof>
  <bsp>true</bsp>
  <gws>true</gws>
  <qgs>true</qgs>
  <qgg>true</qgg>
  <qna>true</qna>
  <qif>true</qif>
  <qvs>true</qvs>
  <qbs>true</qbs>
  <qt1>true</qt1>
  <qtg>true</qtg>
  <qgw>true</qgw>
  <kap>true</kap>
  <vet>true</vet>
  <hyd>true</hyd>
  <bil>true</bil>
  <nmq>true</nmq>
  <spi>true</spi>
  <sub>true</sub>
  <sph>true</sph>
  <spv>true</spv>
  <spn>true</spn>
  <vep>true</vep>
  <automaticCallibration>true</automaticCallibration>
  <Catchments><![CDATA[//:Catchment[@fid="Catchment100" or @fid="Catchment101" or @fid="Catchment102" or @fid="Catchment103" or @fid="Catchment104" or @fid="Catchment105"]]]></Catchments>
  <CatchmentsBianf>1.0</CatchmentsBianf>
  <CatchmentsFaktorRetobTetint>1.0</CatchmentsFaktorRetobTetint>
  <CatchmentsFaktn>1.0</CatchmentsFaktn>
  <CatchmentsFaktorAigw>1.0</CatchmentsFaktorAigw>
  <KMChannels><![CDATA[//:KMChannel[@fid="KMChannel101" or @fid="KMChannel103" or @fid="KMChannel105"]]]></KMChannels>
  <KMChannelsFaktorRkf>1.0</KMChannelsFaktorRkf>
  <KMChannelsFaktorRnf>1.0</KMChannelsFaktorRnf>
  <rootNode>Node103</rootNode>
  <resultForRootNodeOnly>true</resultForRootNodeOnly>
  <pegelZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" linktype="zml" ns1:href="Pegel/Pegel_Node103.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Q" xmlns="obslink.zml.kalypso.org" />
  </pegelZR>
  <qberechnetZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" linktype="zml" ns1:href="Ergebnisse/Berechnet/Abfluss_Node103.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Abfluss" xmlns="obslink.zml.kalypso.org" />
  </qberechnetZR>
</NAModellControl>
