<?xml version="1.0" encoding="UTF-8"?>
<NAModellControl fid="ID000000" xmlns="org.kalypso.namodell.control" xmlns:gml="http://www.opengis.net/gml" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="project:/.model/schema/nacontrol.xsd">
  <description>Beschreibung</description>
  <name>Oberthau</name>
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
  <automaticCallibration>true</automaticCallibration>
  <Catchments><![CDATA[//:Catchment[@fid="Catchment1007" or @fid="Catchment7008" or @fid="Catchment1008" or @fid="Catchment3004" or @fid="Catchment3003" or @fid="Catchment3002" or @fid="Catchment7007" or @fid="Catchment3001" or @fid="Catchment1013" or @fid="Catchment1011" or @fid="Catchment1014" or @fid="Catchment1005" or @fid="Catchment1004" or @fid="Catchment1010" or @fid="Catchment1003" or @fid="Catchment7006" or @fid="Catchment7005" or @fid="Catchment7004" or @fid="Catchment7003" or @fid="Catchment7002" or @fid="Catchment7001" or @fid="Catchment1002" or @fid="Catchment1009" or @fid="Catchment1001"]]]></Catchments>
  <CatchmentsBianf>1.0</CatchmentsBianf>
  <CatchmentsFaktorRetobTetint>1.0</CatchmentsFaktorRetobTetint>
  <CatchmentsFaktn>1.0</CatchmentsFaktn>
  <CatchmentsFaktorAigw>1.0</CatchmentsFaktorAigw>
  <KMChannels><![CDATA[//:KMChannel[@fid="KMChannel1001" or @fid="KMChannel1002" or @fid="KMChannel1003" or @fid="KMChannel1004" or @fid="KMChannel1005" or @fid="KMChannel1007" or @fid="KMChannel1008" or @fid="KMChannel1009" or @fid="KMChannel1010" or @fid="KMChannel1011" or @fid="KMChannel1013" or @fid="KMChannel1014" or @fid="KMChannel7001" or @fid="KMChannel7002" or @fid="KMChannel7003" or @fid="KMChannel7004" or @fid="KMChannel7005" or @fid="KMChannel7006" or @fid="KMChannel7007" or @fid="KMChannel7008" or @fid="KMChannel3001" or @fid="KMChannel3002" or @fid="KMChannel3003" or @fid="KMChannel3004"]]]></KMChannels>
  <KMChannelsFaktorRkf>0.5</KMChannelsFaktorRkf>
  <KMChannelsFaktorRnf>0.5</KMChannelsFaktorRnf>
  <rootNode>Node1001</rootNode>
  <resultForRootNodeOnly>true</resultForRootNodeOnly>
  <pegelZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Q" ns1:href="Pegel/Pegel_Node1001.zml" ns1:type="simple"/>
  </pegelZR>
  <qberechnetZR>
    <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Abfluss" ns1:href="Ergebnisse/Berechnet/Abfluss_Node1001.zml" ns1:type="simple"/>
  </qberechnetZR>
</NAModellControl>
