<?xml version="1.0" encoding="UTF-8"?>
<WiskiCollection fid="root" xmlns:bodecommon="org.kalypso.bode.common" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:obslink="obslink.zml.kalypso.org" xmlns:gml="http://www.opengis.net/gml" xmlns="org.kalypso.bode.wiski" xsi:schemaLocation="org.kalypso.bode.wiski project:/.model/schema/modell.xsd">
	<gml:boundedBy>
		<gml:Box>
			<gml:coord>
				<gml:X>0.0</gml:X>
				<gml:Y>0.0</gml:Y>
			</gml:coord>
			<gml:coord>
				<gml:X>0.0</gml:X>
				<gml:Y>0.0</gml:Y>
			</gml:coord>
		</gml:Box>
	</gml:boundedBy>
	<WiskiMember>
		<Wiski fid="Wiski_Q_0">
			<lokal>
				<TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherabgabe/Überleitung_Rappbode.zml" linktype="zml"/>
			</lokal>
			<wiski_vergangenheit>
				<TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="blubb_prognose" linktype="zml"/>
			</wiski_vergangenheit>
			<wiski_simulation_vergangenheit>
				<TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="blubb_simulation" linktype="zml"/>
			</wiski_simulation_vergangenheit>
		</Wiski>
	</WiskiMember>
</WiskiCollection>
