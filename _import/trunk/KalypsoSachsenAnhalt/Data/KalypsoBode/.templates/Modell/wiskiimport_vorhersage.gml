<?xml version="1.0" encoding="UTF-8"?>
<WiskiCollection fid="root" xmlns:tubigcommon="org.kalypso.tubig.common" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:obslink="obslink.zml.kalypso.org" xmlns:gml="http://www.opengis.net/gml" xmlns="org.kalypso.wiski" xsi:schemaLocation="org.kalypso.wiski project:/.model/schema/modell.xsd">
     <!-- Pseudo-Import, um leere ZML für die Abgabe an der Überleitung zu generieren -->
	<WiskiMember>
		<Wiski fid="Wiski_Q_Sp_4">
			<name>Moni</name>
			<lokal>
				<TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherabgabe/Königshütte.zml" linktype="zml"/>
			</lokal>
			<wiski_vergangenheit>
				<TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="kalypso-ocs://LEER?&lt;request xmlns=&quot;request.zml.kalypso.org&quot;&gt;&lt;name&gt;TS Königshütte&lt;/name&gt;&lt;axes&gt;date,Q&lt;/axes&gt;&lt;statusAxes&gt;Q&lt;/statusAxes&gt;&lt;/request&gt;&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;1&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot; fillLastWithValid=&quot;true&quot;/&gt;&lt;/filter&gt;" linktype="zml"/>
			</wiski_vergangenheit>
			<wiski_simulation_vergangenheit>
				<TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="kalypso-ocs://LEER?&lt;request xmlns=&quot;request.zml.kalypso.org&quot;&gt;&lt;name&gt;TS Königshütte&lt;/name&gt;&lt;axes&gt;date,Q&lt;/axes&gt;&lt;statusAxes&gt;Q&lt;/statusAxes&gt;&lt;/request&gt;&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;1&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot; fillLastWithValid=&quot;true&quot;/&gt;&lt;/filter&gt;" linktype="zml"/>
			</wiski_simulation_vergangenheit>
		</Wiski>
	</WiskiMember>
</WiskiCollection>
