<?xml version="1.0" encoding="UTF-8"?>
<WiskiCollection fid="root" xmlns:tubigcommon="org.kalypso.tubig.common" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:obslink="obslink.zml.kalypso.org" xmlns:gml="http://www.opengis.net/gml" xmlns="org.kalypso.wiski" xsi:schemaLocation="org.kalypso.wiski project:/.model/schema/modell.xsd">
     <!-- Pseudo-Import, um leere ZML für die Abgabe an der Überleitung zu generieren -->
	<WiskiMember>
		<Wiski fid="Wiski_Q_0">
			<lokal>
				<TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherabgabe/Überleitung_Rappbode.zml" linktype="zml"/>
			</lokal>
			<wiski_vergangenheit>
				<TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="kalypso-ocs:wiski://dummy?&lt;request xmlns=&quot;request.zml.kalypso.org&quot;&gt;&lt;name&gt;Überleitung Rappbode&lt;/name&gt;&lt;axes&gt;date,Q&lt;/axes&gt;&lt;statusAxes&gt;Q&lt;/statusAxes&gt;&lt;/request&gt;&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" linktype="zml"/>
			</wiski_vergangenheit>
			<wiski_simulation_vergangenheit>
				<TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="kalypso-ocs:wiski://dummy_simulation?&lt;request xmlns=&quot;request.zml.kalypso.org&quot;&gt;&lt;name&gt;Überleitung Rappbode&lt;/name&gt;&lt;axes&gt;date,Q&lt;/axes&gt;&lt;statusAxes&gt;Q&lt;/statusAxes&gt;&lt;/request&gt;&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" linktype="zml"/>
			</wiski_simulation_vergangenheit>
		</Wiski>
	</WiskiMember>
</WiskiCollection>
