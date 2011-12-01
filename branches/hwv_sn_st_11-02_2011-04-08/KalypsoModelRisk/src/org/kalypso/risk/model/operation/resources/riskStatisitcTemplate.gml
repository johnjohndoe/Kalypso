<?xml version="1.0" encoding="WINDOWS-1252"?>
<om:Observation xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:om="http://www.opengis.net/om"
	xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:sweExt="org.kalypso.swe.ext"
	xs:schemaLocation="http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd     http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd"
	gml:id="RiskResult">
	<gml:name>Statistical Results</gml:name>
	<om:procedure>
		<om:ObservationProcedure gml:id="proc_risk_stat">
			<gml:description>Statistical Results for Risk Model</gml:description>
			<gml:name>Risk Statistic</gml:name>
			<om:method />
		</om:ObservationProcedure>
	</om:procedure>
	<om:observedProperty xlink:href="#null" />
	<om:resultDefinition>
		<sweExt:SortedRecordDefinition gml:id="SortedRecordDefinition11897777681390">
			<swe:component xmlns:swe="http://www.opengis.net/swe" xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionStation" />
			<swe:component xmlns:swe="http://www.opengis.net/swe" xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionRunOff" />
			<swe:component xmlns:swe="http://www.opengis.net/swe" xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSection_v_m" />
			<swe:component xmlns:swe="http://www.opengis.net/swe" xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionGround" />
			<swe:component xmlns:swe="http://www.opengis.net/swe" xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionWaterlevel" />
			<sweExt:sortedComponent xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionStation" />
		</sweExt:SortedRecordDefinition>
	</om:resultDefinition>
	<om:result></om:result>
</om:Observation>