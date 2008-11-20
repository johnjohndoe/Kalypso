<?xml version="1.0" encoding="WINDOWS-1252"?>
<om:Observation xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml"
	xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:om="http://www.opengis.net/om" xmlns:swe="http://www.opengis.net/swe" gml:id="root">
	<gml:description />
	<gml:name />
	<om:time />
	<om:procedure>
		<om:ObservationProcedure gml:id="proc_rmakalypso_simulation">
			<gml:description>RMA·Kalypso Simulation</gml:description>
			<gml:name>RMAKalypso Simulation</gml:name>
			<om:method />
		</om:ObservationProcedure>
	</om:procedure>
	<om:observedProperty>
		<swe:Phenomenon gml:id="phen_iteration_rmakalypso">
			<gml:name>RMAKalypso Iteration</gml:name>
		</swe:Phenomenon>
	</om:observedProperty>
	<om:featureOfInterest />
	<om:resultDefinition>
		<swe:RecordDefinition recordLength="1" gml:id="rd">
			<gml:name />
			<swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber" />
			<!--swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#NSZF" /-->
			<swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxVelocityX" />
			<swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxVelocityY" />
			<swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxVelocityNode" />
			<swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxDepth" />
			<swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxDepthNode" />
			<swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#AverageVelocityX" />
			<swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#AverageVelocityY" />
			<swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#AverageDepth" />
		</swe:RecordDefinition>
	</om:resultDefinition>
	<om:result />
</om:Observation>
