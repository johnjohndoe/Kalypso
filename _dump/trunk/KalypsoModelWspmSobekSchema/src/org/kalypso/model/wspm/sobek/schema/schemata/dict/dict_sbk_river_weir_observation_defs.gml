<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="
     http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd"
	xmlns:xst="http://www.seegrid.csiro.au/xml/st"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:om="http://www.opengis.net/om"
	xmlns:swe="http://www.opengis.net/swe" gml:id="components">
	<gml:description>
		Dictionary for Sobek-RiverWeir-observation components.
	</gml:description>
	<gml:name>Sobek-RiverWeir Component Dictionary</gml:name>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="SUBMERGEANCE">
			<gml:name>Submergeance</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_Submergeance">
					<gml:description>Submergeance</gml:description>
					<gml:name>Submergeance</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Number>
					<gml:unitOfMeasure uom="" />
				</swe:Number>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="REDUCTION">
			<gml:name>Reduction</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_Reduction">
					<gml:description>Reduction</gml:description>
					<gml:name>Reduction</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Number>
					<gml:unitOfMeasure uom="" />
				</swe:Number>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
</gml:Dictionary>
