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
		Dictionary for Sobek-DatabaseStructure-observation components.
	</gml:description>
	<gml:name>Sobek-DatabaseStructure Component Dictionary</gml:name>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="REL_HEIGHT">
			<gml:name>Rel. Height [m]</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_RelHeight">
					<gml:description>Rel. Height [m]</gml:description>
					<gml:name>Rel. Height [m]</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Number>
					<gml:unitOfMeasure uom="m" />
				</swe:Number>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="DISCHARGE">
			<gml:name>Discharge [m�/s]</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_Discharge">
					<gml:description>Discharge [m�/s]</gml:description>
					<gml:name>Discharge [m�/s]</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Number>
					<gml:unitOfMeasure uom="m�/s" />
				</swe:Number>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="USAGE_AXIS1">
			<gml:name>Usage Axis 1</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_UsageAxis1">
					<gml:description>Usage Axis 1</gml:description>
					<gml:name>Usage Axis 1</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:SimpleType>
					<xst:restriction base="int"/>
					<gml:unitOfMeasure uom="" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="USAGE_AXIS2">
			<gml:name>Usage Axis 2</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_UsageAxis2">
					<gml:description>Usage Axis 2</gml:description>
					<gml:name>Usage Axis 2</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:SimpleType>
					<xst:restriction base="int"/>
					<gml:unitOfMeasure uom="" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
</gml:Dictionary>
