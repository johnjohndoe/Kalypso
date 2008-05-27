<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="
     http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd" xmlns:xst="http://www.seegrid.csiro.au/xml/st" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om" xmlns:swe="http://www.opengis.net/swe" gml:id="components">
	<gml:description>Dictionary for boundaryCondition-observation components.</gml:description>
	<gml:name>Boundary Condition Component Dictionary</gml:name>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="DATE">
			<gml:description>Date</gml:description>
			<gml:name>Date</gml:name>
			<swe:property xlink:href="Phenomenon_DATE" />
			<swe:representation>
				<swe:SimpleType>
					<xst:restriction base="dateTime" />
					<gml:unitOfMeasure uom="" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="WATERLEVEL">
			<gml:name>Waterlevel [m NHN]</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_W">
					<gml:description>Waterlevel</gml:description>
					<gml:name>W</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:SimpleType>
					<xst:restriction base="decimal">
						<xst:fractionDigits value="2" />
					</xst:restriction>
					<gml:unitOfMeasure uom="m NHN" />
				</swe:SimpleType>
			</swe:representation>
			<!--
				<swe:representation>
				<swe:Number>
				<gml:unitOfMeasure uom="m NHN"/>
				</swe:Number>
				</swe:representation>-->
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
</gml:Dictionary>
