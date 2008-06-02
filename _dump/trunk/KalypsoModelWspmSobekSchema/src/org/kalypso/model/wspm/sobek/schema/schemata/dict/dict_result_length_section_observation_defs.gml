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
		Dictionary for result length section components.
	</gml:description>
	<gml:name>Result Length Section Component Dictionary</gml:name>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="STATION">
			<gml:description>Station [m]</gml:description>
			<gml:name>Station [m]</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_STATION">
					<gml:description>
						Station along Branch [m]
					</gml:description>
					<gml:name>Station along Branch [m]</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:SimpleType>
					<xst:restriction base="decimal">
						<xst:fractionDigits value="2" />
					</xst:restriction>
					<gml:unitOfMeasure uom="m" />
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
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="DISCHARGE">
			<gml:name>Discharge [m³/s]</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_Q">
					<gml:description>Discharge</gml:description>
					<gml:name>Q</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:SimpleType>
					<xst:restriction base="decimal">
						<xst:fractionDigits value="3" />
					</xst:restriction>
					<gml:unitOfMeasure uom="m³/s" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="LABEL">
			<gml:name>Label</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_LABEL">
					<gml:description>Label</gml:description>
					<gml:name>Label</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Word>
					<swe:restriction>
						<xst:pattern value=".*" />
					</swe:restriction>
					<swe:classification />
				</swe:Word>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
</gml:Dictionary>
