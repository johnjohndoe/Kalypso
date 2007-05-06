<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="
     http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd"
	xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om" xmlns:swe="http://www.opengis.net/swe"
	gml:id="components">

	<gml:description>Ein Dictionary für die Modellierung von Zeitreihendaten</gml:description>
	<gml:name>Zeitreihenkomponente</gml:name>

	<!--  Item Definitions -->

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Time">
			<gml:description>Time</gml:description>
			<gml:name>Time</gml:name>
			<swe:property xlink:href="#phenomenonTime" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="dateTime" />
					<gml:unitOfMeasure uom="dict_uom.xml#time" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<!-- =================================================================
		Under Relaxation Factor
		================================================================== -->
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="UnderRelaxationFactor">
			<gml:name>Wichtungsfaktor [-]</gml:name>
			<swe:property xlink:href="#phenomenonUnderRelaxationFactor" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:minInclusive value="0.1" />
						<st:maxInclusive value="1.0" />
					</st:restriction>
					<gml:unitOfMeasure uom=" " />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Waterlevel">
			<gml:description>WSP [NN+m]</gml:description>
			<gml:name>WSP</gml:name>
			<swe:property xlink:href="#phenomenonWaterlevel" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#mNN" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Discharge">
			<gml:description>Q [m³/s]</gml:description>
			<gml:name>Q</gml:name>
			<swe:property xlink:href="#phenomenonDischarge" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m3_s" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="SpecificDischarge1D">
			<gml:description>Q [m³/s/m²]</gml:description>
			<gml:name>Q</gml:name>
			<swe:property xlink:href="#phenomenonSpecificDischarge1D" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m3_s_m2" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="SpecificDischarge2D">
			<gml:description>Q [m³/s/m]</gml:description>
			<gml:name>Q</gml:name>
			<swe:property xlink:href="#phenomenonSpecificDischarge2D" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m3_s_m" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<!--  Phenomenons -->
	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonTime">
			<gml:description>Zeit</gml:description>
			<gml:name>Zeit</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonUnderRelaxationFactor">
			<gml:description>Wichtungsfaktor [-]</gml:description>
			<gml:name>Wichtungsfaktor [-]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonWaterlevel">
			<gml:description>Wasserspiegel [NN+m]</gml:description>
			<gml:name>Wasserspiegel</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonDischarge">
			<gml:description>Abfluss [m³/s]</gml:description>
			<gml:name>Abfluss [m³/s]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonSpecificDischarge1D">
			<gml:description>spezifischer Abfluss [m³/s/m²]</gml:description>
			<gml:name>spezifischer Abfluss [m³/s/m²]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonSpecificDischarge2D">
			<gml:description>spezifischer Abfluss [m³/s/m]</gml:description>
			<gml:name>spezifischer Abfluss [m³/s/m]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

</gml:Dictionary>
