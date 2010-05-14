<?xml version="1.0" encoding="Cp1252" ?>
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

	<gml:description>Ein Dictionary f�r die Modellierung von Zeitreihendaten</gml:description>
	<gml:name>Zeitreihenkomponente</gml:name>

	<!--  Item Definitions -->

	<!--  TODO: is this really, really, necessary? I don't think so... -->
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="OrdinalNumber">
			<gml:name>lfd. Nr.</gml:name>
			<swe:property xlink:href="#phenomenonOrdinalNumber" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="integer">
						<st:minInclusive value="1" />
					</st:restriction>
					<gml:unitOfMeasure uom="" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Time">
			<gml:description>Zeit</gml:description>
			<gml:name>Zeit</gml:name>
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
	<!--
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="UnderRelaxationFactor">
			<gml:name>Relaxationsfaktor</gml:name>
			<swe:property xlink:href="#phenomenonUnderRelaxationFactor" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:minInclusive value="0.1" />
						<st:maxInclusive value="1.0" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#none" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	-->
	
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="UnderRelaxationFactor">
			<gml:name>Relaxationsfaktor</gml:name>
			<swe:property xlink:href="#phenomenonUnderRelaxationFactor" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="string" />
					<gml:unitOfMeasure uom="dict_uom.xml#none" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Waterlevel">
			<gml:description>Wasserspiegellage</gml:description>
			<gml:name>Wasserspiegellage</gml:name>
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
		<swe:ItemDefinition gml:id="WaterlevelUpstream">
			<gml:description>Oberwasser</gml:description>
			<gml:name>h-O</gml:name>
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
		<swe:ItemDefinition gml:id="WaterlevelDownstream">
			<gml:description>Unterwasser</gml:description>
			<gml:name>h-U</gml:name>
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
			<gml:description>Abfluss</gml:description>
			<gml:name>Abfluss</gml:name>
			<swe:property xlink:href="#phenomenonDischarge" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m�/s" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Velocity">
			<gml:description>Geschwindigkeit</gml:description>
			<gml:name>Geschwindigkeit</gml:name>
			<swe:property xlink:href="#phenomenonVelocity" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m/s" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Depth">
			<gml:description>Flie�tiefe</gml:description>
			<gml:name>Flie�tiefe</gml:name>
			<swe:property xlink:href="#phenomenonDepth" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="SpecificDischarge1D">
			<gml:description>spezifischer Abfluss</gml:description>
			<gml:name>spezifischer Abfluss</gml:name>
			<swe:property xlink:href="#phenomenonSpecificDischarge1D" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m�/s/m�" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="SpecificDischarge2D">
			<gml:description>spezifischer Abfluss</gml:description>
			<gml:name>spezifischer Abfluss</gml:name>
			<swe:property xlink:href="#phenomenonSpecificDischarge2D" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m�/s/m" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<!--  Components for the iteration observations: describing, how the iteration proceeded during the 1d2d-simulation -->
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="MaxVelocityX">
			<gml:description>Maximale Geschwindigkeitsdifferenz in X-Richtung</gml:description>
			<gml:name>Max v-x</gml:name>
			<swe:property xlink:href="#phenomenonVelocity" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m/s" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="MaxVelocityY">
			<gml:description>Maximale Geschwindigkeitsdifferenz in X-Richtung</gml:description>
			<gml:name>Max v-y</gml:name>
			<swe:property xlink:href="#phenomenonVelocity" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m/s" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<!-- TODO: do we really want to use node numbers?  maybe better use the geo-position instead? -->
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="MaxVelocityNode">
			<gml:description>Knoten der maximalen Geschwindigkeitsdifferenz</gml:description>
			<gml:name>Max v-Node</gml:name>
			<swe:property xlink:href="#phenomenonOrdinalNumber" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="integer"/>
					<gml:unitOfMeasure uom="" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="MaxDepth">
			<gml:description>Maximale Flie�tiefendifferenz</gml:description>
			<gml:name>Max Depth</gml:name>
			<swe:property xlink:href="#phenomenonDepth" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<!-- TODO: do we really want to use node numbers?  maybe better use the geo-position instead? -->
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="MaxDepthNode">
			<gml:description>Knoten der maximalen Flie�tiefendifferenz</gml:description>
			<gml:name>Max Depth - Node</gml:name>
			<swe:property xlink:href="#phenomenonOrdinalNumber" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="integer"/>
					<gml:unitOfMeasure uom="" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="AverageVelocityX">
			<gml:description>Mittlere Geschwindigkeitsdifferenz in X-Richtung</gml:description>
			<gml:name>Average v-x</gml:name>
			<swe:property xlink:href="#phenomenonVelocity" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m/s" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="AverageVelocityY">
			<gml:description>Mittlere Geschwindigkeitsdifferenz in X-Richtung</gml:description>
			<gml:name>Average v-y</gml:name>
			<swe:property xlink:href="#phenomenonVelocity" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m/s" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="AverageDepth">
			<gml:description>Mittlere Flie�tiefendifferenz</gml:description>
			<gml:name>Average Depth</gml:name>
			<swe:property xlink:href="#phenomenonDepth" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>



	<!--  Phenomenons -->
	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonOrdinalNumber">
			<gml:description>Ordinalzahl</gml:description>
			<gml:name>Ordinalzahl</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

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
			<gml:description>Abfluss [m�/s]</gml:description>
			<gml:name>Abfluss [m�/s]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonVelocity">
			<gml:description>Geschwindigkeit [m/s]</gml:description>
			<gml:name>Geschwindigkeit [m/s]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonDepth">
			<gml:description>Flie�tiefe [m]</gml:description>
			<gml:name>Flie�tiefe [m]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonSpecificDischarge1D">
			<gml:description>spezifischer Abfluss [m�/s/m�]</gml:description>
			<gml:name>spezifischer Abfluss [m�/s/m�]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonSpecificDischarge2D">
			<gml:description>spezifischer Abfluss [m�/s/m]</gml:description>
			<gml:name>spezifischer Abfluss [m�/s/m]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>


</gml:Dictionary>
