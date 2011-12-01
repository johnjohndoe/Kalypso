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
					<gml:unitOfMeasure uom=" " />
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
			<gml:name>Relaxationsfaktor [-]</gml:name>
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
	-->
	
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="UnderRelaxationFactor">
			<gml:name>Relaxationsfaktor</gml:name>
			<swe:property xlink:href="#phenomenonUnderRelaxationFactor" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="string" />
					<gml:unitOfMeasure uom=" " />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

  <gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="StepInfoText">
			<gml:name>Calculation Information</gml:name>
			<swe:property xlink:href="#phenomenonStepInfoText" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="string" />
					<gml:unitOfMeasure uom=" " />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Waterlevel">
			<gml:description>Wasserstand [NN+m]</gml:description>
			<gml:name>Wasserstand</gml:name>
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
		<swe:ItemDefinition gml:id="WaveHsig">
			<gml:description>Signifikante Wellenhöhe [m]</gml:description>
			<gml:name>Signifikante Wellenhöhe</gml:name>
			<swe:property xlink:href="#phenomenonWaveHsig" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="2" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="WavePeakPeriod">
			<gml:description>Wellenperiode des Energiespektrums</gml:description>
			<gml:name>Wellenperiode</gml:name>
			<swe:property xlink:href="#phenomenonWavePer" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="2" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#time" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="WaveDirection">
			<gml:description>Wellenrichtung [deg]</gml:description>
			<gml:name>Wellenrichtung</gml:name>
			<swe:property xlink:href="#phenomenonWaveDirection" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="2" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#deg" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="WaveDirectionalSpreading">
			<gml:description>Coefficient of directional spreading</gml:description>
			<gml:name>Coefficient of directional spreading</gml:name>
			<swe:property xlink:href="#phenomenonWaveDD" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="2" />
					</st:restriction>
					<gml:unitOfMeasure uom=" " />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="WaterlevelUpstream">
			<gml:description>Oberwasser [NN+m]</gml:description>
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
			<gml:description>Unterwasser [NN+m]</gml:description>
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
			<gml:description>Abfluss [m³/s]</gml:description>
			<gml:name>Abfluss</gml:name>
			<swe:property xlink:href="#phenomenonDischarge" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m³/s" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Velocity">
			<gml:description>Geschwindigkeit [m/s]</gml:description>
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
			<gml:description>Fließtiefe [m]</gml:description>
			<gml:name>Fließtiefe</gml:name>
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
			<gml:description>spezifischer Abfluss [m³/s/m²]</gml:description>
			<gml:name>Abfluss</gml:name>
			<swe:property xlink:href="#phenomenonSpecificDischarge1D" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m³/s/m²" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="SpecificDischarge2D">
			<gml:description>spezifischer Abfluss [m³/s/m]</gml:description>
			<gml:name>Abfluss</gml:name>
			<swe:property xlink:href="#phenomenonSpecificDischarge2D" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#m³/s/m" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<!--  Components for the iteration observations: describing, how the iteration proceeded during the 1d2d-simulation -->
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="MaxVelocityX">
			<gml:description>Maximale Geschwindigkeitsdifferenz in X-Richtung [m/s]</gml:description>
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
			<gml:description>Maximale Geschwindigkeitsdifferenz in X-Richtung [m/s]</gml:description>
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
			<gml:description>Knoten der maximalen Geschwindigkeitsdifferenz [-]</gml:description>
			<gml:name>Max v-Node</gml:name>
			<swe:property xlink:href="#phenomenonOrdinalNumber" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="integer"/>
					<gml:unitOfMeasure uom="dict_uom.xml#ordinal" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="MaxDepth">
			<gml:description>Maximale Fließtiefendifferenz [m]</gml:description>
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
			<gml:description>Knoten der maximalen Fließtiefendifferenz [-]</gml:description>
			<gml:name>Max Depth - Node</gml:name>
			<swe:property xlink:href="#phenomenonOrdinalNumber" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="integer"/>
					<gml:unitOfMeasure uom="dict_uom.xml#ordinal" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="AverageVelocityX">
			<gml:description>Mittlere Geschwindigkeitsdifferenz in X-Richtung [m/s]</gml:description>
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
			<gml:description>Mittlere Geschwindigkeitsdifferenz in X-Richtung [m/s]</gml:description>
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
			<gml:description>Mittlere Fließtiefendifferenz [m]</gml:description>
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
			<gml:name>Wichtungsfaktor</gml:name>
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
			<gml:name>Abfluss</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonVelocity">
			<gml:description>Geschwindigkeit [m/s]</gml:description>
			<gml:name>Geschwindigkeit</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonDepth">
			<gml:description>Fließtiefe [m]</gml:description>
			<gml:name>Fließtiefe</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonSpecificDischarge1D">
			<gml:description>spezifischer Abfluss [m³/s/m²]</gml:description>
			<gml:name>spezifischer Abfluss</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonSpecificDischarge2D">
			<gml:description>spezifischer Abfluss [m³/s/m]</gml:description>
			<gml:name>spezifischer Abfluss</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>
	
	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonWaveHsig">
			<gml:description>Signifikante Wellenhohe [m]</gml:description>
			<gml:name>Signifikante Wellenhohe [m]</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonWavePer">
			<gml:description>Wellenperiode [s]</gml:description>
			<gml:name>Wellenperiode</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonWaveDirection">
			<gml:description>Wellenrichtung [deg]</gml:description>
			<gml:name>Wellenrichtung</gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:Phenomenon gml:id="phenomenonWaveDD">
			<gml:description>Directional spreading </gml:description>
			<gml:name>Directional spreading </gml:name>
		</swe:Phenomenon>
	</gml:dictionaryEntry>


</gml:Dictionary>
