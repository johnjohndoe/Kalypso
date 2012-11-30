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
			<gml:name>%OrdinalNumber.item.name</gml:name>
			<swe:property xlink:href="#phenomenonOrdinalNumber" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="integer">
						<st:minInclusive value="1" />
					</st:restriction>
					<gml:unitOfMeasure uom="-" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
	
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Time">
			<gml:description>%Time.item.description</gml:description>
			<gml:name>%Time.item.name</gml:name>
			<swe:property xlink:href="#phenomenonTime" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="dateTime" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<!-- =================================================================
		Under Relaxation Factor
		================================================================== -->
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="UnderRelaxationFactor">
			<gml:name>%UnderRelaxationFactor.item.name</gml:name>
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
			<gml:name>%StepInfoText.item.name</gml:name>
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
			<gml:description>%Waterlevel.item.description</gml:description>
			<gml:name>%Waterlevel.item.name</gml:name>
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
			<gml:description>%WaveHsig.item.description</gml:description>
			<gml:name>%WaveHsig.item.name</gml:name>
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
			<gml:description>%WavePeakPeriod.item.description</gml:description>
			<gml:name>%WavePeakPeriod.item.name</gml:name>
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
			<gml:description>%WaveDirection.item.description</gml:description>
			<gml:name>%WaveDirection.item.name</gml:name>
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
			<gml:description>%WaveDirectionalSpreading.item.description</gml:description>
			<gml:name>%WaveDirectionalSpreading.item.name</gml:name>
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
			<gml:description>%WaterlevelUpstream.item.description</gml:description>
			<gml:name>%WaterlevelUpstream.item.name</gml:name>
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
			<gml:description>%WaterlevelDownstream.item.description</gml:description>
			<gml:name>%WaterlevelDownstream.item.name</gml:name>
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
			<gml:description>%Discharge.item.description</gml:description>
			<gml:name>%Discharge.item.name</gml:name>
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
			<gml:description>%Velocity.item.description</gml:description>
			<gml:name>%Velocity.item.name</gml:name>
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
		<swe:ItemDefinition gml:id="VelocityDirection">
			<gml:description>%VelocityDirection.item.description</gml:description>
			<gml:name>%VelocityDirection.item.name</gml:name>
			<swe:property xlink:href="#phenomenonVelocityDirection" />
			<swe:representation>
				<swe:SimpleType>
					<st:restriction base="decimal">
						<st:fractionDigits value="4" />
					</st:restriction>
					<gml:unitOfMeasure uom="dict_uom.xml#deg" />
				</swe:SimpleType>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="Depth">
			<gml:description>%Depth.item.description</gml:description>
			<gml:name>%Depth.item.name</gml:name>
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
			<gml:description>%SpecificDischarge1D.item.description</gml:description>
			<gml:name>%SpecificDischarge1D.item.name</gml:name>
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
			<gml:description>%SpecificDischarge2D.item.description</gml:description>
			<gml:name>%SpecificDischarge2D.item.name</gml:name>
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
			<gml:description>%MaxVelocityX.item.description</gml:description>
			<gml:name>%MaxVelocityX.item.name</gml:name>
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
			<gml:description>%MaxVelocityY.item.description</gml:description>
			<gml:name>%MaxVelocityY.item.name</gml:name>
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
			<gml:description>%MaxVelocityNode.item.description</gml:description>
			<gml:name>%MaxVelocityNode.item.name</gml:name>
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
			<gml:description>%MaxDepth.item.description</gml:description>
			<gml:name>%MaxDepth.item.name</gml:name>
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
			<gml:description>%MaxDepthNode.item.description</gml:description>
			<gml:name>%MaxDepthNode.item.name</gml:name>
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
			<gml:description>%AverageVelocityX.item.description</gml:description>
			<gml:name>%AverageVelocityX.item.name</gml:name>
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
			<gml:description>%AverageVelocityY.item.description</gml:description>
			<gml:name>%AverageVelocityY.item.name</gml:name>
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
			<gml:description>%AverageDepth.item.description</gml:description>
			<gml:name>%AverageDepth.item.name</gml:name>
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
		<swe:Phenomenon gml:id="phenomenonVelocityDirection">
			<gml:description>Geschwindigkeitsrichtung [deg]</gml:description>
			<gml:name>Geschwindigkeitsrichtung</gml:name>
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