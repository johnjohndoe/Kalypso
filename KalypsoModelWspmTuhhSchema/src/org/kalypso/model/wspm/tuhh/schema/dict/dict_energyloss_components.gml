<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xst="http://www.seegrid.csiro.au/xml/st" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml"
	xmlns:om="http://www.opengis.net/om" xmlns:swe="http://www.opengis.net/swe" gml:id="components">

	<gml:description>Dictionary for energyloss components.</gml:description>
	<gml:name>Energyloss Component Dictionary</gml:name>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="TYPE">
			<gml:name>%type.name</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_TYPE">
					<gml:description>%type.description</gml:description>
					<gml:name>%type.name</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Word>
					<swe:classification />
				</swe:Word>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="DESCRIPTION">
			<gml:name>%description.name</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_DESCRIPTION">
					<gml:description>%description.description</gml:description>
					<gml:name>%description.name</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Word>
					<swe:classification />
				</swe:Word>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>

	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="VALUE">
			<gml:name>%value.name</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_VALUE">
					<gml:description>%value.description</gml:description>
					<gml:name>%value.name</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Number>
					<gml:unitOfMeasure uom="dict_uom.xml#mNN" />
				</swe:Number>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>



</gml:Dictionary>
