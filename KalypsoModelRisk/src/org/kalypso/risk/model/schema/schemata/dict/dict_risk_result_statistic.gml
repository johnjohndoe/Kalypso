<?xml version="1.0" encoding="UTF-8" ?>
<Dictionary xmlns:xst="http://www.seegrid.csiro.au/xml/st" xmlns="http://www.opengis.net/gml" xmlns:gml="http://www.opengis.net/gml" xmlns:swe="http://www.opengis.net/swe" gml:id="components">
	<description>Dictionary for Risk Model Result Statistic components.</description>
	<name>Risk Model Result Statistic</name>
	<dictionaryEntry>
		<swe:ItemDefinition gml:id="ROW_TITLE">
			<name>Row Title</name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_Row_Title">
					<description>Row Title</description>
					<name>Landuse</name>
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
	</dictionaryEntry>
	<dictionaryEntry>
		<swe:ItemDefinition gml:id="ANNUAL">
			<name>Annual Average Damage</name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_Annual">
					<description>Averaged Annual Damage</description>
					<name>AnnualValue</name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Number>
					<unitOfMeasure uom="dict_uom.xml#€/m²/a" />
				</swe:Number>
			</swe:representation>
		</swe:ItemDefinition>
	</dictionaryEntry>
	<dictionaryEntry>
		<swe:ItemDefinition gml:id="TOTAL">
			<name>Annual Total Damage</name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_Total">
					<description>Averaged Total Damage</description>
					<name>TotalPotentialDamage</name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Number>
					<unitOfMeasure uom="dict_uom.xml#€/a" />
				</swe:Number>
			</swe:representation>
		</swe:ItemDefinition>
	</dictionaryEntry>
</Dictionary>
