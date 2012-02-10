<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="
     http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd"
	xmlns:xst="http://www.seegrid.csiro.au/xml/st" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om"
	xmlns:swe="http://www.opengis.net/swe" gml:id="components">
	<gml:description>Dictionary for Risk Model Result Statistic components.</gml:description>
	<gml:name>Risk Model Result Statistic</gml:name>
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="ROW_TITLE">
			<gml:name>Row Title</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_Row_Title">
					<gml:description>Row Title</gml:description>
					<gml:name>Landuse</gml:name>
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
	<gml:dictionaryEntry>
		<swe:ItemDefinition gml:id="ANNUAL">
			<gml:name>Annual Average Damage</gml:name>
			<swe:property>
				<swe:Phenomenon gml:id="Phenomenon_Annual">
					<gml:description>Averaged Annual Damage</gml:description>
					<gml:name>AnnualValue</gml:name>
				</swe:Phenomenon>
			</swe:property>
			<swe:representation>
				<swe:Number>
					<gml:unitOfMeasure uom="dict_uom.xml#€/m²/a" />
				</swe:Number>
			</swe:representation>
		</swe:ItemDefinition>
	</gml:dictionaryEntry>
</gml:Dictionary>
