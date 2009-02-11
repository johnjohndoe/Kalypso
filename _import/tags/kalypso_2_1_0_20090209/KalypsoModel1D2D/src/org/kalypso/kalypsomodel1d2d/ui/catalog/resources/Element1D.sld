<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>Element1D</Name>
	<Title>Stil für 1D-Elemente</Title>
	<Abstract>Stil für 1D-Elemente</Abstract>
	<FeatureTypeName>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}Element1D</FeatureTypeName>
	<Rule>
		<Name>Element1D</Name>
		<Title>1D Element</Title>
		<Abstract>Eindimensionales Finites Element</Abstract>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>geometry</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#F3C13F</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
</FeatureTypeStyle>
