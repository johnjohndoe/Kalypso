<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>NativeTerrainElevationModelWrapper</Name>
	<Title>NativeTerrainElevationModelWrapper</Title>
	<FeatureTypeName>{http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase}NativeTerrainElevationModelWrapper</FeatureTypeName>
	<Rule>
		<Name>NativeTerrainElevationModelWrapper</Name>
		<Title>NativeTerrainElevationModelWrapper</Title>
		<Abstract>default</Abstract>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>geometry</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#000000</CssParameter>
				<CssParameter name="stroke-width">1.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
</FeatureTypeStyle>
