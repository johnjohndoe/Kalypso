<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>TransitionElement</Name>
	<Title>TransitionElement</Title>
	<FeatureTypeName>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}TransitionElement</FeatureTypeName>
	<Rule>
		<Name>TransitionElement_1D2D</Name>
		<Title>Kopplungselement 1D nach 2D</Title>
		<Abstract>Kopplungselement 1D nach 2D</Abstract>
		<ogc:Filter>
			<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
				<ogc:PropertyName>transitionType</ogc:PropertyName>
				<ogc:Literal>1D2D</ogc:Literal>
			</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
			<Geometry>
				<ogc:PropertyName>geometry</ogc:PropertyName>
			</Geometry>
			<Fill>
				<CssParameter name="fill-opacity">0.25</CssParameter>
				<CssParameter name="fill">#008626</CssParameter>
			</Fill>
			<Stroke>
				<CssParameter name="stroke">#008626</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</PolygonSymbolizer>
	</Rule>
	<Rule>
		<Name>TransitionElement_2D1D</Name>
		<Title>Kopplungselement 2D nach 1D</Title>
		<Abstract>Kopplungselement 2D nach 1D</Abstract>
		<ogc:Filter>
			<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
				<ogc:PropertyName>transitionType</ogc:PropertyName>
				<ogc:Literal>2D1D</ogc:Literal>
			</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
			<Geometry>
				<ogc:PropertyName>geometry</ogc:PropertyName>
			</Geometry>
			<Fill>
				<CssParameter name="fill-opacity">0.25</CssParameter>
				<CssParameter name="fill">#3232F1</CssParameter>
			</Fill>
			<Stroke>
				<CssParameter name="stroke">#3232F1</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</PolygonSymbolizer>
	</Rule>
</FeatureTypeStyle>
