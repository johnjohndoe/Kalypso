<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>fts_Hydrotope</Name>
	<Title>%fts_Hydrotope_title</Title>
	<Rule>
		<Name>rule_Hydrotope</Name>
		<Title>%rule_Hydrotope_title</Title>
		<PolygonSymbolizer>
			<Geometry>
				<ogc:PropertyName>position</ogc:PropertyName>
			</Geometry>
			<Fill>
				<CssParameter name="fill-opacity">1.0</CssParameter>
				<CssParameter name="fill">#808080</CssParameter>
			</Fill>
			<Stroke>
				<CssParameter name="stroke">#000000</CssParameter>
				<CssParameter name="stroke-width">1.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</PolygonSymbolizer>
	</Rule>
</FeatureTypeStyle>