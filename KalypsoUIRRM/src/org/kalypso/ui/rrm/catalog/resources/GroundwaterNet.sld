<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>fts_groundwaterNet</Name>
	<Title>%fts_groundwaterNet_title</Title>
	<Rule>
		<Name>rule_gw_relation</Name>
		<Title>%rule_gw_relation_title</Title>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_grundwasserabflussMember</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#a0a0a0</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
                <CssParameter name="stroke-arrow-type">line</CssParameter>
                <CssParameter name="stroke-arrow-widget">open</CssParameter>
                <CssParameter name="stroke-arrow-alignment">end</CssParameter>
                <CssParameter name="stroke-arrow-size">35</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
</FeatureTypeStyle>