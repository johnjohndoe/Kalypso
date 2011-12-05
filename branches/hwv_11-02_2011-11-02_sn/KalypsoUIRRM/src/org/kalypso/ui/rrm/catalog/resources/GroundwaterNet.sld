<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>fts_groundwaterNet</Name>
	<Title>%fts_groundwaterNet_title</Title>
	<Rule>
		<Name>rule_geometry</Name>
		<Title>%rule_geometry_title</Title>
		<PolygonSymbolizer>
			<Geometry>
				<ogc:PropertyName>Ort</ogc:PropertyName>
			</Geometry>
			<Fill>
				<CssParameter name="fill-opacity">0.2</CssParameter>
				<CssParameter name="fill">#c0c0c0</CssParameter>
			</Fill>
			<Stroke>
				<CssParameter name="stroke">#000000</CssParameter>
				<CssParameter name="stroke-width">1.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</PolygonSymbolizer>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>Ort</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>triangle</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">1.0</CssParameter>
						<CssParameter name="fill">#ffff00</CssParameter>
					</Fill>
					<Stroke>
						<CssParameter name="stroke">#000000</CssParameter>
						<CssParameter name="stroke-width">1.0</CssParameter>
						<CssParameter name="stroke-linejoin">round</CssParameter>
						<CssParameter name="stroke-opacity">1.0</CssParameter>
						<CssParameter name="stroke-linecap">square</CssParameter>
					</Stroke>
				</Mark>
				<Opacity>1.0</Opacity>
				<Size>15.0</Size>
				<Rotation>0.0</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
	<Rule>
		<Name>rule_gw_relation</Name>
		<Title>%rule_gw_relation_title</Title>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_grundwasserabflussMember</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#c0c0c0</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
	<Rule>
		<Name>rule_izkn</Name>
		<Title>%rule_izkn_title</Title>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_izkn</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#808000</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
</FeatureTypeStyle>