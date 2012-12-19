<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>fts_subcatchments</Name>
	<Title>%fts_subcatchments_title</Title>
	<Rule>
		<Name>rule_subcatchment</Name>
		<Title>%rule_subcatchment_title=Sub-Catchment</Title>
		<PolygonSymbolizer>
			<Geometry>
				<ogc:PropertyName>Ort</ogc:PropertyName>
			</Geometry>
			<Fill>
				<CssParameter name="fill-opacity">0.3</CssParameter>
				<CssParameter name="fill">#c0c0c0</CssParameter>
			</Fill>
			<Stroke>
				<CssParameter name="stroke">#000000</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</PolygonSymbolizer>
		<TextSymbolizer>
			<Geometry>
				<ogc:PropertyName>Ort</ogc:PropertyName>
			</Geometry>
			<Label>
				<ogc:PropertyName>name</ogc:PropertyName>
			</Label>
			<Font>
				<CssParameter name="font-color">#000000</CssParameter>
				<CssParameter name="font-size">12.0</CssParameter>
				<CssParameter name="font-style">normal</CssParameter>
				<CssParameter name="font-weight">normal</CssParameter>
			</Font>
			<Halo>
                <Radius>3</Radius>
				<Fill>
					<CssParameter name="fill-opacity">0.3</CssParameter>
					<CssParameter name="fill">#808080</CssParameter>
				</Fill>
				<Stroke>
					<CssParameter name="stroke">#000000</CssParameter>
					<CssParameter name="stroke-width">2.0</CssParameter>
					<CssParameter name="stroke-linejoin">round</CssParameter>
					<CssParameter name="stroke-opacity">1.0</CssParameter>
					<CssParameter name="stroke-linecap">square</CssParameter>
				</Stroke>
			</Halo>
		</TextSymbolizer>
	</Rule>
</FeatureTypeStyle>