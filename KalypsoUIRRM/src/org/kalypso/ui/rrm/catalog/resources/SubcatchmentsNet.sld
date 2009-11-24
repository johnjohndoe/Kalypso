<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>fts_subcatchments_net</Name>
	<Title>%fts_subcatchments_net_title</Title>
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
		<TextSymbolizer>
			<Geometry>
				<ogc:PropertyName>Ort</ogc:PropertyName>
			</Geometry>
			<Label>
				<ogc:PropertyName>name</ogc:PropertyName>
			</Label>
			<Font>
				<CssParameter name="font-family" />
				<CssParameter name="font-color">#000000</CssParameter>
				<CssParameter name="font-size">10.0</CssParameter>
				<CssParameter name="font-style">normal</CssParameter>
				<CssParameter name="font-weight">normal</CssParameter>
			</Font>
			<LabelPlacement>
				<PointPlacement>
					<AnchorPoint>
						<AnchorPointX>0.0</AnchorPointX>
						<AnchorPointY>0.0</AnchorPointY>
					</AnchorPoint>
					<Displacement>
						<DisplacementX>5.0</DisplacementX>
						<DisplacementY>5.0</DisplacementY>
					</Displacement>
					<Rotation>0.0</Rotation>
				</PointPlacement>
			</LabelPlacement>
			<Halo>
				<Fill>
					<CssParameter name="fill-opacity">0.0</CssParameter>
					<CssParameter name="fill">#fbf193</CssParameter>
				</Fill>
				<Stroke>
					<CssParameter name="stroke">#000000</CssParameter>
					<CssParameter name="stroke-width">1.0</CssParameter>
					<CssParameter name="stroke-linejoin">round</CssParameter>
					<CssParameter name="stroke-opacity">0.0</CssParameter>
					<CssParameter name="stroke-linecap">square</CssParameter>
				</Stroke>
			</Halo>
		</TextSymbolizer>		
	</Rule>
	<Rule>
		<Name>rule_linkOutflow</Name>
		<Title>%rule_linkOutflow_title</Title>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_entwaesserungsStrangMember</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#ffff00</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
	<Rule>
		<Name>rule_link_izkn</Name>
		<Title>%rule_link_izkn_title</Title>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_izkn_vers</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-dashoffset">0.0</CssParameter>
				<CssParameter name="stroke-dasharray">1.0,1.0</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke">#008040</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-linecap">square</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
</FeatureTypeStyle>