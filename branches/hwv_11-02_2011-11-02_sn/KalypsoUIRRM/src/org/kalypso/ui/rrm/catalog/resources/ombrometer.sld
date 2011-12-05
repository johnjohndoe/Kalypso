<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle
	xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink">
	<Name>Ombrometer</Name>
	<Rule>
		<Name>default</Name>
		<Title>Ombrometer</Title>
		<Abstract>default</Abstract>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>Ort</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>star</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">1.0</CssParameter>
						<CssParameter name="fill">#80ff00</CssParameter>
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
				<Size>13.0</Size>
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
				<CssParameter name="font-family">sansserif</CssParameter>
				<CssParameter name="font-color">#000000</CssParameter>
				<CssParameter name="font-size">12.0</CssParameter>
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
						<DisplacementX>7.0</DisplacementX>
						<DisplacementY>7.0</DisplacementY>
					</Displacement>
					<Rotation>0.0</Rotation>
				</PointPlacement>
			</LabelPlacement>
			<Halo>
				<Fill>
					<CssParameter name="fill-opacity">0.3</CssParameter>
					<CssParameter name="fill">#808080</CssParameter>
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
</FeatureTypeStyle>
