<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="String">
	<NamedLayer>
		<Name>name</Name>
		<UserStyle>
			<Name>shapeTemplate</Name>
			<Title>Zellpositionen</Title>
			<FeatureTypeStyle>
				<Name>pointFts</Name>
				<Rule>
					<Name>pointrule</Name>
					<Title>Zellpositionen</Title>
					<PointSymbolizer>
						<Graphic>
							<Mark>
								<WellKnownName>circle</WellKnownName>
								<Fill>
									<CssParameter name="fill">#ab0000</CssParameter>
								</Fill>
								<Stroke>
									<CssParameter name="stroke">#000000</CssParameter>
									<CssParameter name="stroke-width">1.0</CssParameter>
								</Stroke>
							</Mark>
							<Opacity>1.0</Opacity>
							<Size>5.0</Size>
						</Graphic>
					</PointSymbolizer>
					<PolygonSymbolizer>
						<Fill>
							<CssParameter name="fill">#ab0000</CssParameter>
							<CssParameter name="fill-opacity">0.4</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
							<CssParameter name="stroke-width">1.0</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>lm2GridTemplate</Name>
			<Title>Zellpositionen</Title>
			<FeatureTypeStyle>
				<Name>pointFts</Name>
				<Rule>
					<Name>pointrule</Name>
					<Title>Zellpositionen</Title>
					<PointSymbolizer>
						<Graphic>
							<Mark>
								<WellKnownName>circle</WellKnownName>
								<Fill>
									<CssParameter name="fill">#00ab00</CssParameter>
								</Fill>
								<Stroke>
									<CssParameter name="stroke">#000000</CssParameter>
									<CssParameter name="stroke-width">1.0</CssParameter>
								</Stroke>
							</Mark>
							<Opacity>1.0</Opacity>
							<Size>5.0</Size>
						</Graphic>
					</PointSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>