<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" 
	xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<NamedLayer>
		<Name>deegree style definition</Name>
		<UserStyle>
			<Name>Teilgebiete</Name>
			<Title>Teilgebiete</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>Teilgebiete</Name>
				<Rule>
					<Name>default</Name>
					<Title>default</Title>
					<Abstract>default</Abstract>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>Ort</ogc:PropertyName>
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
				<Rule>
					<Title>Rule 1</Title>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>302376.6744536605</MaxScaleDenominator>
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>Ort</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>inum</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family">Dialog</CssParameter>
							<CssParameter name="font-color">#000000</CssParameter>
							<CssParameter name="font-size">12.0</CssParameter>
							<CssParameter name="font-style">normal</CssParameter>
							<CssParameter name="font-weight">normal</CssParameter>
						</Font>
						<Halo>
							<Fill>
								<CssParameter name="fill-opacity">0.3</CssParameter>
								<CssParameter name="fill">#fbf193</CssParameter>
							</Fill>
							<Stroke>
								<CssParameter name="stroke">#000000</CssParameter>
								<CssParameter name="stroke-width">1.0</CssParameter>
								<CssParameter name="stroke-linejoin">round</CssParameter>
								<CssParameter name="stroke-opacity">1.0</CssParameter>
								<CssParameter name="stroke-linecap">square</CssParameter>
							</Stroke>
						</Halo>
					</TextSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>