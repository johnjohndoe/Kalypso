<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<NamedLayer>
		<Name>Kalypso-Test</Name>	
		<UserStyle>
			<Name>default:europe:major_rivers</Name>
			<Title>rivers</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule2</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#000088</CssParameter>
							<CssParameter name="stroke-width">2</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
				<Rule>
					<MinScaleDenominator>0</MinScaleDenominator>
					<MaxScaleDenominator>4100000</MaxScaleDenominator>
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>NAME</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family">Serif</CssParameter>
							<CssParameter name="font-style">normal</CssParameter>
							<CssParameter name="font-weight">normal</CssParameter>
							<CssParameter name="font-size">12</CssParameter>
							<CssParameter name="font-color">#000000</CssParameter>
						</Font>
						<LabelPlacement>
							<LinePlacement>
								<PerpendicularOffset>above</PerpendicularOffset>
								<!-- width of the line the label is associated with -->
								<LineWidth>1</LineWidth>
								<!-- gap between labels measured in label width -->
								<Gap>10</Gap>
							</LinePlacement>
						</LabelPlacement>
					</TextSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>			
	</NamedLayer>
</StyledLayerDescriptor>
