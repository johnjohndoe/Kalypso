<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	version="String">
	<NamedLayer>
		<Name>line</Name>
		<UserStyle>
			<Name>lineStyle</Name>
			<Title>Isolinien [0.1]</Title>
			<Abstract>schöne Linien</Abstract>
			<FeatureTypeStyle>
				<Name>Linien Features</Name>
				<Rule>
					<Name>linerule</Name>
					<Title>farbige Linien</Title>
					<Abstract>schöne farbige Linien</Abstract>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>
						1.7976931348623157E308
					</MaxScaleDenominator>
				</Rule>
				<Rule>
					<LineSymbolizer>
						<Geometry>
							<PropertyName>
								velocity iso-line
							</PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">
								#ff00ff
							</CssParameter>
							<CssParameter name="stroke-width">
								2.0
							</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
