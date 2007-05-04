<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	version="1.0.0">
	<NamedLayer>
		<Name>deegree style definition</Name>
		<UserStyle>
			<Name>Vector Style</Name>
			<Title>- generierter Standard-Stil -</Title>
			<Abstract>empty Abstract</Abstract>
			<FeatureTypeStyle>
				<Name>- generierter Standard-Stil -</Name>
				<Rule>
					<Name>default</Name>
					<Title>default</Title>
					<Abstract>default</Abstract>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>
						1.7976931348623157E308
					</MaxScaleDenominator>
					<PointSymbolizer uom="meter">
						<Graphic>
							<Mark>
								<WellKnownName>
									kalypsoArrow
								</WellKnownName>
								<Fill>
									<CssParameter name="fill-opacity">
										1
									</CssParameter>
									<CssParameter name="fill">
										#ccccff
									</CssParameter>
								</Fill>
							</Mark>

							<Opacity>1.0</Opacity>
							<Size><ogc:Mul><ogc:Literal>100</ogc:Literal><ogc:PropertyName>velocityNorm</ogc:PropertyName></ogc:Mul></Size>
							<Rotation><ogc:PropertyName>velocityRotation</ogc:PropertyName></Rotation>
						</Graphic>
					</PointSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
