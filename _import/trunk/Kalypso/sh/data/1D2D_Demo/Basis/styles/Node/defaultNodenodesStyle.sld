<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version='1.0.0'
	xmlns='http://www.opengis.net/sld' xmlns:sld='http://www.opengis.net/sld'
	xmlns:sldExt='http://www.opengis.net/sldExt' xmlns:gml='http://www.opengis.net/gml'
	xmlns:ogc='http://www.opengis.net/ogc' xmlns:xlink='http://www.w3.org/1999/xlink'
	xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
	<NamedLayer>
		<Name>deegree style definition</Name>
		<UserStyle>
			<Name>Vector Style</Name>
			<Title>- generierter Standard-Stil -</Title>
			<Abstract>empty Abstract</Abstract>
			<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
				xmlns:ogc="http://www.opengis.net/ogc">
				<Name>- generierter Standard-Stil -</Name>
				<Rule>
					<Name>default</Name>
					<Title>default</Title>
					<Abstract>default</Abstract>
					<ogc:Filter xmlns:ogc='http://www.opengis.net/ogc'>
						<ogc:And>
							<ogc:Not>
								<ogc:PropertyIsEqualTo>
									<ogc:PropertyName>res1d2d:midside
									</ogc:PropertyName>
									<ogc:Literal>true</ogc:Literal>
								</ogc:PropertyIsEqualTo>
							</ogc:Not>
							<ogc:PropertyIsGreaterThanOrEqualTo>
								<ogc:PropertyName>res1d2d:velocityNorm
								</ogc:PropertyName>
								<ogc:Literal>0.01</ogc:Literal>
							</ogc:PropertyIsGreaterThanOrEqualTo>
						</ogc:And>
					</ogc:Filter>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308
					</MaxScaleDenominator>
					<PointSymbolizer uom="meter">
						<Graphic>
							<Mark>
								<WellKnownName>kalypsoArrow</WellKnownName>
								<Fill>
									<CssParameter name='fill-opacity'>1</CssParameter>
									<CssParameter name='fill'>#ccccff</CssParameter>
								</Fill>
								<Stroke></Stroke>
							</Mark>
							<Opacity>1.0</Opacity>
							<Size>
								<ogc:Mul>
									<ogc:Literal>15</ogc:Literal>
									<ogc:PropertyName>velocityNorm</ogc:PropertyName>
								</ogc:Mul>
							</Size>
							<Rotation>
								<ogc:Mul>
									<ogc:Literal>-1.0</ogc:Literal>
									<ogc:PropertyName>velocityRotation
									</ogc:PropertyName>
								</ogc:Mul>
							</Rotation>
						</Graphic>
					</PointSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>