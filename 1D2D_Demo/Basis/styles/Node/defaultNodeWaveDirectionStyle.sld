<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xmlns:res1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResults"
	version="1.0.0">
	<NamedLayer>
		<Name>deegree style definition</Name>
		<UserStyle>
			<Name>Node Results Style</Name>
			<Title>- generierter Standard-Stil -</Title>
			<Abstract>empty Abstract</Abstract>
			<FeatureTypeStyle>
				<Name>- generierter Standard-Stil -</Name>
				<Rule>
					<Name>default</Name>
					<Title>default</Title>
					<Abstract>default</Abstract>
					<ogc:Filter>
						<ogc:And>
							<ogc:Not>
								<ogc:PropertyIsEqualTo>
									<ogc:PropertyName>res1d2d:midside</ogc:PropertyName>
									<ogc:Literal>true</ogc:Literal>
								</ogc:PropertyIsEqualTo>
							</ogc:Not>
							<ogc:PropertyIsGreaterThanOrEqualTo>
								<ogc:PropertyName>res1d2d:velocityNorm</ogc:PropertyName>
								<ogc:Literal>0.000000001</ogc:Literal>
							</ogc:PropertyIsGreaterThanOrEqualTo>
							<ogc:PropertyIsGreaterThanOrEqualTo>
								<ogc:PropertyName>res1d2d:depth</ogc:PropertyName>
								<ogc:Literal>0.000000010</ogc:Literal>
							</ogc:PropertyIsGreaterThanOrEqualTo>
						</ogc:And>
					</ogc:Filter>
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
								<Stroke></Stroke>
							</Mark>
							<Opacity>1.0</Opacity>
							<Size>
								<ogc:Mul>
									<ogc:Literal>42</ogc:Literal>
									<ogc:PropertyName>wavehsig</ogc:PropertyName>
								</ogc:Mul>
							</Size>
							<Rotation>
								<ogc:Mul>
									<ogc:Literal>-1.0</ogc:Literal>
									<ogc:PropertyName>wavedirection</ogc:PropertyName>
								</ogc:Mul>
							</Rotation>
						</Graphic>
					</PointSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
