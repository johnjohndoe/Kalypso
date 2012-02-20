<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version='1.0.0'
	xmlns='http://www.opengis.net/sld' xmlns:sld='http://www.opengis.net/sld'
	xmlns:sldExt='http://www.opengis.net/sldExt' xmlns:gml='http://www.opengis.net/gml'
	xmlns:ogc='http://www.opengis.net/ogc' xmlns:xlink='http://www.w3.org/1999/xlink'
	xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
	<NamedLayer>
		<Name>deegree style definition</Name>
		<UserStyle>
			<Name>Node Results Style</Name>
			<Title>- generierter Standard-Stil -</Title>
			<Abstract>waterlevel</Abstract>
			<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
				xmlns:ogc="http://www.opengis.net/ogc">
				<Name>- generierter Standard-Stil -</Name>
				<Rule>
					<Name>default</Name>
					<Title>default</Title>
					<Abstract>default</Abstract>

					<ogc:Filter xmlns:ogc='http://www.opengis.net/ogc'>
						<ogc:PropertyIsGreaterThanOrEqualTo>
							<ogc:PropertyName>res1d2d:depth</ogc:PropertyName>
							<ogc:Literal>0.001</ogc:Literal>
						</ogc:PropertyIsGreaterThanOrEqualTo>
					</ogc:Filter>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<PointSymbolizer uom="pixel">
						<Geometry>
							<ogc:PropertyName>location</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>square</WellKnownName>
								<Fill>
									<CssParameter name='fill-opacity'>0.5</CssParameter>
									<CssParameter name='fill'>
										<ogc:PropertyName>waterlevelColor</ogc:PropertyName>
									</CssParameter>
									<CssParameter name='minValue'>0.0</CssParameter>
									<CssParameter name='fill-opacity'>0.5</CssParameter>
									<CssParameter name='maxColor'>#ff0080</CssParameter>
									<CssParameter name='amountClasses'>100</CssParameter>
									<CssParameter name='minColor'>#0000ff</CssParameter>
									<CssParameter name='maxValue'>360.0</CssParameter> 
								</Fill>
								<Stroke>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke'>#FFFFFF</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke-linecap'>square</CssParameter>
									<CssParameter name='stroke-width'>1.0</CssParameter>
								</Stroke>
							</Mark>
							<Opacity>0.5</Opacity>
							<Size>
								10	
							</Size>
							<Rotation>0.0</Rotation>
						</Graphic>
					</PointSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>