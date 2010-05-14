<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version='1.0.0'
	xmlns='http://www.opengis.net/sld' xmlns:sld='http://www.opengis.net/sld'
	xmlns:sldExt='http://www.opengis.net/sldExt' xmlns:gml='http://www.opengis.net/gml'
	xmlns:ogc='http://www.opengis.net/ogc' xmlns:xlink='http://www.w3.org/1999/xlink'
	xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
	<NamedLayer>
		<Name>tinStyles</Name>
		<UserStyle>
			<Name>tinPolygonStyle</Name>
			<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
				xmlns:ogc="http://www.opengis.net/ogc">
				<Name>tinFeatureTypeStyle</Name>
				<Rule>
					<Name>tinRule</Name>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<SurfacePolygonSymbolizer xmlns:sldExt="http://www.opengis.net/sldExt"
						uom="pixel">
						<Geometry>
							<ogc:PropertyName>res1d2d:triangulatedSurfaceMember
							</ogc:PropertyName>
						</Geometry>
						<sldExt:PolygonColorMap>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>366,40 - 366,65</sldExt:label>
								<sldExt:from>366.4</sldExt:from>
								<sldExt:to>366.65</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#00ff00</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#00ff00</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>366,65 - 366,90</sldExt:label>
								<sldExt:from>366.65</sldExt:from>
								<sldExt:to>366.9</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#1cf500</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#1cf500</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>366,90 - 367,15</sldExt:label>
								<sldExt:from>366.9</sldExt:from>
								<sldExt:to>367.15</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#36eb00</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#36eb00</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>367,15 - 367,40</sldExt:label>
								<sldExt:from>367.15</sldExt:from>
								<sldExt:to>367.4</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#4ee200</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#4ee200</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>367,40 - 367,65</sldExt:label>
								<sldExt:from>367.4</sldExt:from>
								<sldExt:to>367.65</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#64d800</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#64d800</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>367,65 - 367,90</sldExt:label>
								<sldExt:from>367.65</sldExt:from>
								<sldExt:to>367.9</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#77ce00</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#77ce00</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>367,90 - 368,15</sldExt:label>
								<sldExt:from>367.9</sldExt:from>
								<sldExt:to>368.15</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#88c400</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#88c400</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>368,15 - 368,40</sldExt:label>
								<sldExt:from>368.15</sldExt:from>
								<sldExt:to>368.4</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#97bb00</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#97bb00</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>368,40 - 368,65</sldExt:label>
								<sldExt:from>368.4</sldExt:from>
								<sldExt:to>368.65</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#a3b100</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#a3b100</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>368,65 - 368,90</sldExt:label>
								<sldExt:from>368.65</sldExt:from>
								<sldExt:to>368.9</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#a7a100</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#a7a100</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>368,90 - 369,15</sldExt:label>
								<sldExt:from>368.9</sldExt:from>
								<sldExt:to>369.15</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#9d8500</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#9d8500</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>369,15 - 369,40</sldExt:label>
								<sldExt:from>369.15</sldExt:from>
								<sldExt:to>369.4</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#946c00</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#946c00</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>369,40 - 369,65</sldExt:label>
								<sldExt:from>369.4</sldExt:from>
								<sldExt:to>369.65</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#8a5500</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#8a5500</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>369,65 - 369,90</sldExt:label>
								<sldExt:from>369.65</sldExt:from>
								<sldExt:to>369.9</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.8</CssParameter>
									<CssParameter name='stroke'>#804000</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#804000</CssParameter>
									<CssParameter name='fill-opacity'>0.8</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
						</sldExt:PolygonColorMap>
					</SurfacePolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>