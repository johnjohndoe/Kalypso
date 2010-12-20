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
								<sldExt:label>Isofläche 0</sldExt:label>
								<sldExt:from>0.2</sldExt:from>
								<sldExt:to>0.4</sldExt:to>
								<Stroke>
									<CssParameter name='stroke'>#0000ff</CssParameter>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-linejoin'>mitre</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke-linecap'>butt</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
									<CssParameter name='fill'>#0000ff</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>Isofläche 1</sldExt:label>
								<sldExt:from>0.4</sldExt:from>
								<sldExt:to>0.6000000000000001</sldExt:to>
								<Stroke>
									<CssParameter name='stroke'>#00aaff</CssParameter>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-linejoin'>mitre</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke-linecap'>butt</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
									<CssParameter name='fill'>#00aaff</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>Isofläche 2</sldExt:label>
								<sldExt:from>0.6000000000000001</sldExt:from>
								<sldExt:to>0.8</sldExt:to>
								<Stroke>
									<CssParameter name='stroke'>#00ffaa</CssParameter>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-linejoin'>mitre</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke-linecap'>butt</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
									<CssParameter name='fill'>#00ffaa</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>Isofläche 3</sldExt:label>
								<sldExt:from>0.8</sldExt:from>
								<sldExt:to>1.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke'>#00ff00</CssParameter>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-linejoin'>mitre</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke-linecap'>butt</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
									<CssParameter name='fill'>#00ff00</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>Isofläche 4</sldExt:label>
								<sldExt:from>1.0</sldExt:from>
								<sldExt:to>1.2</sldExt:to>
								<Stroke>
									<CssParameter name='stroke'>#aaff00</CssParameter>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-linejoin'>mitre</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke-linecap'>butt</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
									<CssParameter name='fill'>#aaff00</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>Isofläche 5</sldExt:label>
								<sldExt:from>1.2</sldExt:from>
								<sldExt:to>1.4000000000000001</sldExt:to>
								<Stroke>
									<CssParameter name='stroke'>#ffaa00</CssParameter>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-linejoin'>mitre</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke-linecap'>butt</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
									<CssParameter name='fill'>#ffaa00</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>Isofläche 6</sldExt:label>
								<sldExt:from>1.4000000000000001</sldExt:from>
								<sldExt:to>1.6</sldExt:to>
								<Stroke>
									<CssParameter name='stroke'>#ff0000</CssParameter>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-linejoin'>mitre</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke-linecap'>butt</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
									<CssParameter name='fill'>#ff0000</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
						</sldExt:PolygonColorMap>
					</SurfacePolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>