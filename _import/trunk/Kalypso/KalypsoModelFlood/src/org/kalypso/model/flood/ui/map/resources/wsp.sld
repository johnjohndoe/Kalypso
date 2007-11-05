<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="1.0.0" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<NamedLayer>
		<Name>wspLayer</Name>
		<UserStyle>
			<Name>wspUserStyle</Name>
			<Title>Wasserspiegel-UserStyle</Title>
			<FeatureTypeStyle>
				<Name>wspFts</Name>
				<Rule>
					<Name>wspRule</Name>
					<Title>Wasserspiegel</Title>
					<sldExt:SurfacePolygonSymbolizer xmlns:sldExt="http://www.opengis.net/sldExt" uom="pixel">
						<Name>wspPolygonSymbolizer</Name>
						<Geometry>
							<ogc:PropertyName>res1d2d:triangulatedSurfaceMember</ogc:PropertyName>
						</Geometry>
						<sldExt:PolygonColorMap>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>Isofläche 24</sldExt:label>
								<sldExt:from>0.0</sldExt:from>
								<sldExt:to>400.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#aa0000</CssParameter>
									<CssParameter name='stroke-linejoin'>mitre</CssParameter>
									<CssParameter name='stroke-linecap'>butt</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#aaaaf00</CssParameter>
									<CssParameter name='fill-opacity'>0.21</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>Isofläche 25</sldExt:label>
								<sldExt:from>400.0</sldExt:from>
								<sldExt:to>9000.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#ff0000</CssParameter>
									<CssParameter name='stroke-linejoin'>mitre</CssParameter>
									<CssParameter name='stroke-linecap'>butt</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#fffff00</CssParameter>
									<CssParameter name='fill-opacity'>0.21</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
						</sldExt:PolygonColorMap>
					</sldExt:SurfacePolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
