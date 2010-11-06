<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld"
	xmlns:sldExt="http://www.opengis.net/sldExt"
	xmlns:res1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResults"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	version="1.0.0">
	<NamedLayer>
		<Name>tinStyles</Name>
		<UserStyle>
			<Name>tinPolygonStyle</Name>
			<Title></Title>
			<Abstract />
			<FeatureTypeStyle>
				<Name>tinFeatureTypeStyle</Name>
				<Rule>
					<Name>tinRule</Name>
					<Title></Title>
					<Abstract />
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>
						1.7976931348623157E308
					</MaxScaleDenominator>
					<sldExt:SurfacePolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>
								res1d2d:triangulatedSurfaceMember
							</ogc:PropertyName>
						</Geometry>
						<sldExt:PolygonColorMap>
							<!-- the initial colormap is defined by the start color (from), 
								the end color (to), the standard stroke width and type, the step width 
								and the definition, which lines are drawn with specific stroke width -->
							<!-- from-entry: Describes, the start color of the color map and the standard fill and stroke -->
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>from</sldExt:label>
								<Fill>
									<CssParameter name="fill-opacity">
										1
									</CssParameter>
									<!-- start value for polygon color interpolation -->
									<CssParameter name="fill">
										#0000ff
									</CssParameter>
								</Fill>
								<!-- from: scale of the step width -->
								<sldExt:from>1.0</sldExt:from>
								<!-- to-entry: Describes the standard step width -->
								<sldExt:to>0.2</sldExt:to>
								<Stroke>
									<!-- start value for stroke color interpolation -->
									<CssParameter name="stroke">
										#ff0000
									</CssParameter>
									<!-- width for all normal entries -->
									<CssParameter name="stroke-width">
										1.0
									</CssParameter>
								</Stroke>
							</sldExt:PolygonColorMapEntry>
							<!-- to-entry: Describes, the end color of the color map -->
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>to</sldExt:label>
								<Fill>
									<CssParameter name="fill-opacity">
										1
									</CssParameter>
									<!-- end value for polygon color interpolation -->
									<CssParameter name="fill">
										#ff0000
									</CssParameter>
								</Fill>
								<Stroke>
									<!-- end value for stroke color interpolation -->
									<CssParameter name="stroke">
										#ff0000
									</CssParameter>
									<!-- not used -->
									<CssParameter name="stroke-width">
										1.0
									</CssParameter>
								</Stroke>
								<!-- not used -->
								<sldExt:from>0.0</sldExt:from>
								<!-- not used -->
								<sldExt:to>0.1</sldExt:to>
							</sldExt:PolygonColorMapEntry>
						</sldExt:PolygonColorMap>
					</sldExt:SurfacePolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
