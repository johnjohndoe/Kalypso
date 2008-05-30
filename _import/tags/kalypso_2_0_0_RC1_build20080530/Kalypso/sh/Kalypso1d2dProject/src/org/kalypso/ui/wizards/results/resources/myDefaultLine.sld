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
			<Name>tinLineStyle</Name>
			<Title></Title>
			<Abstract />
			<FeatureTypeStyle>
				<Name>tinFeatureTypeStyle</Name>
				<Rule>
					<Name>tinWRule</Name>
					<Title></Title>
					<Abstract />
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>
						1.7976931348623157E308
					</MaxScaleDenominator>
					<sldExt:SurfaceLineSymbolizer>
						<Geometry>
							<ogc:PropertyName>
								res1d2d:triangulatedSurfaceMember
							</ogc:PropertyName>
						</Geometry>
						<sldExt:LineColorMap>
							<!-- the initial colormap is defined by the start color (from), 
								the end color (to), the standard stroke width and type, the step width 
								and the definition, which lines are drawn with specific stroke width -->

							<!-- from-entry: Describes, the start color of the color map and the standard stroke -->
							<sldExt:LineColorMapEntry>
								<sldExt:label>from</sldExt:label>
								<!-- quantity: not used -->
								<sldExt:quantity>0.0</sldExt:quantity>
								<Stroke>
									<!-- start value for color interpolation -->
									<CssParameter name="stroke">
										#ff0000
									</CssParameter>
									<!-- width for all normal entries -->
									<CssParameter name="stroke-width">
										1.0
									</CssParameter>
								</Stroke>
							</sldExt:LineColorMapEntry>
							<!-- to-entry: Describes, the end color of the color map -->
							<sldExt:LineColorMapEntry>
								<sldExt:label>to</sldExt:label>
								<!-- to-quantity: Describes the standard step width -->
								<sldExt:quantity>1.0</sldExt:quantity>
								<Stroke>
									<!-- end value for color interpolation -->
									<CssParameter name="stroke">
										#ff0000
									</CssParameter>
									<!-- not used -->
									<CssParameter name="stroke-width">
										1.0
									</CssParameter>
								</Stroke>
							</sldExt:LineColorMapEntry>
							<!-- fat-entry: Describes, which and how lines are drawn with an extra fat width -->
							<sldExt:LineColorMapEntry>
								<sldExt:label>fat</sldExt:label>
								<!-- Module by which fat lines are recognized. -->
								<sldExt:quantity>0.5</sldExt:quantity>
								<Stroke>
									<!-- not used, interpolated color is used instead -->
									<CssParameter name="stroke">
										#ff0000
									</CssParameter>
									<!-- width for all 'fat' entries -->
									<CssParameter name="stroke-width">
										3.0
									</CssParameter>
								</Stroke>
							</sldExt:LineColorMapEntry>
						</sldExt:LineColorMap>
					</sldExt:SurfaceLineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
