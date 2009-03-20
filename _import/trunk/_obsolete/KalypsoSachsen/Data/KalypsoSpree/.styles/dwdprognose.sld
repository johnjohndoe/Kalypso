<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="1.0.0" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<NamedLayer>
		<Name>name</Name>
		<UserStyle>
			<Name>rasterVorschau</Name>
			<Title>Raster-Lage</Title>
			<FeatureTypeStyle>
				<Name>rasterFts</Name>
				<Title>-</Title>
				<Rule>
					<Name>rasterVorschauRule</Name>
					<Title>Lage</Title>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>coveredArea</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill-opacity">0.5</CssParameter>
							<CssParameter name="fill">#01059c</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
							<CssParameter name="stroke-width">1.0</CssParameter>
							<CssParameter name="stroke-linejoin">mitre</CssParameter>
							<CssParameter name="stroke-opacity">1.0</CssParameter>
							<CssParameter name="stroke-linecap">butt</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>