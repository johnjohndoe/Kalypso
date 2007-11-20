<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<NamedLayer>
		<Name>dtmGrid</Name>
		<UserStyle>
			<Name>waterdepthUserStyle</Name>
			<Title>Fließtiefen-UserStyle</Title>
			<FeatureTypeStyle>
				<Name>depthFeatureTypeStyle</Name>
				<Rule>
					<Name>depthGridRule</Name>
					<Title>Fließtiefen</Title>
					<RasterSymbolizer>
						<ColorMap>
							<ColorMapEntry color="#cc3d3d" opacity="0.3" quantity="0" />
							<ColorMapEntry color="#cc933d" opacity="0.3" quantity="1" />
							<ColorMapEntry color="#afcc3d" opacity="0.3" quantity="2" />
							<ColorMapEntry color="#5acc3d" opacity="0.3" quantity="3" />
							<ColorMapEntry color="#3dcc76" opacity="0.3" quantity="4" />
							<ColorMapEntry color="#3dcccc" opacity="0.3" quantity="5" />
							<ColorMapEntry color="#3d76cc" opacity="0.3" quantity="432" />
							<ColorMapEntry color="#5a3dcc" opacity="0.3" quantity="434" />
							<ColorMapEntry color="#af3dcc" opacity="0.3" quantity="436" />
							<ColorMapEntry color="#cc3d93" opacity="0.3" quantity="438" />
						</ColorMap>
					</RasterSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
