<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version='String' xmlns='http://www.opengis.net/sld' xmlns:gml='http://www.opengis.net/gml' xmlns:ogc='http://www.opengis.net/ogc' xmlns:xlink='http://www.w3.org/1999/xlink'
	xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
	<NamedLayer>
		<Name>dtmGrid</Name>
		<UserStyle>
			<Name>waterdepthUserStyle</Name>
			<Title>Fließtiefen-UserStyle</Title>
			<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
				<Name>depthGridFeatureTypeStyle</Name>
				<Rule>
					<Name>depthGridRule</Name>
					<Title>Fließtiefen</Title>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>9.0E99</MaxScaleDenominator>
					<RasterSymbolizer uom="pixel">
						<ColorMap>
							<ColorMapEntry label="0,00" color="#ff0000" opacity="0.8" quantity="0.0" />
							<ColorMapEntry label="0,10" color="#ff1a00" opacity="0.8" quantity="0.1" />
							<ColorMapEntry label="0,20" color="#ff3300" opacity="0.8" quantity="0.2" />
							<ColorMapEntry label="0,30" color="#ff4d00" opacity="0.8" quantity="0.3" />
							<ColorMapEntry label="0,40" color="#ff6600" opacity="0.8" quantity="0.4" />
							<ColorMapEntry label="0,50" color="#ff8000" opacity="0.8" quantity="0.5" />
							<ColorMapEntry label="0,60" color="#ff9900" opacity="0.8" quantity="0.6" />
							<ColorMapEntry label="0,70" color="#ffb300" opacity="0.8" quantity="0.7" />
							<ColorMapEntry label="0,80" color="#ffcc00" opacity="0.8" quantity="0.8" />
							<ColorMapEntry label="0,90" color="#ffe600" opacity="0.8" quantity="0.9" />
							<ColorMapEntry label="1,00" color="#ffff00" opacity="0.8" quantity="1.0" />
							<ColorMapEntry label="1,10" color="#e6ff00" opacity="0.8" quantity="1.1" />
							<ColorMapEntry label="1,20" color="#ccff00" opacity="0.8" quantity="1.2" />
							<ColorMapEntry label="1,30" color="#b3ff00" opacity="0.8" quantity="1.3" />
							<ColorMapEntry label="1,40" color="#99ff00" opacity="0.8" quantity="1.4" />
							<ColorMapEntry label="1,50" color="#80ff00" opacity="0.8" quantity="1.5" />
							<ColorMapEntry label="1,60" color="#66ff00" opacity="0.8" quantity="1.6" />
							<ColorMapEntry label="1,70" color="#4cff00" opacity="0.8" quantity="1.7" />
							<ColorMapEntry label="1,80" color="#33ff00" opacity="0.8" quantity="1.8" />
							<ColorMapEntry label="1,90" color="#1aff00" opacity="0.8" quantity="1.9" />
							<ColorMapEntry label="2,00" color="#00ff00" opacity="0.8" quantity="2.0" />
							<ColorMapEntry label="2,10" color="#00ff19" opacity="0.8" quantity="2.1" />
							<ColorMapEntry label="2,20" color="#00ff33" opacity="0.8" quantity="2.2" />
							<ColorMapEntry label="2,30" color="#00ff4d" opacity="0.8" quantity="2.3" />
							<ColorMapEntry label="2,40" color="#00ff66" opacity="0.8" quantity="2.4" />
							<ColorMapEntry label="2,50" color="#00ff80" opacity="0.8" quantity="2.5" />
							<ColorMapEntry label="2,60" color="#00ff99" opacity="0.8" quantity="2.6" />
							<ColorMapEntry label="2,70" color="#00ffb2" opacity="0.8" quantity="2.7" />
							<ColorMapEntry label="2,80" color="#00ffcc" opacity="0.8" quantity="2.8" />
							<ColorMapEntry label="2,90" color="#00ffe6" opacity="0.8" quantity="2.9" />
							<ColorMapEntry label="3,00" color="#00ffff" opacity="0.8" quantity="3.0" />
							<ColorMapEntry label="3,10" color="#00e5ff" opacity="0.8" quantity="3.1" />
							<ColorMapEntry label="3,20" color="#00ccff" opacity="0.8" quantity="3.2" />
							<ColorMapEntry label="3,30" color="#00b2ff" opacity="0.8" quantity="3.3" />
							<ColorMapEntry label="3,40" color="#0099ff" opacity="0.8" quantity="3.4" />
							<ColorMapEntry label="3,50" color="#007fff" opacity="0.8" quantity="3.5" />
							<ColorMapEntry label="3,60" color="#0066ff" opacity="0.8" quantity="3.6" />
							<ColorMapEntry label="3,70" color="#004cff" opacity="0.8" quantity="3.7" />
							<ColorMapEntry label="3,80" color="#0033ff" opacity="0.8" quantity="3.8" />
							<ColorMapEntry label="3,90" color="#001aff" opacity="0.8" quantity="3.9" />
							<ColorMapEntry label="4,00" color="#0000ff" opacity="0.8" quantity="4.0" />
						</ColorMap>
					</RasterSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>