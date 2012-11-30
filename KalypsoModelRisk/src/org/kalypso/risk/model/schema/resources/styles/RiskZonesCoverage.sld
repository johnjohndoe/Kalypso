<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:sld="http://www.opengis.net/sld" xmlns:sldExt="http://www.opengis.net/sldExt" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.0.0">
	<Rule>
		<Name>default</Name>
		<Title>default</Title>
		<Abstract>default</Abstract>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<RasterSymbolizer uom="pixel">
			<ColorMap>
				<ColorMapEntry color="#8000ff" label="mittlere Betroffenheit, Freiflächen" opacity="0.9" quantity="-100000000000.0" />
				<ColorMapEntry color="#8000ff" label="mittlere Betroffenheit, Freiflächen" opacity="0.9" quantity="-0.1" />
				<ColorMapEntry color="#8080ff" label="mäßige Betroffenheit, Freiflächen" opacity="0.9" quantity="0.0" />
				<ColorMapEntry color="#ffff00" label="mäßige Betroffenheit, bebaute Flächen" opacity="0.9" quantity="0.1" />
				<ColorMapEntry color="#ff8040" label="mittlere Betroffenheit, bebaute Flächen" opacity="0.9" quantity="1.0" />
				<ColorMapEntry color="#ff0000" label="hohe Betroffenheit, bebaute Flächen" opacity="0.9" quantity="1.7976931348623157E308" />
			</ColorMap>
		</RasterSymbolizer>
	</Rule>
</FeatureTypeStyle>
