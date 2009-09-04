<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version='String' xmlns='http://www.opengis.net/sld' xmlns:sld='http://www.opengis.net/sld' xmlns:sldExt='http://www.opengis.net/sldExt' xmlns:gml='http://www.opengis.net/gml'
  xmlns:ogc='http://www.opengis.net/ogc' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
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
              <ColorMapEntry label="0,50" color="#ff2200" opacity="0.8" quantity="0.5" />
              <ColorMapEntry label="1,00" color="#ff4400" opacity="0.8" quantity="1.0" />
              <ColorMapEntry label="1,50" color="#ff6600" opacity="0.8" quantity="1.5" />
              <ColorMapEntry label="2,00" color="#ff8800" opacity="0.8" quantity="2.0" />
              <ColorMapEntry label="2,50" color="#ffaa00" opacity="0.8" quantity="2.5" />
              <ColorMapEntry label="3,00" color="#ffcc00" opacity="0.8" quantity="3.0" />
              <ColorMapEntry label="3,50" color="#ffee00" opacity="0.8" quantity="3.5" />
              <ColorMapEntry label="4,00" color="#eeff00" opacity="0.8" quantity="4.0" />
              <ColorMapEntry label="4,50" color="#ccff00" opacity="0.8" quantity="4.5" />
              <ColorMapEntry label="5,00" color="#aaff00" opacity="0.8" quantity="5.0" />
              <ColorMapEntry label="5,50" color="#88ff00" opacity="0.8" quantity="5.5" />
              <ColorMapEntry label="6,00" color="#66ff00" opacity="0.8" quantity="6.0" />
              <ColorMapEntry label="6,50" color="#44ff00" opacity="0.8" quantity="6.5" />
              <ColorMapEntry label="7,00" color="#22ff00" opacity="0.8" quantity="7.0" />
              <ColorMapEntry label="7,50" color="#00ff00" opacity="0.8" quantity="7.5" />
              <ColorMapEntry label="8,00" color="#00ff22" opacity="0.8" quantity="8.0" />
              <ColorMapEntry label="8,50" color="#00ff44" opacity="0.8" quantity="8.5" />
              <ColorMapEntry label="9,00" color="#00ff66" opacity="0.8" quantity="9.0" />
              <ColorMapEntry label="9,50" color="#00ff88" opacity="0.8" quantity="9.5" />
              <ColorMapEntry label="10,00" color="#00ffaa" opacity="0.8" quantity="10.0" />
              <ColorMapEntry label="10,50" color="#00ffcc" opacity="0.8" quantity="10.5" />
              <ColorMapEntry label="11,00" color="#00ffee" opacity="0.8" quantity="11.0" />
              <ColorMapEntry label="11,50" color="#00eeff" opacity="0.8" quantity="11.5" />
              <ColorMapEntry label="12,00" color="#00ccff" opacity="0.8" quantity="12.0" />
              <ColorMapEntry label="12,50" color="#00aaff" opacity="0.8" quantity="12.5" />
              <ColorMapEntry label="13,00" color="#0088ff" opacity="0.8" quantity="13.0" />
              <ColorMapEntry label="13,50" color="#0066ff" opacity="0.8" quantity="13.5" />
              <ColorMapEntry label="14,00" color="#0044ff" opacity="0.8" quantity="14.0" />
              <ColorMapEntry label="14,50" color="#0022ff" opacity="0.8" quantity="14.5" />
              <ColorMapEntry label="15,00" color="#0000ff" opacity="0.8" quantity="15.0" />
            </ColorMap>
          </RasterSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>