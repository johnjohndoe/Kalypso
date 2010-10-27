<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version='String' xmlns='http://www.opengis.net/sld' xmlns:sld='http://www.opengis.net/sld' xmlns:sldExt='http://www.opengis.net/sldExt' xmlns:gml='http://www.opengis.net/gml'
  xmlns:ogc='http://www.opengis.net/ogc' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
  <NamedLayer>
    <Name>dtmGrid</Name>
    <UserStyle>
      <Name>dtmGridUserStyle</Name>
      <Title>Höhenmodell-UserStyle</Title>
      <FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
        <Name>dtmGridFeatureTypeStyle</Name>
        <Rule>
          <Name>dtmGridRule</Name>
          <Title>Höhenmodelle</Title>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>9.0E99</MaxScaleDenominator>
          <RasterSymbolizer uom="pixel">
            <ColorMap>
              <ColorMapEntry label="350,00" color="#cc3d3d" opacity="0.3" quantity="350.0" />
              <ColorMapEntry label="355,00" color="#ccbe3d" opacity="0.3" quantity="355.0" />
              <ColorMapEntry label="360,00" color="#5acc3d" opacity="0.3" quantity="360.0" />
              <ColorMapEntry label="365,00" color="#3dcca1" opacity="0.3" quantity="365.0" />
              <ColorMapEntry label="370,00" color="#3d76cc" opacity="0.3" quantity="370.0" />
              <ColorMapEntry label="375,00" color="#843dcc" opacity="0.3" quantity="375.0" />
              <ColorMapEntry label="380,00" color="#cc3d93" opacity="0.3" quantity="380.0" />
            </ColorMap>
          </RasterSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>