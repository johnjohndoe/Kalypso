<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>sachsenrivers</Name>
      <Title>rivers</Title>
      <IsDefault>1</IsDefault>
      <FeatureTypeStyle>
        <Name>GewÃ¤sser</Name>
        <Rule>
          <Name>GewÃ¤sser</Name>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>9.0E99</MaxScaleDenominator>
          <LineSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Stroke>
              <CssParameter name="stroke-width">2</CssParameter>
              <CssParameter name="stroke">#0000c6</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </LineSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>
