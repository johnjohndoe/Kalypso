<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>Node</Name>
      <Title>Node</Title>
      <IsDefault>1</IsDefault>
      <FeatureTypeStyle>
        <Name>Node</Name>
        <Rule>
          <Name>default</Name>
          <Title>net</Title>
          <Abstract>default</Abstract>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>3467892.8907272248</MaxScaleDenominator>
          <LineSymbolizer>
            <Geometry>
              <ogc:PropertyName>link_downStreamChannelMember</ogc:PropertyName>
            </Geometry>
            <Stroke>
              <CssParameter name="stroke">#0000ff</CssParameter>
              <CssParameter name="stroke-width">2.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </LineSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>
