<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>Auslaesse</Name>
      <Title>Auslaesse</Title>
      <Abstract>empty Abstract</Abstract>
      <FeatureTypeStyle>
        <Name></Name>
        <Rule>
          <Name>default</Name>
          <Title>default</Title>
          <Abstract>default</Abstract>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PointSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Graphic>
              <Mark>
                <WellKnownName>square</WellKnownName>
                <Fill>
                  <CssParameter name="fill-opacity">1.0</CssParameter>
                  <CssParameter name="fill">#004080</CssParameter>
                </Fill>
                <Stroke>
                  <CssParameter name="stroke">#00ff00</CssParameter>
                  <CssParameter name="stroke-width">1.0</CssParameter>
                  <CssParameter name="stroke-linejoin">round</CssParameter>
                  <CssParameter name="stroke-opacity">1.0</CssParameter>
                  <CssParameter name="stroke-linecap">square</CssParameter>
                </Stroke>
              </Mark>
              <Opacity>1.0</Opacity>
              <Size>8.0</Size>
              <Rotation>0.0</Rotation>
            </Graphic>
          </PointSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>
