<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>Bodentyp</Name>
      <Title>Bodentyp</Title>
      <Abstract>empty Abstract</Abstract>
      <FeatureTypeStyle>
        <Name>Bodentyp</Name>
        <Rule>
          <Name>default</Name>
          <Title>Boden</Title>
          <Abstract>default</Abstract>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>150000.01</MaxScaleDenominator>
          <TextSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Label>
              <ogc:PropertyName>BODEN</ogc:PropertyName>
            </Label>
            <Font>
              <CssParameter name="font-family">SansSerif</CssParameter>
              <CssParameter name="font-color">#000000</CssParameter>
              <CssParameter name="font-size">8.0</CssParameter>
              <CssParameter name="font-style">normal</CssParameter>
              <CssParameter name="font-weight">normal</CssParameter>
            </Font>
            <Halo>
              <Fill>
                <CssParameter name="fill-opacity">0.0</CssParameter>
                <CssParameter name="fill">#808080</CssParameter>
              </Fill>
              <Stroke>
                <CssParameter name="stroke">#000000</CssParameter>
                <CssParameter name="stroke-width">1.0</CssParameter>
                <CssParameter name="stroke-linejoin">round</CssParameter>
                <CssParameter name="stroke-opacity">0.24</CssParameter>
                <CssParameter name="stroke-linecap">square</CssParameter>
              </Stroke>
            </Halo>
          </TextSymbolizer>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">0.0</CssParameter>
              <CssParameter name="fill">#808080</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#ffffff</CssParameter>
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
