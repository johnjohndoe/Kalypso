<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>RHBChannel</Name>
      <Title>RHBChannel</Title>
      <IsDefault>1</IsDefault>
      <FeatureTypeStyle>
        <Name>RHBChannel</Name>
        <Rule>
          <Name>default</Name>
          <Title>RHBChannel</Title>
          <Abstract>default</Abstract>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>77277.8382171277</MaxScaleDenominator>
          <LineSymbolizer>
            <Geometry>
              <ogc:PropertyName>Ort</ogc:PropertyName>
            </Geometry>
            <Stroke>
              <CssParameter name="stroke">#0080c0</CssParameter>
              <CssParameter name="stroke-width">2.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-dasharray">1.0,0.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </LineSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>RHBChannel-Number</Title>
          <Abstract>default</Abstract>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>10216.930727378667</MaxScaleDenominator>
          <TextSymbolizer>
            <Geometry>
              <ogc:PropertyName>Ort</ogc:PropertyName>
            </Geometry>
            <Label>
              <ogc:PropertyName>inum</ogc:PropertyName>
            </Label>
            <Font>
              <CssParameter name="font-family"/>
              <CssParameter name="font-color">#000000</CssParameter>
              <CssParameter name="font-size">12.0</CssParameter>
              <CssParameter name="font-style">normal</CssParameter>
              <CssParameter name="font-weight">normal</CssParameter>
            </Font>
            <LabelPlacement>
              <LinePlacement>
                <PerpendicularOffset>center</PerpendicularOffset>
                <LineWidth>2</LineWidth>
                <Gap>10</Gap>
              </LinePlacement>
            </LabelPlacement>
            <Halo>
              <Fill>
                <CssParameter name="fill-opacity">1.0</CssParameter>
                <CssParameter name="fill">#0080c0</CssParameter>
              </Fill>
              <Stroke>
                <CssParameter name="stroke">#000000</CssParameter>
                <CssParameter name="stroke-width">1.0</CssParameter>
                <CssParameter name="stroke-linejoin">round</CssParameter>
                <CssParameter name="stroke-opacity">1.0</CssParameter>
                <CssParameter name="stroke-linecap">square</CssParameter>
              </Stroke>
            </Halo>
          </TextSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>
