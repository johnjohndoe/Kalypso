<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>GroundwaterNet</Name>
      <Title>GroundwaterNet</Title>
      <IsDefault>1</IsDefault>
      <FeatureTypeStyle>
        <Name>GroundwaterNet</Name>
        <Rule>
          <Name>default</Name>
          <Title>GroundwaterNet</Title>
          <Abstract>default</Abstract>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>161229.58987861115</MaxScaleDenominator>
          <LineSymbolizer>
            <Geometry>
              <ogc:PropertyName>link_grundwasserabflussMember</ogc:PropertyName>
            </Geometry>
            <Stroke>
              <CssParameter name="stroke">#c0c0c0</CssParameter>
              <CssParameter name="stroke-width">2.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </LineSymbolizer>
          <PointSymbolizer>
            <Geometry>
              <ogc:PropertyName>Ort</ogc:PropertyName>
            </Geometry>
            <Graphic>
              <Mark>
                <WellKnownName>triangle</WellKnownName>
                <Fill>
                  <CssParameter name="fill-opacity">1.0</CssParameter>
                  <CssParameter name="fill">#ffff00</CssParameter>
                </Fill>
                <Stroke>
                  <CssParameter name="stroke">#000000</CssParameter>
                  <CssParameter name="stroke-width">1.0</CssParameter>
                  <CssParameter name="stroke-linejoin">round</CssParameter>
                  <CssParameter name="stroke-opacity">1.0</CssParameter>
                  <CssParameter name="stroke-linecap">square</CssParameter>
                </Stroke>
              </Mark>
              <Opacity>1.0</Opacity>
              <Size>15.0</Size>
              <Rotation>0.0</Rotation>
            </Graphic>
          </PointSymbolizer>
        </Rule>
        <Rule>
          <Title>Subcatchment-Number</Title>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>12768.341359369135</MaxScaleDenominator>
          <TextSymbolizer>
            <Geometry>
              <ogc:PropertyName>Ort</ogc:PropertyName>
            </Geometry>
            <Label>
              <ogc:PropertyName>name</ogc:PropertyName>
            </Label>
            <Font>
              <CssParameter name="font-family"/>
              <CssParameter name="font-color">#000000</CssParameter>
              <CssParameter name="font-size">10.0</CssParameter>
              <CssParameter name="font-style">normal</CssParameter>
              <CssParameter name="font-weight">normal</CssParameter>
            </Font>
            <LabelPlacement>
              <PointPlacement>
                <AnchorPoint>
                  <AnchorPointX>0.0</AnchorPointX>
                  <AnchorPointY>0.0</AnchorPointY>
                </AnchorPoint>
                <Displacement>
                  <DisplacementX>-10.0</DisplacementX>
                  <DisplacementY>10.0</DisplacementY>
                </Displacement>
                <Rotation>0.0</Rotation>
              </PointPlacement>
            </LabelPlacement>
            <Halo>
              <Fill>
                <CssParameter name="fill-opacity">0.0</CssParameter>
                <CssParameter name="fill">#fbf193</CssParameter>
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
