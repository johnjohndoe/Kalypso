<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>NodeNet</Name>
      <Title>NodeNet</Title>
      <IsDefault>1</IsDefault>
      <FeatureTypeStyle>
        <Name>NodeNet</Name>
        <Rule>
          <Name>default</Name>
          <Title>NodeNet</Title>
          <Abstract>default</Abstract>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>161229.58987861115</MaxScaleDenominator>
          <LineSymbolizer>
            <Geometry>
              <ogc:PropertyName>link_downStreamChannelMember</ogc:PropertyName>
            </Geometry>
            <Stroke>
              <CssParameter name="stroke">#ff8040</CssParameter>
              <CssParameter name="stroke-width">2.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </LineSymbolizer>
          <PointSymbolizer>
            <Graphic>
              <Mark>
                <WellKnownName>circle</WellKnownName>
                <Fill>
                  <CssParameter name="fill-opacity">1.0</CssParameter>
                  <CssParameter name="fill">#ff8040</CssParameter>
                </Fill>
                <Stroke>
                  <CssParameter name="stroke">#000000</CssParameter>
                  <CssParameter name="stroke-width">5.0</CssParameter>
                  <CssParameter name="stroke-linejoin">round</CssParameter>
                  <CssParameter name="stroke-opacity">1.0</CssParameter>
                  <CssParameter name="stroke-linecap">square</CssParameter>
                </Stroke>
              </Mark>
              <Opacity>1.0</Opacity>
              <Size>10.0</Size>
              <Rotation>0.0</Rotation>
            </Graphic>
          </PointSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Node-Name</Title>
          <Abstract>default</Abstract>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>42531.025376162834</MaxScaleDenominator>
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
                  <DisplacementX>4.0</DisplacementX>
                  <DisplacementY>-10.0</DisplacementY>
                </Displacement>
                <Rotation>0.0</Rotation>
              </PointPlacement>
            </LabelPlacement>
            <Halo>
              <Fill>
                <CssParameter name="fill-opacity">0.0</CssParameter>
                <CssParameter name="fill">#ff0000</CssParameter>
              </Fill>
              <Stroke>
                <CssParameter name="stroke">#000000</CssParameter>
                <CssParameter name="stroke-width">1.0</CssParameter>
                <CssParameter name="stroke-linejoin">round</CssParameter>
                <CssParameter name="stroke-opacity">0.0</CssParameter>
                <CssParameter name="stroke-linecap">square</CssParameter>
              </Stroke>
            </Halo>
          </TextSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Pegel</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:Not>
              <ogc:PropertyIsNull>
                <ogc:PropertyName>pegelZR</ogc:PropertyName>
              </ogc:PropertyIsNull>
            </ogc:Not>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PointSymbolizer>
            <Geometry>
              <ogc:PropertyName>Ort</ogc:PropertyName>
            </Geometry>
            <Graphic>
              <Mark>
                <WellKnownName>circle</WellKnownName>
                <Fill>
                  <CssParameter name="fill-opacity">1.0</CssParameter>
                  <CssParameter name="fill">#ff8040</CssParameter>
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
              <Size>14.0</Size>
              <Rotation>0.0</Rotation>
            </Graphic>
          </PointSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>NodeToNode</Title>
          <Abstract>default</Abstract>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>161229.58987861115</MaxScaleDenominator>
          <LineSymbolizer>
            <Geometry>
              <ogc:PropertyName>link_verzweigungNodeMember</ogc:PropertyName>
            </Geometry>
            <Stroke>
              <CssParameter name="stroke">#80ff00</CssParameter>
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
