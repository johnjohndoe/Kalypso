<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>Eingabepegel</Name>
      <Title>Eingabepegel</Title>
      <IsDefault>1</IsDefault>
      <FeatureTypeStyle>
        <Name>Eingabepegel</Name>
        <Rule>
          <Name>Eingabepegel</Name>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:And>
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>QDAT</ogc:PropertyName>
              <ogc:Literal>1</ogc:Literal>
            </ogc:PropertyIsEqualTo>
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>TS_Steuerung_Gruppe</ogc:PropertyName>
              <ogc:Literal>20</ogc:Literal>
            </ogc:PropertyIsEqualTo>
            </ogc:And>
          </ogc:Filter>
          <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>9.0E99</MaxScaleDenominator>
          <PointSymbolizer>
            <Geometry>
              <ogc:PropertyName>location</ogc:PropertyName>
            </Geometry>
            <Graphic>
              <Mark>
                <WellKnownName>triangle</WellKnownName>
                <Fill>
                  <CssParameter name="fill">#c850ef</CssParameter>
                </Fill>
                <Stroke>
                  <CssParameter name="stroke">#111111</CssParameter>
                  <CssParameter name="stroke-width">1.0</CssParameter>
                </Stroke>
              </Mark>
              <Size>15.0</Size>
            </Graphic>
          </PointSymbolizer>
          <TextSymbolizer>
            <Geometry>
              <ogc:PropertyName>location</ogc:PropertyName>
            </Geometry>
            <Label>
              <ogc:PropertyName>Name</ogc:PropertyName>
            </Label>
            <Font>
              <CssParameter name="font-family"/>
              <CssParameter name="font-color">#000000</CssParameter>
              <CssParameter name="font-size">11.0</CssParameter>
              <CssParameter name="font-style">normal</CssParameter>
              <CssParameter name="font-weight">normal</CssParameter>
            </Font>
            <LabelPlacement>
              <PointPlacement auto="true">
                <Displacement>
                  <DisplacementX>10.0</DisplacementX>
                  <DisplacementY>10.0</DisplacementY>
                </Displacement>
              </PointPlacement>
            </LabelPlacement>
            <Halo>
              <Fill>
                <CssParameter name="fill-opacity">1.0</CssParameter>
                <CssParameter name="fill">#ffffcc</CssParameter>
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
    <UserStyle>
      <Name>PNR</Name>
      <Title>Eingabepegel mit Nummer</Title>
      <IsDefault>0</IsDefault>
      <FeatureTypeStyle>
        <Name>Eingabepegel mit Nummer</Name>
        <Rule>
          <Name>Eingabepegel</Name>
		  <MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>9.0E99</MaxScaleDenominator>
          <PointSymbolizer>
            <Geometry>
              <ogc:PropertyName>location</ogc:PropertyName>
            </Geometry>
            <Graphic>
              <Mark>
                <WellKnownName>triangle</WellKnownName>
                <Fill>
                  <CssParameter name="fill">#c850ef</CssParameter>
                </Fill>
                <Stroke>
                  <CssParameter name="stroke">#111111</CssParameter>
                  <CssParameter name="stroke-width">1.0</CssParameter>
                </Stroke>
              </Mark>
              <Size>15.0</Size>
            </Graphic>
          </PointSymbolizer>
          <TextSymbolizer>
            <Geometry>
              <ogc:PropertyName>location</ogc:PropertyName>
            </Geometry>
            <Label>
              <ogc:PropertyName>PNR</ogc:PropertyName>
            </Label>
            <Font>
              <CssParameter name="font-family"/>
              <CssParameter name="font-color">#000000</CssParameter>
              <CssParameter name="font-size">11.0</CssParameter>
              <CssParameter name="font-style">normal</CssParameter>
              <CssParameter name="font-weight">normal</CssParameter>
            </Font>
            <LabelPlacement>
              <PointPlacement auto="true">
                <Displacement>
                  <DisplacementX>10.0</DisplacementX>
                  <DisplacementY>10.0</DisplacementY>
                </Displacement>
              </PointPlacement>
            </LabelPlacement>
            <Halo>
              <Fill>
                <CssParameter name="fill-opacity">1.0</CssParameter>
                <CssParameter name="fill">#ffffcc</CssParameter>
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
