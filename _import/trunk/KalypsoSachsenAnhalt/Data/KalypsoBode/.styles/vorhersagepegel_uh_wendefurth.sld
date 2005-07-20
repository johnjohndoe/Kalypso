<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>Vorhersagepegel</Name>
      <Title>Vorhersagepegel</Title>
      <IsDefault>1</IsDefault>
      <FeatureTypeStyle>
        <Name>Vorhersagepegel</Name>
        <Rule>
          <Name>Vorhersagepegel</Name>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>istErgebnisPegel</ogc:PropertyName>
                <ogc:Literal>true</ogc:Literal>
              </ogc:PropertyIsEqualTo>
          
	          <ogc:Or>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>wend</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>thal</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>silb</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>meis</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>haus</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>ditf</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>wege</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>mahn</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>nien</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>osch</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>hadm</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>stas</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>neug</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	              <ogc:PropertyIsEqualTo>
	                <ogc:PropertyName>Kurz_Name</ogc:PropertyName>
	                <ogc:Literal>stei</ogc:Literal>
	              </ogc:PropertyIsEqualTo>
	            </ogc:Or>
            </ogc:And>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>9.0E99</MaxScaleDenominator>
          <PointSymbolizer>
            <Geometry>
              <ogc:PropertyName>Ort</ogc:PropertyName>
            </Geometry>
            <Graphic>
              <Mark>
                <WellKnownName>triangle</WellKnownName>
                <Fill>
                  <CssParameter name="fill">#ab0000</CssParameter>
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
              <ogc:PropertyName>Ort</ogc:PropertyName>
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
  </NamedLayer>
</StyledLayerDescriptor>
