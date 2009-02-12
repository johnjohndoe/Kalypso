<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>HydrogeologischeProfiltypen</Name>
      <Title>HydrogeologischeProfiltypen</Title>
      <Abstract>empty Abstract</Abstract>
      <FeatureTypeStyle>
        <Name>HydrogeologischeProfiltypen</Name>
        <Rule>
          <Name>default</Name>
          <Title>hydr. Profil 1</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>TYP_NR</ogc:PropertyName>
              <ogc:Literal>1</ogc:Literal>
            </ogc:PropertyIsLike>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>150000.01</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#ffff00</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">2.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>hydr. Profil 2</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>TYP_NR</ogc:PropertyName>
              <ogc:Literal>2</ogc:Literal>
            </ogc:PropertyIsLike>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>150000.01</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#ff8040</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">2.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>hydr. Profil 3</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>TYP_NR</ogc:PropertyName>
              <ogc:Literal>3</ogc:Literal>
            </ogc:PropertyIsLike>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>150000.01</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#ff0000</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">2.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>hydr. Profil 4</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>TYP_NR</ogc:PropertyName>
              <ogc:Literal>4</ogc:Literal>
            </ogc:PropertyIsLike>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>150000.01</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#00ff00</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">2.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>hydr. Profil 5</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>TYP_NR</ogc:PropertyName>
              <ogc:Literal>5</ogc:Literal>
            </ogc:PropertyIsLike>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>150000.01</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#008080</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">2.0</CssParameter>
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
