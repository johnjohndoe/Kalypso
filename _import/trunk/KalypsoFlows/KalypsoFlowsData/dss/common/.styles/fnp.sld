<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>fnp_reg_nutzung</Name>
      <Title>fnp_reg_nutzung</Title>
      <Abstract>empty Abstract</Abstract>
      <FeatureTypeStyle>
        <Name>fnp_reg_nutzung</Name>
        <Rule>
          <Name>default</Name>
          <Title>Wohnbebauung</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>52114</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#dc480a</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Grünflächen</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:Or>
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
                <ogc:Literal>54124</ogc:Literal>
              </ogc:PropertyIsEqualTo>
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
                <ogc:Literal>52254</ogc:Literal>
              </ogc:PropertyIsEqualTo>
            </ogc:Or>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#09b923</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Wald</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>54234</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#149506</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Wasserflächen</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>55004</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#9b9bff</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Landwirtschaft</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>54114</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#deed76</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Hafenflächen</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>55154</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#778cc6</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Gemischte Baufläche, Char Dienstleistungszentrum</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>52314</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#804000</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Gewerbliche Baufläche</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>52144</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#c0c0c0</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Verkehrsflächen</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>53114</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
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
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>Flächen für Gemeinbedarf</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>52154</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#ff84be</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
              <CssParameter name="stroke-width">1.0</CssParameter>
              <CssParameter name="stroke-linejoin">mitre</CssParameter>
              <CssParameter name="stroke-opacity">1.0</CssParameter>
              <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
          </PolygonSymbolizer>
        </Rule>
        <Rule>
          <Name>default</Name>
          <Title>default</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsEqualTo>
              <ogc:PropertyName>NUTZUNG</ogc:PropertyName>
              <ogc:Literal>52164</ogc:Literal>
            </ogc:PropertyIsEqualTo>
          </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
          <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
          <PolygonSymbolizer>
            <Geometry>
              <ogc:PropertyName>GEOM</ogc:PropertyName>
            </Geometry>
            <Fill>
              <CssParameter name="fill-opacity">1.0</CssParameter>
              <CssParameter name="fill">#fffc80</CssParameter>
            </Fill>
            <Stroke>
              <CssParameter name="stroke">#000000</CssParameter>
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
