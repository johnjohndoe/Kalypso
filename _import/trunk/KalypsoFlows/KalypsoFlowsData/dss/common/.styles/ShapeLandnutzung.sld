<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>deegree style definition</Name>
    <UserStyle>
      <Name>Landnutzungsklasse</Name>
      <Title>Landnutzungsklasse</Title>
      <Abstract>empty Abstract</Abstract>
      <FeatureTypeStyle>
        <Name>Landnutzungsklasse</Name>
        <Rule>
          <Name>default</Name>
          <Title>Klasse 1</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
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
          <Title>Klasse 2</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
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
              <CssParameter name="fill">#80ff00</CssParameter>
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
          <Title>Klasse 3</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
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
        <Rule>
          <Name>default</Name>
          <Title>Klasse 4</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
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
              <CssParameter name="fill">#8080ff</CssParameter>
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
          <Title>Klasse 5</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
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
              <CssParameter name="fill">#804000</CssParameter>
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
          <Title>Klasse 6</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>6</ogc:Literal>
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
              <CssParameter name="fill">#ff8000</CssParameter>
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
          <Title>Klasse 7</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>7</ogc:Literal>
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
              <CssParameter name="fill">#804040</CssParameter>
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
          <Title>Klasse 8</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>8</ogc:Literal>
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
          <Title>Klasse 9</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>9</ogc:Literal>
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
              <CssParameter name="fill">#808080</CssParameter>
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
          <Title>Klasse 10</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>10</ogc:Literal>
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
              <CssParameter name="fill">#ff0080</CssParameter>
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
          <Title>Klasse 11</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>11</ogc:Literal>
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
              <CssParameter name="fill">#80ffff</CssParameter>
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
          <Title>Klasse 12</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>12</ogc:Literal>
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
              <CssParameter name="fill">#000040</CssParameter>
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
          <Title>Klasse 13</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>13</ogc:Literal>
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
              <CssParameter name="fill">#8080c0</CssParameter>
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
          <Title>Klasse 14</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>14</ogc:Literal>
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
              <CssParameter name="fill">#0080ff</CssParameter>
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
          <Title>Klasse 15</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>15</ogc:Literal>
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
              <CssParameter name="fill">#004000</CssParameter>
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
          <Title>Klasse 16</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>16</ogc:Literal>
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
          <Title>Klasse 17</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>17</ogc:Literal>
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
              <CssParameter name="fill">#808080</CssParameter>
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
          <Title>Klasse 18</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>18</ogc:Literal>
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
              <CssParameter name="fill">#ff8080</CssParameter>
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
          <Title>Klasse 19</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>19</ogc:Literal>
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
              <CssParameter name="fill">#0000a0</CssParameter>
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
          <Title>Klasse 20</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>20</ogc:Literal>
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
              <CssParameter name="fill">#8000ff</CssParameter>
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
          <Title>Klasse 21</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>21</ogc:Literal>
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
              <CssParameter name="fill">#808040</CssParameter>
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
          <Title>Klasse 22</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>22</ogc:Literal>
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
        <Rule>
          <Name>default</Name>
          <Title>Klasse 23</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>23</ogc:Literal>
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
              <CssParameter name="fill">#000000</CssParameter>
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
          <Title>Klasse 24</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>24</ogc:Literal>
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
              <CssParameter name="fill">#808000</CssParameter>
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
          <Title>Klasse 25</Title>
          <Abstract>default</Abstract>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:PropertyIsLike wildCard="*" singleChar="?" escape="\">
              <ogc:PropertyName>KLASSENEU</ogc:PropertyName>
              <ogc:Literal>25</ogc:Literal>
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
              <CssParameter name="fill">#8080ff</CssParameter>
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
