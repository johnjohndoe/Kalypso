<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc">
  <Name>fts_name</Name>
  <Title>%fts_title</Title>
  <Rule>
    <Name>rule_Station_on</Name>
    <Title>%rule_station_on_title</Title>

    <ogc:Filter>
      <ogc:PropertyIsEqualTo>
        <ogc:PropertyName>active</ogc:PropertyName>
        <ogc:Literal>true</ogc:Literal>
      </ogc:PropertyIsEqualTo>
    </ogc:Filter>

    <PointSymbolizer>
      <Geometry>
        <ogc:PropertyName>stationLocation</ogc:PropertyName>
      </Geometry>
      <Graphic>
        <Mark>
          <WellKnownName>circle</WellKnownName>
          <Fill>
            <CssParameter name="fill-opacity">1.0</CssParameter>
            <CssParameter name="fill">#00cc00</CssParameter>
          </Fill>
          <Stroke>
            <CssParameter name="stroke">#000000</CssParameter>
            <CssParameter name="stroke-width">2.0</CssParameter>
            <CssParameter name="stroke-linejoin">round</CssParameter>
            <CssParameter name="stroke-opacity">1.0</CssParameter>
            <CssParameter name="stroke-linecap">butt</CssParameter>
          </Stroke>
        </Mark>
        <Opacity>1.0</Opacity>
        <Size>15.0</Size>
        <Rotation>0.0</Rotation>
      </Graphic>

    </PointSymbolizer>
  </Rule>

  <Rule>
    <Name>rule_Station_off</Name>
    <Title>%rule_station_off_title</Title>

    <ogc:Filter>
      <ogc:PropertyIsEqualTo>
        <ogc:PropertyName>active</ogc:PropertyName>
        <ogc:Literal>false</ogc:Literal>
      </ogc:PropertyIsEqualTo>
    </ogc:Filter>

    <PointSymbolizer>
      <Geometry>
        <ogc:PropertyName>stationLocation</ogc:PropertyName>
      </Geometry>
      <Graphic>
        <Mark>
          <WellKnownName>circle</WellKnownName>
          <Fill>
            <CssParameter name="fill-opacity">1.0</CssParameter>
            <CssParameter name="fill">#ffcccc</CssParameter>
          </Fill>
          <Stroke>
            <CssParameter name="stroke">#000000</CssParameter>
            <CssParameter name="stroke-width">2.0</CssParameter>
            <CssParameter name="stroke-linejoin">round</CssParameter>
            <CssParameter name="stroke-opacity">1.0</CssParameter>
            <CssParameter name="stroke-linecap">butt</CssParameter>
          </Stroke>
        </Mark>
        <Opacity>1.0</Opacity>
        <Size>15.0</Size>
        <Rotation>0.0</Rotation>
      </Graphic>
    </PointSymbolizer>
  </Rule>

  <Rule>
    <Name>rule_Area</Name>
    <Title>%rule_area_title</Title>

    <PolygonSymbolizer>
      <Geometry>
        <ogc:PropertyName>thiessenArea</ogc:PropertyName>
      </Geometry>
      <Fill>
        <CssParameter name="fill-opacity">0.2</CssParameter>
        <CssParameter name="fill">#019c05</CssParameter>
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