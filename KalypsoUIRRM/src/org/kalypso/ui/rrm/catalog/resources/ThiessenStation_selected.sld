<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc">
  <Name>fts_name</Name>
  <Title>%fts_title</Title>
  <Rule>
    <Name>rule_Station_selected</Name>
    <Title>%rule_station_selected</Title>

    <PointSymbolizer>
      <Geometry>
        <ogc:PropertyName>stationLocation</ogc:PropertyName>
      </Geometry>
      <Graphic>
        <Mark>
          <WellKnownName>circle</WellKnownName>
          <Fill>
            <CssParameter name="fill-opacity">1.0</CssParameter>
            <CssParameter name="fill">#ffff00</CssParameter>
          </Fill>
          <Stroke>
            <CssParameter name="stroke">#ff0000</CssParameter>
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
    <Name>rule_Area_selected</Name>
    <Title>%rule_area_title_selected</Title>

    <PolygonSymbolizer>
      <Geometry>
        <ogc:PropertyName>thiessenArea</ogc:PropertyName>
      </Geometry>
      <Fill>
        <CssParameter name="fill-opacity">0.2</CssParameter>
        <CssParameter name="fill">#ffff00</CssParameter>
      </Fill>
      <Stroke>
        <CssParameter name="stroke">#ff0000</CssParameter>
        <CssParameter name="stroke-width">3.0</CssParameter>
        <CssParameter name="stroke-linejoin">mitre</CssParameter>
        <CssParameter name="stroke-opacity">1.0</CssParameter>
        <CssParameter name="stroke-linecap">butt</CssParameter>
      </Stroke>
    </PolygonSymbolizer>

  </Rule>
</FeatureTypeStyle>