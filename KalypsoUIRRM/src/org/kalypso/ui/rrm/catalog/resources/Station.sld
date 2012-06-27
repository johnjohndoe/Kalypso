<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:tm="http://kalypso.sorgeforge.net/schemata/hydrology/timeseriesManagement">
  <Name>stations</Name>
  <Title>%fts_title</Title>

  <Rule>
    <Name>rule_hydrologicalStation</Name>
    <Title>%rule_hydrologicalStation_title</Title>

    <ogc:Filter>
      <ogc:PropertyIsEqualTo>
        <ogc:Function name="org.kalypso.deeegree.functionExpression.featureTypeName" />
        <ogc:Literal>{http://kalypso.sorgeforge.net/schemata/hydrology/timeseriesManagement}HydrologicalStation</ogc:Literal>
      </ogc:PropertyIsEqualTo>
    </ogc:Filter>

    <PointSymbolizer>
      <Geometry>
        <ogc:PropertyName>tm:location</ogc:PropertyName>
      </Geometry>
      <Graphic>
        <ExternalGraphic>
          <OnlineResource xlink:href="station_hydrological.png" />
          <Format>png</Format>
        </ExternalGraphic>

        <Mark>
          <WellKnownName>rectangle</WellKnownName>
          <Stroke>
            <CssParameter name="stroke">#000000</CssParameter>
            <CssParameter name="stroke-width">1.0</CssParameter>
            <CssParameter name="stroke-linejoin">round</CssParameter>
            <CssParameter name="stroke-opacity">1.0</CssParameter>
            <CssParameter name="stroke-linecap">butt</CssParameter>
          </Stroke>
        </Mark>

        <Opacity>1.0</Opacity>
        <Size>16.0</Size>
        <Rotation>0.0</Rotation>
      </Graphic>

    </PointSymbolizer>


    <TextSymbolizer>
      <Geometry>
        <ogc:PropertyName>tm:location</ogc:PropertyName>
      </Geometry>
      <Label>
        <ogc:PropertyName>gml:description</ogc:PropertyName>
      </Label>
      <Font>
        <CssParameter name="font-color">#000000</CssParameter>
        <CssParameter name="font-size">10.0</CssParameter>
        <CssParameter name="font-style">normal</CssParameter>
        <CssParameter name="font-weight">normal</CssParameter>
      </Font>
      <LabelPlacement>
        <PointPlacement>
          <Displacement>
            <DisplacementX>15</DisplacementX>
            <DisplacementY>15</DisplacementY>
          </Displacement>
        </PointPlacement>
      </LabelPlacement>
      <Halo>
        <Radius>3</Radius>
        <Fill>
          <CssParameter name="fill-opacity">0.7</CssParameter>
          <CssParameter name="fill">#FFFF80</CssParameter>
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

  <Rule>
    <Name>rule_meteorologicalStation</Name>
    <Title>%rule_meteorologicalStation_title</Title>

    <ogc:Filter>
      <ogc:PropertyIsEqualTo>
        <ogc:Function name="org.kalypso.deeegree.functionExpression.featureTypeName" />
        <ogc:Literal>{http://kalypso.sorgeforge.net/schemata/hydrology/timeseriesManagement}MeteorologicalStation</ogc:Literal>
      </ogc:PropertyIsEqualTo>
    </ogc:Filter>

    <PointSymbolizer>
      <Geometry>
        <ogc:PropertyName>tm:location</ogc:PropertyName>
      </Geometry>

      <Graphic>
        <ExternalGraphic>
          <OnlineResource xlink:href="station_meteorological.png" />
          <Format>png</Format>
        </ExternalGraphic>

        <Mark>
          <WellKnownName>rectangle</WellKnownName>
          <Stroke>
            <CssParameter name="stroke">#000000</CssParameter>
            <CssParameter name="stroke-width">1.0</CssParameter>
            <CssParameter name="stroke-linejoin">round</CssParameter>
            <CssParameter name="stroke-opacity">1.0</CssParameter>
            <CssParameter name="stroke-linecap">butt</CssParameter>
          </Stroke>
        </Mark>

        <Opacity>1.0</Opacity>
        <Size>16.0</Size>
        <Rotation>0.0</Rotation>
      </Graphic>
    </PointSymbolizer>
  </Rule>

</FeatureTypeStyle>