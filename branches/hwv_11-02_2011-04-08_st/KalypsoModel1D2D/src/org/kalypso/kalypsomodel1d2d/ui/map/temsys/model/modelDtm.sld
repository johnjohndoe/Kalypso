<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version='1.0.0' xmlns='http://www.opengis.net/sld'
  xmlns:sldExt='http://www.opengis.net/sldExt' xmlns:gml='http://www.opengis.net/gml' xmlns:ogc='http://www.opengis.net/ogc'
  xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'
>
  <NamedLayer>
    <Name>tinNamedLayer</Name>
    <UserStyle>
      <Name>tinLineStyle</Name>
      <Title>Isolines</Title>
      <FeatureTypeStyle>
        <Name>ftsIsolines</Name>
        <Rule>
          <Name>ruleIsolines</Name>
          <SurfaceLineSymbolizer uom="pixel">
            <Geometry>
              <ogc:PropertyName>res1d2d:triangulatedSurfaceMember</ogc:PropertyName>
            </Geometry>
            <sldExt:LineColorMap>
              <sldExt:LineColorMapEntry>
                <Stroke>
                  <CssParameter name='stroke-width'>1.0</CssParameter>
                  <CssParameter name='stroke-opacity'>1.0</CssParameter>
                  <CssParameter name='stroke'>#ff0000</CssParameter>
                  <CssParameter name='stroke-linejoin'>round</CssParameter>
                  <CssParameter name='stroke-linecap'>square</CssParameter>
                </Stroke>
                <sldExt:label>Template for thin entry</sldExt:label>
                <sldExt:quantity>0.0</sldExt:quantity>
              </sldExt:LineColorMapEntry>
              <sldExt:LineColorMapEntry>
                <Stroke>
                  <CssParameter name='stroke-width'>4.0</CssParameter>
                  <CssParameter name='stroke-opacity'>1.0</CssParameter>
                  <CssParameter name='stroke'>#ff0000</CssParameter>
                  <CssParameter name='stroke-linejoin'>round</CssParameter>
                  <CssParameter name='stroke-linecap'>square</CssParameter>
                </Stroke>
                <sldExt:label>Template for fat entry</sldExt:label>
                <sldExt:quantity>0.25</sldExt:quantity>
              </sldExt:LineColorMapEntry>
            </sldExt:LineColorMap>
          </SurfaceLineSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
    <UserStyle>
      <Name>tinPolyStyle</Name>
      <FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
        <Name>ftsPolygones</Name>
        <Title>Surface</Title>
        <Rule>
          <Name>rulePolygones</Name>
          <SurfacePolygonSymbolizer uom="pixel">
            <Geometry>
              <ogc:PropertyName>res1d2d:triangulatedSurfaceMember</ogc:PropertyName>
            </Geometry>
            <sldExt:PolygonColorMap>
              <sldExt:PolygonColorMapEntry>
                <sldExt:label>0,00 - 0,25</sldExt:label>
                <sldExt:from>0.0</sldExt:from>
                <sldExt:to>0.25</sldExt:to>
                <Stroke>
                  <CssParameter name='stroke-width'>0.01</CssParameter>
                  <CssParameter name='stroke-opacity'>0.0</CssParameter>
                  <CssParameter name='stroke'>#80ffff</CssParameter>
                  <CssParameter name='stroke-linejoin'>round</CssParameter>
                  <CssParameter name='stroke-linecap'>round</CssParameter>
                </Stroke>
                <Fill>
                  <CssParameter name='fill'>#80ffff</CssParameter>
                  <CssParameter name='fill-opacity'>0.4</CssParameter>
                </Fill>
              </sldExt:PolygonColorMapEntry>
              <sldExt:PolygonColorMapEntry>
                <sldExt:label>10,25 - 10,50</sldExt:label>
                <sldExt:from>10.25</sldExt:from>
                <sldExt:to>10.5</sldExt:to>
                <Stroke>
                  <CssParameter name='stroke-width'>0.01</CssParameter>
                  <CssParameter name='stroke-opacity'>0.0</CssParameter>
                  <CssParameter name='stroke'>#000080</CssParameter>
                  <CssParameter name='stroke-linejoin'>round</CssParameter>
                  <CssParameter name='stroke-linecap'>round</CssParameter>
                </Stroke>
                <Fill>
                  <CssParameter name='fill'>#000080</CssParameter>
                  <CssParameter name='fill-opacity'>0.4</CssParameter>
                </Fill>
              </sldExt:PolygonColorMapEntry>
            </sldExt:PolygonColorMap>
          </SurfacePolygonSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>