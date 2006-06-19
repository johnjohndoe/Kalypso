<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="String">
<NamedLayer>
<Name>deegree style definition</Name>
<UserStyle>
<Name>Landnutzung</Name>
<Title>Landnutzung</Title>
<Abstract>empty Abstract</Abstract>
<FeatureTypeStyle>
<Name>Landnutzung</Name>
<Rule>
<Name>default</Name>
<Title>default</Title>
<Abstract>default</Abstract>
<ogc:Filter>
<ogc:PropertyIsBetween>
<ogc:PropertyName>KLASSENEU</ogc:PropertyName>
<ogc:LowerBoundary>1.0</ogc:LowerBoundary>
<ogc:UpperBoundary>10.0</ogc:UpperBoundary>
</ogc:PropertyIsBetween>
</ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<PolygonSymbolizer>
<Geometry>
<ogc:PropertyName>GEOM</ogc:PropertyName>
</Geometry>
<Fill>
<CssParameter name="fill-opacity">0.3</CssParameter>
<CssParameter name="fill">#ff8080</CssParameter>
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
<ogc:Filter>
<ogc:PropertyIsBetween>
<ogc:PropertyName>KLASSENEU</ogc:PropertyName>
<ogc:LowerBoundary>20.0</ogc:LowerBoundary>
<ogc:UpperBoundary>200.0</ogc:UpperBoundary>
</ogc:PropertyIsBetween>
</ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<PolygonSymbolizer>
<Geometry>
<ogc:PropertyName>GEOM</ogc:PropertyName>
</Geometry>
<Fill>
<CssParameter name="fill-opacity">0.4</CssParameter>
<CssParameter name="fill">#008040</CssParameter>
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
<ogc:Filter>
<ogc:PropertyIsBetween>
<ogc:PropertyName>KLASSENEU</ogc:PropertyName>
<ogc:LowerBoundary>11.0</ogc:LowerBoundary>
<ogc:UpperBoundary>11.0</ogc:UpperBoundary>
</ogc:PropertyIsBetween>
</ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<PolygonSymbolizer>
<Geometry>
<ogc:PropertyName>GEOM</ogc:PropertyName>
</Geometry>
<Fill>
<CssParameter name="fill-opacity">0.4</CssParameter>
<CssParameter name="fill">#000000</CssParameter>
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
<ogc:Filter>
<ogc:PropertyIsBetween>
<ogc:PropertyName>KLASSENEU</ogc:PropertyName>
<ogc:LowerBoundary>12.0</ogc:LowerBoundary>
<ogc:UpperBoundary>17.0</ogc:UpperBoundary>
</ogc:PropertyIsBetween>
</ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<PolygonSymbolizer>
<Geometry>
<ogc:PropertyName>GEOM</ogc:PropertyName>
</Geometry>
<Fill>
<CssParameter name="fill-opacity">0.4</CssParameter>
<CssParameter name="fill">#008040</CssParameter>
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
<ogc:Filter>
<ogc:PropertyIsBetween>
<ogc:PropertyName>KLASSENEU</ogc:PropertyName>
<ogc:LowerBoundary>19.0</ogc:LowerBoundary>
<ogc:UpperBoundary>19.0</ogc:UpperBoundary>
</ogc:PropertyIsBetween>
</ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<PolygonSymbolizer>
<Geometry>
<ogc:PropertyName>GEOM</ogc:PropertyName>
</Geometry>
<Fill>
<CssParameter name="fill-opacity">0.4</CssParameter>
<CssParameter name="fill">#ffff80</CssParameter>
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
<ogc:Filter>
<ogc:PropertyIsBetween>
<ogc:PropertyName>KLASSENEU</ogc:PropertyName>
<ogc:LowerBoundary>18.0</ogc:LowerBoundary>
<ogc:UpperBoundary>18.0</ogc:UpperBoundary>
</ogc:PropertyIsBetween>
</ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<PolygonSymbolizer>
<Geometry>
<ogc:PropertyName>GEOM</ogc:PropertyName>
</Geometry>
<Fill>
<CssParameter name="fill-opacity">0.7</CssParameter>
<CssParameter name="fill">#0000ff</CssParameter>
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
