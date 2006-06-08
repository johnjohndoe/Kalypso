<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="String">
<NamedLayer>
<Name>deegree style definition</Name>
<UserStyle>
<Name>Rhb</Name>
<Title>Rhb</Title>
<Abstract>empty Abstract</Abstract>
<FeatureTypeStyle>
<Name>- generierter Standard-Stil -</Name>
<Rule>
<Name>default</Name>
<Title>Rhb</Title>
<Abstract>default</Abstract>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<PolygonSymbolizer>
<Geometry>
<ogc:PropertyName>position</ogc:PropertyName>
</Geometry>
<Fill>
<CssParameter name="fill-opacity">0.46</CssParameter>
<CssParameter name="fill">#0000ff</CssParameter>
</Fill>
<Stroke>
<CssParameter name="stroke-width">2.0</CssParameter>
<CssParameter name="stroke-dashoffset">3.0</CssParameter>
<CssParameter name="stroke-dasharray">15.0,5.0</CssParameter>
<CssParameter name="stroke-opacity">1.0</CssParameter>
<CssParameter name="stroke">#000080</CssParameter>
<CssParameter name="stroke-linejoin">mitre</CssParameter>
<CssParameter name="stroke-linecap">butt</CssParameter>
</Stroke>
</PolygonSymbolizer>
</Rule>
<Rule>
<Name>default</Name>
<Title>TiefeOK</Title>
<Abstract>default</Abstract>
<ogc:Filter>
<ogc:PropertyIsGreaterThan>
<ogc:PropertyName>depth</ogc:PropertyName>
<ogc:Literal>0</ogc:Literal>
</ogc:PropertyIsGreaterThan>
</ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<TextSymbolizer>
<Geometry>
<ogc:PropertyName>position</ogc:PropertyName>
</Geometry>
<Label>
<ogc:PropertyName>depth</ogc:PropertyName>
</Label>
<Font>
<CssParameter name="font-family"/>
<CssParameter name="font-color">#000000</CssParameter>
<CssParameter name="font-size">12.0</CssParameter>
<CssParameter name="font-style">normal</CssParameter>
<CssParameter name="font-weight">bold</CssParameter>
</Font>
<Halo>
<Fill>
<CssParameter name="fill-opacity">0.3</CssParameter>
<CssParameter name="fill">#0080c0</CssParameter>
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
<Name>default</Name>
<Title>TiefeNull</Title>
<Abstract>default</Abstract>
<ogc:Filter>
<ogc:PropertyIsNull>
<ogc:PropertyName>depth</ogc:PropertyName>
</ogc:PropertyIsNull>
</ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<TextSymbolizer>
<Geometry>
<ogc:PropertyName>position</ogc:PropertyName>
</Geometry>
<Label>Tiefe des RHBs noch nicht definiert</Label>
<Font>
<CssParameter name="font-family">Dialog</CssParameter>
<CssParameter name="font-color">#000000</CssParameter>
<CssParameter name="font-size">12.0</CssParameter>
<CssParameter name="font-style">normal</CssParameter>
<CssParameter name="font-weight">normal</CssParameter>
</Font>
<Halo>
<Fill>
<CssParameter name="fill-opacity">1.0</CssParameter>
<CssParameter name="fill">#ff0080</CssParameter>
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
