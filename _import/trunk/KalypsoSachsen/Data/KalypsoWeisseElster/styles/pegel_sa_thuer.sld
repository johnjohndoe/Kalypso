<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<NamedLayer><Name>deegree style definition</Name>
<UserStyle><Name>DefaultStyle</Name>
<Title>cite:MapNeatline</Title><IsDefault>1</IsDefault>

<FeatureTypeStyle><Name>cite:MapNeatline</Name>
<Rule>
<Name>Vorhersage</Name>
<ogc:Filter>
<ogc:PropertyIsEqualTo>
<ogc:PropertyName>ANZEIGE</ogc:PropertyName>
<ogc:Literal>1</ogc:Literal>
</ogc:PropertyIsEqualTo>
</ogc:Filter>
<PointSymbolizer><Graphic><Mark><WellKnownName>triangle</WellKnownName><Fill>
<CssParameter name="fill-opacity">1.0</CssParameter><CssParameter name="fill">#0dff00</CssParameter></Fill>
<Stroke><CssParameter name="stroke">#000000</CssParameter><CssParameter name="stroke-width">1.0</CssParameter>
<CssParameter name="stroke-linejoin">round</CssParameter><CssParameter name="stroke-opacity">1.0</CssParameter>
<CssParameter name="stroke-linecap">square</CssParameter></Stroke></Mark><Opacity>1.0</Opacity><Size>12.0</Size>
<Rotation>0.0</Rotation></Graphic></PointSymbolizer>
</Rule>
<Rule>
<Name>Zulauf</Name>
<ogc:Filter>
<ogc:PropertyIsEqualTo>
<ogc:PropertyName>ANZEIGE</ogc:PropertyName>
<ogc:Literal>2</ogc:Literal>
</ogc:PropertyIsEqualTo>
</ogc:Filter>
<PointSymbolizer><Graphic><Mark><WellKnownName>triangle</WellKnownName><Fill>
<CssParameter name="fill-opacity">1.0</CssParameter><CssParameter name="fill">#0dff00</CssParameter></Fill>
<Stroke><CssParameter name="stroke">#000000</CssParameter><CssParameter name="stroke-width">1.0</CssParameter>
<CssParameter name="stroke-linejoin">round</CssParameter><CssParameter name="stroke-opacity">1.0</CssParameter>
<CssParameter name="stroke-linecap">square</CssParameter></Stroke></Mark><Opacity>1.0</Opacity><Size>12.0</Size>
<Rotation>0.0</Rotation></Graphic></PointSymbolizer>
</Rule>
</FeatureTypeStyle>
</UserStyle>
</NamedLayer>
</StyledLayerDescriptor>