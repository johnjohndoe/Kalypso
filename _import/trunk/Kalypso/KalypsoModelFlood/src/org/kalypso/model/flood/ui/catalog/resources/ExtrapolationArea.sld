<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<Name>ExtrapolationArea</Name>
<Title>ExtrapolationArea</Title>
<FeatureTypeName>{org.kalypso.model.flood}ExtrapolationArea</FeatureTypeName>
<Rule>
<Name>default</Name>
<Title>default</Title>
<Abstract>default</Abstract>
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<PolygonSymbolizer>
<Geometry>
<ogc:PropertyName>area</ogc:PropertyName>
</Geometry>
<Fill>
<CssParameter name="fill-opacity">0.8</CssParameter>
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
<MinScaleDenominator>0.0</MinScaleDenominator>
<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
<PointSymbolizer>
<Geometry>
<ogc:PropertyName>referencePoint</ogc:PropertyName>
</Geometry>
<Graphic>
<Mark>
<WellKnownName>triangle</WellKnownName>
<Fill>
<CssParameter name="fill-opacity">1.0</CssParameter>
<CssParameter name="fill">#0000ff</CssParameter>
</Fill>
<Stroke>
<CssParameter name="stroke">#000000</CssParameter>
<CssParameter name="stroke-width">1.0</CssParameter>
<CssParameter name="stroke-linejoin">round</CssParameter>
<CssParameter name="stroke-opacity">1.0</CssParameter>
<CssParameter name="stroke-linecap">square</CssParameter>
</Stroke>
</Mark>
<Opacity>1.0</Opacity>
<Size>10.0</Size>
<Rotation>0.0</Rotation>
</Graphic>
</PointSymbolizer>
</Rule>
</FeatureTypeStyle>