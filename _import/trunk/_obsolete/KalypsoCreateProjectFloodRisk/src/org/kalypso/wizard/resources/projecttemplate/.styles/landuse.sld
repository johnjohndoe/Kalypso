<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="String">
<NamedLayer>
<Name>deegree style definition</Name>
	<UserStyle>
	<Name>Landnutzung</Name>
	<Title>Landnutzung</Title>
	<FeatureTypeStyle>
		<Rule>
		<Name>default</Name>
		<Title>nutzung</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
		<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
		<ogc:PropertyName>LANDUSE</ogc:PropertyName>
		<ogc:Literal>Wohnen</ogc:Literal>
		</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
		<Geometry>
		<ogc:PropertyName>GEOM</ogc:PropertyName>
		</Geometry>
		<Fill>
		<CssParameter name="fill-opacity">0.65</CssParameter>
		<CssParameter name="fill">#ff8000</CssParameter>
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
		<Title>nutzung</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
		<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
		<ogc:PropertyName>LANDUSE</ogc:PropertyName>
		<ogc:Literal>Wasser</ogc:Literal>
		</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
		<Geometry>
		<ogc:PropertyName>GEOM</ogc:PropertyName>
		</Geometry>
		<Fill>
		<CssParameter name="fill-opacity">0.65</CssParameter>
		<CssParameter name="fill">#003366</CssParameter>
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
		<Title>nutzung</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
		<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
		<ogc:PropertyName>LANDUSE</ogc:PropertyName>
		<ogc:Literal>Wald</ogc:Literal>
		</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
		<Geometry>
		<ogc:PropertyName>GEOM</ogc:PropertyName>
		</Geometry>
		<Fill>
		<CssParameter name="fill-opacity">0.65</CssParameter>
		<CssParameter name="fill">#005500</CssParameter>
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
		<Title>nutzung</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
		<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
		<ogc:PropertyName>LANDUSE</ogc:PropertyName>
		<ogc:Literal>Naturraum</ogc:Literal>
		</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
		<Geometry>
		<ogc:PropertyName>GEOM</ogc:PropertyName>
		</Geometry>
		<Fill>
		<CssParameter name="fill-opacity">0.65</CssParameter>
		<CssParameter name="fill">#555555</CssParameter>
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
		<Title>nutzung</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
		<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
		<ogc:PropertyName>LANDUSE</ogc:PropertyName>
		<ogc:Literal>Ackerland</ogc:Literal>
		</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
		<Geometry>
		<ogc:PropertyName>GEOM</ogc:PropertyName>
		</Geometry>
		<Fill>
		<CssParameter name="fill-opacity">0.65</CssParameter>
		<CssParameter name="fill">#005555</CssParameter>
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
		<Title>nutzung</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
		<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
		<ogc:PropertyName>LANDUSE</ogc:PropertyName>
		<ogc:Literal>Verkehr</ogc:Literal>
		</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
		<Geometry>
		<ogc:PropertyName>GEOM</ogc:PropertyName>
		</Geometry>
		<Fill>
		<CssParameter name="fill-opacity">0.65</CssParameter>
		<CssParameter name="fill">#dddddd</CssParameter>
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
		<Title>nutzung</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
		<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
		<ogc:PropertyName>LANDUSE</ogc:PropertyName>
		<ogc:Literal>Industrie</ogc:Literal>
		</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
		<Geometry>
		<ogc:PropertyName>GEOM</ogc:PropertyName>
		</Geometry>
		<Fill>
		<CssParameter name="fill-opacity">0.65</CssParameter>
		<CssParameter name="fill">#550000</CssParameter>
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
		<Title>nutzung</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
		<ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
		<ogc:PropertyName>LANDUSE</ogc:PropertyName>
		<ogc:Literal>Gruenland</ogc:Literal>
		</ogc:PropertyIsLike>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<PolygonSymbolizer>
		<Geometry>
		<ogc:PropertyName>GEOM</ogc:PropertyName>
		</Geometry>
		<Fill>
		<CssParameter name="fill-opacity">0.65</CssParameter>
		<CssParameter name="fill">#00ff00</CssParameter>
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
