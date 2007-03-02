<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.0">
    <NamedLayer>
        <Name>deegree style definition</Name>
        <UserStyle>
            <Name>Roughness style</Name>
            <Title>Roughness style</Title>
            <FeatureTypeStyle>
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>_DEFAULT_STYLE_</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#ff5555</CssParameter>
                            <CssParameter name="fill-opacity">0.5</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke-width">3.0</CssParameter>
                            <CssParameter name="stroke-opacity">0.8</CssParameter>
                            <CssParameter name="stroke">#ff0000</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Acker</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#003366</CssParameter>
                            <CssParameter name="fill-opacity">0.5</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                            <CssParameter name="stroke-opacity">0.5</CssParameter>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Grasland</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#ff5555</CssParameter>
                            <CssParameter name="fill-opacity">0.5</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                            <CssParameter name="stroke-opacity">0.5</CssParameter>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Wald</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#09cd74</CssParameter>
                            <CssParameter name="fill-opacity">0.5</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                            <CssParameter name="stroke-opacity">0.5</CssParameter>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
            </FeatureTypeStyle>
        </UserStyle>
    </NamedLayer>
</StyledLayerDescriptor>
