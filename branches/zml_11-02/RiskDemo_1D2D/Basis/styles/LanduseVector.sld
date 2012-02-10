<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:sld="http://www.opengis.net/sld" xmlns:sldExt="http://www.opengis.net/sldExt" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.0.0">
    <NamedLayer>
        <Name>deegree style definition</Name>
        <UserStyle>
            <Name>Kalypso style</Name>
            <Title>Kalypso style</Title>
            <FeatureTypeStyle>
                <Rule>
                    <Name>undefinierterStilID</Name>
                    <Title>Undefined style</Title>
                    <Abstract>Undefined style</Abstract>
                    <ElseFilter/>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#ffffff</CssParameter>
                            <CssParameter name="fill-opacity">0.0</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#ff0000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-dasharray">2.0,3.5</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.5</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>LanduseClass123314195663216</Name>
                    <Title>light developed area</Title>
                    <Abstract>light developed area</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>light developed area</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#88fcd9</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>LanduseClass123314195663214</Name>
                    <Title>dense vegetation area</Title>
                    <Abstract>dense vegetation area</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>dense vegetation area</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#f0059c</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>LanduseClass12331419566323</Name>
                    <Title>no data</Title>
                    <Abstract>no data</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>no data</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#ded5c1</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>LanduseClass123314195663243</Name>
                    <Title>river bank, with vegetation</Title>
                    <Abstract>river bank, with vegetation</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>river bank, with vegetation</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#919eb7</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>LanduseClass123314195663224</Name>
                    <Title>grassland</Title>
                    <Abstract>grassland</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>grassland</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#5f522a</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>LanduseClass12331419566321</Name>
                    <Title>river</Title>
                    <Abstract>river</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>river</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#e2e97b</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>LanduseClass123314195663239</Name>
                    <Title>heavy developed area</Title>
                    <Abstract>heavy developed area</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>heavy developed area</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#dda22c</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>LanduseClass123314195663241</Name>
                    <Title>river bank, no vegetation</Title>
                    <Abstract>river bank, no vegetation</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>river bank, no vegetation</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#843214</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
                <Rule>
                    <Name>LanduseClass12331419566329</Name>
                    <Title>light vegetation area</Title>
                    <Abstract>light vegetation area</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>light vegetation area</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#6004b1</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke">#000000</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke-width">1.0</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
            </FeatureTypeStyle>
        </UserStyle>
    </NamedLayer>
</StyledLayerDescriptor>
