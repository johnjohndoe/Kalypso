<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.0">
    <NamedLayer>
        <Name>deegree style definition</Name>
        <UserStyle>
            <Name>Kalypso style</Name>
            <Title>Kalypso style</Title>
            <FeatureTypeStyle>
                <Rule>
                    <Name>LanduseClass11930532749211</Name>
                    <Title>Wohnen</Title>
                    <Abstract>Wohnen</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>Wohnen</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#ff8040</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
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
                    <Name>LanduseClass11930532749532</Name>
                    <Title>Gruenland</Title>
                    <Abstract>Gruenland</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>Gruenland</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#008000</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
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
                    <Name>LanduseClass11930532749533</Name>
                    <Title>Industrie</Title>
                    <Abstract>Industrie</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>Industrie</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#400000</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
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
                    <Name>LanduseClass11930532749531</Name>
                    <Title>Verkehr</Title>
                    <Abstract>Verkehr</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>Verkehr</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#c0c0c0</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
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
                    <Name>LanduseClass11930532749534</Name>
                    <Title>Ackerland</Title>
                    <Abstract>Ackerland</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>Ackerland</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#00ff00</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
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
                    <Name>LanduseClass11930532749535</Name>
                    <Title>Naturraum</Title>
                    <Abstract>Naturraum</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>Naturraum</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#808040</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
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
                    <Name>LanduseClass11930532749530</Name>
                    <Title>Wasser</Title>
                    <Abstract>Wasser</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>Wasser</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#0080ff</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
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
                    <Name>LanduseClass11930532749539</Name>
                    <Title>Wald</Title>
                    <Abstract>Wald</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                            <ogc:Literal>Wald</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
<MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
                    <PolygonSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#408080</CssParameter>
                            <CssParameter name="fill-opacity">0.75</CssParameter>
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
                    <Name>Labelle</Name>
                    <Title>Labelle</Title>
                    <Abstract>Labelle</Abstract>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>10.0</MaxScaleDenominator>
                    <TextSymbolizer uom="pixel">
                        <Geometry>
                            <ogc:PropertyName>krvdm:polygonGeometry</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>sldStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-size">11.0</CssParameter>
                            <CssParameter name="font-weight">normal</CssParameter>
                            <CssParameter name="font-color">#000000</CssParameter>
                            <CssParameter name="font-style">normal</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true">
                                <AnchorPoint>
                                    <AnchorPointX>0.5</AnchorPointX>
                                    <AnchorPointY>0.5</AnchorPointY>
                                </AnchorPoint>
                                <Rotation>0.0</Rotation>
                            </PointPlacement>
                        </LabelPlacement>
                        <Fill>
                            <CssParameter name="fill">#808080</CssParameter>
                            <CssParameter name="fill-opacity">1.0</CssParameter>
                        </Fill>
                    </TextSymbolizer>
                </Rule>
                <Rule>
                    <Name>undefinierterStilID</Name>
                    <Title>undefinierter Stil</Title>
                    <Abstract>undefinierter Stil</Abstract>
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
                            <CssParameter name="stroke-width">2.0</CssParameter>
                            <CssParameter name="stroke-dasharray">2.0,5.0</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke">#ff0000</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                </Rule>
            </FeatureTypeStyle>
        </UserStyle>
    </NamedLayer>
</StyledLayerDescriptor>
