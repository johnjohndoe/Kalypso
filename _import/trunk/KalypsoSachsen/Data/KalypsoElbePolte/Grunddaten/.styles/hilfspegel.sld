<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <NamedLayer>
        <Name>deegree style definition</Name>
        <UserStyle>
            <Name>hilfspegel</Name>
            <Title>interne Hilfspegel</Title>
            <IsDefault>1</IsDefault>
            <FeatureTypeStyle>
                <Name>interne Hilfspegel</Name>
                <Rule>
                    <Name>interne Hilfspegel</Name>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>9.0E99</MaxScaleDenominator>
                                    <PointSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>lage</ogc:PropertyName>
                        </Geometry>
                        <Graphic>
                            <Mark>
                                <WellKnownName>triangle</WellKnownName>
                                <Fill>
                                    <CssParameter name="fill">#ab0000</CssParameter>
                                </Fill>
                                <Stroke>
                                    <CssParameter name="stroke">#111111</CssParameter>
                                    <CssParameter name="stroke-width">1.0</CssParameter>
                                </Stroke>
                            </Mark>
                            <Size>15.0</Size>
                        </Graphic>
                    </PointSymbolizer>
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>lage</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>name</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family"/>
                            <CssParameter name="font-color">#000000</CssParameter>
                            <CssParameter name="font-size">11.0</CssParameter>
                            <CssParameter name="font-style">normal</CssParameter>
                            <CssParameter name="font-weight">normal</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true">
                                <Displacement>
                                    <DisplacementX>10.0</DisplacementX>
                                    <DisplacementY>15.0</DisplacementY>
                                </Displacement>
                            </PointPlacement>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            </FeatureTypeStyle>
        </UserStyle>
    </NamedLayer>
</StyledLayerDescriptor>
