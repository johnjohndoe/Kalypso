<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
    <Name>default</Name>
    <Title>Grenzpunkte</Title>
    <FeatureTypeName>{org.kalypso.model.wspmboundary}WspPoint</FeatureTypeName>
    <Rule>
        <Name>default</Name>
        <Title>default</Title>
        <Abstract>default</Abstract>
        <MinScaleDenominator>0.0</MinScaleDenominator>
        <MaxScaleDenominator>10000.01</MaxScaleDenominator>
        <PointSymbolizer>
            <Geometry>
                <ogc:PropertyName>geometry</ogc:PropertyName>
            </Geometry>
            <Graphic>
                <Mark>
                    <WellKnownName>circle</WellKnownName>
                    <Fill>
                        <CssParameter name="fill-opacity">0.5</CssParameter>
                        <CssParameter name="fill">#0080ff</CssParameter>
                    </Fill>
                    <Stroke>
                        <CssParameter name="stroke">#0080ff</CssParameter>
                        <CssParameter name="stroke-width">1.0</CssParameter>
                        <CssParameter name="stroke-linejoin">round</CssParameter>
                        <CssParameter name="stroke-opacity">0.8</CssParameter>
                        <CssParameter name="stroke-linecap">square</CssParameter>
                    </Stroke>
                </Mark>
                <Opacity>1.0</Opacity>
                <Size>7.0</Size>
                <Rotation>0.0</Rotation>
            </Graphic>
        </PointSymbolizer>
    </Rule>
</FeatureTypeStyle>
