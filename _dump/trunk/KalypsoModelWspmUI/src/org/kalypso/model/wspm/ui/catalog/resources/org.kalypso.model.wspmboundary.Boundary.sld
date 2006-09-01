<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
    <Name>default</Name>
    <Title>Grenze</Title>
    <FeatureTypeName>{org.kalypso.model.wspmboundary}Boundary</FeatureTypeName>
    <Rule>
        <Name>default</Name>
        <Title>default</Title>
        <Abstract>default</Abstract>
        <MinScaleDenominator>0.0</MinScaleDenominator>
        <MaxScaleDenominator>9.9999999901E8</MaxScaleDenominator>
        <PolygonSymbolizer>
            <Geometry>
                <ogc:PropertyName>geometry</ogc:PropertyName>
            </Geometry>
            <Fill>
                <CssParameter name="fill-opacity">0.5</CssParameter>
                <CssParameter name="fill">#0080ff</CssParameter>
            </Fill>
            <Stroke>
                <CssParameter name="stroke">#0080ff</CssParameter>
                <CssParameter name="stroke-width">1.0</CssParameter>
                <CssParameter name="stroke-dashoffset">0.0</CssParameter>
                <CssParameter name="stroke-linejoin">mitre</CssParameter>
                <CssParameter name="stroke-opacity">0.8</CssParameter>
                <CssParameter name="stroke-linecap">butt</CssParameter>
            </Stroke>
        </PolygonSymbolizer>
    </Rule>
</FeatureTypeStyle>
