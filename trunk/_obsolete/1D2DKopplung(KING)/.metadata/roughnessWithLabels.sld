<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.0">
    <NamedLayer>
        <Name>deegree style definition</Name>
        <UserStyle>
            <Name>Roughness style</Name>
            <Title>Roughness style</Title>
            <FeatureTypeStyle>
            	<!-- =============================================================
            		Default style
            	============================================================== -->
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
                            <CssParameter name="fill">#aaaaaa</CssParameter>
                            <CssParameter name="fill-opacity">0.5</CssParameter>
                        </Fill>
                        <Stroke>
                            <CssParameter name="stroke-width">1.5</CssParameter>
                            <CssParameter name="stroke-opacity">1.0</CssParameter>
                            <CssParameter name="stroke">#777777</CssParameter>
                            <CssParameter name="stroke-linejoin">mitre</CssParameter>
                            <CssParameter name="stroke-linecap">butt</CssParameter>
                        </Stroke>
                    </PolygonSymbolizer>
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>Nicht definiert</Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: R�hricht
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>R�hricht</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#99cc00</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: Str�ucher, einj�hrig
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Str�ucher, einj�hrig</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#66ff00</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: Str�ucher, mehrj�hrig
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Str�ucher, mehrj�hrig</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#669933</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: Schilf, dicht
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Schilf, dicht</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#cc9966</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: B�sche (gleichm��iger), klein
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>B�sche (gleichm��iger), klein</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#666633</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: B�sche (gleichm��iger), mittlere Gr��e
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>B�sche (gleichm��iger), mittlere Gr��e</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#006633</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: Besatz, gro�
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Besatz, gro�</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#006666</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: Besatz, aufgelockert
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Besatz, aufgelockert</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#999900</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: Besatz, in kleineren Gruppen
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Besatz, in kleineren Gruppen</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#cc6600</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: Besatz, in gro�en Gruppen
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Besatz, in gro�en Gruppen</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#ffcc00</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: Besatz, mit Kronenschlu�
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>Besatz, mit Kronenschlu�</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#999966</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: B�ume, lockerer Besatz
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>B�ume, lockerer Besatz</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#006600</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: B�ume, dichter junger Besatz
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>B�ume, dichter junger Besatz</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#ccff33</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: B�ume, mehrj�hriger Besatz
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>B�ume, mehrj�hriger Besatz</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#006666</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		Style: B�ume, dichter mehrj�hriger Besatz
            	============================================================== -->
                <Rule>
                    <Name>default</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <ogc:Filter>
                        <ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                            <ogc:Literal>B�ume, dichter mehrj�hriger Besatz</ogc:Literal>
                        </ogc:PropertyIsLike>
                    </ogc:Filter>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <PolygonSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Fill>
                            <CssParameter name="fill">#333300</CssParameter>
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
                    <TextSymbolizer>
                        <Geometry>
                            <ogc:PropertyName>polygonProperty</ogc:PropertyName>
                        </Geometry>
                        <Label>
                            <ogc:PropertyName>roughnessStyle</ogc:PropertyName>
                        </Label>
                        <Font>
                            <CssParameter name="font-family">Arial</CssParameter>
                            <CssParameter name="font-family">Sans-Serif</CssParameter>
                            <CssParameter name="font-style">italic</CssParameter>
                            <CssParameter name="font-size">10</CssParameter>
                            <CssParameter name="font-color">#222222</CssParameter>
                        </Font>
                        <LabelPlacement>
                            <PointPlacement auto="true"/>
                        </LabelPlacement>
                    </TextSymbolizer>
                </Rule>
            	<!-- =============================================================
            		End of styles definition
            	============================================================== -->
            </FeatureTypeStyle>
        </UserStyle>
    </NamedLayer>
</StyledLayerDescriptor>
