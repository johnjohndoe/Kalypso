<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>FE1D2DNode</Name>
	<Title>FE1D2DNode</Title>
    <FeatureTypeName>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}Node</FeatureTypeName>
	<Rule>
		<Name>Node</Name>
		<Title>FE-Knoten</Title>
		<Abstract>default</Abstract>
		
		<ogc:Filter>
			<ogc:PropertyIsEqualTo>
				<ogc:PropertyName>wb1d2d:hasElevation</ogc:PropertyName>
				<ogc:Literal>true</ogc:Literal>
			</ogc:PropertyIsEqualTo>
            <!--ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                <ogc:PropertyName>hasElevation</ogc:PropertyName>
                <ogc:Literal>true</ogc:Literal>
            </ogc:PropertyIsLike-->
        </ogc:Filter>
		
		<MinScaleDenominator>0.0</MinScaleDenominator>
        <MaxScaleDenominator>10000000.00</MaxScaleDenominator>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>pointProperty</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>square</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">1.0</CssParameter>
						<CssParameter name="fill">#00ff00</CssParameter>
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
				<Size>6.0</Size>
				<Rotation>0.0</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
	
	<Rule>
		<Name>Node_NO_ELEVATION</Name>
		<Title>FE-Knoten</Title>
		<Abstract>default</Abstract>
		
		<ogc:Filter>
			<ogc:PropertyIsEqualTo>
				<ogc:PropertyName>wb1d2d:hasElevation</ogc:PropertyName>
				<ogc:Literal>false</ogc:Literal>
			</ogc:PropertyIsEqualTo>
            <!--ogc:PropertyIsLike escape="/" singleChar="$" wildCard="*">
                <ogc:PropertyName>hasElevation</ogc:PropertyName>
                <ogc:Literal>true</ogc:Literal>
            </ogc:PropertyIsLike-->
        </ogc:Filter>
		
		<MinScaleDenominator>0.0</MinScaleDenominator>
        <MaxScaleDenominator>10000000.00</MaxScaleDenominator>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>pointProperty</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>square</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">1.0</CssParameter>
						<CssParameter name="fill">#ff0000</CssParameter>
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
				<Size>6.0</Size>
				<Rotation>0.0</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
	
	
</FeatureTypeStyle>
