<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>_FlowRelationship</Name>
	<Title>_FlowRelationship</Title>
    <FeatureTypeName>{http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel}BoundaryCondition</FeatureTypeName>
	<Rule>
		<Name>_FlowRelationship</Name>
		<Title>_FlowRelationship</Title>
		<Abstract>_FlowRelationship</Abstract>
		<MinScaleDenominator>0.0</MinScaleDenominator>
        <MaxScaleDenominator>10000000.00</MaxScaleDenominator>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>position</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>circle</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">1.0</CssParameter>
						<CssParameter name="fill">#ffff00</CssParameter>
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
				<Size>20.0</Size>
				<Rotation>0.0</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
</FeatureTypeStyle>
