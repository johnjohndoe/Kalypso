<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>FE1D2DEdge</Name>
	<Title>Kanten Stil</Title>
	<FeatureTypeName>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}Edge</FeatureTypeName>
	<Rule>
		<Name>innerEdge</Name>
		<Title>innere Kanten</Title>
		<Abstract>Kanten innerhalb des Netzes</Abstract>
		<ogc:Filter>
			<ogc:And>
				<ogc:PropertyIsNotEqualTo>
					<ogc:Function name="org.kalypso.deeegree.functionExpression.featureTypeName" />
					<ogc:Literal>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}EdgeInv</ogc:Literal>
				</ogc:PropertyIsNotEqualTo>
				<ogc:PropertyIsEqualTo>
					<ogc:Function name="org.kalypso.kalypsomodel1d2d.ui.map.function.BorderEdgeFunctionExpression">
						<ogc:PropertyName xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d">
							wb1d2d:edgeContainer
						</ogc:PropertyName>
					</ogc:Function>
					<ogc:Literal>false</ogc:Literal>
				</ogc:PropertyIsEqualTo>
			</ogc:And>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>wb1d2d:geometry</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#30A0FF</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
	<Rule>
		<Name>borderEdge</Name>
		<Title>äußere Kanten</Title>
		<Abstract>Kanten am Netzrand</Abstract>
		<ogc:Filter>
			<ogc:And>
				<ogc:PropertyIsNotEqualTo>
					<ogc:Function name="org.kalypso.deeegree.functionExpression.featureTypeName" />
					<ogc:Literal>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}EdgeInv</ogc:Literal>
				</ogc:PropertyIsNotEqualTo>
				<ogc:PropertyIsEqualTo>
					<ogc:Function name="org.kalypso.kalypsomodel1d2d.ui.map.function.BorderEdgeFunctionExpression">
						<ogc:PropertyName xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d">
							wb1d2d:edgeContainer
						</ogc:PropertyName>
					</ogc:Function>
					<ogc:Literal>true</ogc:Literal>
				</ogc:PropertyIsEqualTo>
			</ogc:And>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>wb1d2d:geometry</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#0000A0</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
</FeatureTypeStyle>
