<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>FE1D2DEdge</Name>
	<Title>FE1D2DEdge</Title>
	<FeatureTypeName>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}Edge</FeatureTypeName>
	<Rule>
		<Name>Edge</Name>
		<Title>FE-Kante</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
			<ogc:PropertyIsEqualTo>
				<ogc:Function name="org.kalypsodeegree_impl.filterencoding.ListSizeExpression">
					<ogc:PropertyName xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d">wb1d2d:edgeContainer</ogc:PropertyName>
				</ogc:Function>
				<ogc:Literal>2</ogc:Literal>
			</ogc:PropertyIsEqualTo>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<LineSymbolizer>
			<!--Geometry>
				<ogc:PropertyName>edgeGeometry</ogc:PropertyName>
				</Geometry-->
			<Stroke>
				<CssParameter name="stroke">#0C3EC0</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
	<Rule>
		<Name>Edge</Name>
		<Title>keine Elemente</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
			<ogc:PropertyIsEqualTo>
				<ogc:Function name="org.kalypsodeegree_impl.filterencoding.ListSizeExpression">
					<ogc:PropertyName xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d">wb1d2d:edgeContainer</ogc:PropertyName>
				</ogc:Function>
				<ogc:Literal>0</ogc:Literal>
			</ogc:PropertyIsEqualTo>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<LineSymbolizer>
			<!--Geometry>
				<ogc:PropertyName>edgeGeometry</ogc:PropertyName>
				</Geometry-->
			<Stroke>
				<CssParameter name="stroke">#FFFF00</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
	<Rule>
		<Name>Edge</Name>
		<Title>ein Element</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
			<ogc:PropertyIsEqualTo>
				<ogc:Function name="org.kalypsodeegree_impl.filterencoding.ListSizeExpression">
					<ogc:PropertyName xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d">wb1d2d:edgeContainer</ogc:PropertyName>
				</ogc:Function>
				<ogc:Literal>1</ogc:Literal>
			</ogc:PropertyIsEqualTo>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<LineSymbolizer>
			<!--Geometry>
				<ogc:PropertyName>edgeGeometry</ogc:PropertyName>
				</Geometry-->
			<Stroke>
				<CssParameter name="stroke">#FF8800</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
	<Rule>
		<Name>Edge</Name>
		<Title>3 Elemente</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
			<ogc:PropertyIsEqualTo>
				<ogc:Function name="org.kalypsodeegree_impl.filterencoding.ListSizeExpression">
					<ogc:PropertyName xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d">wb1d2d:edgeContainer</ogc:PropertyName>
				</ogc:Function>
				<ogc:Literal>3</ogc:Literal>
			</ogc:PropertyIsEqualTo>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<LineSymbolizer>
			<!--Geometry>
				<ogc:PropertyName>edgeGeometry</ogc:PropertyName>
				</Geometry-->
			<Stroke>
				<CssParameter name="stroke">#FF0000</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
	<Rule>
		<Name>Edge</Name>
		<Title>Elemente 3++</Title>
		<Abstract>default</Abstract>
		<ogc:Filter>
			<ogc:PropertyIsGreaterThan>
				<ogc:Function name="org.kalypsodeegree_impl.filterencoding.ListSizeExpression">
					<ogc:PropertyName xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d">wb1d2d:edgeContainer</ogc:PropertyName>
				</ogc:Function>
				<ogc:Literal>3</ogc:Literal>
			</ogc:PropertyIsGreaterThan>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
		<LineSymbolizer>
			<!--Geometry>
				<ogc:PropertyName>edgeGeometry</ogc:PropertyName>
				</Geometry-->
			<Stroke>
				<CssParameter name="stroke">#000000</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
</FeatureTypeStyle>
