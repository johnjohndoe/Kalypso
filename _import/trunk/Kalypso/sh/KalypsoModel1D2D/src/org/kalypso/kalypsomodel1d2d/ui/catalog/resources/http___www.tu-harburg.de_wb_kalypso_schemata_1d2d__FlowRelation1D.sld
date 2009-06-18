<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase"
	xmlns:wb1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/1d2d"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>_FlowRelationship</Name>
	<Title>_FlowRelationship</Title>
	<FeatureTypeName>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}_FlowRelation1D</FeatureTypeName>
<!-- 
	<Rule>
		<Name>otherFlowRelationShips</Name>
		<Title>Netzparameter</Title>
		<Abstract>Netzparameter</Abstract>
		<ElseFilter />
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
						<CssParameter name="fill">#ff952b</CssParameter>
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
				<Size>14.0</Size>
				<Rotation>0.0</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
-->
	<Rule>
		<Name>polynome</Name>
		<Title>Polynomfunktion</Title>
		<ogc:Filter>
			<ogc:PropertyIsEqualTo>
				<ogc:Literal>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}TeschkeFlowRelation</ogc:Literal>
				<ogc:Function name="org.kalypso.deeegree.functionExpression.featureTypeName" />
			</ogc:PropertyIsEqualTo>
		</ogc:Filter>
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
						<CssParameter name="fill">#ff952b</CssParameter>
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
				<Size>14.0</Size>
				<Rotation>0.0</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
	<Rule>
		<Name>weir</Name>
		<Title>Wehre</Title>
		<Abstract />
		<ogc:Filter>
			<ogc:PropertyIsEqualTo>
				<ogc:Literal>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}WeirFlowRelation</ogc:Literal>
				<ogc:Function name="org.kalypso.deeegree.functionExpression.featureTypeName" />
			</ogc:PropertyIsEqualTo>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>1E10</MaxScaleDenominator>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>simBase:position</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<ExternalGraphic>
					<OnlineResource xlink:href="images/weir.png"></OnlineResource>
					<Format>GIF</Format>
				</ExternalGraphic>
			</Graphic>
		</PointSymbolizer>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>simBase:position</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>kalypsoArrow</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">1.0</CssParameter>
						<CssParameter name="fill">#FFEE00</CssParameter>
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
				<Size>60.0</Size>
     			<!-- Multiply by -1 in order to increase anti-clockwise. This is the 1d2d convention for interpreting the direction. -->
				<Rotation>
					<ogc:Mul>
						<ogc:Literal>-1</ogc:Literal>
						<ogc:PropertyName>wb1d2d:direction</ogc:PropertyName>
					</ogc:Mul>
				</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
	<Rule>
		<Name>bridge</Name>
		<Title>Brücken</Title>
		<Abstract />
		<ogc:Filter>
			<ogc:PropertyIsEqualTo>
				<ogc:Literal>{http://www.tu-harburg.de/wb/kalypso/schemata/1d2d}BridgeFlowRelation</ogc:Literal>
				<ogc:Function name="org.kalypso.deeegree.functionExpression.featureTypeName" />
			</ogc:PropertyIsEqualTo>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>10000000.00</MaxScaleDenominator>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>simBase:position</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<ExternalGraphic>
					<OnlineResource xlink:href="images/bridge.png"></OnlineResource>
					<Format>GIF</Format>
				</ExternalGraphic>
			</Graphic>
		</PointSymbolizer>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>simBase:position</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>kalypsoArrow</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">1.0</CssParameter>
						<CssParameter name="fill">#FFEE00</CssParameter>
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
				<Size>60.0</Size>
     			<!-- Multiply by -1 in order to increase anti-clockwise. This is the 1d2d convention for interpreting the direction. -->
				<Rotation>
					<ogc:Mul>
						<ogc:Literal>-1</ogc:Literal>
						<ogc:PropertyName>wb1d2d:direction</ogc:PropertyName>
					</ogc:Mul>
				</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
</FeatureTypeStyle>
