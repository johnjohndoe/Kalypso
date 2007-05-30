<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xmlns:simBase="http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase"
	xmlns:op1d2d="http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel">
	<Name>_FlowRelationship</Name>
	<Title>Randbediungung</Title>
	<FeatureTypeName>
		{http://www.tu-harburg.de/wb/kalypso/schemata/operationalmodel}BoundaryCondition
	</FeatureTypeName>
	<Rule>
		<Name>no_direction</Name>
		<Title>ohne Richtung</Title>
		<Abstract>Randbedingungen ohne Richtungsangabe</Abstract>
		<ogc:Filter>
			<ogc:PropertyIsNull>
				<ogc:PropertyName>
					op1d2d:timeserie/op1d2d:DirectedTimeserie/op1d2d:direction
				</ogc:PropertyName>
			</ogc:PropertyIsNull>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>10000000.00</MaxScaleDenominator>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>simBase:position</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>circle</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">
							1.0
						</CssParameter>
						<CssParameter name="fill">#FFEE00</CssParameter>
					</Fill>
					<Stroke>
						<CssParameter name="stroke">
							#000000
						</CssParameter>
						<CssParameter name="stroke-width">
							1.0
						</CssParameter>
						<CssParameter name="stroke-linejoin">
							round
						</CssParameter>
						<CssParameter name="stroke-opacity">
							1.0
						</CssParameter>
						<CssParameter name="stroke-linecap">
							square
						</CssParameter>
					</Stroke>
				</Mark>
				<Opacity>1.0</Opacity>
				<Size>20.0</Size>
				<Rotation>0.0</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
	<Rule>
		<Name>with_direction</Name>
		<Title>mit Richtung</Title>
		<Abstract>
			(Abfluss-)Randbedingungen mit Richtungsangabe
		</Abstract>
		<ogc:Filter>
			<ogc:Not>
				<ogc:PropertyIsNull>
					<ogc:PropertyName>
						op1d2d:timeserie/op1d2d:DirectedTimeserie/op1d2d:direction
					</ogc:PropertyName>
				</ogc:PropertyIsNull>
			</ogc:Not>
		</ogc:Filter>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>10000000.00</MaxScaleDenominator>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>simBase:position</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>circle</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">
							1.0
						</CssParameter>
						<CssParameter name="fill">#FFEE00</CssParameter>
					</Fill>
					<Stroke>
						<CssParameter name="stroke">
							#000000
						</CssParameter>
						<CssParameter name="stroke-width">
							1.0
						</CssParameter>
						<CssParameter name="stroke-linejoin">
							round
						</CssParameter>
						<CssParameter name="stroke-opacity">
							1.0
						</CssParameter>
						<CssParameter name="stroke-linecap">
							square
						</CssParameter>
					</Stroke>
				</Mark>
				<Opacity>1.0</Opacity>
				<Size>20.0</Size>
				<Rotation>0.0</Rotation>
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
						<CssParameter name="fill-opacity">
							1.0
						</CssParameter>
						<CssParameter name="fill">#FFEE00</CssParameter>
					</Fill>
					<Stroke>
						<CssParameter name="stroke">
							#000000
						</CssParameter>
						<CssParameter name="stroke-width">
							1.0
						</CssParameter>
						<CssParameter name="stroke-linejoin">
							round
						</CssParameter>
						<CssParameter name="stroke-opacity">
							1.0
						</CssParameter>
						<CssParameter name="stroke-linecap">
							square
						</CssParameter>
					</Stroke>
				</Mark>
				<Opacity>1.0</Opacity>
				<Size>50.0</Size>
				<Rotation>
					<ogc:PropertyName>
						op1d2d:timeserie/op1d2d:DirectedTimeserie/op1d2d:direction
					</ogc:PropertyName>
				</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
</FeatureTypeStyle>
