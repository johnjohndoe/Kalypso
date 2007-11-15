<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:flood="org.kalypso.model.flood"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<NamedLayer>
		<Name>dtmGrid</Name>
		<UserStyle>
			<Name>extrapolationPolygonUserStyle</Name>
			<Title>Extrapolations-Polygone</Title>
			<FeatureTypeStyle>
				<Name>extrapolationPolygonStyle</Name>
				<Title>Extrapolations-Polygone</Title>
				<FeatureTypeName>{org.kalypso.model.flood}FloodExtrapolationPolygon</FeatureTypeName>
				<Rule>
					<Name>lineRule</Name>
					<Title>Verbindungslinie</Title>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>flood:connectorLine</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#11aa11</CssParameter>
							<CssParameter name="stroke-width">1.0</CssParameter>
							<CssParameter name="stroke-linejoin">round</CssParameter>
							<CssParameter name="stroke-opacity">1.0</CssParameter>
							<CssParameter name="stroke-linecap">square</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
				<Rule>
					<Name>areaRule</Name>
					<Title>ExtrapolationsflÃ¤che</Title>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>flood:areaMember</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill-opacity">0.6</CssParameter>
							<CssParameter name="fill">#88dd88</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">#11aa11</CssParameter>
							<CssParameter name="stroke-width">1.0</CssParameter>
							<CssParameter name="stroke-linejoin">mitre</CssParameter>
							<CssParameter name="stroke-opacity">1.0</CssParameter>
							<CssParameter name="stroke-linecap">butt</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
				<Rule>
					<Name>pointRule</Name>
					<Title>Bezugspunkt</Title>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<PointSymbolizer>
						<Geometry>
							<ogc:PropertyName>flood:referencePoint</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>triangle</WellKnownName>
								<Fill>
									<CssParameter name="fill-opacity">1.0</CssParameter>
									<CssParameter name="fill">#3333ff</CssParameter>
								</Fill>
								<Stroke>
									<CssParameter name="stroke">#11aa11</CssParameter>
									<CssParameter name="stroke-width">1.0</CssParameter>
									<CssParameter name="stroke-linejoin">round</CssParameter>
									<CssParameter name="stroke-opacity">1.0</CssParameter>
									<CssParameter name="stroke-linecap">square</CssParameter>
								</Stroke>
							</Mark>
							<Opacity>1.0</Opacity>
							<Size>10.0</Size>
							<Rotation>0.0</Rotation>
						</Graphic>
					</PointSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>clipPolygonUserStyle</Name>
			<Title>Clip-Polygone</Title>
			<FeatureTypeStyle >
				<Name>clipPolygonStyle</Name>
				<FeatureTypeName>{org.kalypso.model.flood}FloodClipPolygon</FeatureTypeName>
				<Rule>
					<Name>areaRule</Name>
					<Title>Clip</Title>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>flood:areaMember</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill-opacity">0.6</CssParameter>
							<CssParameter name="fill">#dd8888</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">#aa1111</CssParameter>
							<CssParameter name="stroke-width">1.0</CssParameter>
							<CssParameter name="stroke-linejoin">mitre</CssParameter>
							<CssParameter name="stroke-opacity">1.0</CssParameter>
							<CssParameter name="stroke-linecap">butt</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>
