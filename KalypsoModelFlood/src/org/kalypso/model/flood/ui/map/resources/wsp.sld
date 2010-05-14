<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="1.0.0" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml"
	xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:flood="org.kalypso.model.flood">
	<NamedLayer>
		<Name>wspLayer</Name>
		<UserStyle>
			<Name>wspUserStyle</Name>
			<Title>%userStyle.wspUserStyle.title</Title>
			<FeatureTypeStyle>
				<Name>wspFts</Name>
				<Rule>
					<Name>wspRule</Name>
					<Title>Wasserspiegel</Title>
					<sldExt:SurfacePolygonSymbolizer xmlns:sldExt="http://www.opengis.net/sldExt">
						<Geometry>
							<ogc:PropertyName>flood:tin</ogc:PropertyName>
						</Geometry>
						<sldExt:PolygonColorMap></sldExt:PolygonColorMap>
					</sldExt:SurfacePolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
	<NamedLayer>
		<Name>dtmGrid</Name>
		<UserStyle>
			<Name>extrapolationPolygonUserStyle</Name>
			<Title>%userStyle.extrapolationPolygonUserStyle.title</Title>
			<FeatureTypeStyle>
				<Name>extrapolationPolygonStyle</Name>
				<Title>Extrapolations-Polygone</Title>
				<FeatureTypeName>{org.kalypso.model.flood}FloodExtrapolationPolygon</FeatureTypeName>
				<Rule>
					<Name>areaRule</Name>
					<Title>Extrapolationsfläche</Title>
					<ogc:Filter>
						<ogc:PropertyIsEqualTo>
							<ogc:Function name="org.kalypso.model.flood.ui.map.EventFilterExpression">
								<ogc:Literal>%eventFeatureId%</ogc:Literal>
							</ogc:Function>
							<ogc:Literal>true</ogc:Literal>
						</ogc:PropertyIsEqualTo>
					</ogc:Filter>
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
			<Title>%userStyle.clipPolygonUserStyle.title</Title>
			<FeatureTypeStyle>
				<Name>clipPolygonStyle</Name>
				<FeatureTypeName>{org.kalypso.model.flood}FloodClipPolygon</FeatureTypeName>
				<Rule>
					<Name>areaRule</Name>
					<Title>Clip</Title>
					<ogc:Filter xmlns:ogc='http://www.opengis.net/ogc'>
						<ogc:PropertyIsEqualTo>
							<ogc:Function name="org.kalypso.model.flood.ui.map.EventFilterExpression">
								<ogc:Literal>%eventFeatureId%</ogc:Literal>
							</ogc:Function>
							<ogc:Literal>true</ogc:Literal>
						</ogc:PropertyIsEqualTo>
					</ogc:Filter>
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
		<UserStyle>
			<Name>volumePolygonUserStyle</Name>
			<Title>%userStyle.volumePolygonUserStyle.title</Title>
			<FeatureTypeStyle>
				<Name>volumePolygonStyle</Name>
				<FeatureTypeName>{org.kalypso.model.flood}FloodVolumePolygon</FeatureTypeName>
				<Rule>
					<Name>areaRule</Name>
					<Title>Clip</Title>
					<ogc:Filter xmlns:ogc='http://www.opengis.net/ogc'>
						<ogc:PropertyIsEqualTo>
							<ogc:Function name="org.kalypso.model.flood.ui.map.EventFilterExpression">
								<ogc:Literal>%eventFeatureId%</ogc:Literal>
							</ogc:Function>
							<ogc:Literal>true</ogc:Literal>
						</ogc:PropertyIsEqualTo>
					</ogc:Filter>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>flood:areaMember</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill-opacity">0.6</CssParameter>
							<CssParameter name="fill">#8888dd</CssParameter>
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
