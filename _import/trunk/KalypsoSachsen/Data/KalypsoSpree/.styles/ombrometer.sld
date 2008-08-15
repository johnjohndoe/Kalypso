<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="1.0.0" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<NamedLayer>
		<Name>deegree style definition</Name>
		<UserStyle>
			<Name>thiessen</Name>
			<Title></Title>
			<FeatureTypeStyle>
				<Name>ThiessenFts</Name>
				<Title>-</Title>
				<Rule>
					<Name>thiessenPolygon</Name>
					<Title>Thiessen Polygon</Title>					
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>affectedArea</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill-opacity">0.2</CssParameter>
							<CssParameter name="fill">#019c05</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
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
			<Name>thiessenSchwach</Name>
			<Title></Title>
			<FeatureTypeStyle>
				<Name>ThiessenSchwachFts</Name>
				<Title>-</Title>
				<Rule>
					<Name>thiessenSchwachPolygon</Name>
					<Title>Thiessen Polygon</Title>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>affectedArea</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill-opacity">0.05</CssParameter>
							<CssParameter name="fill">#019c05</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
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
			<Name>ombrometerIsUsed</Name>
			<Title></Title>
			<FeatureTypeStyle>
				<Name>OmbrometerIsUsedFts</Name>
				<Rule>
					<Name>ombrometerOn</Name>
					<Title>an</Title>
					<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
						<ogc:PropertyIsEqualTo>
							<ogc:PropertyName>isUsed</ogc:PropertyName>
							<ogc:Literal>true</ogc:Literal>
						</ogc:PropertyIsEqualTo>
					</ogc:Filter>
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>stationLocation</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>name</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family" />
							<CssParameter name="font-color">#000000</CssParameter>
							<CssParameter name="font-size">11.0</CssParameter>
							<CssParameter name="font-style">normal</CssParameter>
							<CssParameter name="font-weight">normal</CssParameter>
						</Font>
						<LabelPlacement>
							<PointPlacement>
								<AnchorPoint>
									<AnchorPointX>0.0</AnchorPointX>
									<AnchorPointY>0.0</AnchorPointY>
								</AnchorPoint>
								<Displacement>
									<DisplacementX>-10.0</DisplacementX>
									<DisplacementY>-20.0</DisplacementY>
								</Displacement>
								<Rotation>0.0</Rotation>
							</PointPlacement>
						</LabelPlacement>
						<Halo>
							<Fill>
								<CssParameter name="fill-opacity">1.0</CssParameter>
								<CssParameter name="fill">#ffffcc</CssParameter>
							</Fill>
							<Stroke>
								<CssParameter name="stroke">#000000</CssParameter>
								<CssParameter name="stroke-width">1.0</CssParameter>
								<CssParameter name="stroke-linejoin">round</CssParameter>
								<CssParameter name="stroke-opacity">1.0</CssParameter>
								<CssParameter name="stroke-linecap">square</CssParameter>
							</Stroke>
						</Halo>
					</TextSymbolizer>
					<PointSymbolizer>
						<Geometry>
							<ogc:PropertyName>stationLocation</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>circle</WellKnownName>
								<Fill>
									<CssParameter name="fill-opacity">1.0</CssParameter>
									<CssParameter name="fill">#ff0080</CssParameter>
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
							<Size>10.0</Size>
							<Rotation>0.0</Rotation>
						</Graphic>
					</PointSymbolizer>
				</Rule>
				<Rule>
					<Name>ombrometerOff</Name>
					<Title>aus</Title>
					<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
						<ogc:PropertyIsEqualTo>
							<ogc:PropertyName>isUsed</ogc:PropertyName>
							<ogc:Literal>false</ogc:Literal>
						</ogc:PropertyIsEqualTo>
					</ogc:Filter>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>9.0E99</MaxScaleDenominator>
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>stationLocation</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>name</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family" />
							<CssParameter name="font-color">#000000</CssParameter>
							<CssParameter name="font-size">11.0</CssParameter>
							<CssParameter name="font-style">normal</CssParameter>
							<CssParameter name="font-weight">normal</CssParameter>
						</Font>
						<LabelPlacement>
							<PointPlacement>
								<AnchorPoint>
									<AnchorPointX>0.0</AnchorPointX>
									<AnchorPointY>0.0</AnchorPointY>
								</AnchorPoint>
								<Displacement>
									<DisplacementX>-10.0</DisplacementX>
									<DisplacementY>-20.0</DisplacementY>
								</Displacement>
								<Rotation>0.0</Rotation>
							</PointPlacement>
						</LabelPlacement>
						<Halo>
							<Fill>
								<CssParameter name="fill-opacity">1.0</CssParameter>
								<CssParameter name="fill">#ffffcc</CssParameter>
							</Fill>
							<Stroke>
								<CssParameter name="stroke">#000000</CssParameter>
								<CssParameter name="stroke-width">1.0</CssParameter>
								<CssParameter name="stroke-linejoin">round</CssParameter>
								<CssParameter name="stroke-opacity">1.0</CssParameter>
								<CssParameter name="stroke-linecap">square</CssParameter>
							</Stroke>
						</Halo>
					</TextSymbolizer>
					<PointSymbolizer>
						<Geometry>
							<ogc:PropertyName>stationLocation</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>circle</WellKnownName>
								<Fill>
									<CssParameter name="fill-opacity">1.0</CssParameter>
									<CssParameter name="fill">#f0c0c0</CssParameter>
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
							<Size>10.0</Size>
							<Rotation>0.0</Rotation>
						</Graphic>
					</PointSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>