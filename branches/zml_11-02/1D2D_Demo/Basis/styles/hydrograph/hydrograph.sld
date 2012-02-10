<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor xmlns="http://www.opengis.net/sld"
	xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc"
	xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	version="String">
	<NamedLayer>
		<Name>hydrograph</Name>
		<UserStyle>
			<Name>hydrographUserStyle</Name>
			<Title>Ganglinien</Title>
			<Abstract>schoene Ganglinien</Abstract>
			<FeatureTypeStyle>
				<Name>hydrographFeatures</Name>
				<Title>hydrographFeaturesTypeStyle</Title>
				<Rule>
					<Name>pointrule</Name>
					<Title>Ganglinien</Title>
					<Abstract></Abstract>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<PointSymbolizer>
						<Graphic>
							<ExternalGraphic>
								<OnlineResource xlink:href="hydrograph.gif"></OnlineResource>
								<Format></Format>
							</ExternalGraphic>
							<Rotation>0.0</Rotation>
						</Graphic>
					</PointSymbolizer>
					<!--
						<TextSymbolizer> <Geometry>
						<ogc:PropertyName>gml:location</ogc:PropertyName> </Geometry>
						<Label> <ogc:PropertyName>name</ogc:PropertyName> </Label> <Font>
						<CssParameter name="font-family">Dialog</CssParameter>
						<CssParameter name="font-color">#222222</CssParameter>
						<CssParameter name="font-size">12.0</CssParameter> <CssParameter
						name="font-style">normal</CssParameter> <CssParameter
						name="font-weight">normal</CssParameter> </Font> <LabelPlacement>
						<PointPlacement> <Displacement> <DisplacementX>10</DisplacementX>
						<DisplacementY>0</DisplacementY> </Displacement> </PointPlacement>
						</LabelPlacement> <Halo> <Fill> <CssParameter
						name="fill-opacity">0.25</CssParameter> <CssParameter
						name="fill">#ffff33</CssParameter> </Fill> <Stroke> <CssParameter
						name="stroke">#666666</CssParameter> <CssParameter
						name="stroke-width">1.0</CssParameter> <CssParameter
						name="stroke-linejoin">round</CssParameter> <CssParameter
						name="stroke-opacity">1.0</CssParameter> <CssParameter
						name="stroke-linecap">square</CssParameter> </Stroke> </Halo>
						</TextSymbolizer>
					-->
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>