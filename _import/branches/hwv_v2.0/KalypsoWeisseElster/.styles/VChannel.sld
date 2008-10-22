<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" 
	xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<NamedLayer>
		<Name>deegree style definition</Name>
		<UserStyle>
			<Name>VChannel</Name>
			<Title>VChannel</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>VChannel</Name>
				<Rule>
					<Name>default</Name>
					<Title>default</Title>
					<Abstract>default</Abstract>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>Ort</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#0000ff</CssParameter>
							<CssParameter name="stroke-width">2.0</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>