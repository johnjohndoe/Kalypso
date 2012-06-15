<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor 
			xmlns="http://www.opengis.net/sld" 
			xmlns:gml="http://www.opengis.net/gml" 
			xmlns:ogc="http://www.opengis.net/ogc" 
			xmlns:xlink="http://www.w3.org/1999/xlink" 
			xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.0">
    <NamedLayer>
        <Name>Roughness von fe elemente</Name>
        <UserStyle>
            <Name>static_model1d2d_roughness_style</Name>
            <Title>Roughness style</Title>
            <FeatureTypeStyle>
                <Rule>
                    <Name>StaticModel1D2D_Style</Name>
                    <Title>default</Title>
                    <Abstract>default</Abstract>
                    <MinScaleDenominator>0.0</MinScaleDenominator>
                    <MaxScaleDenominator>1.0E15</MaxScaleDenominator>
                    <LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>geometry</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
							<CssParameter name="stroke-width">1.0</CssParameter>
							<CssParameter name="stroke-linejoin">mitre</CssParameter>
							<CssParameter name="stroke-opacity">1.0</CssParameter>
							<CssParameter name="stroke-linecap">butt</CssParameter>
						</Stroke>
					</LineSymbolizer>
                </Rule>                
            </FeatureTypeStyle>
        </UserStyle>
    </NamedLayer>
</StyledLayerDescriptor>