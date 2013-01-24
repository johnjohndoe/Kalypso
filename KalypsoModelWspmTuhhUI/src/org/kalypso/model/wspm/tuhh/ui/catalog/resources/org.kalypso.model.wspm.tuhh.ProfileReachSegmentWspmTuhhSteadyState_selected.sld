<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
    <Name>default</Name>
    <Title>Profilsegment (TU-HH)</Title>
    <FeatureTypeName>{org.kalypso.model.wspm.tuhh}ProfileReachSegmentWspmTuhhSteadyState</FeatureTypeName>
    <Rule>
        <Name>default</Name>
        <Title>default</Title>
        <Abstract>default</Abstract>
		<TextSymbolizer>
			<Geometry>
		    	<ogc:PropertyName>profileLocation</ogc:PropertyName>
		  	</Geometry>
	  		<Label>
	    		<ogc:PropertyName>station</ogc:PropertyName>
	  		</Label>
            <Font>
                <CssParameter name="font-family">Dialog</CssParameter>
                <CssParameter name="font-color">#FF0000</CssParameter>
                <CssParameter name="font-size">12.0</CssParameter>
                <CssParameter name="font-style">normal</CssParameter>
                <CssParameter name="font-weight">normal</CssParameter>
            </Font>
	  		<LabelPlacement>
	    		<LinePlacement>
	      			<PerpendicularOffset>auto</PerpendicularOffset>
	      			<LineWidth>2</LineWidth>
	      			<Gap>10</Gap>
	    		</LinePlacement>
	  		</LabelPlacement>
		  	<Halo>
                <Fill>
                    <CssParameter name="fill-opacity">0.25</CssParameter>
                    <CssParameter name="fill">#FFFF00</CssParameter>
                </Fill>
                <Stroke>
                    <CssParameter name="stroke">#000000</CssParameter>
                    <CssParameter name="stroke-width">1.0</CssParameter>
                    <CssParameter name="stroke-linejoin">round</CssParameter>
                    <CssParameter name="stroke-opacity">0.0</CssParameter>
                    <CssParameter name="stroke-linecap">square</CssParameter>
                </Stroke>
		  	</Halo>
		</TextSymbolizer>
        <LineSymbolizer uom="meter">
            <Geometry>
                <ogc:PropertyName>profileLocation</ogc:PropertyName>
            </Geometry>
            <Stroke>
                <CssParameter name="stroke">#669933</CssParameter>
                <CssParameter name="stroke-width">3.0</CssParameter>
                <CssParameter name="stroke-linejoin">mitre</CssParameter>
                <CssParameter name="stroke-opacity">1.0</CssParameter>
                <CssParameter name="stroke-linecap">butt</CssParameter>
                <CssParameter name="stroke-arrow-type">line</CssParameter>
                <CssParameter name="stroke-arrow-widget">fill</CssParameter>
                <CssParameter name="stroke-arrow-alignment">end</CssParameter>
                <CssParameter name="stroke-arrow-size">12</CssParameter>                
            </Stroke>
        </LineSymbolizer>
    </Rule>
</FeatureTypeStyle>
