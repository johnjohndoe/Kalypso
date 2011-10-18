<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<Name>fts</Name>
	<Title>%fts_title</Title>
	<Rule>
		<Name>rule_geometry</Name>
		<Title>%rule_geometry_title</Title>
		<PointSymbolizer>
			<Geometry>
				<ogc:PropertyName>Ort</ogc:PropertyName>
			</Geometry>
			<Graphic>
				<Mark>
					<WellKnownName>square</WellKnownName>
					<Fill>
						<CssParameter name="fill-opacity">1.0</CssParameter>
						<CssParameter name="fill">#23969c</CssParameter>
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
				<Size>11.0</Size>
				<Rotation>0.0</Rotation>
			</Graphic>
		</PointSymbolizer>
	</Rule>
	<Rule>
		<Name>rule_downStreamNode</Name>
		<Title>%rule_downStreamNode_title</Title>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_downStreamNodeMember</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#23969c</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
                <CssParameter name="stroke-arrow-type">line</CssParameter>
                <CssParameter name="stroke-arrow-widget">open</CssParameter>
                <CssParameter name="stroke-arrow-alignment">end</CssParameter>
                <CssParameter name="stroke-arrow-size">35</CssParameter>
			</Stroke>
		</LineSymbolizer>
		<TextSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_downStreamNodeMember</ogc:PropertyName>
			</Geometry>
			<Label>
				<ogc:PropertyName>name</ogc:PropertyName>
			</Label>
			<Font>
                <CssParameter name="font-color">#000000</CssParameter>
                <CssParameter name="font-size">12.0</CssParameter>
                <CssParameter name="font-style">normal</CssParameter>
                <CssParameter name="font-weight">normal</CssParameter>
            </Font>
            <LabelPlacement>
                <LinePlacement>
                    <PerpendicularOffset>center</PerpendicularOffset>
                    <LineWidth>2</LineWidth>
                    <Gap>10</Gap>
                </LinePlacement>
            </LabelPlacement>
            <Halo>
                <Radius>3</Radius>
                <Fill>
                    <CssParameter name="fill-opacity">1.0</CssParameter>
                    <CssParameter name="fill">#23969c</CssParameter>
                </Fill>
                <Stroke>
                    <CssParameter name="stroke">#000000</CssParameter>
                    <CssParameter name="stroke-width">2.0</CssParameter>
                    <CssParameter name="stroke-linejoin">round</CssParameter>
                    <CssParameter name="stroke-opacity">1.0</CssParameter>
                    <CssParameter name="stroke-linecap">square</CssParameter>
                </Stroke>
            </Halo>
		</TextSymbolizer>		
	</Rule>
	<Rule>
		<Name>rule_downStreamNode_2nd</Name>
		<Title>%rule_downStreamNode_2nd_title</Title>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_downStreamNodeMember_2nd</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#8DB6CD</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
                <CssParameter name="stroke-arrow-type">line</CssParameter>
                <CssParameter name="stroke-arrow-widget">open</CssParameter>
                <CssParameter name="stroke-arrow-alignment">end</CssParameter>
                <CssParameter name="stroke-arrow-size">35</CssParameter>
			</Stroke>
		</LineSymbolizer>
		<TextSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_downStreamNodeMember_2nd</ogc:PropertyName>
			</Geometry>
			<Label>
				<ogc:PropertyName>name</ogc:PropertyName>
			</Label>
			<Font>
				<CssParameter name="font-color">#000000</CssParameter>
				<CssParameter name="font-size">12.0</CssParameter>
				<CssParameter name="font-style">normal</CssParameter>
				<CssParameter name="font-weight">normal</CssParameter>
			</Font>
			<LabelPlacement>
                <LinePlacement>
                    <PerpendicularOffset>center</PerpendicularOffset>
                    <LineWidth>2</LineWidth>
                    <Gap>10</Gap>
                </LinePlacement>
			</LabelPlacement>
			<Halo>
                <Radius>3</Radius>
				<Fill>
					<CssParameter name="fill-opacity">1.0</CssParameter>
					<CssParameter name="fill">#8DB6CD</CssParameter>
				</Fill>
				<Stroke>
					<CssParameter name="stroke">#000000</CssParameter>
					<CssParameter name="stroke-width">2.0</CssParameter>
					<CssParameter name="stroke-linejoin">round</CssParameter>
					<CssParameter name="stroke-opacity">1.0</CssParameter>
					<CssParameter name="stroke-linecap">square</CssParameter>
				</Stroke>
			</Halo>
		</TextSymbolizer>		
	</Rule>
	<Rule>
		<Name>rule_downStreamNode_3rd</Name>
		<Title>%rule_downStreamNode_3rd_title</Title>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_downStreamNodeMember_3rd</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#87CEFA</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
                <CssParameter name="stroke-arrow-type">line</CssParameter>
                <CssParameter name="stroke-arrow-widget">open</CssParameter>
                <CssParameter name="stroke-arrow-alignment">end</CssParameter>
                <CssParameter name="stroke-arrow-size">35</CssParameter>
			</Stroke>
		</LineSymbolizer>
		<TextSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_downStreamNodeMember_3rd</ogc:PropertyName>
			</Geometry>
			<Label><ogc:PropertyName>name</ogc:PropertyName></Label>
			<Font>
				<CssParameter name="font-color">#000000</CssParameter>
				<CssParameter name="font-size">12.0</CssParameter>
				<CssParameter name="font-style">normal</CssParameter>
				<CssParameter name="font-weight">normal</CssParameter>
			</Font>
			<LabelPlacement>
                <LinePlacement>
                    <PerpendicularOffset>center</PerpendicularOffset>
                    <LineWidth>2</LineWidth>
                    <Gap>10</Gap>
                </LinePlacement>
			</LabelPlacement>
			<Halo>
                <Radius>3</Radius>
				<Fill>
					<CssParameter name="fill-opacity">1.0</CssParameter>
					<CssParameter name="fill">#87CEFA</CssParameter>
				</Fill>
				<Stroke>
					<CssParameter name="stroke">#000000</CssParameter>
					<CssParameter name="stroke-width">2.0</CssParameter>
					<CssParameter name="stroke-linejoin">round</CssParameter>
					<CssParameter name="stroke-opacity">1.0</CssParameter>
					<CssParameter name="stroke-linecap">square</CssParameter>
				</Stroke>
			</Halo>
		</TextSymbolizer>		
	</Rule>
	<Rule>
		<Name>rule_overflowNode</Name>
		<Title>%rule_overflowNode_title</Title>
		<LineSymbolizer>
			<Geometry>
				<ogc:PropertyName>link_iknotNodeMember</ogc:PropertyName>
			</Geometry>
			<Stroke>
				<CssParameter name="stroke">#ff80c0</CssParameter>
				<CssParameter name="stroke-width">2.0</CssParameter>
				<CssParameter name="stroke-linejoin">mitre</CssParameter>
				<CssParameter name="stroke-opacity">1.0</CssParameter>
				<CssParameter name="stroke-linecap">butt</CssParameter>
                <CssParameter name="stroke-arrow-type">line</CssParameter>
                <CssParameter name="stroke-arrow-widget">open</CssParameter>
                <CssParameter name="stroke-arrow-alignment">end</CssParameter>
                <CssParameter name="stroke-arrow-size">35</CssParameter>
			</Stroke>
		</LineSymbolizer>
	</Rule>
</FeatureTypeStyle>