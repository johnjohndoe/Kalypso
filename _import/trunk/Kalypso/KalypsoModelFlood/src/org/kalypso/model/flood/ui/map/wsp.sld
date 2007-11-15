<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version='1.0.0' xmlns='http://www.opengis.net/sld' xmlns:gml='http://www.opengis.net/gml'
	xmlns:ogc='http://www.opengis.net/ogc' xmlns:xlink='http://www.w3.org/1999/xlink'
	xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
	<NamedLayer>
		<Name>wspLayer</Name>
		<UserStyle>
			<Name>wspUserStyle</Name>
			<Title>Wasserspiegel-UserStyle</Title>
			<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
				<Name>wspFts</Name>
				<Rule>
					<Name>wspRule</Name>
					<Title>Wasserspiegel</Title>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>9.0E99</MaxScaleDenominator>
					<SurfacePolygonSymbolizer xmlns:sldExt="http://www.opengis.net/sldExt" uom="pixel">
						<Geometry>
							<ogc:PropertyName>flood:tin</ogc:PropertyName>
						</Geometry>
						<sldExt:PolygonColorMap>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,35 - 8,36</sldExt:label>
								<sldExt:from>8.35</sldExt:from>
								<sldExt:to>8.36</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#ff0000</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#ff0000</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,36 - 8,37</sldExt:label>
								<sldExt:from>8.36</sldExt:from>
								<sldExt:to>8.37</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#ff5d00</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#ff5d00</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,37 - 8,38</sldExt:label>
								<sldExt:from>8.37</sldExt:from>
								<sldExt:to>8.38</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#ffb900</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#ffb900</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,38 - 8,39</sldExt:label>
								<sldExt:from>8.38</sldExt:from>
								<sldExt:to>8.39</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#e8ff00</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#e8ff00</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,39 - 8,40</sldExt:label>
								<sldExt:from>8.39</sldExt:from>
								<sldExt:to>8.4</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#8bff00</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#8bff00</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,40 - 8,41</sldExt:label>
								<sldExt:from>8.4</sldExt:from>
								<sldExt:to>8.41</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#2eff00</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#2eff00</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,41 - 8,42</sldExt:label>
								<sldExt:from>8.41</sldExt:from>
								<sldExt:to>8.42</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#00ff2e</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#00ff2e</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,42 - 8,43</sldExt:label>
								<sldExt:from>8.42</sldExt:from>
								<sldExt:to>8.43</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#00ff8b</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#00ff8b</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,43 - 8,44</sldExt:label>
								<sldExt:from>8.43</sldExt:from>
								<sldExt:to>8.44</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#00ffe8</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#00ffe8</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,44 - 8,45</sldExt:label>
								<sldExt:from>8.44</sldExt:from>
								<sldExt:to>8.45</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#00b9ff</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#00b9ff</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,45 - 8,46</sldExt:label>
								<sldExt:from>8.45</sldExt:from>
								<sldExt:to>8.46</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#005dff</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#005dff</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,46 - 8,47</sldExt:label>
								<sldExt:from>8.46</sldExt:from>
								<sldExt:to>8.47</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke'>#0000ff</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#0000ff</CssParameter>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
						</sldExt:PolygonColorMap>
					</SurfacePolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
	<NamedLayer>
		<Name>dtmGrid</Name>
		<UserStyle>
			<Name>extrapolationPolygonUserStyle</Name>
			<Title>Extrapolations-Polygone</Title>
			<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
				<Name>extrapolationPolygonStyle</Name>
				<Title>Extrapolations-Polygone</Title>
				<FeatureTypeName>{org.kalypso.model.flood}FloodExtrapolationPolygon</FeatureTypeName>
				<Rule>
					<Name>areaRule</Name>
					<Title>Extrapolationsfläche</Title>
					<ogc:Filter xmlns:ogc='http://www.opengis.net/ogc'>
						<ogc:PropertyIsEqualTo>
							<ogc:Function name="org.kalypso.model.flood.ui.map.EventFilterExpression">
								<ogc:Literal>RunoffEvent11951496414172</ogc:Literal>
							</ogc:Function>
							<ogc:Literal>true</ogc:Literal>
						</ogc:PropertyIsEqualTo>
					</ogc:Filter>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<LineSymbolizer uom="pixel">
						<Geometry>
							<ogc:PropertyName>flood:connectorLine</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name='stroke-width'>1.0</CssParameter>
							<CssParameter name='stroke'>#11aa11</CssParameter>
							<CssParameter name='stroke-linejoin'>round</CssParameter>
							<CssParameter name='stroke-opacity'>1.0</CssParameter>
							<CssParameter name='stroke-linecap'>square</CssParameter>
						</Stroke>
					</LineSymbolizer>
					<PolygonSymbolizer uom="pixel">
						<Geometry>
							<ogc:PropertyName>flood:areaMember</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name='fill-opacity'>0.6</CssParameter>
							<CssParameter name='fill'>#88dd88</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name='stroke-width'>1.0</CssParameter>
							<CssParameter name='stroke'>#11aa11</CssParameter>
							<CssParameter name='stroke-linejoin'>mitre</CssParameter>
							<CssParameter name='stroke-opacity'>1.0</CssParameter>
							<CssParameter name='stroke-linecap'>butt</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
					<PointSymbolizer uom="pixel">
						<Geometry>
							<ogc:PropertyName>flood:referencePoint</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>triangle</WellKnownName>
								<Fill>
									<CssParameter name='fill-opacity'>1.0</CssParameter>
									<CssParameter name='fill'>#3333ff</CssParameter>
								</Fill>
								<Stroke>
									<CssParameter name='stroke-width'>1.0</CssParameter>
									<CssParameter name='stroke'>#11aa11</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-opacity'>1.0</CssParameter>
									<CssParameter name='stroke-linecap'>square</CssParameter>
								</Stroke>
							</Mark>
							<Opacity>1.0</Opacity>
							<Size>10.0</Size>
							<Rotation>0.0</Rotation>
						</Graphic>
					</PointSymbolizer>
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>flood:areaMember</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:Function
								name="org.kalypsodeegree_impl.filterencoding.ListPropertyToStringExpression">
								<ogc:PropertyName>flood:eventMember</ogc:PropertyName>
								<ogc:Literal>/</ogc:Literal>
							</ogc:Function>
						</Label>
						<Font>
							<CssParameter name="font-family" />
							<CssParameter name="font-color">#000000</CssParameter>
							<CssParameter name="font-size">12.0</CssParameter>
							<CssParameter name="font-style">normal</CssParameter>
							<CssParameter name="font-weight">normal</CssParameter>
						</Font>
						<Halo>
							<Fill>
								<CssParameter name="fill-opacity">0.4</CssParameter>
								<CssParameter name="fill">#fbf193</CssParameter>
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
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>clipPolygonUserStyle</Name>
			<Title>Clip-Polygone</Title>
			<FeatureTypeStyle xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc">
				<Name>clipPolygonStyle</Name>
				<FeatureTypeName>{org.kalypso.model.flood}FloodClipPolygon</FeatureTypeName>
				<Rule>
					<Name>areaRule</Name>
					<Title>Clip</Title>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<PolygonSymbolizer uom="pixel">
						<Geometry>
							<ogc:PropertyName>flood:areaMember</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name='fill-opacity'>0.6</CssParameter>
							<CssParameter name='fill'>#dd8888</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name='stroke-width'>1.0</CssParameter>
							<CssParameter name='stroke'>#aa1111</CssParameter>
							<CssParameter name='stroke-linejoin'>mitre</CssParameter>
							<CssParameter name='stroke-opacity'>1.0</CssParameter>
							<CssParameter name='stroke-linecap'>butt</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>flood:areaMember</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:Function
								name="org.kalypsodeegree_impl.filterencoding.ListPropertyToStringExpression">
								<ogc:PropertyName>flood:eventMember</ogc:PropertyName>
								<ogc:Literal>/</ogc:Literal>
							</ogc:Function>
						</Label>
						<Font>
							<CssParameter name="font-family" />
							<CssParameter name="font-color">#000000</CssParameter>
							<CssParameter name="font-size">12.0</CssParameter>
							<CssParameter name="font-style">normal</CssParameter>
							<CssParameter name="font-weight">normal</CssParameter>
						</Font>
						<Halo>

							<Fill>
								<CssParameter name="fill-opacity">0.4</CssParameter>
								<CssParameter name="fill">#fbf193</CssParameter>
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
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>