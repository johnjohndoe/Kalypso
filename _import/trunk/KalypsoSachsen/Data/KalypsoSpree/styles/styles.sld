<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version="String" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<NamedLayer>
		<Name>deegree style definition</Name>
		<UserStyle>
			<Name>default:cite:MapNeatline</Name>
			<Title>cite:MapNeatline</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>cite:MapNeatline</Name>
				<Rule>
					<Name>cite:MapNeatline</Name>
					<MinScaleDenominator>0</MinScaleDenominator>
					<MaxScaleDenominator>99999999</MaxScaleDenominator>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
							<CssParameter name="stroke-width">1</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:BuildingCenters</Name>
			<Title>cite:BuildingCenters</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>cite:BuildingCenters</Name>
				<Rule>
					<Name>cite:BuildingCenters</Name>
					<MinScaleDenominator>0</MinScaleDenominator>
					<MaxScaleDenominator>999999999</MaxScaleDenominator>
					<PointSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>square</WellKnownName>
								<Fill>
									<CssParameter name="fill">#000000</CssParameter>
								</Fill>
								<Stroke>
									<CssParameter name="stoke">#111111</CssParameter>
									<CssParameter name="stoke-width">1.0</CssParameter>
								</Stroke>
							</Mark>
							<Size>3</Size>
						</Graphic>
					</PointSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:Bridges</Name>
			<Title>cite:Bridges</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>cite:Bridges</Name>
				<Rule>
					<Name>cite:Bridges</Name>
					<MinScaleDenominator>0</MinScaleDenominator>
					<MaxScaleDenominator>999999999</MaxScaleDenominator>
					<PointSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>square</WellKnownName>
								<Fill>
									<CssParameter name="fill">#ff0000</CssParameter>
								</Fill>
								<Stroke>
									<CssParameter name="stoke">#0000ff</CssParameter>
									<CssParameter name="stoke-width">1.0</CssParameter>
								</Stroke>
							</Mark>
						</Graphic>
					</PointSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:Lakes</Name>
			<Title>BlueFill</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>BlueFill</Name>
					<MinScaleDenominator>0</MinScaleDenominator>
					<MaxScaleDenominator>99999999</MaxScaleDenominator>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill">0x0000FF</CssParameter>
							<CssParameter name="fill-opacity">0.5</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">0x000000</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:Forests</Name>
			<Title>BlueFill</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>BlueFill</Name>
					<MinScaleDenominator>0</MinScaleDenominator>
					<MaxScaleDenominator>99999999</MaxScaleDenominator>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill">0x00F200</CssParameter>
							<CssParameter name="fill-opacity">0.5</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">0x000000</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:Buildings</Name>
			<Title>default:cite:Buildings</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>default:cite:Buildings</Name>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill">0xFFFFFF</CssParameter>
							<CssParameter name="fill-opacity">0.5</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">0x000000</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:NamedPlaces</Name>
			<Title>default:cite:NamedPlaces</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>default:cite:NamedPlaces</Name>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill">0xCCCCCC</CssParameter>
							<CssParameter name="fill-opacity">0.7</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">0x000000</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:Ponds</Name>
			<Title>default:cite:Ponds</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>default:cite:Ponds</Name>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill">0xDDDDDD</CssParameter>
							<CssParameter name="fill-opacity">0.7</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">0x000000</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:Streams</Name>
			<Title>default:cite:Streams</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#0000FF</CssParameter>
							<CssParameter name="stroke-width">2</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:DividedRoutes</Name>
			<Title>default:cite:DividedRoutes</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
							<CssParameter name="stroke-width">3</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:RoadSegments</Name>
			<Title>default:cite:RoadSegments</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
							<CssParameter name="stroke-width">3</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:cite:Rivers</Name>
			<Title>default:cite:Rivers</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#0000FF</CssParameter>
							<CssParameter name="stroke-width">1</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:europe:major_rivers</Name>
			<Title>rivers</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule2</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#000088</CssParameter>
							<CssParameter name="stroke-width">2</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
				<Rule>
					<MinScaleDenominator>0</MinScaleDenominator>
					<MaxScaleDenominator>4100000</MaxScaleDenominator>
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>NAME</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family">Serif</CssParameter>
							<CssParameter name="font-style">normal</CssParameter>
							<CssParameter name="font-weight">normal</CssParameter>
							<CssParameter name="font-size">12</CssParameter>
							<CssParameter name="font-color">#000000</CssParameter>
						</Font>
						<LabelPlacement>
							<LinePlacement>
								<PerpendicularOffset>above</PerpendicularOffset>
								<!-- width of the line the label is associated with -->
								<LineWidth>1</LineWidth>
								<!-- gap between labels measured in label width -->
								<Gap>10</Gap>
							</LinePlacement>
						</LabelPlacement>
					</TextSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:europe:country</Name>
			<Title>country</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule2</Name>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
							<CssParameter name="stroke-width">1</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:europe:railroad</Name>
			<Title>railroad</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
							<CssParameter name="stroke-width">5</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
				<Rule>
					<Name>LineRule</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#FFFFFF</CssParameter>
							<CssParameter name="stroke-width">3</CssParameter>
							<CssParameter name="stroke-linecap">square</CssParameter>
							<CssParameter name="stroke-linejoin">round</CssParameter>
							<CssParameter name="stroke-dasharray">10 15</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:europe:major_urban_places</Name>
			<Title>major_urban_places</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule2</Name>
					<PolygonSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Fill>
							<CssParameter name="fill">#FF7777</CssParameter>
							<CssParameter name="fill-opacity">1.0</CssParameter>
						</Fill>
						<Stroke>
							<CssParameter name="stroke">#000000</CssParameter>
							<CssParameter name="stroke-width">1</CssParameter>
						</Stroke>
					</PolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:europe:urban_places_label</Name>
			<Title>urban_places_label</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle098</Name>
				<Rule>
					<Name>urban_Text</Name>
					<MinScaleDenominator>0</MinScaleDenominator>
					<MaxScaleDenominator>3000000</MaxScaleDenominator>
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>NAME</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family">Arial</CssParameter>
							<CssParameter name="font-family">Sans-Serif</CssParameter>
							<CssParameter name="font-style">italic</CssParameter>
							<CssParameter name="font-size">10</CssParameter>
							<CssParameter name="font-color">#222222</CssParameter>
						</Font>
						<LabelPlacement>
							<PointPlacement auto="true"/>
						</LabelPlacement>
						<Halo>
							<Fill>
								<CssParameter name="fill">#00FFFF</CssParameter>
								<CssParameter name="fill-opacity">0.8</CssParameter>
							</Fill>
						</Halo>
					</TextSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		<UserStyle>
			<Name>default:europe:major_roads</Name>
			<Title>default:railroad</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle09876</Name>
				<Rule>
					<Name>LineRule</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#FF702A</CssParameter>
							<CssParameter name="stroke-width">5</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
				<Rule>
					<Name>LineRule</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#FFFF00</CssParameter>
							<CssParameter name="stroke-width">3</CssParameter>
							<CssParameter name="stroke-linecap">square</CssParameter>
							<CssParameter name="stroke-linejoin">round</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		
		<UserStyle>
			<Name>country:labels</Name>
			<Title>Laendernamen</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>MyFeatureTypeStyle4986</Name>
				<Rule>
					<Name>urban_Text</Name>
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>GEOM</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>NAME</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family">Arial</CssParameter>
							<CssParameter name="font-family">Sans-Serif</CssParameter>
							<CssParameter name="font-style">italic</CssParameter>
							<CssParameter name="font-size">10</CssParameter>
							<CssParameter name="font-color">#222222</CssParameter>
						</Font>
						<LabelPlacement>
							<PointPlacement auto="true"/>
						</LabelPlacement>
						<Halo>
							<Fill>
								<CssParameter name="fill">#00FFFF</CssParameter>
								<CssParameter name="fill-opacity">0.8</CssParameter>
							</Fill>
						</Halo>
					</TextSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>

		<UserStyle>
			<Name>Talsperren</Name>
			<Title>Talsperren</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>Talsperren</Name>
				<Rule>
					<Name>Talsperren</Name>
					<PointSymbolizer>
						<Geometry>
							<ogc:PropertyName>Ort</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>triangle</WellKnownName>
								<Fill>
									<CssParameter name="fill">#0000ab</CssParameter>
								</Fill>
								<Stroke>
									<CssParameter name="stroke">#111111</CssParameter>
									<CssParameter name="stroke-width">1.0</CssParameter>
								</Stroke>
								
							</Mark>
							<Size>10</Size>
						</Graphic>
					</PointSymbolizer>
					
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>Ort</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>Name</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family">Arial</CssParameter>
							<CssParameter name="font-family">Sans-Serif</CssParameter>
							<CssParameter name="font-style">italic</CssParameter>
							<CssParameter name="font-size">10</CssParameter>
							<CssParameter name="font-color">#222222</CssParameter>
						</Font>
						<LabelPlacement>
							<PointPlacement auto="true"/>
						</LabelPlacement>
					</TextSymbolizer>
					
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>

		<UserStyle>
			<Name>Einzugsgebiete</Name>
			<Title>Einzugsgebiete</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>Einzugsgebiete</Name>
				<Rule>
					<Name>Einzugsgebiete</Name>
					<PointSymbolizer>
						<Geometry>
							<ogc:PropertyName>Ort</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>triangle</WellKnownName>
								<Fill>
									<CssParameter name="fill">#00ab00</CssParameter>
								</Fill>
								<Stroke>
									<CssParameter name="stroke">#111111</CssParameter>
									<CssParameter name="stroke-width">1.0</CssParameter>
								</Stroke>
							</Mark>
							<Size>10</Size>
						</Graphic>
					</PointSymbolizer>
					
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>Ort</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>Name</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family">Arial</CssParameter>
							<CssParameter name="font-family">Sans-Serif</CssParameter>
							<CssParameter name="font-style">italic</CssParameter>
							<CssParameter name="font-size">10</CssParameter>
							<CssParameter name="font-color">#222222</CssParameter>
						</Font>
						<LabelPlacement>
							<PointPlacement auto="true"/>
						</LabelPlacement>
					</TextSymbolizer>
					
					
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>

		<UserStyle>
			<Name>Flusslaufmodelle</Name>
			<Title>Flusslaufmodelle</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>Flusslaufmodelle</Name>
				<Rule>
					<Name>Flusslaufmodelle</Name>
					<PointSymbolizer>
						<Geometry>
							<ogc:PropertyName>Ort</ogc:PropertyName>
						</Geometry>
						<Graphic>
							<Mark>
								<WellKnownName>triangle</WellKnownName>
								<Fill>
									<CssParameter name="fill">#ab0000</CssParameter>
								</Fill>
								<Stroke>
									<CssParameter name="stroke">#111111</CssParameter>
									<CssParameter name="stroke-width">1.0</CssParameter>
								</Stroke>
							</Mark>
							<Size>10</Size>
						</Graphic>
					</PointSymbolizer>
					
					<TextSymbolizer>
						<Geometry>
							<ogc:PropertyName>Ort</ogc:PropertyName>
						</Geometry>
						<Label>
							<ogc:PropertyName>Name</ogc:PropertyName>
						</Label>
						<Font>
							<CssParameter name="font-family">Arial</CssParameter>
							<CssParameter name="font-family">Sans-Serif</CssParameter>
							<CssParameter name="font-style">italic</CssParameter>
							<CssParameter name="font-size">10</CssParameter>
							<CssParameter name="font-color">#222222</CssParameter>
						</Font>
						<LabelPlacement>
							<PointPlacement auto="true"/>
						</LabelPlacement>
					</TextSymbolizer>
					
					
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		
		<UserStyle>
			<Name>Spree:Gewaesserverlauf</Name>
			<Title>Spree:Gewaesserverlauf</Title>
			<IsDefault>1</IsDefault>
			<FeatureTypeStyle>
				<Name>Spree:Gewaesserverlauf</Name>
				<Rule>
					<Name>LineRule</Name>
					<LineSymbolizer>
						<Geometry>
							<ogc:PropertyName>Shape</ogc:PropertyName>
						</Geometry>
						<Stroke>
							<CssParameter name="stroke">#0000FF</CssParameter>
							<CssParameter name="stroke-width">1</CssParameter>
						</Stroke>
					</LineSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
		
		
	</NamedLayer>
</StyledLayerDescriptor>
