<?xml version="1.0" encoding="UTF-8"?>
<StyledLayerDescriptor version='1.0.0'
	xmlns='http://www.opengis.net/sld' xmlns:sld='http://www.opengis.net/sld'
	xmlns:sldExt='http://www.opengis.net/sldExt' xmlns:gml='http://www.opengis.net/gml'
	xmlns:ogc='http://www.opengis.net/ogc' xmlns:xlink='http://www.w3.org/1999/xlink'
	xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
	<NamedLayer>
		<Name>tinStyles</Name>
		<UserStyle>
			<Name>tinPolygonStyle</Name>
			<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
				xmlns:ogc="http://www.opengis.net/ogc">
				<Name>tinFeatureTypeStyle</Name>
				<Rule>
					<Name>tinRule</Name>
					<MinScaleDenominator>0.0</MinScaleDenominator>
					<MaxScaleDenominator>1.7976931348623157E308</MaxScaleDenominator>
					<SurfacePolygonSymbolizer xmlns:sldExt="http://www.opengis.net/sldExt"
						uom="pixel">
						<Geometry>
							<ogc:PropertyName>res1d2d:triangulatedSurfaceMember
							</ogc:PropertyName>
						</Geometry>
						<sldExt:PolygonColorMap>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>0,00 - 0,25</sldExt:label>
								<sldExt:from>0.0</sldExt:from>
								<sldExt:to>0.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#80ffff</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#80ffff</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>0,25 - 0,50</sldExt:label>
								<sldExt:from>0.25</sldExt:from>
								<sldExt:to>0.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#7bf9fc</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#7bf9fc</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>0,50 - 0,75</sldExt:label>
								<sldExt:from>0.5</sldExt:from>
								<sldExt:to>0.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#77f2f9</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#77f2f9</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>0,75 - 1,00</sldExt:label>
								<sldExt:from>0.75</sldExt:from>
								<sldExt:to>1.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#72ecf6</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#72ecf6</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>1,00 - 1,25</sldExt:label>
								<sldExt:from>1.0</sldExt:from>
								<sldExt:to>1.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#6ee6f3</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#6ee6f3</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>1,25 - 1,50</sldExt:label>
								<sldExt:from>1.25</sldExt:from>
								<sldExt:to>1.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#6adff0</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#6adff0</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>1,50 - 1,75</sldExt:label>
								<sldExt:from>1.5</sldExt:from>
								<sldExt:to>1.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#65d9ec</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#65d9ec</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>1,75 - 2,00</sldExt:label>
								<sldExt:from>1.75</sldExt:from>
								<sldExt:to>2.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#61d2e9</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#61d2e9</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>2,00 - 2,25</sldExt:label>
								<sldExt:from>2.0</sldExt:from>
								<sldExt:to>2.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#5dcbe6</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#5dcbe6</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>2,25 - 2,50</sldExt:label>
								<sldExt:from>2.25</sldExt:from>
								<sldExt:to>2.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#59c5e3</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#59c5e3</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>2,50 - 2,75</sldExt:label>
								<sldExt:from>2.5</sldExt:from>
								<sldExt:to>2.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#55bee0</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#55bee0</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>2,75 - 3,00</sldExt:label>
								<sldExt:from>2.75</sldExt:from>
								<sldExt:to>3.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#51b7dd</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#51b7dd</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>3,00 - 3,25</sldExt:label>
								<sldExt:from>3.0</sldExt:from>
								<sldExt:to>3.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#4db1da</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#4db1da</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>3,25 - 3,50</sldExt:label>
								<sldExt:from>3.25</sldExt:from>
								<sldExt:to>3.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#4aaad7</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#4aaad7</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>3,50 - 3,75</sldExt:label>
								<sldExt:from>3.5</sldExt:from>
								<sldExt:to>3.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#46a3d4</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#46a3d4</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>3,75 - 4,00</sldExt:label>
								<sldExt:from>3.75</sldExt:from>
								<sldExt:to>4.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#429dd1</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#429dd1</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>4,00 - 4,25</sldExt:label>
								<sldExt:from>4.0</sldExt:from>
								<sldExt:to>4.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#3f96cd</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#3f96cd</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>4,25 - 4,50</sldExt:label>
								<sldExt:from>4.25</sldExt:from>
								<sldExt:to>4.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#3b8fca</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#3b8fca</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>4,50 - 4,75</sldExt:label>
								<sldExt:from>4.5</sldExt:from>
								<sldExt:to>4.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#3888c7</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#3888c7</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>4,75 - 5,00</sldExt:label>
								<sldExt:from>4.75</sldExt:from>
								<sldExt:to>5.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#3582c4</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#3582c4</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>5,00 - 5,25</sldExt:label>
								<sldExt:from>5.0</sldExt:from>
								<sldExt:to>5.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#327bc1</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#327bc1</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>5,25 - 5,50</sldExt:label>
								<sldExt:from>5.25</sldExt:from>
								<sldExt:to>5.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#2f74be</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#2f74be</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>5,50 - 5,75</sldExt:label>
								<sldExt:from>5.5</sldExt:from>
								<sldExt:to>5.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#2b6ebb</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#2b6ebb</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>5,75 - 6,00</sldExt:label>
								<sldExt:from>5.75</sldExt:from>
								<sldExt:to>6.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#2867b8</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#2867b8</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>6,00 - 6,25</sldExt:label>
								<sldExt:from>6.0</sldExt:from>
								<sldExt:to>6.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#2661b5</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#2661b5</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>6,25 - 6,50</sldExt:label>
								<sldExt:from>6.25</sldExt:from>
								<sldExt:to>6.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#235bb2</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#235bb2</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>6,50 - 6,75</sldExt:label>
								<sldExt:from>6.5</sldExt:from>
								<sldExt:to>6.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#2054ae</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#2054ae</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>6,75 - 7,00</sldExt:label>
								<sldExt:from>6.75</sldExt:from>
								<sldExt:to>7.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#1d4eab</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#1d4eab</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>7,00 - 7,25</sldExt:label>
								<sldExt:from>7.0</sldExt:from>
								<sldExt:to>7.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#1b48a8</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#1b48a8</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>7,25 - 7,50</sldExt:label>
								<sldExt:from>7.25</sldExt:from>
								<sldExt:to>7.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#1842a5</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#1842a5</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>7,50 - 7,75</sldExt:label>
								<sldExt:from>7.5</sldExt:from>
								<sldExt:to>7.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#163ba2</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#163ba2</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>7,75 - 8,00</sldExt:label>
								<sldExt:from>7.75</sldExt:from>
								<sldExt:to>8.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#13359f</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#13359f</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,00 - 8,25</sldExt:label>
								<sldExt:from>8.0</sldExt:from>
								<sldExt:to>8.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#11309c</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#11309c</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,25 - 8,50</sldExt:label>
								<sldExt:from>8.25</sldExt:from>
								<sldExt:to>8.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#0f2a99</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#0f2a99</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,50 - 8,75</sldExt:label>
								<sldExt:from>8.5</sldExt:from>
								<sldExt:to>8.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#0d2496</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#0d2496</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>8,75 - 9,00</sldExt:label>
								<sldExt:from>8.75</sldExt:from>
								<sldExt:to>9.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#0b1f93</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#0b1f93</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>9,00 - 9,25</sldExt:label>
								<sldExt:from>9.0</sldExt:from>
								<sldExt:to>9.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#09198f</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#09198f</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>9,25 - 9,50</sldExt:label>
								<sldExt:from>9.25</sldExt:from>
								<sldExt:to>9.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#07148c</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#07148c</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>9,50 - 9,75</sldExt:label>
								<sldExt:from>9.5</sldExt:from>
								<sldExt:to>9.75</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#050f89</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#050f89</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>9,75 - 10,00</sldExt:label>
								<sldExt:from>9.75</sldExt:from>
								<sldExt:to>10.0</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#030a86</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#030a86</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>10,00 - 10,25</sldExt:label>
								<sldExt:from>10.0</sldExt:from>
								<sldExt:to>10.25</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#020583</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#020583</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
							<sldExt:PolygonColorMapEntry>
								<sldExt:label>10,25 - 10,50</sldExt:label>
								<sldExt:from>10.25</sldExt:from>
								<sldExt:to>10.5</sldExt:to>
								<Stroke>
									<CssParameter name='stroke-width'>0.01</CssParameter>
									<CssParameter name='stroke-opacity'>0.0</CssParameter>
									<CssParameter name='stroke'>#000080</CssParameter>
									<CssParameter name='stroke-linejoin'>round</CssParameter>
									<CssParameter name='stroke-linecap'>round</CssParameter>
								</Stroke>
								<Fill>
									<CssParameter name='fill'>#000080</CssParameter>
									<CssParameter name='fill-opacity'>0.4</CssParameter>
								</Fill>
							</sldExt:PolygonColorMapEntry>
						</sldExt:PolygonColorMap>
					</SurfacePolygonSymbolizer>
				</Rule>
			</FeatureTypeStyle>
		</UserStyle>
	</NamedLayer>
</StyledLayerDescriptor>