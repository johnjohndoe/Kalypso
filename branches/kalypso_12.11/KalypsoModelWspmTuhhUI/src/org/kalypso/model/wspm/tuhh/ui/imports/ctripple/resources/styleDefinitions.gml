<?xml version="1.0" encoding="UTF-8"?>
<WspmProject xmlns:gml="http://www.opengis.net/gml"
	xmlns:swe="http://www.opengis.net/swe" xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:common="org.kalypso.gml.common" xmlns:wspm="org.kalypso.model.wspm"
	xmlns:wspmClasses="org.kalypso.model.wspm.classifications" xmlns:tuhh="org.kalypso.model.wspm.tuhh"
	xmlns:prof="org.kalypso.model.wspmprofile" xmlns:runoff="org.kalypso.model.wspmrunoff"
	xmlns:sweExt="org.kalypso.swe.ext" xmlns="org.kalypso.model.wspmproj"
	gml:id="root">
	<wspmClasses:classificationMember>
		<wspmClasses:WspmClassification gml:id="WspmClassification1">
			<gml:name>Beispielklassen</gml:name>
			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition1">
					<gml:description>Schlammsohle</gml:description>
					<gml:name>mudHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style1A">
							<gml:description />
							<gml:name>Schlammsohle (Linie)</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter1A">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>2</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter1B">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#FF9600</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter1C">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>line</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style1B">
							<gml:description />
							<gml:name>Schlammsohle (Punkt)</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter1D">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter1E">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter1F">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#FF9600</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter1G">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter1H">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter1I">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition2">
					<gml:description>Bauwerkspunkte</gml:description>
					<gml:name>structureHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style2A">
							<gml:description />
							<gml:name>Bauwerkspunkte (Linie)</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter2A">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>2</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter2B">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#0080B3</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter2C">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>line</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style2B">
							<gml:description />
							<gml:name>Bauwerkspunkte (Punkt)</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter2D">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter2E">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter2F">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#0080B3</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter2G">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter2H">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter2I">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition3">
					<gml:description>Unbekannte Punkte</gml:description>
					<gml:name>unknownPointsHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style3A">
							<gml:description />
							<gml:name>Unbekannte Punkte</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter3A">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter3B">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter3C">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#9900FF</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter3D">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter3E">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter3F">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition4">
					<gml:description>Kreisdurchlass</gml:description>
					<gml:name>circleHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style4A">
							<gml:description />
							<gml:name>Kreisdurchlass</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter4A">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter4B">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter4C">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#FF00FF</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter4D">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter4E">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter4F">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition5">
					<gml:description>Fotostandorte</gml:description>
					<gml:name>fotoHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style5A">
							<gml:description />
							<gml:name>Fotostandorte</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter5A">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter5B">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter5C">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#9900FF</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter5D">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter5E">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter5F">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition6">
					<gml:description>Brücke</gml:description>
					<gml:name>bridgeHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style6A">
							<gml:description />
							<gml:name>Brücke</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter6A">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter6B">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter6C">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#FF00FF</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter6D">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter6E">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter6F">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition7">
					<gml:description>Oberkante Brücke</gml:description>
					<gml:name>bridgeOkHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style7A">
							<gml:description />
							<gml:name>Oberkante Brücke</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter7A">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter7B">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter7C">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#FF00FF</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter7D">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter7E">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter7F">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition8">
					<gml:description>Durchlass</gml:description>
					<gml:name>culvertHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style8A">
							<gml:description />
							<gml:name>Durchlass</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter8A">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter8B">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter8C">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#FF00FF</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter8D">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter8E">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter8F">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition9">
					<gml:description>Leitungen</gml:description>
					<gml:name>conductsHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style9A">
							<gml:description />
							<gml:name>Leitungen</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter9A">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter9B">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter9C">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#999999</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter9D">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter9E">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter9F">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition10">
					<gml:description>Geländer</gml:description>
					<gml:name>gelaenderHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style10A">
							<gml:description />
							<gml:name>Geländer (Linie)</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter10A">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>2</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter10B">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#999999</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter10C">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>line</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style10B">
							<gml:description />
							<gml:name>Geländer (Punkt)</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter10D">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter10E">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter10F">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#999999</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter10G">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter10H">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter10I">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition11">
					<gml:description>Leitplanke</gml:description>
					<gml:name>leitplankeHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style11A">
							<gml:description />
							<gml:name>Leitplanke (Linie)</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter11A">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>2</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter11B">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#999999</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter11C">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>line</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style11B">
							<gml:description />
							<gml:name>Leitplanke (Punkt)</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter11D">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter11E">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter11F">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#999999</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter11G">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter11H">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter11I">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>5</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>

			<wspmClasses:styleDefinition>
				<wspmClasses:StyleDefinition gml:id="StyleDefinition12">
					<gml:description>Sohlschwelle</gml:description>
					<gml:name>sohlschwelleHorizon</gml:name>
					<wspmClasses:styleMember>
						<wspmClasses:Style gml:id="Style12A">
							<gml:description />
							<gml:name>Sohlschwelle</gml:name>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter12A">
									<wspmClasses:key>type</wspmClasses:key>
									<wspmClasses:value>point</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter12B">
									<wspmClasses:key>strokeWidth</wspmClasses:key>
									<wspmClasses:value>1</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter12C">
									<wspmClasses:key>fillColor</wspmClasses:key>
									<wspmClasses:value>#FF9600</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter12D">
									<wspmClasses:key>markerWidth</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter12E">
									<wspmClasses:key>strokeColor</wspmClasses:key>
									<wspmClasses:value>#000000</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
							<wspmClasses:parameterMember>
								<wspmClasses:StyleParameter gml:id="StyleParameter12F">
									<wspmClasses:key>markerHeight</wspmClasses:key>
									<wspmClasses:value>8</wspmClasses:value>
								</wspmClasses:StyleParameter>
							</wspmClasses:parameterMember>
						</wspmClasses:Style>
					</wspmClasses:styleMember>
				</wspmClasses:StyleDefinition>
			</wspmClasses:styleDefinition>
		</wspmClasses:WspmClassification>
	</wspmClasses:classificationMember>
</WspmProject>
