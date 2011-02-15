<?xml version="1.0" encoding="UTF-8"?>
<FeatureTypeStyle xmlns="http://www.opengis.net/sld"
	xmlns:ogc="http://www.opengis.net/ogc" xmlns:sldExt='http://www.opengis.net/sldExt'
	xmlns:gml='http://www.opengis.net/gml' xmlns:xlink='http://www.w3.org/1999/xlink'
	xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:wspmcommon="org.kalypso.model.wspmcommon">
	<Name>wspFts</Name>
	<Rule>
		<Name>wspRule</Name>
		<Title>Wasserspiegel</Title>
		<MinScaleDenominator>0.0</MinScaleDenominator>
		<MaxScaleDenominator>9.0E99</MaxScaleDenominator>
		<SurfacePolygonSymbolizer xmlns:sldExt="http://www.opengis.net/sldExt"
			uom="pixel">
			<Geometry>
				<ogc:PropertyName>wspmcommon:triangulatedSurfaceMember</ogc:PropertyName>
			</Geometry>
			<sldExt:PolygonColorMap>
			</sldExt:PolygonColorMap>
		</SurfacePolygonSymbolizer>
	</Rule>
</FeatureTypeStyle>
	