/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.geometry;

import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import ogc2.www.opengis.net.gml.BoxType;
import ogc2.www.opengis.net.gml.CoordinatesType;
import ogc2.www.opengis.net.gml.GeometryAssociationType;
import ogc2.www.opengis.net.gml.LineStringMemberType;
import ogc2.www.opengis.net.gml.LineStringType;
import ogc2.www.opengis.net.gml.LinearRingMemberType;
import ogc2.www.opengis.net.gml.LinearRingType;
import ogc2.www.opengis.net.gml.MultiLineStringType;
import ogc2.www.opengis.net.gml.MultiPointType;
import ogc2.www.opengis.net.gml.MultiPolygonType;
import ogc2.www.opengis.net.gml.ObjectFactory;
import ogc2.www.opengis.net.gml.PointMemberType;
import ogc2.www.opengis.net.gml.PointType;
import ogc2.www.opengis.net.gml.PolygonMemberType;
import ogc2.www.opengis.net.gml.PolygonType;

import org.kalypso.contribs.ogc2x.KalypsoOGC2xJAXBcontext;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceBoundary;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author doemming
 */
public class AdapterValueToBinding_GML2x implements AdapterValueToGMLBinding
{
  public final static ObjectFactory OF_GML2 = new ObjectFactory();

  private static final String COORDINATES_SEPARATOR = " ";

  private static final String DECIMAL_SEPARATOR = ".";

  private static final String TUPPLE_SEPARATOR = ",";

  private String getCSName( final GM_Object geometry, final String csNameDefault )
  {
    final String coordinateSystem = geometry.getCoordinateSystem();
    if( coordinateSystem == null )
      return csNameDefault;

    return coordinateSystem;
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.IValueToGMLBindingAdapter#wrapToBinding(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public Object wrapToBinding( final GM_Object geometry ) throws GM_Exception
  {
    final String csNameDefault = getCSName( geometry, null );
    if( geometry instanceof GM_Point )
      return createPointType( (GM_Point) geometry, csNameDefault );
    if( geometry instanceof GM_Curve )
      return createLineStringType( (GM_Curve) geometry, csNameDefault );
    if( geometry instanceof GM_Surface )
      return createPolygonType( (GM_Surface< ? >) geometry, csNameDefault );
    if( geometry instanceof GM_MultiPoint )
      return createMultiPointType( (GM_MultiPoint) geometry, csNameDefault );
    if( geometry instanceof GM_MultiCurve )
      return createMultiLineStringType( (GM_MultiCurve) geometry, csNameDefault );
    if( geometry instanceof GM_MultiSurface )
      return createMultiPolygonType( (GM_MultiSurface) geometry, csNameDefault );

    throw new UnsupportedOperationException( geometry.getClass().getName() + " is not supported" );
  }

  private MultiPolygonType createMultiPolygonType( final GM_MultiSurface multiSurface, final String csNameDefault )
  {
    final String csName = getCSName( multiSurface, csNameDefault );
    final MultiPolygonType multiPolygonType = AdapterValueToBinding_GML2x.OF_GML2.createMultiPolygonType();
    final GM_Surface< ? >[] allSurfaces = multiSurface.getAllSurfaces();

    final List<JAXBElement< ? extends GeometryAssociationType>> geometryMember = multiPolygonType.getGeometryMember();

    for( final GM_Surface< ? > surface : allSurfaces )
    {
      final PolygonType polygonType = createPolygonType( surface, csName );
      final PolygonMemberType polygonMemberType = AdapterValueToBinding_GML2x.OF_GML2.createPolygonMemberType();
      final JAXBElement<PolygonType> polygonElement = AdapterValueToBinding_GML2x.OF_GML2.createPolygon( polygonType );
      polygonMemberType.setGeometry( polygonElement );
      final JAXBElement<PolygonMemberType> polygonMember = AdapterValueToBinding_GML2x.OF_GML2.createPolygonMember( polygonMemberType );
      geometryMember.add( polygonMember );
    }
    multiPolygonType.setSrsName( csName );
    return multiPolygonType;
  }

  private MultiLineStringType createMultiLineStringType( final GM_MultiCurve multiCurve, final String csNameDefault ) throws GM_Exception
  {
    final String csName = getCSName( multiCurve, csNameDefault );
    final MultiLineStringType multiLineStringType = AdapterValueToBinding_GML2x.OF_GML2.createMultiLineStringType();
    final GM_Curve[] allCurves = multiCurve.getAllCurves();

    final List<JAXBElement< ? extends GeometryAssociationType>> geometryMember = multiLineStringType.getGeometryMember();

    for( final GM_Curve curve : allCurves )
    {
      final LineStringType curveType = createLineStringType( curve, csName );
      final LineStringMemberType lineStringMemberType = AdapterValueToBinding_GML2x.OF_GML2.createLineStringMemberType();
      final JAXBElement<LineStringType> lineStringElement = AdapterValueToBinding_GML2x.OF_GML2.createLineString( curveType );
      lineStringMemberType.setGeometry( lineStringElement );
      final JAXBElement<LineStringMemberType> lineStringMember = AdapterValueToBinding_GML2x.OF_GML2.createLineStringMember( lineStringMemberType );
      geometryMember.add( lineStringMember );
    }
    multiLineStringType.setSrsName( csName );
    return multiLineStringType;
  }

  private MultiPointType createMultiPointType( final GM_MultiPoint multiPoint, final String csNameDefault )
  {
    final String csName = getCSName( multiPoint, csNameDefault );
    final MultiPointType multiPointType = AdapterValueToBinding_GML2x.OF_GML2.createMultiPointType();
    final GM_Point[] allPoints = multiPoint.getAllPoints();

    final List<JAXBElement< ? extends GeometryAssociationType>> pointList = multiPointType.getGeometryMember();
    for( final GM_Point point : allPoints )
    {
      final PointType pointType = createPointType( point, csName );
      final PointMemberType type = AdapterValueToBinding_GML2x.OF_GML2.createPointMemberType();
      final JAXBElement<PointType> pointElement = AdapterValueToBinding_GML2x.OF_GML2.createPoint( pointType );
      type.setGeometry( pointElement );
      final JAXBElement<PointMemberType> pointMember = AdapterValueToBinding_GML2x.OF_GML2.createPointMember( type );
      pointList.add( pointMember );
    }
    multiPointType.setSrsName( csName );
    return multiPointType;
  }

  private PolygonType createPolygonType( final GM_Surface< ? > surface, final String csNameDefault )
  {
    final String csName = getCSName( surface, csNameDefault );
    final GM_SurfaceBoundary surfaceBoundary = surface.getSurfaceBoundary();
    final GM_Ring exteriorRing = surfaceBoundary.getExteriorRing();
    final GM_Ring[] interiorRings = surfaceBoundary.getInteriorRings();

    final PolygonType polygonType = AdapterValueToBinding_GML2x.OF_GML2.createPolygonType();

    // exterior
    final LinearRingType outerLinearRingType = AdapterValueToBinding_GML2x.OF_GML2.createLinearRingType();
    final CoordinatesType outerRingCoordinatesType = createCoordinatesType( exteriorRing.getPositions() );
    outerLinearRingType.setCoordinates( outerRingCoordinatesType );
    final JAXBElement<LinearRingType> outerLinearRing = AdapterValueToBinding_GML2x.OF_GML2.createLinearRing( outerLinearRingType );
    final LinearRingMemberType outerLinearRingMemberType = AdapterValueToBinding_GML2x.OF_GML2.createLinearRingMemberType();
    outerLinearRingMemberType.setGeometry( outerLinearRing );
    polygonType.setOuterBoundaryIs( outerLinearRingMemberType );

    // interior

    final List<LinearRingMemberType> interiorContainer = polygonType.getInnerBoundaryIs();
    if( interiorRings != null )
      for( final GM_Ring ring : interiorRings )
      {
        final LinearRingType innerLinearRingType = AdapterValueToBinding_GML2x.OF_GML2.createLinearRingType();
        final CoordinatesType innerRingCoordinatesType = createCoordinatesType( ring.getPositions() );
        innerLinearRingType.setCoordinates( innerRingCoordinatesType );
        final JAXBElement<LinearRingType> innerLinearRing = AdapterValueToBinding_GML2x.OF_GML2.createLinearRing( innerLinearRingType );
        final LinearRingMemberType innerLinearRingMemberType = AdapterValueToBinding_GML2x.OF_GML2.createLinearRingMemberType();
        innerLinearRingMemberType.setGeometry( innerLinearRing );
        interiorContainer.add( innerLinearRingMemberType );
      }
    //
    polygonType.setSrsName( csName );
    return polygonType;
  }

  private LineStringType createLineStringType( final GM_Curve lineString, final String csNameDefault ) throws GM_Exception
  {
    final String csName = getCSName( lineString, csNameDefault );
    final LineStringType lineStringType = AdapterValueToBinding_GML2x.OF_GML2.createLineStringType();
    final GM_LineString asLineString = lineString.getAsLineString();
    final GM_Position[] positions = asLineString.getPositions();
    final CoordinatesType coordinatesType = createCoordinatesType( positions );
    lineStringType.setCoordinates( coordinatesType );
    lineStringType.setSrsName( csName );
    return lineStringType;
  }

  private PointType createPointType( final GM_Point point, final String csNameDefault )
  {
    final PointType pointType = AdapterValueToBinding_GML2x.OF_GML2.createPointType();
    final GM_Position position = point.getPosition();
    final CoordinatesType coordinatesType = createCoordinatesType( new GM_Position[] { position } );
    pointType.setSrsName( csNameDefault );
    pointType.setCoordinates( coordinatesType );
    return pointType;
  }

  private CoordinatesType createCoordinatesType( final GM_Position[] positions )
  {
    final CoordinatesType coordinatesType = AdapterValueToBinding_GML2x.OF_GML2.createCoordinatesType();
    coordinatesType.setCs( AdapterValueToBinding_GML2x.COORDINATES_SEPARATOR );
    coordinatesType.setDecimal( AdapterValueToBinding_GML2x.DECIMAL_SEPARATOR );
    coordinatesType.setTs( AdapterValueToBinding_GML2x.TUPPLE_SEPARATOR );
    final StringBuffer buffer = new StringBuffer();
    for( int i = 0; i < positions.length; i++ )
    {
      if( i > 0 )
        buffer.append( AdapterValueToBinding_GML2x.TUPPLE_SEPARATOR );
      final GM_Position position = positions[i];
      final double[] posArray = position.getAsArray();
      final int max = posArray.length;
      for( int j = 0; j < max; j++ )
      {
        if( j > 0 )
          buffer.append( AdapterValueToBinding_GML2x.COORDINATES_SEPARATOR );
        buffer.append( posArray[j] );
      }
    }
    coordinatesType.setValue( buffer.toString() );
    return coordinatesType;
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.AdapterValueToGMLBinding#wrapToElement(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public Element wrapToElement( final GM_Object geometry ) throws GM_Exception
  {
    try
    {
      final Object bindingGeometry = wrapToBinding( geometry );
      final Marshaller marshaller = KalypsoOGC2xJAXBcontext.getContext().createMarshaller();
      final DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance();
      fac.setNamespaceAware( true );
      final DocumentBuilder builder = fac.newDocumentBuilder();
      final Document document = builder.newDocument();
      final JAXBElement< ? extends Object> jaxbElement = createJAXBGeometryElement( bindingGeometry );
      marshaller.marshal( jaxbElement, document );
      return document.getDocumentElement();
    }
    catch( final Exception e )
    {
      throw new GM_Exception( "could not marshall to Element", e );
    }
  }

  public JAXBElement< ? extends Object> createJAXBGeometryElement( final Object geometry )
  {
    if( geometry instanceof PointType )
      return OF_GML2.createPoint( (PointType) geometry );
    if( geometry instanceof LineStringType )
      return OF_GML2.createLineString( (LineStringType) geometry );
    if( geometry instanceof PolygonType )
      return OF_GML2.createPolygon( (PolygonType) geometry );
    if( geometry instanceof MultiPointType )
      return OF_GML2.createMultiPoint( (MultiPointType) geometry );
    if( geometry instanceof MultiLineStringType )
      return OF_GML2.createMultiLineString( (MultiLineStringType) geometry );
    if( geometry instanceof MultiPolygonType )
      return OF_GML2.createMultiPolygon( (MultiPolygonType) geometry );

    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.AdapterValueToGMLBinding#wrapToBinding(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public Object wrapToBinding( final GM_Envelope envelope )
  {
    final BoxType boxType = AdapterValueToBinding_GML2x.OF_GML2.createBoxType();
    final GM_Position[] positions = new GM_Position[] { envelope.getMin(), envelope.getMax() };
    final CoordinatesType coordinatesType = createCoordinatesType( positions );
    boxType.setCoordinates( coordinatesType );
    boxType.setSrsName( envelope.getCoordinateSystem() );
    return boxType;
  }
}
