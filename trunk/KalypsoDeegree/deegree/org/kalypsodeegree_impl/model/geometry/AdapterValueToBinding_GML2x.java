/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.geometry;

import java.rmi.RemoteException;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import ogc2.www.opengis.net.gml.BoxType;
import ogc2.www.opengis.net.gml.CoordinatesType;
import ogc2.www.opengis.net.gml.GeometryAssociationType;
import ogc2.www.opengis.net.gml.LineStringMember;
import ogc2.www.opengis.net.gml.LineStringMemberType;
import ogc2.www.opengis.net.gml.LineStringType;
import ogc2.www.opengis.net.gml.LinearRingMemberType;
import ogc2.www.opengis.net.gml.LinearRingType;
import ogc2.www.opengis.net.gml.MultiLineStringType;
import ogc2.www.opengis.net.gml.MultiPointType;
import ogc2.www.opengis.net.gml.MultiPolygonType;
import ogc2.www.opengis.net.gml.ObjectFactory;
import ogc2.www.opengis.net.gml.PointMember;
import ogc2.www.opengis.net.gml.PointMemberType;
import ogc2.www.opengis.net.gml.PointType;
import ogc2.www.opengis.net.gml.PolygonMember;
import ogc2.www.opengis.net.gml.PolygonMemberType;
import ogc2.www.opengis.net.gml.PolygonType;

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
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author doemming
 */
public class AdapterValueToBinding_GML2x implements AdapterValueToGMLBinding
{
  final static ObjectFactory gml2Fac = new ObjectFactory();

  private static final String COORDINATES_SEPARATOR = " ";

  private static final String DECIMAL_SEPARATOR = ".";

  private static final String TUPPLE_SEPARATOR = ",";

  private String getCSName( final GM_Object geometry, String csNameDefault )
  {
    final CS_CoordinateSystem coordinateSystem = geometry.getCoordinateSystem();
    if( coordinateSystem == null )
      return csNameDefault;
    try
    {
      return coordinateSystem.getName();
    }
    catch( RemoteException e )
    {
      return csNameDefault;
    }
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.IValueToGMLBindingAdapter#wrapToBinding(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public Object wrapToBinding( GM_Object geometry ) throws GM_Exception
  {
    final String csNameDefault = getCSName( geometry, null );
    if( geometry instanceof GM_Point )
      return createPointType( (GM_Point) geometry, csNameDefault );
    if( geometry instanceof GM_Curve )
      return createLineStringType( (GM_Curve) geometry, csNameDefault );
    if( geometry instanceof GM_Surface )
      return createPolygonType( (GM_Surface) geometry, csNameDefault );
    if( geometry instanceof GM_MultiPoint )
      return createMultiPointType( (GM_MultiPoint) geometry, csNameDefault );
    if( geometry instanceof GM_MultiCurve )
      return createMultiLineStringType( (GM_MultiCurve) geometry, csNameDefault );
    if( geometry instanceof GM_MultiSurface )
      return createMultiPolygonType( (GM_MultiSurface) geometry, csNameDefault );

    throw new UnsupportedOperationException( geometry.getClass().getName() + " is not supported" );
  }

  private Object createMultiPolygonType( GM_MultiSurface multiSurface, String csNameDefault )
  {
    final String csName = getCSName( multiSurface, csNameDefault );
    final MultiPolygonType multiPolygonType = gml2Fac.createMultiPolygonType();
    final GM_Surface[] allSurfaces = multiSurface.getAllSurfaces();

    final List<JAXBElement< ? extends GeometryAssociationType>> geometryMember = multiPolygonType.getGeometryMember();

    for( final GM_Surface surface : allSurfaces )
    {
      final PolygonType polygonType = createPolygonType( surface, csName );
      final PolygonMemberType polygonMemberType = gml2Fac.createPolygonMemberType();
      final JAXBElement<PolygonType> polygonElement = gml2Fac.createPolygon( polygonType );
      polygonMemberType.setGeometry( polygonElement );
      final PolygonMember polygonMember = gml2Fac.createPolygonMember( polygonMemberType );
      geometryMember.add( polygonMember );
    }
    multiPolygonType.setSrsName( csName );
    return multiPolygonType;
  }

  private Object createMultiLineStringType( GM_MultiCurve multiCurve, String csNameDefault ) throws GM_Exception
  {
    final String csName = getCSName( multiCurve, csNameDefault );
    final MultiLineStringType multiLineStringType = gml2Fac.createMultiLineStringType();
    final GM_Curve[] allCurves = multiCurve.getAllCurves();

    final List<JAXBElement< ? extends GeometryAssociationType>> geometryMember = multiLineStringType.getGeometryMember();

    for( final GM_Curve curve : allCurves )
    {
      final LineStringType curveType = createLineStringType( curve, csName );
      final LineStringMemberType lineStringMemberType = gml2Fac.createLineStringMemberType();
      final JAXBElement<LineStringType> lineStringElement = gml2Fac.createLineString( curveType );
      lineStringMemberType.setGeometry( lineStringElement );
      final LineStringMember lineStringMember = gml2Fac.createLineStringMember( lineStringMemberType );
      geometryMember.add( lineStringMember );
    }
    multiLineStringType.setSrsName( csName );
    return multiLineStringType;
  }

  private MultiPointType createMultiPointType( GM_MultiPoint multiPoint, String csNameDefault )
  {
    final String csName = getCSName( multiPoint, csNameDefault );
    final MultiPointType multiPointType = gml2Fac.createMultiPointType();
    final GM_Point[] allPoints = multiPoint.getAllPoints();

    final List<JAXBElement< ? extends GeometryAssociationType>> pointList = multiPointType.getGeometryMember();
    for( int i = 0; i < allPoints.length; i++ )
    {
      final GM_Point point = allPoints[i];
      final PointType pointType = createPointType( point, csName );
      final PointMemberType type = gml2Fac.createPointMemberType();
      final JAXBElement<PointType> pointElement = gml2Fac.createPoint( pointType );
      type.setGeometry( pointElement );
      final PointMember pointMember = gml2Fac.createPointMember( type );
      pointList.add( pointMember );
    }
    multiPointType.setSrsName( csName );
    return multiPointType;
  }

  private PolygonType createPolygonType( GM_Surface surface, String csNameDefault )
  {
    final String csName = getCSName( surface, csNameDefault );
    final GM_SurfaceBoundary surfaceBoundary = surface.getSurfaceBoundary();
    final GM_Ring exteriorRing = surfaceBoundary.getExteriorRing();
    final GM_Ring[] interiorRings = surfaceBoundary.getInteriorRings();

    final PolygonType polygonType = gml2Fac.createPolygonType();

    // exterior
    final LinearRingType outerLinearRingType = gml2Fac.createLinearRingType();
    final CoordinatesType outerRingCoordinatesType = createCoordinatesType( exteriorRing.getPositions() );
    outerLinearRingType.setCoordinates( outerRingCoordinatesType );
    final JAXBElement<LinearRingType> outerLinearRing = gml2Fac.createLinearRing( outerLinearRingType );
    final LinearRingMemberType outerLinearRingMemberType = gml2Fac.createLinearRingMemberType();
    outerLinearRingMemberType.setGeometry( outerLinearRing );
    polygonType.setOuterBoundaryIs( outerLinearRingMemberType );

    // interior

    final List<LinearRingMemberType> interiorContainer = polygonType.getInnerBoundaryIs();
    if( interiorRings != null )
    {
      for( int i = 0; i < interiorRings.length; i++ )
      {
        final GM_Ring ring = interiorRings[i];
        final LinearRingType innerLinearRingType = gml2Fac.createLinearRingType();
        final CoordinatesType innerRingCoordinatesType = createCoordinatesType( ring.getPositions() );
        innerLinearRingType.setCoordinates( innerRingCoordinatesType );
        final JAXBElement<LinearRingType> innerLinearRing = gml2Fac.createLinearRing( innerLinearRingType );
        final LinearRingMemberType innerLinearRingMemberType = gml2Fac.createLinearRingMemberType();
        innerLinearRingMemberType.setGeometry( innerLinearRing );
        interiorContainer.add( innerLinearRingMemberType );
      }
    }
    //
    polygonType.setSrsName( csName );
    return polygonType;
  }

  private LineStringType createLineStringType( GM_Curve lineString, String csNameDefault ) throws GM_Exception
  {
    final String csName = getCSName( lineString, csNameDefault );
    final LineStringType lineStringType = gml2Fac.createLineStringType();
    final GM_LineString asLineString = lineString.getAsLineString();
    final GM_Position[] positions = asLineString.getPositions();
    final CoordinatesType coordinatesType = createCoordinatesType( positions );
    lineStringType.setCoordinates( coordinatesType );
    lineStringType.setSrsName( csName );
    return lineStringType;
  }

  private PointType createPointType( GM_Point point, String csNameDefault )
  {
    final PointType pointType = gml2Fac.createPointType();
    final GM_Position position = point.getPosition();
    final CoordinatesType coordinatesType = createCoordinatesType( new GM_Position[] { position } );
    pointType.setSrsName( csNameDefault );
    pointType.setCoordinates( coordinatesType );
    return pointType;
  }

  private CoordinatesType createCoordinatesType( GM_Position[] positions )
  {
    final CoordinatesType coordinatesType = gml2Fac.createCoordinatesType();
    coordinatesType.setCs( COORDINATES_SEPARATOR );
    coordinatesType.setDecimal( DECIMAL_SEPARATOR );
    coordinatesType.setTs( TUPPLE_SEPARATOR );
    final StringBuffer buffer = new StringBuffer();
    for( int i = 0; i < positions.length; i++ )
    {
      if( i > 0 )
        buffer.append( TUPPLE_SEPARATOR );
      final GM_Position position = positions[i];
      final double[] posArray = position.getAsArray();
      final int max = posArray.length;
      for( int j = 0; j < max; j++ )
      {
        if( j > 0 )
          buffer.append( COORDINATES_SEPARATOR );
        buffer.append( posArray[j] );
      }
    }
    coordinatesType.setValue( buffer.toString() );
    return coordinatesType;
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.AdapterValueToGMLBinding#wrapToElement(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public Element wrapToElement( GM_Object geometry ) throws GM_Exception
  {
    final Object bindingGeometry;
    try
    {
      bindingGeometry = wrapToBinding( geometry );
      Marshaller marshaller = AdapterBindingToValue_GML2x.GML2_JAXCONTEXT.createMarshaller();
      final DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance();
      fac.setNamespaceAware( true );
      final DocumentBuilder builder = fac.newDocumentBuilder();
      final Document document = builder.newDocument();
      final JAXBElement jaxbElement = createJAXBGeometryElement( bindingGeometry );
      marshaller.marshal( jaxbElement, document );
      return document.getDocumentElement();
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      throw new GM_Exception( "could not marshall to Element", e );
    }
  }

  private JAXBElement createJAXBGeometryElement( final Object geometry )
  {
    if( geometry instanceof PointType )
      return gml2Fac.createPoint( (PointType) geometry );
    if( geometry instanceof LineStringType )
      return gml2Fac.createLineString( (LineStringType) geometry );
    if( geometry instanceof PolygonType )
      return gml2Fac.createPolygon( (PolygonType) geometry );

    if( geometry instanceof MultiPointType )
      return gml2Fac.createMultiPoint( (MultiPointType) geometry );
    if( geometry instanceof MultiLineStringType )
      return gml2Fac.createMultiLineString( (MultiLineStringType) geometry );
    if( geometry instanceof MultiPolygonType )
      return gml2Fac.createMultiPolygon( (MultiPolygonType) geometry );
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.AdapterValueToGMLBinding#wrapToBinding(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public Object wrapToBinding( GM_Envelope geometry )
  {
    final BoxType boxType = gml2Fac.createBoxType();
    GM_Position[] positions = new GM_Position[] { geometry.getMin(), geometry.getMax() };
    CoordinatesType coordinatesType = createCoordinatesType( positions );
    boxType.setCoordinates( coordinatesType );
    // TODO support srsname when refactored GM_Envelop to have a SRS
    return boxType;
  }
}
