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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import javax.xml.bind.JAXBElement;

import ogc31.www.opengis.net.gml.AbstractGeometryType;
import ogc31.www.opengis.net.gml.AbstractRingPropertyType;
import ogc31.www.opengis.net.gml.AbstractRingType;
import ogc31.www.opengis.net.gml.CoordType;
import ogc31.www.opengis.net.gml.CoordinatesType;
import ogc31.www.opengis.net.gml.DirectPositionType;
import ogc31.www.opengis.net.gml.EnvelopeType;
import ogc31.www.opengis.net.gml.Exterior;
import ogc31.www.opengis.net.gml.Interior;
import ogc31.www.opengis.net.gml.LineStringPropertyType;
import ogc31.www.opengis.net.gml.LineStringType;
import ogc31.www.opengis.net.gml.LinearRingType;
import ogc31.www.opengis.net.gml.MultiLineStringType;
import ogc31.www.opengis.net.gml.MultiPointType;
import ogc31.www.opengis.net.gml.MultiPolygonType;
import ogc31.www.opengis.net.gml.ObjectFactory;
import ogc31.www.opengis.net.gml.PointArrayPropertyType;
import ogc31.www.opengis.net.gml.PointPropertyType;
import ogc31.www.opengis.net.gml.PointType;
import ogc31.www.opengis.net.gml.PolygonPropertyType;
import ogc31.www.opengis.net.gml.PolygonType;

import org.apache.commons.lang.ArrayUtils;
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
import org.kalypsodeegree_impl.model.cs.Adapters;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Element;

/**
 * factory class to wrap from binding geometries to GM_Object geometries and visa versa
 * 
 * @author doemming
 */
public class GML3BindingGM_ObjectAdapter
{
  final static ConvenienceCSFactoryFull m_csFac = new ConvenienceCSFactoryFull();

  final static Adapters m_csAdapter = org.kalypsodeegree_impl.model.cs.Adapters.getDefault();

  final static ObjectFactory gml3Fac = new ObjectFactory();

  private static final String COORDINATES_SEPARATOR = " ";

  private static final String DECIMAL_SEPARATOR = ".";

  private static final String TUPPLE_SEPARATOR = ",";

  final static HashMap<Class, Class> gm_object2BindingHash = new HashMap<Class, Class>();
  static
  {
    gm_object2BindingHash.put( GM_Envelope.class, EnvelopeType.class );
    gm_object2BindingHash.put( GM_Point.class, PointType.class );
    gm_object2BindingHash.put( GM_Curve.class, LineStringType.class );
    gm_object2BindingHash.put( GM_Surface.class, PolygonType.class );
    gm_object2BindingHash.put( GM_MultiPoint.class, MultiPointType.class );
    gm_object2BindingHash.put( GM_MultiCurve.class, MultiLineStringType.class );
    gm_object2BindingHash.put( GM_MultiSurface.class, MultiPolygonType.class );

  }

  private GML3BindingGM_ObjectAdapter( )
  {
    // do not instantiate
  }

  public static GM_Object createGM_Object( Element element )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public static Element createElement( GM_Object geometry )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public static GM_Envelope createGM_Object( final EnvelopeType bindingEnvelope )
  {
    return createGM_Envelope( bindingEnvelope );
  }

  private static GM_Envelope createGM_Envelope( EnvelopeType bindingEnvelope )
  {
    final List<CoordType> coord = bindingEnvelope.getCoord();
    if( coord != null && !coord.isEmpty() )
    {
      final CoordType min = coord.get( 0 );
      final CoordType max = coord.get( 1 );
      final GM_Position minPos = createGM_Position( min );
      final GM_Position maxPos = createGM_Position( max );
      return GeometryFactory.createGM_Envelope( minPos, maxPos );
    }
    final CoordinatesType coordinates = bindingEnvelope.getCoordinates();
    final GM_Position[] positions = createGM_Positions( coordinates );
    return GeometryFactory.createGM_Envelope( positions[0], positions[1] );
  }

  public static Object createGM_Object( final Object bindingObject, final Class geometryClass ) throws GM_Exception
  {
    if( bindingObject == null )
      return null;
    if( bindingObject instanceof JAXBElement )
    {
      return createGM_Object( ((JAXBElement) bindingObject).getValue(), geometryClass );
    }
    if( bindingObject instanceof AbstractGeometryType )
    {
      final AbstractGeometryType bindingTypeObject = (AbstractGeometryType) bindingObject;
      final CS_CoordinateSystem cs = getCS_CoordinateSystem( null, bindingTypeObject );
      if( bindingTypeObject instanceof PointType )
        return createGM_Point( (PointType) bindingTypeObject, cs );
      if( bindingTypeObject instanceof PolygonType )
      {
        final GM_Surface surface = createGM_Surface( (PolygonType) bindingTypeObject, cs );
        if( geometryClass == GeometryUtilities.getMultiPolygonClass() )
        {
          final GM_Surface[] surfaces = new GM_Surface[] { surface };
          return GeometryFactory.createGM_MultiSurface( surfaces, cs );
        }
        return surface;
      }
      if( bindingTypeObject instanceof LineStringType )
        return createGM_LineString( (LineStringType) bindingTypeObject, cs );
      if( bindingTypeObject instanceof MultiPolygonType )
        return createGM_MultiSurface( (MultiPolygonType) bindingTypeObject, cs );
      if( bindingTypeObject instanceof MultiLineStringType )
        return createGM_MultiLineString( (MultiLineStringType) bindingTypeObject, cs );
      if( bindingTypeObject instanceof MultiPointType )
        return createGM_MultiPoint( (MultiPointType) bindingTypeObject, cs );
    }
    if( bindingObject instanceof EnvelopeType )
    {
      return createGM_Envelope( (EnvelopeType) bindingObject );
    }

    throw new UnsupportedOperationException( bindingObject.getClass().getName() + " is not supported" );
  }

  private static GM_MultiPoint createGM_MultiPoint( MultiPointType type, CS_CoordinateSystem cs )
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    int i = 0;
    int size = 0;

    // pointMember
    final List<PointPropertyType> pointMember = type.getPointMember();
    if( pointMember != null )
      size += pointMember.size();

    // pointMembers
    List<PointType> pointList = null;
    final PointArrayPropertyType pointMembers = type.getPointMembers();
    if( pointMembers != null )
    {
      pointList = pointMembers.getPoint();
      if( pointList != null )
        size += pointList.size();
    }

    final GM_Point[] resultPoints = new GM_Point[size];

    // pointMember
    final Iterator<PointPropertyType> iterator = pointMember.iterator();
    while( iterator.hasNext() )
    {
      final PointPropertyType pointPropType = iterator.next();
      final PointType pointType = pointPropType.getPoint();
      resultPoints[i] = createGM_Point( pointType, co );
      i++;
    }

    // pointMembers
    if( pointList != null )
    {
      final Iterator<PointType> iterator2 = pointList.iterator();
      while( iterator2.hasNext() )
      {
        final PointType pointType = iterator2.next();
        resultPoints[i] = createGM_Point( pointType, co );
        i++;
      }
    }
    return GeometryFactory.createGM_MultiPoint( resultPoints, co );
  }

  private static GM_MultiCurve createGM_MultiLineString( MultiLineStringType type, CS_CoordinateSystem cs ) throws GM_Exception
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    final List<LineStringPropertyType> lineStringMember = type.getLineStringMember();
    final GM_Curve[] curves = new GM_Curve[lineStringMember.size()];
    final Iterator<LineStringPropertyType> iterator = lineStringMember.iterator();
    int i = 0;
    while( iterator.hasNext() )
    {
      final LineStringPropertyType lineStringPropType = iterator.next();
      final LineStringType lineString = lineStringPropType.getLineString();
      curves[i] = createGM_LineString( lineString, co );
      i++;
    }
    return GeometryFactory.createGM_MultiCurve( curves );
  }

  private static GM_MultiSurface createGM_MultiSurface( MultiPolygonType type, CS_CoordinateSystem cs ) throws GM_Exception
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    final List<PolygonPropertyType> polygonMember = type.getPolygonMember();
    final GM_Surface[] surfaces = new GM_Surface[polygonMember.size()];
    final Iterator<PolygonPropertyType> iterator = polygonMember.iterator();
    int i = 0;
    while( iterator.hasNext() )
    {
      final PolygonPropertyType polyPropType = iterator.next();
      final PolygonType polyType = polyPropType.getPolygon();
      surfaces[i] = createGM_Surface( polyType, co );
      i++;
    }
    return GeometryFactory.createGM_MultiSurface( surfaces ,co);
  }

  private static GM_Surface createGM_Surface( PolygonType type, CS_CoordinateSystem cs ) throws GM_Exception
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    final AbstractRingPropertyType ringType = type.getExterior().getValue();

    final JAXBElement< ? extends AbstractRingType> ring = ringType.getRing();
    final AbstractRingType abstractLinearRing = ring.getValue();
    final GM_Position[] exteriorRing = createGM_PositionFrom( abstractLinearRing );
    final List<JAXBElement<AbstractRingPropertyType>> interior = type.getInterior();
    final Iterator<JAXBElement<AbstractRingPropertyType>> iterator = interior.iterator();
    final List<GM_Position[]> interiorList = new ArrayList<GM_Position[]>();
    while( iterator.hasNext() )
    {
      final AbstractRingPropertyType abstractRingPropertyType = iterator.next().getValue();
      final GM_Position[] positions = createGM_PositionFrom( abstractRingPropertyType.getRing().getValue() );
      interiorList.add( positions );
    }
    final GM_Position[][] interiorRings = interiorList.toArray( new GM_Position[interiorList.size()][] );
    return GeometryFactory.createGM_Surface( exteriorRing, interiorRings, null, co );
  }

  private static GM_Position[] createGM_PositionFrom( AbstractRingType abstractRingType )
  {
    if( abstractRingType instanceof LinearRingType )
    {
      final LinearRingType linearRingType = (LinearRingType) abstractRingType;
      final CoordinatesType coordinates = linearRingType.getCoordinates();
      if( coordinates != null )
        return createGM_Positions( coordinates );
      else
        throw new UnsupportedOperationException();
    }
    else
      throw new UnsupportedOperationException();
  }

  private static GM_Curve createGM_LineString( LineStringType type, CS_CoordinateSystem cs ) throws GM_Exception
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    final CoordinatesType coordinates = type.getCoordinates();
    final GM_Position[] positions = createGM_Positions( coordinates );
    return GeometryFactory.createGM_Curve( positions, co );
  }

  private static GM_Point createGM_Point( PointType type, CS_CoordinateSystem cs )
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    final CoordType coord = type.getCoord();
    if( coord != null )
    {
      final GM_Position position = createGM_Position( coord );
      return GeometryFactory.createGM_Point( position, co );
    }

    final GM_Position position;

    final DirectPositionType pos = type.getPos();
    if( pos != null )
      position = createGM_Position( pos );
    else
    {
      final CoordinatesType coordinates = type.getCoordinates();
      if( coordinates == null )
        throw new UnsupportedOperationException( "Either pos or coordinates must be set." );
      final GM_Position[] positions = createGM_Positions( coordinates );
      position = positions[0];
    }

    return GeometryFactory.createGM_Point( position, co );
  }

  private static GM_Position createGM_Position( CoordType coord )
  {
    final double x = coord.getX().doubleValue();
    final double y = coord.getX().doubleValue();
    final BigDecimal z = coord.getZ();
    if( z == null )
      return GeometryFactory.createGM_Position( x, y );
    return GeometryFactory.createGM_Position( x, y, z.doubleValue() );
  }

  private static GM_Position createGM_Position( final DirectPositionType positionType )
  {
    final BigInteger srsDimension = positionType.getSrsDimension();
    final List<Double> crdsList = positionType.getValue();

    if( srsDimension != null && crdsList.size() != srsDimension.intValue() )
      throw new IllegalArgumentException( "Wrong count of coordinates: " + crdsList.size() );

    final double[] crds = ArrayUtils.toPrimitive( crdsList.toArray( new Double[crdsList.size()] ) );

    return GeometryFactory.createGM_Position( crds );
  }

  private static GM_Position[] createGM_Positions( final CoordinatesType coordinates )
  {
    final String coordinateSepearator = coordinates.getCs();
    final String tuppleSeparator = coordinates.getTs();
    final String decimal = coordinates.getDecimal();
    final String value = coordinates.getValue();
    if( !".".equals( decimal ) )
      throw new UnsupportedOperationException(); // TODO
    final List<GM_Position> result = new ArrayList<GM_Position>();
    final StringTokenizer tuppleTokenizer = new StringTokenizer( value, tuppleSeparator, false );
    while( tuppleTokenizer.hasMoreTokens() )
    {
      final String tupple = tuppleTokenizer.nextToken();
      final StringTokenizer coordinatesTokenizer = new StringTokenizer( tupple, coordinateSepearator, false );
      final double pos[] = new double[coordinatesTokenizer.countTokens()];
      int i = 0;
      while( coordinatesTokenizer.hasMoreTokens() )
      {
        final String coordinate = coordinatesTokenizer.nextToken();
        pos[i] = Double.parseDouble( coordinate );
        i++;
      }
      result.add( GeometryFactory.createGM_Position( pos ) );
    }
    return result.toArray( new GM_Position[result.size()] );
  }

  public static CS_CoordinateSystem getCS_CoordinateSystem( CS_CoordinateSystem defaultCS, AbstractGeometryType geom )
  {
    final String srsName = geom.getSrsName();
    if( srsName == null )
      return defaultCS;
    final CoordinateSystem csByName = m_csFac.getCSByName( srsName );
    return m_csAdapter.export( csByName );
  }

  private static String getCSName( final GM_Object geometry, String csNameDefault )
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

  public static EnvelopeType createBindingGeometryType( final GM_Envelope envelope )
  {
    final GM_Position min = envelope.getMin();
    final GM_Position max = envelope.getMax();
    final EnvelopeType envelopeType = gml3Fac.createEnvelopeType();
    final CoordinatesType coordinatesType = createCoordinatesType( new GM_Position[] { min, max } );
    envelopeType.setCoordinates( coordinatesType );
    return envelopeType;
  }

  public static AbstractGeometryType createBindingGeometryType( final GM_Object geometry ) throws GM_Exception
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

  private static MultiPolygonType createMultiPolygonType( GM_MultiSurface multiSurface, String csNameDefault )
  {
    final String csName = getCSName( multiSurface, csNameDefault );
    final MultiPolygonType multiPolygonType = gml3Fac.createMultiPolygonType();
    final List<PolygonPropertyType> polygonMember = multiPolygonType.getPolygonMember();
    final GM_Surface[] allSurfaces = multiSurface.getAllSurfaces();
    for( int i = 0; i < allSurfaces.length; i++ )
    {
      final GM_Surface surface = allSurfaces[i];
      final PolygonPropertyType polygonPropertyType = gml3Fac.createPolygonPropertyType();
      final PolygonType polygonType = createPolygonType( surface, csName );
      polygonPropertyType.setPolygon( polygonType );
      polygonMember.add( polygonPropertyType );
    }
    multiPolygonType.setSrsName( csName );
    return multiPolygonType;
  }

  private static MultiLineStringType createMultiLineStringType( GM_MultiCurve multiCurve, String csNameDefault ) throws GM_Exception
  {
    final String csName = getCSName( multiCurve, csNameDefault );
    final MultiLineStringType multiLineStringType = gml3Fac.createMultiLineStringType();
    final List<LineStringPropertyType> lineStringMember = multiLineStringType.getLineStringMember();
    final GM_Curve[] allCurves = multiCurve.getAllCurves();
    for( int i = 0; i < allCurves.length; i++ )
    {
      final GM_Curve curve = allCurves[i];
      final LineStringPropertyType lineStringPropertyType = gml3Fac.createLineStringPropertyType();
      final LineStringType lineStringType = createLineStringType( curve, csName );
      lineStringPropertyType.setLineString( lineStringType );
      lineStringMember.add( lineStringPropertyType );
    }
    return multiLineStringType;
  }

  private static MultiPointType createMultiPointType( GM_MultiPoint multiPoint, String csNameDefault )
  {
    final String csName = getCSName( multiPoint, csNameDefault );
    final MultiPointType multiPointType = gml3Fac.createMultiPointType();
    final GM_Point[] allPoints = multiPoint.getAllPoints();

    final List<PointType> pointList = multiPointType.getPointMembers().getPoint();
    for( int i = 0; i < allPoints.length; i++ )
    {
      final GM_Point point = allPoints[i];
      final PointType pointType = createPointType( point, csName );
      pointList.add( pointType );
    }
    multiPointType.setSrsName( csName );
    return multiPointType;
  }

  private static PolygonType createPolygonType( GM_Surface surface, String csNameDefault )
  {
    final String csName = getCSName( surface, csNameDefault );
    final PolygonType polygonType = gml3Fac.createPolygonType();
    final GM_SurfaceBoundary surfaceBoundary = surface.getSurfaceBoundary();
    final GM_Ring exteriorRing = surfaceBoundary.getExteriorRing();
    final GM_Ring[] interiorRings = surfaceBoundary.getInteriorRings();

    // exterior
    final LinearRingType linearRingType = gml3Fac.createLinearRingType();

    final CoordinatesType coordinatesType = createCoordinatesType( exteriorRing.getPositions() );
    linearRingType.setCoordinates( coordinatesType );
    final JAXBElement<LinearRingType> linearRing = gml3Fac.createLinearRing( linearRingType );

    final AbstractRingPropertyType abstractRingPropertyType = gml3Fac.createAbstractRingPropertyType();
    abstractRingPropertyType.setRing( linearRing );

    final Exterior exterior = gml3Fac.createExterior( abstractRingPropertyType );

    polygonType.setExterior( exterior );
    // interior

    final List<JAXBElement<AbstractRingPropertyType>> interiorContainer = polygonType.getInterior();
    if( interiorRings != null )
    {
      for( int i = 0; i < interiorRings.length; i++ )
      {
        final GM_Ring ring = interiorRings[i];
        final LinearRingType interiorLinearRingType = gml3Fac.createLinearRingType();

        final CoordinatesType interiorCoordinatesType = createCoordinatesType( ring.getPositions() );
        interiorLinearRingType.setCoordinates( interiorCoordinatesType );
        final JAXBElement<LinearRingType> interiorLinearRing = gml3Fac.createLinearRing( interiorLinearRingType );

        final AbstractRingPropertyType interiorAbstractRingPropertyType = gml3Fac.createAbstractRingPropertyType();
        interiorAbstractRingPropertyType.setRing( interiorLinearRing );

        final Interior interior = gml3Fac.createInterior( interiorAbstractRingPropertyType );
        interiorContainer.add( interior );
      }
    }
    //
    polygonType.setSrsName( csName );
    return polygonType;
  }

  private static LineStringType createLineStringType( GM_Curve lineString, String csNameDefault ) throws GM_Exception
  {
    final String csName = getCSName( lineString, csNameDefault );
    final LineStringType lineStringType = gml3Fac.createLineStringType();
    final GM_LineString asLineString = lineString.getAsLineString();
    final GM_Position[] positions = asLineString.getPositions();
    final CoordinatesType coordinatesType = createCoordinatesType( positions );
    lineStringType.setCoordinates( coordinatesType );
    lineStringType.setSrsName( csName );
    return lineStringType;
  }

  private static PointType createPointType( GM_Point point, String csNameDefault )
  {
    final PointType pointType = gml3Fac.createPointType();
    final GM_Position position = point.getPosition();
    final CoordinatesType coordinatesType = createCoordinatesType( new GM_Position[] { position } );
    pointType.setSrsName( csNameDefault );
    pointType.setCoordinates( coordinatesType );
    return pointType;
  }

  private static CoordinatesType createCoordinatesType( GM_Position[] positions )
  {
    final CoordinatesType coordinatesType = gml3Fac.createCoordinatesType();
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

  public static Class getBindingClassFor( Class gm_objectClass )
  {
    return gm_object2BindingHash.get( gm_objectClass );
  }

}
