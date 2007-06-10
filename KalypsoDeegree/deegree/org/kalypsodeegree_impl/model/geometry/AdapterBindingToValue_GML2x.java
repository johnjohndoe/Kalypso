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
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;

import ogc2.www.opengis.net.gml.AbstractGeometryType;
import ogc2.www.opengis.net.gml.BoxType;
import ogc2.www.opengis.net.gml.CoordType;
import ogc2.www.opengis.net.gml.CoordinatesType;
import ogc2.www.opengis.net.gml.GeometryAssociationType;
import ogc2.www.opengis.net.gml.LineStringType;
import ogc2.www.opengis.net.gml.LinearRingMemberType;
import ogc2.www.opengis.net.gml.LinearRingType;
import ogc2.www.opengis.net.gml.MultiLineStringType;
import ogc2.www.opengis.net.gml.MultiPointType;
import ogc2.www.opengis.net.gml.MultiPolygonType;
import ogc2.www.opengis.net.gml.ObjectFactory;
import ogc2.www.opengis.net.gml.PointType;
import ogc2.www.opengis.net.gml.PolygonType;

import org.kalypso.contribs.ogc2x.KalypsoOGC2xJAXBcontext;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.Adapters;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.kalypsodeegree_impl.model.ct.TransformException;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Node;

/**
 * @author doemming
 */
public class AdapterBindingToValue_GML2x implements AdapterBindingToValue
{
  final static ConvenienceCSFactoryFull m_csFac = new ConvenienceCSFactoryFull();

  final static Adapters m_csAdapter = org.kalypsodeegree_impl.model.cs.Adapters.getDefault();

  final static ObjectFactory gml2Fac = new ObjectFactory();

  final static JAXBContext GML2_JAXCONTEXT = KalypsoOGC2xJAXBcontext.getContext();

  private GM_MultiPoint createGM_MultiPoint( final MultiPointType type, final CS_CoordinateSystem cs ) throws TransformException
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    int i = 0;

    // pointMember
    final List<JAXBElement< ? extends GeometryAssociationType>> pointMember = type.getGeometryMember();
    final GM_Point[] resultPoints = new GM_Point[pointMember.size()];

    for( final JAXBElement< ? extends GeometryAssociationType> element : pointMember )
    {
      final GeometryAssociationType value = element.getValue();
      final JAXBElement< ? extends AbstractGeometryType> geometry = value.getGeometry();
      final AbstractGeometryType abstractgeometryType = geometry.getValue();
      resultPoints[i] = createGM_Point( (PointType) abstractgeometryType, co );
      i++;
    }
    return GeometryFactory.createGM_MultiPoint( resultPoints, co );
  }

  private GM_MultiCurve createGM_MultiLineString( final MultiLineStringType multiLineStringType, final CS_CoordinateSystem cs ) throws GM_Exception, TransformException
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, multiLineStringType );
    final List<JAXBElement< ? extends GeometryAssociationType>> geometryMember = multiLineStringType.getGeometryMember();
    final GM_Curve[] curves = new GM_Curve[geometryMember.size()];
    int i = 0;
    for( final JAXBElement< ? extends GeometryAssociationType> element : geometryMember )
    {
      final GeometryAssociationType value = element.getValue();
      final JAXBElement< ? extends AbstractGeometryType> geometry = value.getGeometry();
      final AbstractGeometryType abstractGeometryType = geometry.getValue();
      curves[i] = createGM_LineString( (LineStringType) abstractGeometryType, co );
      i++;
    }
    return GeometryFactory.createGM_MultiCurve( curves );
  }

  private GM_MultiSurface createGM_MultiSurface( final MultiPolygonType multiPolygonType, final CS_CoordinateSystem cs ) throws GM_Exception, TransformException
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, multiPolygonType );
    final List<JAXBElement< ? extends GeometryAssociationType>> geometryMember = multiPolygonType.getGeometryMember();
    final GM_Surface[] surfaces = new GM_Surface[geometryMember.size()];

    int i = 0;
    for( final JAXBElement< ? extends GeometryAssociationType> element : geometryMember )
    {
      final GeometryAssociationType value = element.getValue();
      final JAXBElement< ? extends AbstractGeometryType> geometry = value.getGeometry();
      final AbstractGeometryType abstractGeometryType = geometry.getValue();
      surfaces[i] = createGM_Surface( (PolygonType) abstractGeometryType, co );
      i++;
    }
    return GeometryFactory.createGM_MultiSurface( surfaces, co );
  }

  private GM_Surface createGM_Surface( final PolygonType polygonType, final CS_CoordinateSystem cs ) throws GM_Exception, TransformException
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, polygonType );
    // outer...
    final JAXBElement< ? extends AbstractGeometryType> outerBoundary = polygonType.getOuterBoundaryIs().getGeometry();
    final AbstractGeometryType outerAbstractGeometryType = outerBoundary.getValue();

    final GM_Position[] exteriorRing = createGM_PositionFrom( (LinearRingType) outerAbstractGeometryType );
    // inner..

    final List<LinearRingMemberType> innerBoundaryIs = polygonType.getInnerBoundaryIs();

    final GM_Position[][] interiorRings = new GM_Position[innerBoundaryIs.size()][];
    int i = 0;
    for( final LinearRingMemberType linearRingMemberType : innerBoundaryIs )
    {
      final JAXBElement< ? extends AbstractGeometryType> geometry = linearRingMemberType.getGeometry();
      final AbstractGeometryType abstractGeometryType = geometry.getValue();
      interiorRings[i] = createGM_PositionFrom( (LinearRingType) abstractGeometryType );
      i++;
    }
    return GeometryFactory.createGM_Surface( exteriorRing, interiorRings, null, co );
  }

  private GM_Position[] createGM_PositionFrom( final LinearRingType linearRingType )
  {
    final CoordinatesType coordinates = linearRingType.getCoordinates();
    if( coordinates != null )
      return createGM_Positions( coordinates );

    final List<CoordType> coord = linearRingType.getCoord();
    return createGM_Positions( coord );
  }

  private GM_Position[] createGM_Positions( final List<CoordType> coords )
  {
    final GM_Position[] positions = new GM_Position[coords.size()];
    int i = 0;
    for( final CoordType coordType : coords )
    {
      positions[i] = createGM_Position( coordType );
      i++;
    }
    return positions;
  }

  private GM_Curve createGM_LineString( final LineStringType type, final CS_CoordinateSystem cs ) throws GM_Exception, TransformException
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    final CoordinatesType coordinates = type.getCoordinates();
    final GM_Position[] positions = createGM_Positions( coordinates );
    return GeometryFactory.createGM_Curve( positions, co );
  }

  private GM_Point createGM_Point( final PointType type, final CS_CoordinateSystem cs ) throws TransformException
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    final CoordType coord = type.getCoord();
    if( coord != null )
    {
      final GM_Position position = createGM_Position( coord );
      return GeometryFactory.createGM_Point( position, co );
    }

    final GM_Position position;

    final CoordType cordTyoe = type.getCoord();
    if( cordTyoe != null )
      position = createGM_Position( cordTyoe );
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

  private GM_Position createGM_Position( final CoordType coord )
  {
    final double x = coord.getX().doubleValue();
    final double y = coord.getX().doubleValue();
    final BigDecimal z = coord.getZ();
    if( z == null )
      return GeometryFactory.createGM_Position( x, y );
    return GeometryFactory.createGM_Position( x, y, z.doubleValue() );
  }

  private GM_Position[] createGM_Positions( final CoordinatesType coordinates )
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

  private CS_CoordinateSystem getCS_CoordinateSystem( final CS_CoordinateSystem defaultCS, final AbstractGeometryType geom ) throws TransformException
  {
    final String srsName = geom.getSrsName();
    if( srsName == null )
      return defaultCS;
    final CoordinateSystem csByName;
    if( srsName.startsWith( "http://www.opengis.net/gml/srs/epsg.xml#" ) )
    {
      final String[] split = srsName.split( "#" );
      csByName = m_csFac.getCSByName( "EPSG:" + split[1] );
    }
    else
      csByName = m_csFac.getCSByName( srsName );
    if( csByName == null )
      throw new TransformException( "The coordinate system: " + srsName + " is not known" );
    return m_csAdapter.export( csByName );
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.IGMLBindingToValueAdapter#wrapFromBinding(java.lang.Object,
   *      java.lang.Class)
   */
  /**
   * @see org.kalypsodeegree_impl.model.geometry.IGMLBindingToValueAdapter#wrapFromBinding(java.lang.Object)
   */
  public Object wrapFromBinding( final Object bindingGeometry, final Class geometryClass ) throws GM_Exception
  {
    if( bindingGeometry == null )
      return null;
    if( bindingGeometry instanceof JAXBElement )
    {
      return wrapFromBinding( ((JAXBElement) bindingGeometry).getValue(), geometryClass );
    }
    if( bindingGeometry instanceof AbstractGeometryType )
    {
      try
      {
        final AbstractGeometryType bindingTypeObject = (AbstractGeometryType) bindingGeometry;
        final CS_CoordinateSystem cs = getCS_CoordinateSystem( null, bindingTypeObject );
        if( bindingTypeObject instanceof PointType )
          return createGM_Point( (PointType) bindingTypeObject, cs );
        if( bindingTypeObject instanceof PolygonType )
        {
          final GM_Surface surface = createGM_Surface( (PolygonType) bindingTypeObject, cs );
          // if multisurface is expected but surface is provided, then we create a multisurface with this surface
          // inside.
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
      catch( final TransformException e )
      {
        throw new GM_Exception( e.getMessage() );
      }
    }
    if( bindingGeometry instanceof BoxType )
    {
      return createGM_Envelope( (BoxType) bindingGeometry );
    }
    throw new UnsupportedOperationException( bindingGeometry.getClass().getName() + " is not supported" );
  }

  private Object createGM_Envelope( final BoxType boxType )
  {
    final GM_Position[] positions;
    final List<CoordType> coords = boxType.getCoord();
    if( coords != null && (!coords.isEmpty()) )
      positions = createGM_Positions( coords );
    else
      positions = createGM_Positions( boxType.getCoordinates() );
    if( positions.length == 2 )
      return GeometryFactory.createGM_Envelope( positions[0], positions[1] );
    throw new UnsupportedOperationException( "invalid bbox" );
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.IGMLBindingToValueAdapter#wrapFromElement(org.w3c.dom.Element)
   */
  public Object wrapFromNode( final Node node ) throws Exception
  {
    final Unmarshaller unmarshaller = GML2_JAXCONTEXT.createUnmarshaller();
    final Object bindingGeometry = unmarshaller.unmarshal( node );
    final Object object = wrapFromBinding( bindingGeometry, null );
    return object;
  }
}
