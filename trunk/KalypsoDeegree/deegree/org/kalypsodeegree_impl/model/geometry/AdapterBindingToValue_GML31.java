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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;

import ogc31.www.opengis.net.gml.AbstractGeometryType;
import ogc31.www.opengis.net.gml.AbstractRingPropertyType;
import ogc31.www.opengis.net.gml.AbstractRingType;
import ogc31.www.opengis.net.gml.AbstractSurfacePatchType;
import ogc31.www.opengis.net.gml.CoordType;
import ogc31.www.opengis.net.gml.CoordinatesType;
import ogc31.www.opengis.net.gml.DirectPositionListType;
import ogc31.www.opengis.net.gml.DirectPositionType;
import ogc31.www.opengis.net.gml.EnvelopeType;
import ogc31.www.opengis.net.gml.LineStringPropertyType;
import ogc31.www.opengis.net.gml.LineStringType;
import ogc31.www.opengis.net.gml.LinearRingType;
import ogc31.www.opengis.net.gml.MultiLineStringType;
import ogc31.www.opengis.net.gml.MultiPointType;
import ogc31.www.opengis.net.gml.MultiPolygonType;
import ogc31.www.opengis.net.gml.PointArrayPropertyType;
import ogc31.www.opengis.net.gml.PointPropertyType;
import ogc31.www.opengis.net.gml.PointType;
import ogc31.www.opengis.net.gml.PolygonPatchType;
import ogc31.www.opengis.net.gml.PolygonPropertyType;
import ogc31.www.opengis.net.gml.PolygonType;
import ogc31.www.opengis.net.gml.SurfacePatchArrayPropertyType;
import ogc31.www.opengis.net.gml.SurfaceType;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.NotImplementedException;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.w3c.dom.Node;

/**
 * factory class to wrap from binding geometries to GM_Object geometries and visa versa
 * 
 * @author doemming
 */
public class AdapterBindingToValue_GML31 implements AdapterBindingToValue
{
  public AdapterBindingToValue_GML31( )
  {
    // do not instantiate
  }

  private GM_Envelope createGM_Envelope( final EnvelopeType bindingEnvelope )
  {
    final List<CoordType> coord = bindingEnvelope.getCoord();
    final String bindingSrsName = bindingEnvelope.getSrsName();
    // REMARK: backwards compablity: use kalypso-srs if no srs was found
    final String srsName = bindingSrsName == null ? KalypsoDeegreePlugin.getDefault().getCoordinateSystem() : bindingSrsName;

    if( coord != null && !coord.isEmpty() )
    {
      final CoordType min = coord.get( 0 );
      final CoordType max = coord.get( 1 );
      final GM_Position minPos = createGM_Position( min );
      final GM_Position maxPos = createGM_Position( max );
      return GeometryFactory.createGM_Envelope( minPos, maxPos, srsName );
    }
    final CoordinatesType coordinates = bindingEnvelope.getCoordinates();
    if( coordinates != null )
    {

      final GM_Position[] positions = createGM_Positions( coordinates );
      return GeometryFactory.createGM_Envelope( positions[0], positions[1], srsName );
    }

    // TODO coordinates

    final DirectPositionType lowerCorner = bindingEnvelope.getLowerCorner();
    final DirectPositionType upperCorner = bindingEnvelope.getUpperCorner();
    if( lowerCorner != null && upperCorner != null )
    {
      final List<Double> min = lowerCorner.getValue();
      final List<Double> max = upperCorner.getValue();
      final GM_Position minPos = GeometryFactory.createGM_Position( min.get( 0 ), min.get( 1 ) );
      final GM_Position maxPos = GeometryFactory.createGM_Position( max.get( 0 ), max.get( 1 ) );
      return GeometryFactory.createGM_Envelope( minPos, maxPos, srsName );
    }

    final List<DirectPositionType> pos = bindingEnvelope.getPos();
    if( pos != null && !pos.isEmpty() )
    {
      final List<Double> min = pos.get( 0 ).getValue();
      final List<Double> max = pos.get( 1 ).getValue();
      final GM_Position minPos = GeometryFactory.createGM_Position( min.get( 0 ), min.get( 1 ) );
      final GM_Position maxPos = GeometryFactory.createGM_Position( max.get( 0 ), max.get( 1 ) );
      return GeometryFactory.createGM_Envelope( minPos, maxPos, srsName );
    }

    throw new UnsupportedOperationException();
  }

  private GM_MultiPoint createGM_MultiPoint( final MultiPointType type, final String cs )
  {
    final String co = getCS_CoordinateSystem( cs, type );
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

  private GM_MultiCurve createGM_MultiLineString( final MultiLineStringType type, final String cs ) throws GM_Exception
  {
    final String co = getCS_CoordinateSystem( cs, type );
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

  private GM_MultiSurface createGM_MultiSurface( final MultiPolygonType type, final String cs ) throws GM_Exception
  {
    final String co = getCS_CoordinateSystem( cs, type );
    final List<PolygonPropertyType> polygonMember = type.getPolygonMember();
    final GM_Surface<GM_SurfacePatch>[] surfaces = new GM_Surface[polygonMember.size()];
    final Iterator<PolygonPropertyType> iterator = polygonMember.iterator();
    int i = 0;
    while( iterator.hasNext() )
    {
      final PolygonPropertyType polyPropType = iterator.next();
      final PolygonType polyType = polyPropType.getPolygon();
      surfaces[i] = createGM_Surface( polyType, co );
      i++;
    }
    return GeometryFactory.createGM_MultiSurface( surfaces, co );
  }

  private GM_Surface<GM_SurfacePatch> createGM_Surface( final PolygonType type, final String cs ) throws GM_Exception
  {
    final String co = getCS_CoordinateSystem( cs, type );
    final AbstractRingPropertyType ringType = type.getExterior().getValue();

    final JAXBElement< ? extends AbstractRingType> ring = ringType.getRing();
    final AbstractRingType abstractLinearRing = ring.getValue();

    final List<JAXBElement<AbstractRingPropertyType>> interior = type.getInterior();

    return createSurface( abstractLinearRing, interior, co );
  }

  private GM_Surface<GM_SurfacePatch> createGM_Surface( final SurfaceType type, final String cs ) throws GM_Exception
  {
    final String co = getCS_CoordinateSystem( cs, type );

    final SurfacePatchArrayPropertyType surfacePatchArrayPropertyType = type.getPatches().getValue();
    final List<JAXBElement< ? extends AbstractSurfacePatchType>> patches = surfacePatchArrayPropertyType.getSurfacePatch();
    if( patches.size() > 1 )
      throw new NotImplementedException();

    for( final JAXBElement< ? extends AbstractSurfacePatchType> patch : patches )
    {
      final AbstractSurfacePatchType patchType = patch.getValue();

      if( patchType instanceof PolygonPatchType )
      {
        final PolygonPatchType polygonType = (PolygonPatchType) patchType;

        final JAXBElement< ? extends AbstractRingType> ring = polygonType.getExterior().getValue().getRing();
        final AbstractRingType abstractLinearRing = ring.getValue();

        final List<JAXBElement<AbstractRingPropertyType>> interior = polygonType.getInterior();

        return createSurface( abstractLinearRing, interior, co );
      }
      else
        throw new NotImplementedException();
    }

    throw new NotImplementedException();
// return GeometryFactory.createGM_Surface( exteriorRing, interiorRings, null, co );
  }

  private GM_Surface<GM_SurfacePatch> createSurface( final AbstractRingType abstractLinearRing, final List<JAXBElement<AbstractRingPropertyType>> interior, final String co ) throws GM_Exception
  {
    final GM_Position[] exteriorRing = createGM_Positions( abstractLinearRing );

    final List<GM_Position[]> interiorList = new ArrayList<GM_Position[]>();

    for( final JAXBElement<AbstractRingPropertyType> element : interior )
    {
      final AbstractRingPropertyType abstractRingPropertyType = element.getValue();
      final GM_Position[] positions = createGM_Positions( abstractRingPropertyType.getRing().getValue() );
      interiorList.add( positions );
    }

    final GM_Position[][] interiorRings = interiorList.toArray( new GM_Position[interiorList.size()][] );
    return GeometryFactory.createGM_Surface( exteriorRing, interiorRings, null, co );
  }

  private GM_Position[] createGM_Positions( final AbstractRingType abstractRingType )
  {
    if( abstractRingType instanceof LinearRingType )
    {
      final LinearRingType linearRingType = (LinearRingType) abstractRingType;
      final CoordinatesType coordinates = linearRingType.getCoordinates();
      if( coordinates != null )
        return createGM_Positions( coordinates );

      final List<JAXBElement< ? >> posList = linearRingType.getPosOrPointPropertyOrPointRep();
      if( posList != null )
      {
        final DirectPositionListType directPositionListType = linearRingType.getPosList();
        if( posList.size() == 0 && directPositionListType != null )
          return createGM_Positions( directPositionListType );

        return createGM_Positions( posList );
      }

      throw new UnsupportedOperationException( "LinearRing has neither coordinates nor points" );
    }

    throw new UnsupportedOperationException();
  }

  private GM_Curve createGM_LineString( final LineStringType type, final String cs ) throws GM_Exception
  {
    final String co = getCS_CoordinateSystem( cs, type );
    final GM_Position[] positions = create_GM_Positions( type );
    return GeometryFactory.createGM_Curve( positions, co );
  }

  private GM_Position[] create_GM_Positions( final LineStringType lineString )
  {
    final CoordinatesType coordinates = lineString.getCoordinates();
    if( coordinates != null )
      return createGM_Positions( coordinates );

    final List<JAXBElement< ? >> posList = lineString.getPosOrPointPropertyOrPointRep();
    if( posList != null )
      return createGM_Positions( posList );

    throw new UnsupportedOperationException( "LineString has neither coordinates nor points" );
  }

  private GM_Point createGM_Point( final PointType type, final String cs )
  {
    final String co = getCS_CoordinateSystem( cs, type );
    final CoordType coord = type.getCoord();
    if( coord != null )
    {
      final GM_Position position = createGM_Position( coord );
      return GeometryFactory.createGM_Point( position, co );
    }

    final DirectPositionType pos = type.getPos();

    final GM_Position position;
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

  private GM_Position createGM_Position( final CoordType coord )
  {
    final double x = coord.getX().doubleValue();
    final double y = coord.getX().doubleValue();
    final BigDecimal z = coord.getZ();
    if( z == null )
      return GeometryFactory.createGM_Position( x, y );
    return GeometryFactory.createGM_Position( x, y, z.doubleValue() );
  }

  private GM_Position createGM_Position( final DirectPositionType positionType )
  {
    final BigInteger srsDimension = positionType.getSrsDimension();
    final List<Double> crdsList = positionType.getValue();

    if( srsDimension != null && crdsList.size() != srsDimension.intValue() )
      throw new IllegalArgumentException( "Wrong count of coordinates: " + crdsList.size() );

    final double[] crds = ArrayUtils.toPrimitive( crdsList.toArray( new Double[crdsList.size()] ) );

    return GeometryFactory.createGM_Position( crds );
  }

  private GM_Position[] createGM_Positions( final List<JAXBElement< ? >> posList )
  {
    final List<GM_Position> result = new ArrayList<GM_Position>();
    for( final JAXBElement< ? > element : posList )
    {
      final GM_Position pos = createGM_PositionForPosOrPointPropertyOrPointRep( element.getValue() );
      result.add( pos );
    }
    return result.toArray( new GM_Position[result.size()] );
  }

  private GM_Position[] createGM_Positions( final DirectPositionListType positionListType )
  {
    final int dimension = positionListType.getSrsDimension().intValue();
    if( dimension != 2 && dimension != 3 )
      throw new IllegalStateException();

    final List<GM_Position> results = new ArrayList<GM_Position>();
    final List<Double> values = positionListType.getValue();

    for( int i = 0; i < values.size(); i += dimension )
    {
      final double x = values.get( i );
      final double y = values.get( i + 1 );

      if( dimension == 3 )
      {
        final double z = values.get( i + 2 );
        results.add( GeometryFactory.createGM_Position( x, y, z ) );
      }
      else
        results.add( GeometryFactory.createGM_Position( x, y ) );

    }

    return results.toArray( new GM_Position[] {} );
  }

  private GM_Position createGM_PositionForPosOrPointPropertyOrPointRep( final Object pointOrPos )
  {
    if( pointOrPos instanceof DirectPositionType )
      return createGM_Position( (DirectPositionType) pointOrPos );

    throw new UnsupportedOperationException( "Not supported: " + pointOrPos );
  }

  private GM_Position[] createGM_Positions( final CoordinatesType coordinates )
  {
    final String coordinateSepearator = coordinates.getCs();
    final String tuppleSeparator = coordinates.getTs();
    final String decimal = coordinates.getDecimal();
    final String value = coordinates.getValue();
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
        pos[i] = Double.parseDouble( coordinate.replace( decimal, "." ) );
        i++;
      }
      result.add( GeometryFactory.createGM_Position( pos ) );
    }
    return result.toArray( new GM_Position[result.size()] );
  }

  private String getCS_CoordinateSystem( final String defaultCS, final AbstractGeometryType geom )
  {
    final String srsName = geom.getSrsName();
    if( srsName == null )
      return defaultCS;

    return srsName;
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.IGMLBindingToValueAdapter#wrapFromBinding(java.lang.Object)
   */
  public Object wrapFromBinding( final Object bindingGeometry, final Class geometryClass ) throws GM_Exception
  {
    if( bindingGeometry == null )
      return null;
    else if( bindingGeometry instanceof JAXBElement )
    {
      return wrapFromBinding( ((JAXBElement) bindingGeometry).getValue(), geometryClass );
    }
    else if( bindingGeometry instanceof AbstractGeometryType )
    {
      final AbstractGeometryType bindingTypeObject = (AbstractGeometryType) bindingGeometry;
      final String cs = getCS_CoordinateSystem( null, bindingTypeObject );
      if( bindingTypeObject instanceof PointType )
        return createGM_Point( (PointType) bindingTypeObject, cs );
      else if( bindingTypeObject instanceof PolygonType )
      {
        final GM_Surface surface = createGM_Surface( (PolygonType) bindingTypeObject, cs );
        if( geometryClass == GeometryUtilities.getMultiPolygonClass() )
        {
          final GM_Surface[] surfaces = new GM_Surface[] { surface };
          return GeometryFactory.createGM_MultiSurface( surfaces, cs );
        }
        return surface;
      }
      else if( bindingTypeObject instanceof SurfaceType )
      {
        final GM_Surface surface = createGM_Surface( (SurfaceType) bindingTypeObject, cs );

        return surface;
      }
      else if( bindingTypeObject instanceof LineStringType )
        return createGM_LineString( (LineStringType) bindingTypeObject, cs );
      else if( bindingTypeObject instanceof MultiPolygonType )
        return createGM_MultiSurface( (MultiPolygonType) bindingTypeObject, cs );
      else if( bindingTypeObject instanceof MultiLineStringType )
        return createGM_MultiLineString( (MultiLineStringType) bindingTypeObject, cs );
      else if( bindingTypeObject instanceof MultiPointType )
        return createGM_MultiPoint( (MultiPointType) bindingTypeObject, cs );
    }
    else if( bindingGeometry instanceof EnvelopeType )
    {
      return createGM_Envelope( (EnvelopeType) bindingGeometry );
    }

    throw new UnsupportedOperationException( bindingGeometry.getClass().getName() + " is not supported" );
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.IGMLBindingToValueAdapter#wrapFromElement(org.w3c.dom.Element)
   */
  public Object wrapFromNode( final Node node ) throws Exception
  {
    final Unmarshaller unmarshaller = KalypsoOGC31JAXBcontext.getContext().createUnmarshaller();
    final Object bindingGeometry = unmarshaller.unmarshal( node );
    final Object object = wrapFromBinding( bindingGeometry, null );
    return object;
  }

}
