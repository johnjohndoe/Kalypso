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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;

import ogc31.www.opengis.net.gml.AbstractGeometryType;
import ogc31.www.opengis.net.gml.AbstractRingPropertyType;
import ogc31.www.opengis.net.gml.AbstractRingType;
import ogc31.www.opengis.net.gml.CoordType;
import ogc31.www.opengis.net.gml.CoordinatesType;
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
import ogc31.www.opengis.net.gml.PolygonPropertyType;
import ogc31.www.opengis.net.gml.PolygonType;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.Adapters;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * factory class to wrap from binding geometries to GM_Object geometries and visa versa
 * 
 * @author doemming
 */
public class AdapterBindingToValue_GML31 implements AdapterBindingToValue
{
  final static ConvenienceCSFactoryFull m_csFac = new ConvenienceCSFactoryFull();

  final static Adapters m_csAdapter = org.kalypsodeegree_impl.model.cs.Adapters.getDefault();

  final static JAXBContext GML3_JAXCONTEXT = KalypsoOGC31JAXBcontext.getContext();

  public AdapterBindingToValue_GML31( )
  {
    // do not instantiate
  }

  public GM_Object createGM_Object( Element element )
  {
    // see gml2xadapter
    throw new UnsupportedOperationException();
  }

  /**
   * TODO implement for filter...
   */
  public Element createElement( final String gmlVersion, GM_Object geometry )
  {
    // see gml2xadapter
    throw new UnsupportedOperationException();
    // TODO
    // try
    // {
    // final ITypeRegistry<IMarshallingTypeHandler> marshallingregistry =
    // MarshallingTypeRegistrySingleton.getTypeRegistry();
    // final QName qName = new QName( "namespaceabc", "geometry", "pre" );
    // final IMarshallingTypeHandler typeHandler = marshallingregistry.getTypeHandlerForClassName(
    // GeometryUtilities.getPolygonClass() );
    //
    // // final AbstractGeometryType bindingGeometry = createBindingGeometryType( geometry );
    // // final JAXBElement bindingElement = new JAXBElement( qName, bindingGeometry.getClass(), bindingGeometry );
    //
    // // final Marshaller marshaller = GML3_JAXCONTEXT.createMarshaller();
    //
    // final DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance();
    // fac.setNamespaceAware( true );
    // final DocumentBuilder builder = fac.newDocumentBuilder();
    // final Document document = builder.newDocument();
    // final UnMarshallResultEater eater = new UnMarshallResultEater()
    // {
    // public void eat( Object value )
    // {
    // if( value instanceof Node )
    // {
    // String string = XMLHelper.toString( (Node) value );
    // System.out.println( string );
    // }
    // }
    // };
    // // Element element = document.createElement("test");
    // // document.appendChild(element);
    // final DOMConstructor constructor = new DOMConstructor( document, eater );
    // constructor.startElement( "bla", "blub", "b:blub", new AttributesImpl() );
    // typeHandler.marshal( qName, geometry, constructor, null, null, gmlVersion );
    //
    // // marshaller.marshal( bindingElement, document );
    // String string1 = XMLHelper.toString( document );
    // Node node = constructor.getNode();
    // String string2 = XMLHelper.toString( node );
    // return document.getDocumentElement();
    // }
    // catch( Exception e )
    // {
    // // TODO Auto-generated catch block
    // e.printStackTrace();
    // }
    // // called from SpatialOperation.toXML()
  }

  private GM_Envelope createGM_Envelope( EnvelopeType bindingEnvelope )
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
    if( coordinates != null )
    {

      final GM_Position[] positions = createGM_Positions( coordinates );
      return GeometryFactory.createGM_Envelope( positions[0], positions[1] );
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
      return GeometryFactory.createGM_Envelope( minPos, maxPos );
    }

    final List<DirectPositionType> pos = bindingEnvelope.getPos();
    if( pos != null && !pos.isEmpty() )
    {
      final List<Double> min = pos.get( 0 ).getValue();
      final List<Double> max = pos.get( 1 ).getValue();
      final GM_Position minPos = GeometryFactory.createGM_Position( min.get( 0 ), min.get( 1 ) );
      final GM_Position maxPos = GeometryFactory.createGM_Position( max.get( 0 ), max.get( 1 ) );
      return GeometryFactory.createGM_Envelope( minPos, maxPos );
    }
    throw new UnsupportedOperationException();
  }

  private GM_MultiPoint createGM_MultiPoint( MultiPointType type, CS_CoordinateSystem cs )
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

  private GM_MultiCurve createGM_MultiLineString( MultiLineStringType type, CS_CoordinateSystem cs ) throws GM_Exception
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

  private GM_MultiSurface createGM_MultiSurface( MultiPolygonType type, CS_CoordinateSystem cs ) throws GM_Exception
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
    return GeometryFactory.createGM_MultiSurface( surfaces, co );
  }

  private GM_Surface createGM_Surface( PolygonType type, CS_CoordinateSystem cs ) throws GM_Exception
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

  private GM_Position[] createGM_PositionFrom( AbstractRingType abstractRingType )
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

  private GM_Curve createGM_LineString( LineStringType type, CS_CoordinateSystem cs ) throws GM_Exception
  {
    final CS_CoordinateSystem co = getCS_CoordinateSystem( cs, type );
    final CoordinatesType coordinates = type.getCoordinates();
    final GM_Position[] positions = createGM_Positions( coordinates );
    return GeometryFactory.createGM_Curve( positions, co );
  }

  private GM_Point createGM_Point( PointType type, CS_CoordinateSystem cs )
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

  private GM_Position createGM_Position( CoordType coord )
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

  private CS_CoordinateSystem getCS_CoordinateSystem( CS_CoordinateSystem defaultCS, AbstractGeometryType geom )
  {
    final String srsName = geom.getSrsName();
    if( srsName == null )
      return defaultCS;
    final CoordinateSystem csByName = m_csFac.getCSByName( srsName );
    return m_csAdapter.export( csByName );
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.IGMLBindingToValueAdapter#wrapFromBinding(java.lang.Object)
   */
  public Object wrapFromBinding( Object bindingGeometry, Class geometryClass ) throws GM_Exception
  {
    if( bindingGeometry == null )
      return null;
    if( bindingGeometry instanceof JAXBElement )
    {
      return wrapFromBinding( ((JAXBElement) bindingGeometry).getValue(), geometryClass );
    }
    if( bindingGeometry instanceof AbstractGeometryType )
    {
      final AbstractGeometryType bindingTypeObject = (AbstractGeometryType) bindingGeometry;
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
    if( bindingGeometry instanceof EnvelopeType )
    {
      return createGM_Envelope( (EnvelopeType) bindingGeometry );
    }

    throw new UnsupportedOperationException( bindingGeometry.getClass().getName() + " is not supported" );
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.IGMLBindingToValueAdapter#wrapFromElement(org.w3c.dom.Element)
   */
  public Object wrapFromNode( Node node ) throws Exception
  {
    final Unmarshaller unmarshaller = GML3_JAXCONTEXT.createUnmarshaller();
    final Object bindingGeometry = unmarshaller.unmarshal( node );
    final Object object = wrapFromBinding( bindingGeometry, null );
    return object;
  }

}
