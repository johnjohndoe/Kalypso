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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import ogc31.www.opengis.net.gml.AbstractGeometryType;
import ogc31.www.opengis.net.gml.AbstractRingPropertyType;
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
import ogc31.www.opengis.net.gml.PointPropertyType;
import ogc31.www.opengis.net.gml.PointType;
import ogc31.www.opengis.net.gml.PolygonPropertyType;
import ogc31.www.opengis.net.gml.PolygonType;

import org.kalypso.contribs.ogc31.KalypsoOGC31JAXBcontext;
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
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * factory class to wrap from binding geometries to GM_Object geometries and visa versa
 * 
 * @author doemming
 */
public class AdapterValueToBinding_GML31 implements AdapterValueToGMLBinding
{
  final static ConvenienceCSFactoryFull m_csFac = new ConvenienceCSFactoryFull();

  final static Adapters m_csAdapter = org.kalypsodeegree_impl.model.cs.Adapters.getDefault();

  final static JAXBContext GML3_JAXCONTEXT = KalypsoOGC31JAXBcontext.getContext();

  private static final String COORDINATES_SEPARATOR = " ";

  private static final String DECIMAL_SEPARATOR = ".";

  private static final String TUPPLE_SEPARATOR = ",";

  public AdapterValueToBinding_GML31( )
  {
    // do not instantiate
  }

  // public EnvelopeType createBindingGeometryType( final GM_Envelope envelope )
  // {
  // final GM_Position min = envelope.getMin();
  // final GM_Position max = envelope.getMax();
  // final EnvelopeType envelopeType = gml3Fac.createEnvelopeType();
  // final CoordinatesType coordinatesType = createCoordinatesType( new GM_Position[] { min, max } );
  // envelopeType.setCoordinates( coordinatesType );
  // // BoundingBox box = gml3Fac.createBoundingBox(envelopeType);
  // return envelopeType;
  // }

  /**
   * wrap {@link GM_Object } to BindingType
   * 
   * @param geometry
   *          the geometry to wrap
   * @param gmlVersion
   *          requested compatibility or <code>null</code>for unspecified GML compatibility
   */
  private AbstractGeometryType createBindingGeometryType( final GM_Object geometry ) throws GM_Exception
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

  private MultiPolygonType createMultiPolygonType( GM_MultiSurface multiSurface, String csNameDefault )
  {
    final String csName = getCSName( multiSurface, csNameDefault );
    final MultiPolygonType multiPolygonType = KalypsoOGC31JAXBcontext.GML3_FAC.createMultiPolygonType();
    final List<PolygonPropertyType> polygonMember = multiPolygonType.getPolygonMember();
    final GM_Surface[] allSurfaces = multiSurface.getAllSurfaces();
    for( int i = 0; i < allSurfaces.length; i++ )
    {
      final GM_Surface surface = allSurfaces[i];
      final PolygonPropertyType polygonPropertyType = KalypsoOGC31JAXBcontext.GML3_FAC.createPolygonPropertyType();
      final PolygonType polygonType = createPolygonType( surface, csName );
      polygonPropertyType.setPolygon( polygonType );
      polygonMember.add( polygonPropertyType );
    }
    multiPolygonType.setSrsName( csName );
    return multiPolygonType;
  }

  private MultiLineStringType createMultiLineStringType( GM_MultiCurve multiCurve, String csNameDefault ) throws GM_Exception
  {
    final String csName = getCSName( multiCurve, csNameDefault );
    final MultiLineStringType multiLineStringType = KalypsoOGC31JAXBcontext.GML3_FAC.createMultiLineStringType();
    final List<LineStringPropertyType> lineStringMember = multiLineStringType.getLineStringMember();
    final GM_Curve[] allCurves = multiCurve.getAllCurves();
    for( int i = 0; i < allCurves.length; i++ )
    {
      final GM_Curve curve = allCurves[i];
      final LineStringPropertyType lineStringPropertyType = KalypsoOGC31JAXBcontext.GML3_FAC.createLineStringPropertyType();
      final LineStringType lineStringType = createLineStringType( curve, csName );
      lineStringPropertyType.setLineString( lineStringType );
      lineStringMember.add( lineStringPropertyType );
    }
    return multiLineStringType;
  }

  private MultiPointType createMultiPointType( GM_MultiPoint multiPoint, String csNameDefault )
  {
    final String csName = getCSName( multiPoint, csNameDefault );
    final MultiPointType multiPointType = KalypsoOGC31JAXBcontext.GML3_FAC.createMultiPointType();
    final GM_Point[] allPoints = multiPoint.getAllPoints();

//    final List<PointType> pointList = multiPointType.getPointMembers().getPoint();
//    for( int i = 0; i < allPoints.length; i++ )
//    {
//      final GM_Point point = allPoints[i];
//      final PointType pointType = createPointType( point, csName );
//      pointList.add( pointType );
//    }
//    multiPointType.setSrsName( csName );
//    return multiPointType;
    
    final List<PointPropertyType> pointList = multiPointType.getPointMember();//.getPoint();
    for( int i = 0; i < allPoints.length; i++ )
    {
      final GM_Point point = allPoints[i];
      PointPropertyType pointPropertyType = KalypsoOGC31JAXBcontext.GML3_FAC.createPointPropertyType();
      final PointType pointType = createPointType( point, csName );
      pointPropertyType.setPoint( pointType );
      pointList.add( pointPropertyType );
    }
    multiPointType.setSrsName( csName );
    return multiPointType;
  }

  private PolygonType createPolygonType( GM_Surface surface, String csNameDefault )
  {
    final String csName = getCSName( surface, csNameDefault );
    final PolygonType polygonType = KalypsoOGC31JAXBcontext.GML3_FAC.createPolygonType();
    final GM_SurfaceBoundary surfaceBoundary = surface.getSurfaceBoundary();
    final GM_Ring exteriorRing = surfaceBoundary.getExteriorRing();
    final GM_Ring[] interiorRings = surfaceBoundary.getInteriorRings();

    // exterior
    final LinearRingType linearRingType = KalypsoOGC31JAXBcontext.GML3_FAC.createLinearRingType();

    final CoordinatesType coordinatesType = createCoordinatesType( exteriorRing.getPositions() );
    linearRingType.setCoordinates( coordinatesType );
    final JAXBElement<LinearRingType> linearRing = KalypsoOGC31JAXBcontext.GML3_FAC.createLinearRing( linearRingType );

    final AbstractRingPropertyType abstractRingPropertyType = KalypsoOGC31JAXBcontext.GML3_FAC.createAbstractRingPropertyType();
    abstractRingPropertyType.setRing( linearRing );

    final Exterior exterior = KalypsoOGC31JAXBcontext.GML3_FAC.createExterior( abstractRingPropertyType );

    polygonType.setExterior( exterior );
    // interior

    final List<JAXBElement<AbstractRingPropertyType>> interiorContainer = polygonType.getInterior();
    if( interiorRings != null )
    {
      for( int i = 0; i < interiorRings.length; i++ )
      {
        final GM_Ring ring = interiorRings[i];
        final LinearRingType interiorLinearRingType = KalypsoOGC31JAXBcontext.GML3_FAC.createLinearRingType();

        final CoordinatesType interiorCoordinatesType = createCoordinatesType( ring.getPositions() );
        interiorLinearRingType.setCoordinates( interiorCoordinatesType );
        final JAXBElement<LinearRingType> interiorLinearRing = KalypsoOGC31JAXBcontext.GML3_FAC.createLinearRing( interiorLinearRingType );

        final AbstractRingPropertyType interiorAbstractRingPropertyType = KalypsoOGC31JAXBcontext.GML3_FAC.createAbstractRingPropertyType();
        interiorAbstractRingPropertyType.setRing( interiorLinearRing );

        final Interior interior = KalypsoOGC31JAXBcontext.GML3_FAC.createInterior( interiorAbstractRingPropertyType );
        interiorContainer.add( interior );
      }
    }
    //
    polygonType.setSrsName( csName );
    return polygonType;
  }

  private LineStringType createLineStringType( GM_Curve lineString, String csNameDefault ) throws GM_Exception
  {
    final String csName = getCSName( lineString, csNameDefault );
    final LineStringType lineStringType = KalypsoOGC31JAXBcontext.GML3_FAC.createLineStringType();
    final GM_LineString asLineString = lineString.getAsLineString();
    final GM_Position[] positions = asLineString.getPositions();
    final CoordinatesType coordinatesType = createCoordinatesType( positions );
    lineStringType.setCoordinates( coordinatesType );
    lineStringType.setSrsName( csName );
    return lineStringType;
  }

  private PointType createPointType( GM_Point point, String csNameDefault )
  {
    final PointType pointType = KalypsoOGC31JAXBcontext.GML3_FAC.createPointType();
    final GM_Position position = point.getPosition();
    // TODO: coordinates is deprecates; use pos instead
    final CoordinatesType coordinatesType = createCoordinatesType( new GM_Position[] { position } );
    pointType.setSrsName( csNameDefault );
    pointType.setCoordinates( coordinatesType );
    return pointType;
  }

  private CoordinatesType createCoordinatesType( GM_Position[] positions )
  {
    final CoordinatesType coordinatesType = KalypsoOGC31JAXBcontext.GML3_FAC.createCoordinatesType();
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
   * @see org.kalypsodeegree_impl.model.geometry.IGMLBindingToValueAdapter#wrapToBinding(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public Object wrapToBinding( GM_Object geometry ) throws GM_Exception
  {
    return createBindingGeometryType( geometry );
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
      Marshaller marshaller = AdapterBindingToValue_GML31.GML3_JAXCONTEXT.createMarshaller();
      final DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance();
      fac.setNamespaceAware( true );
      final DocumentBuilder builder = fac.newDocumentBuilder();
      final Document document = builder.newDocument();
      marshaller.marshal( bindingGeometry, document );
      return document.getDocumentElement();
    }
    catch( Exception e )
    {
      throw new GM_Exception( "could not marshall to Element", e );
    }
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.AdapterValueToGMLBinding#wrapToBinding(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public Object wrapToBinding( GM_Envelope geometry )
  {
    final GM_Position min = geometry.getMin();
    final GM_Position max = geometry.getMax();

    final EnvelopeType envelopeType = KalypsoOGC31JAXBcontext.GML3_FAC.createEnvelopeType();

    final DirectPositionType lowerCorner = KalypsoOGC31JAXBcontext.GML3_FAC.createDirectPositionType();
    final DirectPositionType upperCorner = KalypsoOGC31JAXBcontext.GML3_FAC.createDirectPositionType();

    final List<Double> lowers = lowerCorner.getValue();
    lowers.clear();
    lowers.add( min.getX() );
    lowers.add( min.getY() );

    final List<Double> uppers = upperCorner.getValue();
    uppers.clear();
    uppers.add( max.getX() );
    uppers.add( max.getY() );

    envelopeType.setLowerCorner( lowerCorner );
    envelopeType.setUpperCorner( upperCorner );

    // envelopeType.setSrsName( )

    return envelopeType;
  }
}
