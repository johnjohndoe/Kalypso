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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.xml.namespace.QName;

import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmReachProfileSegment;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author thuel2
 */
public class BreakLinesHelper implements IWspmConstants
{

  public BreakLinesHelper( )
  {
    // don't instantiate
  }

  public static void createBreaklines( final WspmReachProfileSegment[] reachProfileSegments, final TupleResult result, final String strStationierung, final String strWsp, Double epsThinning, final File breaklineFile ) throws GM_Exception, InvocationTargetException, GMLSchemaException, GmlSerializeException, IOException
  {
    final Map<Double, Double> wspMap = createWspMap( result, strStationierung, strWsp );

    if( reachProfileSegments.length > 0 )
    {
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( NS_WSPM_BREAKLINE, new QName( NS_WSPM_BREAKLINE, "BreaklineCollection" ) );
      final Feature rootFeature = workspace.getRootFeature();

      final String gmlVersion = workspace.getGMLSchema().getGMLVersion();
      
      for( final WspmReachProfileSegment reach : reachProfileSegments )
      {
        final Feature breakLineFeature = FeatureHelper.addFeature( rootFeature, new QName( NS_WSPM_BREAKLINE, "breaklineMember" ), new QName( NS_WSPM_BREAKLINE, "Breakline" ) );

        final GM_Curve thinProfile = thinnedOutClone( reach.getGeometry(), epsThinning, gmlVersion );
        final BigDecimal station = reach.getStation();
        final Double wsp = wspMap.get( station.doubleValue() );
        final GM_Curve newProfile = setValueZ( thinProfile.getAsLineString(), wsp );

        breakLineFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "geometry" ), newProfile );
        breakLineFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "station" ), station );
        breakLineFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "wsp" ), wsp );
      }

      GmlSerializer.serializeWorkspace( breaklineFile, workspace, WspmTuhhCalcJob.WSPMTUHH_CODEPAGE );
    }
  }

  private static Map<Double, Double> createWspMap( final TupleResult result, final String strStationierung, final String strWsp )
  {
    IComponent stationComp = null;
    IComponent wspComp = null;
    final IComponent[] components = result.getComponents();
    for( final IComponent comp : components )
    {
      // TODO: get component via phenomenon
      if( comp.getName().equals( strWsp ) )
        wspComp = comp;
      if( comp.getName().equals( strStationierung ) )
        stationComp = comp;
    }

    if( stationComp == null )
      throw new IllegalArgumentException( "L‰ngsschnitt hat keine Spalte mit Namen " + strStationierung );
    if( wspComp == null )
      throw new IllegalArgumentException( "L‰ngsschnitt hat keine Spalte mit Namen " + strWsp );

    final Map<Double, Double> wspMap = new TreeMap<Double, Double>();
    for( final IRecord record : result )
      wspMap.put( ((BigDecimal) record.getValue( stationComp )).doubleValue(), ((BigDecimal) record.getValue( wspComp )).doubleValue() );
    return wspMap;
  }

  private static GM_Curve setValueZ( final GM_LineString newProfile, double value ) throws GM_Exception
  {
    final GM_Position[] positions = newProfile.getPositions();
    final CS_CoordinateSystem crs = newProfile.getCoordinateSystem();
    GM_Position[] newPositions = new GM_Position[positions.length];
    for( int i = 0; i < positions.length; i++ )
    {
      GM_Position position = positions[i];
      newPositions[i] = GeometryFactory.createGM_Position( position.getX(), position.getY(), value );
    }
    return GeometryFactory.createGM_Curve( newPositions, crs );
  }

  private static GM_Curve thinnedOutClone( final GM_Curve curve, final Double epsThinning, final String gmlVersion )
  {
    try
    {
      final IMarshallingTypeHandler lineStringTypeHandler = MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( GeometryUtilities.QN_LINE_STRING_PROPERTY );
      final Object cloneObject = lineStringTypeHandler.cloneObject( curve, gmlVersion );

      return (GM_Curve) cloneObject;
    }
    catch( CloneNotSupportedException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    // TODO thin out

    return curve;
  }

  public static void createModelBoundary( final WspmReachProfileSegment[] reachProfileSegments, final TupleResult result, final String strStationierung, final String strWsp, final File file, final boolean useWsp ) throws GMLSchemaException, GM_Exception, InvocationTargetException, IOException, GmlSerializeException, ProfilDataException
  {
    final Map<Double, Double> wspMap = createWspMap( result, strStationierung, strWsp );

    final LinkedList<GM_Position> leftPoints = new LinkedList<GM_Position>();
    final LinkedList<GM_Position> rightPoints = new LinkedList<GM_Position>();

    if( reachProfileSegments.length > 0 )
    {
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( NS_WSPM_BOUNDARY, new QName( NS_WSPM_BOUNDARY, "Boundary" ) );
      final Feature rootFeature = workspace.getRootFeature();

      // we assume that all points have the same crs
      final CS_CoordinateSystem crs = reachProfileSegments[0].getGeometry().getCoordinateSystem();

      for( final WspmReachProfileSegment reach : reachProfileSegments )
      {
        final WspmProfile profileMember = reach.getProfileMember();

        final IProfil profil = ProfileFeatureFactory.toProfile( profileMember.getFeature() );

        final BigDecimal station = reach.getStation();
        final Double wsp = wspMap.get( station.doubleValue() );

        final GM_Position[] points;
        if( useWsp )
          points = calculateWspPoints( profil, wsp.doubleValue() );
        else
          points = calculateWspPoints( profil, Double.MAX_VALUE );

        if( points.length > 1 )
        {
          leftPoints.add( points[0] );
          rightPoints.add( points[points.length - 1] );
        }

        for( final GM_Position pos : points )
        {
          final Feature pointFeature = FeatureHelper.addFeature( rootFeature, new QName( NS_WSPM_BOUNDARY, "wspPointMember" ), new QName( NS_WSPM_BOUNDARY, "WspPoint" ) );
          pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "geometry" ), GeometryFactory.createGM_Point( pos, crs ) );
          pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "station" ), station );
          pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "wsp" ), wsp );
        }
      }

      final List<GM_Position> posList = new ArrayList<GM_Position>();
      for( GM_Position pos : leftPoints )
        posList.add( pos );
      for( final ListIterator<GM_Position> iter = rightPoints.listIterator( rightPoints.size() ); iter.hasPrevious(); )
        posList.add( iter.previous() );

      // add first point to close the ring
      posList.add( leftPoints.getFirst() );

      final GM_Surface surface = GeometryFactory.createGM_Surface( posList.toArray( new GM_Position[posList.size()] ), null, null, crs );
      rootFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "geometry" ), surface );

      GmlSerializer.serializeWorkspace( file, workspace, WspmTuhhCalcJob.WSPMTUHH_CODEPAGE );
    }
  }

  private static GM_Position[] calculateWspPoints( final IProfil profil, final double wspHoehe ) throws ProfilDataException
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final IProfilPoint firstPoint = points.getFirst();
    final IProfilPoint lastPoint = points.getLast();

    final double firstX = firstPoint.getValueFor( POINT_PROPERTY.BREITE );
    final double firstY = firstPoint.getValueFor( POINT_PROPERTY.HOEHE );
    final double lastX = lastPoint.getValueFor( POINT_PROPERTY.BREITE );
    final double lastY = lastPoint.getValueFor( POINT_PROPERTY.HOEHE );

    final double[] breiteValues = profil.getValuesFor( POINT_PROPERTY.BREITE );

    final PolyLine wspLine = new PolyLine( new double[] { firstX, lastX }, new double[] { wspHoehe, wspHoehe }, 0.0001 );
    final PolyLine profilLine = new PolyLine( breiteValues, profil.getValuesFor( POINT_PROPERTY.HOEHE ), 0.0001 );
    final PolyLine rwLine = new PolyLine( breiteValues, profil.getValuesFor( POINT_PROPERTY.RECHTSWERT ), 0.0001 );
    final PolyLine hwLine = new PolyLine( breiteValues, profil.getValuesFor( POINT_PROPERTY.HOCHWERT ), 0.0001 );

    final double[] intersectionXs = profilLine.intersect( wspLine );

    final TreeSet<Double> intersections = new TreeSet<Double>();
    if( firstY < wspHoehe )
      intersections.add( new Double( firstX ) );
    for( final double d : intersectionXs )
      intersections.add( new Double( d ) );
    if( lastY < wspHoehe )
      intersections.add( new Double( lastX ) );

    final GM_Position[] poses = new GM_Position[intersections.size()];
    int count = 0;
    for( final Double x : intersections )
    {
      final double rw = rwLine.getYFor( x, false );
      final double hw = hwLine.getYFor( x, false );
      final GM_Position position = GeometryFactory.createGM_Position( rw, hw, wspHoehe );
      poses[count++] = position;
    }

    return poses;
  }
}
