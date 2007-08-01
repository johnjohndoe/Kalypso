/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Monika Thül
 */
public class BreakLinesHelper implements IWspmConstants
{
  public BreakLinesHelper( )
  {
    // don't instantiate
  }

  public static void createBreaklines( final TuhhReachProfileSegment[] reachProfileSegments, final TupleResult result, final String strStationierung, final String strWsp, final Double epsThinning, final File breaklineFile ) throws GM_Exception, InvocationTargetException, GMLSchemaException, GmlSerializeException, IOException
  {
    final Map<Double, Double> wspMap = createWspMap( result, strStationierung, strWsp );

    if( reachProfileSegments.length > 0 )
    {
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( new QName( NS_WSPM_BREAKLINE, "BreaklineCollection" ), breaklineFile.toURL(), null );
      final Feature rootFeature = workspace.getRootFeature();

      final String gmlVersion = workspace.getGMLSchema().getGMLVersion();

      for( final TuhhReachProfileSegment reach : reachProfileSegments )
      {
        final GM_Curve geometry = reach.getGeometry();
        if( geometry == null ) // ignore profiles without geometry
          continue;

        final BigDecimal station = reach.getStation();
        final Double wsp = wspMap.get( station.doubleValue() );

        final GM_Curve thinProfile = thinnedOutClone( geometry, epsThinning, gmlVersion );
        if( wsp != null ) // ignore profiles without result (no value in laengsschnitt). This can occur if the
        // simulation does not concern the whole reach.
        {
          final GM_Curve newProfile = GeometryUtilities.setValueZ( thinProfile.getAsLineString(), wsp );

          final Feature breakLineFeature = FeatureHelper.addFeature( rootFeature, new QName( NS_WSPM_BREAKLINE, "breaklineMember" ), new QName( NS_WSPM_BREAKLINE, "Breakline" ) );
          breakLineFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "geometry" ), newProfile );
          breakLineFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "station" ), station );
          breakLineFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "wsp" ), wsp );
        }
      }

      GmlSerializer.serializeWorkspace( breaklineFile, workspace, IWspmTuhhConstants.WSPMTUHH_CODEPAGE );
    }
  }

  private static Map<Double, Double> createWspMap( final TupleResult result, final String strStationierung, final String strWsp )
  {
    final IComponent statComponent = TupleResultUtilities.findComponentById( result, strStationierung );
    final IComponent wspComp = TupleResultUtilities.findComponentById( result, strWsp );

    if( statComponent == null )
      throw new IllegalArgumentException( "Längsschnitt hat keine Spalte mit Namen " + strStationierung );
    if( wspComp == null )
      throw new IllegalArgumentException( "Längsschnitt hat keine Spalte mit Namen " + strWsp );

    final Map<Double, Double> wspMap = new TreeMap<Double, Double>();
    for( final IRecord record : result )
      wspMap.put( ((BigDecimal) record.getValue( statComponent )).doubleValue(), ((BigDecimal) record.getValue( wspComp )).doubleValue() );
    return wspMap;
  }

  private static GM_Curve thinnedOutClone( final GM_Curve curve, final Double epsThinning, final String gmlVersion )
  {
    try
    {
      if( epsThinning > 0.0 )
      {
        // TODO thin out: sollten "Zwangspunkte" wie Marker für Trennflächen / durchströmte Bereiche erhalten bleiben?
        return GeometryUtilities.getThinnedCurve( curve, epsThinning );
      }
      else
      {
        final IMarshallingTypeHandler lineStringTypeHandler = MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( GeometryUtilities.QN_LINE_STRING_PROPERTY );
        final Object cloneObject = lineStringTypeHandler.cloneObject( curve, gmlVersion );
        return (GM_Curve) cloneObject;
      }
    }
    catch( final CloneNotSupportedException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return curve;
  }

  /** Creates a boundary from the given profiles. The profiles must be sorted. */
  public static void createModelBoundary( final TuhhReachProfileSegment[] reachProfileSegments, final TupleResult result, final String strStationierung, final String strWsp, final File file, final boolean useWsp ) throws GMLSchemaException, GM_Exception, InvocationTargetException, IOException, GmlSerializeException
  {
    final Map<Double, Double> wspMap = createWspMap( result, strStationierung, strWsp );

    final LinkedList<GM_Point> leftPoints = new LinkedList<GM_Point>();
    final LinkedList<GM_Point> rightPoints = new LinkedList<GM_Point>();

    if( reachProfileSegments.length > 0 )
    {
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( new QName( NS_WSPM_BOUNDARY, "Boundary" ), file.toURL(), null );
      final Feature rootFeature = workspace.getRootFeature();

      // we assume that all points have the same crs

      for( final TuhhReachProfileSegment reach : reachProfileSegments )
      {
        final GM_Curve geometry = reach.getGeometry();
        if( geometry == null ) // ignore profiles without geometry
          continue;

        final WspmProfile profileMember = reach.getProfileMember();

        final IProfil profil = ProfileFeatureFactory.toProfile( profileMember.getFeature() );

        final BigDecimal station = reach.getStation();
        final Double wsp = wspMap.get( station.doubleValue() );

        GM_Point[] points = null;
        if( useWsp )
        {
          if( wsp != null ) // ignore profiles without result (no value in laengsschnitt). This can occur if the
          // simulation does not concern the whole reach.
          {
            points = WspmProfileHelper.calculateWspPoints( profil, wsp.doubleValue() );
          }
        }
        else
          points = WspmProfileHelper.calculateWspPoints( profil, Double.MAX_VALUE );

        if( points != null )
        {
          if( points.length > 1 )
          {
            leftPoints.add( points[0] );
            rightPoints.add( points[points.length - 1] );
          }

          for( final GM_Point pos : points )
          {
            final Feature pointFeature = FeatureHelper.addFeature( rootFeature, new QName( NS_WSPM_BOUNDARY, "wspPointMember" ), new QName( NS_WSPM_BOUNDARY, "WspPoint" ) );
            pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "geometry" ), pos );
            pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "station" ), station );
            pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "wsp" ), wsp );
          }
        }
      }

      final List<GM_Position> posList = new ArrayList<GM_Position>();

      for( final GM_Point pos : leftPoints )
        posList.add( pos.getPosition() );
      for( final ListIterator<GM_Point> iter = rightPoints.listIterator( rightPoints.size() ); iter.hasPrevious(); )
        posList.add( iter.previous().getPosition() );

      if( !leftPoints.isEmpty() )
      {
        final GM_Point firstLeftPoint = leftPoints.getFirst();
        /* We assume here that all points have the same crs */
        final CS_CoordinateSystem crs = firstLeftPoint.getCoordinateSystem();
        // add first point to close the ring
        posList.add( firstLeftPoint.getPosition() );

        final GM_Surface<GM_SurfacePatch> surface = GeometryFactory.createGM_Surface( posList.toArray( new GM_Position[posList.size()] ), null, null, crs );
        rootFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "geometry" ), surface );
      }

      GmlSerializer.serializeWorkspace( file, workspace, IWspmTuhhConstants.WSPMTUHH_CODEPAGE );
    }
  }

}