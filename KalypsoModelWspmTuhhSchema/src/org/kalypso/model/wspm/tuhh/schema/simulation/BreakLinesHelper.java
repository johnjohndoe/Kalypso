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
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GM_Triangle_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Monika Thül
 */
public class BreakLinesHelper implements IWspmConstants
{
  public BreakLinesHelper( )
  {
    // don't instantiate
  }

  public static void createBreaklines( final TuhhReachProfileSegment[] reachProfileSegments, final TupleResult result, final String strStationierung, final String strWsp, final Double epsThinning, final File breaklineFile, final File tinFile ) throws GM_Exception, InvocationTargetException, GMLSchemaException, GmlSerializeException, IOException
  {
    final Map<Double, Double> wspMap = createWspMap( result, strStationierung, strWsp );

    if( reachProfileSegments.length > 0 )
    {
      final GMLWorkspace triangleWorkspace = FeatureFactory.createGMLWorkspace( new QName( NS_WSPMCOMMONS, "TriangulatedSurfaceFeature" ), tinFile.toURI().toURL(), null ); //$NON-NLS-1$
      final String defaultCrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
      final GM_TriangulatedSurface surface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( defaultCrs );
      final Feature triangleFeature = triangleWorkspace.getRootFeature();
      triangleFeature.setProperty( new QName( NS_WSPMCOMMONS, "triangulatedSurfaceMember" ), surface ); //$NON-NLS-1$
      // TODO: set tin name
      NamedFeatureHelper.setDescription( triangleFeature, "Wasserspiegel-Tin: " ); //$NON-NLS-1$
      NamedFeatureHelper.setName( triangleFeature, "Triangulierter Wasserpiegel der Berechnung ''" ); //$NON-NLS-1$
      triangleFeature.setProperty( new QName( NS_WSPMCOMMONS, "unit" ), "NN+m" ); //$NON-NLS-1$ //$NON-NLS-2$
      triangleFeature.setProperty( new QName( NS_WSPMCOMMONS, "parameter" ), "h" ); //$NON-NLS-1$ //$NON-NLS-2$
      triangleFeature.setProperty( new QName( NS_WSPMCOMMONS, "date" ), DateUtilities.toXMLGregorianCalendar( new Date() ) ); //$NON-NLS-1$

      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( new QName( NS_WSPM_BREAKLINE, "BreaklineCollection" ), breaklineFile.toURI().toURL(), null ); //$NON-NLS-1$
      final Feature rootFeature = workspace.getRootFeature();

      final String gmlVersion = workspace.getGMLSchema().getGMLVersion();

      // debug
// final SimpleShapeWriter writer = new SimpleShapeWriter( new File( "c://test.shp" ), GeometryUtilities.QN_POINT,
      // XmlTypes.XS_DOUBLE, XmlTypes.XS_DOUBLE );
      
      GM_Curve lastProfile = null;
      for( final TuhhReachProfileSegment reach : reachProfileSegments )
      {
        final GM_Curve geometry = reach.getGeometry();
        final BigDecimal station = reach.getStation();
        if( geometry == null ) // ignore profiles without geometry
          continue;
        
        

        final Double wsp = wspMap.get( station.doubleValue() );
        final GM_Curve thinProfile = thinnedOutClone( geometry, epsThinning, gmlVersion );
        if( wsp != null )
        {
          // ignore profiles without result (no value in length section). This can occur if the
          // simulation does not cover the whole reach.
          final GM_Curve newProfile = GeometryUtilities.setValueZ( thinProfile.getAsLineString(), wsp );

          /* Triangulate two adjacent profiles */
          if( lastProfile != null )
          {
            final GM_Position[] polygonPosesClosed = GeometryUtilities.getPolygonfromCurves( lastProfile, newProfile );

            // Write the curve as breakline into breakline file
            final GM_Curve polygoneRing = GeometryFactory.createGM_Curve( polygonPosesClosed, defaultCrs );
            final Feature ringFeature = FeatureHelper.addFeature( rootFeature, new QName( NS_WSPM_BREAKLINE, "breaklineMember" ), new QName( NS_WSPM_BREAKLINE, "Breakline" ) ); //$NON-NLS-1$ //$NON-NLS-2$
            ringFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "geometry" ), polygoneRing ); //$NON-NLS-1$
            ringFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "station" ), station ); //$NON-NLS-1$
            ringFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "wsp" ), wsp ); //$NON-NLS-1$

            // Interpolate triangles between two adjacent curves and add them to the triangulated surface
            final GM_Position[] polygonPosesOpen = (GM_Position[]) ArrayUtils.remove( polygonPosesClosed, polygonPosesClosed.length - 1 );
            final GM_Position[][] triangles = GeometryUtilities.triangulateRing( polygonPosesOpen );
            for( final GM_Position[] triangle : triangles )
            {
              final GM_Triangle_Impl gmTriangle = GeometryFactory.createGM_Triangle( triangle, defaultCrs );
              surface.add( gmTriangle );
              
              try
              {
                final GM_TriangulatedSurface mySurface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( defaultCrs );
                mySurface.add( gmTriangle );

// writer.add( mySurface, station.doubleValue(), wsp );
              }
              catch( final Exception e )
              {
                e.printStackTrace();
              }
            }
          }

          lastProfile = newProfile;
        }
      }

      GmlSerializer.serializeWorkspace( tinFile, triangleWorkspace, IWspmTuhhConstants.WSPMTUHH_CODEPAGE );
      GmlSerializer.serializeWorkspace( breaklineFile, workspace, IWspmTuhhConstants.WSPMTUHH_CODEPAGE );

// writer.close();
    }
  }

  private static Map<Double, Double> createWspMap( final TupleResult result, final String strStationierung, final String strWsp )
  {
    final IComponent statComponent = TupleResultUtilities.findComponentById( result, strStationierung );
    final IComponent wspComp = TupleResultUtilities.findComponentById( result, strWsp );

    if( statComponent == null )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.model.wspm.tuhh.schema.simulation.BreakLinesHelper.0", strStationierung )); //$NON-NLS-1$
    if( wspComp == null )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.model.wspm.tuhh.schema.simulation.BreakLinesHelper.0", strWsp )); //$NON-NLS-1$

    final Map<Double, Double> wspMap = new TreeMap<Double, Double>();
    for( final IRecord record : result )
      wspMap.put( ((BigDecimal) record.getValue( result.indexOf( statComponent ) )).doubleValue(), ((BigDecimal) record.getValue( result.indexOf( wspComp ) )).doubleValue() );
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
        final IMarshallingTypeHandler lineStringTypeHandler = MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( GeometryUtilities.QN_LINE_STRING );
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
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( new QName( NS_WSPM_BOUNDARY, "Boundary" ), file.toURI().toURL(), null ); //$NON-NLS-1$
      final Feature rootFeature = workspace.getRootFeature();

      // we assume that all points have the same crs

      for( final TuhhReachProfileSegment reach : reachProfileSegments )
      {
        final GM_Curve geometry = reach.getGeometry();
        if( geometry == null ) // ignore profiles without geometry
          continue;

        final IProfileFeature profileMember = reach.getProfileMember();
        final IProfil profil = profileMember.getProfil();

        final BigDecimal station = reach.getStation();
        final Double wsp = wspMap.get( station.doubleValue() );

        GM_Point[] points = null;
        if( useWsp )
        {
          if( wsp != null ) // ignore profiles without result (no value in laengsschnitt). This can occur if the
          // simulation does not concern the whole reach.
          {
            points = WspmProfileHelper.calculateWspPoints( profil, wsp.doubleValue(), null );
          }
        }
        else
          points = WspmProfileHelper.calculateWspPoints( profil, Double.MAX_VALUE, null );

        if( points != null )
        {
          if( points.length > 1 )
          {
            leftPoints.add( points[0] );
            rightPoints.add( points[points.length - 1] );
          }

          for( final GM_Point pos : points )
          {
            final Feature pointFeature = FeatureHelper.addFeature( rootFeature, new QName( NS_WSPM_BOUNDARY, "wspPointMember" ), new QName( NS_WSPM_BOUNDARY, "WspPoint" ) ); //$NON-NLS-1$ //$NON-NLS-2$
            pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "geometry" ), pos ); //$NON-NLS-1$
            pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "station" ), station ); //$NON-NLS-1$
            pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "wsp" ), wsp ); //$NON-NLS-1$
          }
        }
      }

      final List<GM_Position> posList = new ArrayList<GM_Position>();

      for( final GM_Point pos : leftPoints )
        posList.add( pos.getPosition() );
      for( final ListIterator<GM_Point> iter = rightPoints.listIterator( rightPoints.size() ); iter.hasPrevious(); )
        posList.add( iter.previous().getPosition() );

      if( leftPoints.size() > 1 )
      {
        final GM_Point firstLeftPoint = leftPoints.getFirst();
        /* We assume here that all points have the same crs */
        final String crs = firstLeftPoint.getCoordinateSystem();
        // add first point to close the ring
        posList.add( firstLeftPoint.getPosition() );

        final GM_Surface<GM_SurfacePatch> surface = GeometryFactory.createGM_Surface( posList.toArray( new GM_Position[posList.size()] ), null, crs );
        rootFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "geometry" ), surface ); //$NON-NLS-1$
      }

      GmlSerializer.serializeWorkspace( file, workspace, IWspmTuhhConstants.WSPMTUHH_CODEPAGE );
    }
  }

}