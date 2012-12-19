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
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Monika Thül
 */
public class BreakLinesHelper implements IWspmConstants
{
  public BreakLinesHelper( )
  {
    // don't instantiate
  }

  static Map<Double, Double> createWspMap( final TupleResult result, final String componentStation, final String componentWaterlevel )
  {
    final int statIndex = result.indexOfComponent( componentStation );
    if( statIndex == -1 )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.BreakLinesHelper.0", componentStation ) ); //$NON-NLS-1$

    final int wspIndex = result.indexOfComponent( componentWaterlevel );
    if( wspIndex == -1 )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.BreakLinesHelper.0", componentWaterlevel ) ); //$NON-NLS-1$

    final Map<Double, Double> wspMap = new TreeMap<>();
    for( final IRecord record : result )
      wspMap.put( ((BigDecimal)record.getValue( statIndex )).doubleValue(), ((BigDecimal)record.getValue( wspIndex )).doubleValue() );

    return wspMap;
  }

  /** Creates a boundary from the given profiles. The profiles must be sorted. */
  public static void createModelBoundary( final TuhhReachProfileSegment[] reachProfileSegments, final TupleResult result, final String strStationierung, final String strWsp, final File file, final boolean useWsp ) throws GMLSchemaException, GM_Exception, IOException, GmlSerializeException
  {
    final Map<Double, Double> wspMap = createWspMap( result, strStationierung, strWsp );

    final LinkedList<GM_Point> leftPoints = new LinkedList<>();
    final LinkedList<GM_Point> rightPoints = new LinkedList<>();

    if( reachProfileSegments.length > 0 )
    {
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( new QName( NS_WSPM_BOUNDARY, "Boundary" ), file.toURI().toURL(), null ); //$NON-NLS-1$
      final Feature rootFeature = workspace.getRootFeature();

      for( final TuhhReachProfileSegment reach : reachProfileSegments )
      {
        final GM_Curve geometry = reach.getGeometry();
        if( geometry == null ) // ignore profiles without geometry
          continue;

        final IProfileFeature profileMember = reach.getProfileMember();
        final IProfile profil = profileMember.getProfile();

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
            final Feature pointFeature = FeatureHelper.addFeature( rootFeature, new QName( NS_WSPM_BOUNDARY, "wspPointMember" ), new QName( NS_WSPM_BOUNDARY, "WspPoint" ) ); //$NON-NLS-1$ //$NON-NLS-2$
            pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "geometry" ), pos ); //$NON-NLS-1$
            pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "station" ), station ); //$NON-NLS-1$
            pointFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "wsp" ), wsp ); //$NON-NLS-1$
          }
        }
      }

      final List<GM_Position> posList = new ArrayList<>();

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

        final GM_Polygon surface = GeometryFactory.createGM_Surface( posList.toArray( new GM_Position[posList.size()] ), null, crs );
        rootFeature.setProperty( new QName( NS_WSPM_BOUNDARY, "geometry" ), surface ); //$NON-NLS-1$
      }

      GmlSerializer.serializeWorkspace( file, workspace, IWspmTuhhConstants.WSPMTUHH_CODEPAGE );
    }
  }

}