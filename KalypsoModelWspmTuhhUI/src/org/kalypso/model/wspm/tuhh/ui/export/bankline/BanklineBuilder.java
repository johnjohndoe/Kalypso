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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import java.util.SortedMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Builds banklines from a {@link org.kalypso.model.wspm.core.gml.WspmWaterBody} or a
 * {@link org.kalypso.model.wspm.tuhh.core.gml.TuhhReach} using the centerline of the water body and the markers of the
 * involved cross sections.
 *
 * @author Gernot Belger
 */
public class BanklineBuilder implements ICoreRunnableWithProgress
{
  private final Feature m_waterOrReach;

  private Polygon m_mainChannel;

  public BanklineBuilder( final Feature waterOrReach )
  {
    m_waterOrReach = waterOrReach;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final WspmWaterBody water = getWaterBody();
    final IProfileFeature[] profiles = getProfiles();

    if( water == null )
    {
      final String message = String.format( "Failed to find water body for element: %s", m_waterOrReach.getName() );
      return new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message );
    }

    final GM_Curve centerLine = water.getCenterLine();
    if( centerLine == null )
    {
      final String message = String.format( "Unable to build bank lines for element '%s': no center line defined in water body", m_waterOrReach.getName() );
      return new Status( IStatus.INFO, KalypsoModelWspmTuhhUIPlugin.getID(), message );
    }

    if( profiles == null )
    {
      final String message = String.format( "Failed to find cross sections for element: %s", m_waterOrReach.getName() );
      return new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message );
    }

    try
    {
      final LineString riverLine = (LineString) JTSAdapter.export( centerLine );
      return buildBankLines( riverLine, profiles );
    }
    catch( final GM_Exception e )
    {
      final String message = String.format( "Unable to build bank lines for element '%s': bad no center line in water body", m_waterOrReach.getName() );
      return new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message, e );
    }
  }

  private IStatus buildBankLines( final LineString riverLine, final IProfileFeature[] profiles )
  {
    final IStatusCollector log = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

    /* Calculate bankline distances along the rivder line */
    // TODO: find intersecting profiles; calculate left/right distances
    final BanklineDistanceBuilder distanceBuilder = new BanklineDistanceBuilder( riverLine, profiles );
    log.add( distanceBuilder.execute() );
    final SortedMap<Double, BanklineDistances> banklineDistances = distanceBuilder.getDistances();

    /* build left and right river banks */
    // m_mainChannel = buildBuffer( riverLine, banklineDistances, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE + "_0" );
    final Polygon leftBank = buildBuffer( riverLine, banklineDistances, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE + "_0", 1.0 );
    final Polygon rightBank = buildBuffer( riverLine, banklineDistances, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE + "_1", -1.0 );

    m_mainChannel = (Polygon) leftBank.union( rightBank );
// m_mainChannel = new Polygon[leftBank.length + rightBank.length];
// System.arraycopy( leftBank, 0, m_mainChannel, 0, leftBank.length );
// System.arraycopy( rightBank, 0, m_mainChannel, leftBank.length, rightBank.length );

    final String logMessage = String.format( "Compute bank line for '%s'", m_waterOrReach.getName() );
    return log.asMultiStatusOrOK( logMessage, logMessage );
  }

  private Polygon buildBuffer( final LineString riverLine, final SortedMap<Double, BanklineDistances> distances, final String name, final double distanceSignum )
  {
    final BanklineBufferBuilder builder = new BanklineBufferBuilder( distances, name, riverLine, distanceSignum );
    return builder.buffer();
  }

// private Polygon buildMainChannel( final LineString riverLine, final LineString leftBank, final LineString rightBank )
// {
// final GeometryFactory factory = riverLine.getFactory();
//
// final Collection<Coordinate> crds = new ArrayList<>( leftBank.getNumPoints() + rightBank.getNumPoints() );
//
// crds.addAll( Arrays.asList( leftBank.getCoordinates() ) );
// final Coordinate[] rightCoordinates = rightBank.getCoordinates();
// ArrayUtils.reverse( rightCoordinates );
// crds.addAll( Arrays.asList( rightCoordinates ) );
//
// final LinearRing shell = factory.createLinearRing( crds.toArray( new Coordinate[crds.size()] ) );
//
// return factory.createPolygon( shell, null );
// }

  WspmWaterBody getWaterBody( )
  {
    if( m_waterOrReach instanceof WspmWaterBody )
      return (WspmWaterBody) m_waterOrReach;

    if( m_waterOrReach instanceof TuhhReach )
      return ((TuhhReach) m_waterOrReach).getWaterBody();

    return null;
  }

  public TuhhReach getReach( )
  {
    if( m_waterOrReach instanceof TuhhReach )
      return (TuhhReach) m_waterOrReach;

    return null;
  }

  private IProfileFeature[] getProfiles( )
  {
    final TuhhReach reach = getReach();
    if( reach != null )
    {
      final TuhhReachProfileSegment[] profileSegments = reach.getReachProfileSegments();
      final IProfileFeature[] profiles = new IProfileFeature[profileSegments.length];
      for( int i = 0; i < profiles.length; i++ )
        profiles[i] = profileSegments[i].getProfileMember();
    }

    final WspmWaterBody waterBody = getWaterBody();
    if( waterBody != null )
    {
      final IFeatureBindingCollection<IProfileFeature> profiles = waterBody.getProfiles();
      return profiles.toArray( new IProfileFeature[profiles.size()] );
    }

    return null;
  }

  public Geometry getMainChannel( )
  {
    return m_mainChannel;
  }
}