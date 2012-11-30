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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import java.util.Arrays;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.ProfileFeatureStationComparator;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.export.bankline.BanklineDistanceBuilder.SIDE;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.densify.Densifier;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;

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

  private Geometry m_mainChannel;

  private final IBanklineMarkerProvider m_markerProvider;

  private final double m_densifyDistance;

  /**
   * @param If
   *          set to non {@link Double#NaN}, the river line will be densified with this distance.
   */
  public BanklineBuilder( final Feature waterOrReach, final IBanklineMarkerProvider markerProvider, final double densifyDistance )
  {
    m_waterOrReach = waterOrReach;
    m_markerProvider = markerProvider;
    m_densifyDistance = densifyDistance;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final WspmWaterBody water = getWaterBody();
    if( water == null )
    {
      final String message = String.format( Messages.getString( "BanklineBuilder_0" ), m_waterOrReach.getName() ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message );
    }

    final IProfileFeature[] profiles = getProfiles();

    final GM_Curve centerLine = water.getCenterLine();
    if( centerLine == null )
    {
      final String message = String.format( Messages.getString( "BanklineBuilder_1" ), m_waterOrReach.getName() ); //$NON-NLS-1$
      return new Status( IStatus.INFO, KalypsoModelWspmTuhhUIPlugin.getID(), message );
    }

    if( profiles == null )
    {
      final String message = String.format( Messages.getString( "BanklineBuilder_2" ), m_waterOrReach.getName() ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message );
    }

    try
    {
      final LineString riverLine = (LineString) JTSAdapter.export( centerLine );

      final LineString denseRiverLine = densifyRiverLine( riverLine, profiles );

      final boolean directionUpstreams = findFlowDirection( water, riverLine, profiles );

      return buildBankLines( denseRiverLine, profiles, directionUpstreams );
    }
    catch( final GM_Exception | CoreException e )
    {
      final String message = String.format( Messages.getString( "BanklineBuilder_3" ), m_waterOrReach.getName() ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message, e );
    }
  }

  private boolean findFlowDirection( final WspmWaterBody water, final LineString riverLine, final IProfileFeature[] profiles )
  {
    /* Sort profiles in flow direction */
    final ProfileFeatureStationComparator tuhhStationComparator = new ProfileFeatureStationComparator( water.isDirectionUpstreams() );
    Arrays.sort( profiles, tuhhStationComparator );

    // TODO: intersect each with river line

    // FIXME: invert this, if centerline goes in wrong direction
    // we could/should do this automatically:

    return water.isDirectionUpstreams();
  }

  private IStatus buildBankLines( final LineString riverLine, final IProfileFeature[] profiles, final boolean flowDirection ) throws CoreException
  {
    final IStatusCollector log = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

    /* build left and right river banks */
    final boolean createPatches = false;
    if( createPatches )
    {
      final Geometry leftBank = buildPatchesBuffer( riverLine, profiles, BanklineDistanceBuilder.SIDE.left, log, flowDirection );
      final Geometry rightBank = buildPatchesBuffer( riverLine, profiles, BanklineDistanceBuilder.SIDE.right, log, flowDirection );
      m_mainChannel = buildMainChannel( leftBank, rightBank );
    }
    else
      m_mainChannel = buildVariableBuffer( riverLine, profiles, log, flowDirection );

    final String logMessage = String.format( Messages.getString( "BanklineBuilder_4" ), m_waterOrReach.getName() ); //$NON-NLS-1$
    return log.asMultiStatusOrOK( logMessage, logMessage );
  }

  private Geometry buildMainChannel( final Geometry leftBank, final Geometry rightBank )
  {
    if( leftBank instanceof GeometryCollection || rightBank instanceof GeometryCollection )
      return GeometryGatherer.collect( leftBank, rightBank );

    return leftBank.union( rightBank );
  }

  private Geometry buildPatchesBuffer( final LineString riverLine, final IProfileFeature[] profiles, final SIDE side, final IStatusCollector log, final boolean flowDirection )
  {
    /* Calculate bankline distances along the river line */
    final BanklineDistanceBuilder distanceBuilder = new BanklineDistanceBuilder( riverLine, profiles, m_markerProvider, side );
    log.add( distanceBuilder.execute() );
    final PolyLine banklineDistances = distanceBuilder.getDistances();

    final double distanceSignum = side == SIDE.right ? -1.0 : +1.0;
    final double flowSignum = flowDirection ? -1.0 : +1.0;

    final BanklinePatchesBuilder builder = new BanklinePatchesBuilder( banklineDistances, riverLine, distanceSignum * flowSignum );
    return builder.buffer();
  }

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
      return profiles;
    }

    final WspmWaterBody waterBody = getWaterBody();
    if( waterBody != null )
    {
      final IFeatureBindingCollection<IProfileFeature> profiles = waterBody.getProfiles();
      return profiles.toArray( new IProfileFeature[profiles.size()] );
    }

    return null;
  }

  public Geometry getBanklineGeometry( )
  {
    return m_mainChannel;
  }

  private LineString densifyRiverLine( final LineString riverLine, @SuppressWarnings("unused") final IProfileFeature[] profiles )
  {
    if( Double.isNaN( m_densifyDistance ) )
      return riverLine;

    return (LineString) Densifier.densify( riverLine, m_densifyDistance );

    // FIXME: we should also / alternatively add the intersewction points between river line and profile
    // but our own method is buggy and produces later NaN-coordinates

    // FIXME looking for an effective way to insert the intersection points...

// final CoordinateList intersectionPoints = new CoordinateList();
// for( final IProfileFeature profileFeature : profiles )
// {
// final GM_Curve line = profileFeature.getLine();
// final Geometry crossSection = JTSAdapter.export( line );
// if( crossSection != null )
// {
// final Geometry intersection = riverLine.intersection( crossSection );
// final Coordinate[] coordinates = intersection.getCoordinates();
//
// for( final Coordinate coordinate : coordinates )
// {
// if( Double.isNaN( coordinate.x ) )
// {
//            System.out.println( "soso" ); //$NON-NLS-1$
// }
// }
//
// intersectionPoints.add( coordinates, false );
// }
// }
//
// return JTSUtilities.addPointsToLine( riverLine, intersectionPoints.toCoordinateArray() );
  }

  private Geometry buildVariableBuffer( final LineString riverLine, final IProfileFeature[] profiles, final IStatusCollector log, final boolean flowDirection ) throws CoreException
  {
    final SIDE left = flowDirection ? SIDE.left : SIDE.right;
    final SIDE right = flowDirection ? SIDE.right : SIDE.left;

    /* Calculate bankline distances along the river line */
    final BanklineDistanceBuilder leftDistanceBuilder = new BanklineDistanceBuilder( riverLine, profiles, m_markerProvider, left );
    log.add( leftDistanceBuilder.execute() );
    final PolyLine leftDistances = leftDistanceBuilder.getDistances();

    final BanklineDistanceBuilder rightDistanceBuilder = new BanklineDistanceBuilder( riverLine, profiles, m_markerProvider, right );
    log.add( rightDistanceBuilder.execute() );
    final PolyLine rightDistances = rightDistanceBuilder.getDistances();

    if( leftDistances == null || rightDistances == null )
    {
      final IStatus status = new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString("BanklineBuilder.0") ); //$NON-NLS-1$
      throw new CoreException( status );
    }

    final BanklineVariableBufferBuilder builder = new BanklineVariableBufferBuilder( riverLine, leftDistances, rightDistances );
    return builder.buffer();
  }
}