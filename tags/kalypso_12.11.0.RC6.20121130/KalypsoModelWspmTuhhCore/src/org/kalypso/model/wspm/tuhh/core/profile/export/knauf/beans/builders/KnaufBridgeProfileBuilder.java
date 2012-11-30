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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.builders;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.jts.JtsVectorUtilities;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.base.ChangeProfilePointHeight;
import org.kalypso.model.wspm.core.profil.base.FLOW_DIRECTION;
import org.kalypso.model.wspm.core.profil.base.MoveProfileRunnable;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.utils.TuhhProfiles;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Dirk Kuch
 */
public class KnaufBridgeProfileBuilder extends AbstractKnaufProfileBeanBuilder
{

  private final KnaufProfileWrapper m_profile;

  private final BuildingBruecke m_bridge;

  public KnaufBridgeProfileBuilder( final KnaufProfileWrapper profile, final BuildingBruecke bridge )
  {
    m_profile = profile;
    m_bridge = bridge;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final Set<IStatus> stati = new LinkedHashSet<>();

    final Coordinate vector = getBaseVector();
    final Double distanceValue = m_bridge.getBreite();
    if( distanceValue == null )
    {
      Collections.addAll( stati, buildDefaultBeans( m_profile ) );
      final String message = String.format( Messages.getString("KnaufBridgeProfileBuilder.2"), m_profile.getStation() ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, KalypsoModelWspmTuhhCorePlugin.PLUGIN_ID, message );
    }

    final double distance = distanceValue.doubleValue();

    /** move bridge into upstream direction */
    final IStatus status = moveBridgeProfile( distance / 2.0 );
    Collections.addAll( stati, status );
    if( !status.isOK() )
      return StatusUtilities.createStatus( stati, Messages.getString( "KnaufBridgeProfileBuilder_0" ) ); //$NON-NLS-1$

    /** generate new upstream and downstream profiles */
    final KnaufProfileWrapper downstream = getOberwasserProfile( vector, distance );
    final KnaufProfileWrapper upstream = getUnterwasserProfile( vector, distance );

    /** lowering of upstream profile */
    final double deltaH = getDeltaH( m_bridge, upstream );
    upstream.getProfile().accept( new ChangeProfilePointHeight( deltaH ), 1 );

    Collections.addAll( stati, addProfile( upstream, distance ) );
    Collections.addAll( stati, buildDefaultBeans( m_profile ) );
    Collections.addAll( stati, addProfile( downstream, distance ) );

    return StatusUtilities.createStatus( stati, Messages.getString( "KnaufBridgeProfileBuilder_0" ) ); //$NON-NLS-1$
  }

  private IStatus moveBridgeProfile( final double distance )
  {
    final KnaufReach reach = m_profile.getReach();

    final KnaufProfileWrapper previous = reach.findPreviousProfile( m_profile );
    final KnaufProfileWrapper next = reach.findNextProfile( m_profile );

    double station;

    final FLOW_DIRECTION direction = m_profile.getReach().getDirection();

    if( FLOW_DIRECTION.eSrc2Estuary == direction )
      station = m_profile.getStation() + distance / 1000.0;
    else
      station = m_profile.getStation() - distance / 1000.0;

    if( previous == null || next == null )
    {
      final String msg = String.format( Messages.getString("KnaufBridgeProfileBuilder.3"), station ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.getID(), msg );
    }

    if( !isBetween( Math.min( previous.getStation(), next.getStation() ), station, Math.max( previous.getStation(), next.getStation() ) ) )
    {
      final String msg = String.format( Messages.getString( "KnaufBridgeProfileBuilder.0" ), station, previous.getStation(), next.getStation() ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.getID(), msg );
    }

    m_profile.getProfile().setStation( station );

    return new Status( IStatus.OK, KalypsoModelWspmTuhhCorePlugin.getID(), Messages.getString( "KnaufBridgeProfileBuilder.1" ) ); //$NON-NLS-1$
  }

  private boolean isBetween( final double before, final double bridge, final double next )
  {
    if( bridge > before && bridge < next )
      return true;

    return false;
  }

  private IStatus[] addProfile( final KnaufProfileWrapper current, final double distance )
  {
    final KnaufReach reach = m_profile.getReach();
    final KnaufProfileWrapper previous = reach.findPreviousProfile( m_profile );
    final KnaufProfileWrapper next = reach.findNextProfile( m_profile );

    if( !(isBetween( previous, current ) || isBetween( next, current )) )
    {
      final Status status = new Status( IStatus.WARNING, KalypsoModelWspmTuhhCorePlugin.getID(), String.format( Messages.getString( "KnaufBridgeProfileBuilder_1" ), current.getStation() ) ); //$NON-NLS-1$

      return new IStatus[] { status };
    }

    final Set<IStatus> stati = new LinkedHashSet<>();

    if( isTooClose( previous, current, distance ) || isTooClose( next, current, distance ) )
    {
      stati.add( new Status( IStatus.INFO, KalypsoModelWspmTuhhCorePlugin.getID(), String.format( Messages.getString( "KnaufBridgeProfileBuilder_2" ), current.getStation() ) ) ); //$NON-NLS-1$
    }

    m_profile.getReach().addProfiles( current );
    Collections.addAll( stati, buildDefaultBeans( current ) );

    return stati.toArray( new IStatus[] {} );
  }

  private boolean isTooClose( final KnaufProfileWrapper neighbour, final KnaufProfileWrapper current, final double distance )
  {
    if( Objects.isNull( current, neighbour ) )
      return false;

    final double d = Math.abs( neighbour.getStation() - current.getStation() ) * 1000;

    return d < distance;
  }

  private boolean isBetween( final KnaufProfileWrapper neighbour, final KnaufProfileWrapper current )
  {
    if( Objects.isNull( m_profile, neighbour ) )
      return false;

    final double min = Math.min( m_profile.getStation(), neighbour.getStation() ) * 1000;
    final double max = Math.max( m_profile.getStation(), neighbour.getStation() ) * 1000;

    final double station = current.getStation() * 1000;

    if( Math.abs( min - station ) < 1.0 )
      return false;
    if( Math.abs( max - station ) < 1.0 )
      return false;

    if( station > min && station < max )
      return true;

    return false;
  }

  private KnaufProfileWrapper getUnterwasserProfile( final Coordinate vector, final double distance )
  {
    final FLOW_DIRECTION direction = m_profile.getReach().getDirection();

    if( FLOW_DIRECTION.eSrc2Estuary == direction )
      return buildBridgeProfile( vector, distance, 1 );

    return buildBridgeProfile( vector, distance, -1 );
  }

  private KnaufProfileWrapper getOberwasserProfile( final Coordinate vector, final double distance )
  {
    final FLOW_DIRECTION direction = m_profile.getReach().getDirection();

    if( FLOW_DIRECTION.eSrc2Estuary == direction )
      return buildBridgeProfile( vector, distance, -1 );

    return buildBridgeProfile( vector, distance, 1 );
  }

  private double getDeltaH( final BuildingBruecke bridge, final KnaufProfileWrapper profile )
  {
    final IProfileRecord point = org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte.getSohlpunktPoint( profile );
    if( Objects.isNull( point ) )
      return 0.0;

    final double sohlpunkt = point.getHoehe();
    final double unterwasser = bridge.getUnterwasser();

    if( Doubles.isNaN( sohlpunkt, unterwasser ) )
      return 0.0;

    return (sohlpunkt - unterwasser) * -1;
  }

  private KnaufProfileWrapper buildBridgeProfile( final Coordinate vector, final double distance, final int direction )
  {
    final KnaufProfileWrapper profile = new KnaufProfileWrapper( m_profile.getReach(), TuhhProfiles.clone( m_profile.getProfile() ) );
    new MoveProfileRunnable( profile.getProfile(), vector, distance, direction ).execute( new NullProgressMonitor() );

    profile.getProfile().setStation( m_profile.getStation() + distance / 1000.0 * direction );

    return profile;
  }

  private Coordinate getBaseVector( )
  {
    Coordinate vector = null;

    vector = toVector( m_profile.getProfilePointMarkerWrapper( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( Objects.isNotNull( vector ) )
      return vector;

    vector = toVector( m_profile.getProfilePointMarkerWrapper( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    if( Objects.isNotNull( vector ) )
      return vector;

    final IProfileRecord[] points = m_profile.getProfile().getPoints();
    if( Arrays.isEmpty( points ) || ArrayUtils.getLength( points ) < 2 )
      return null;

    return toVector( points[0], points[ArrayUtils.getLength( points ) - 1] );
  }

  private Coordinate toVector( final IProfilePointMarker... markers )
  {
    if( Arrays.isEmpty( markers ) || ArrayUtils.getLength( markers ) < 2 )
      return null;

    return toVector( markers[0].getPoint(), markers[ArrayUtils.getLength( markers ) - 1].getPoint() );
  }

  private Coordinate toVector( final IProfileRecord... records )
  {
    if( Arrays.isEmpty( records ) || ArrayUtils.getLength( records ) < 2 )
      return null;

    final IProfileRecord marker1 = records[0];
    final IProfileRecord marker2 = records[ArrayUtils.getLength( records ) - 1];

    final double x1 = marker1.getRechtswert();
    final double y1 = marker1.getHochwert();
    final double x2 = marker2.getRechtswert();
    final double y2 = marker2.getHochwert();

    if( Doubles.isNaN( x1, y1, x2, y2 ) )
      return null;

    final Coordinate c1 = new Coordinate( x1, y1 );
    final Coordinate c2 = new Coordinate( x2, y2 );

    /** get orthogonal vector ! */
    final GeometryFactory factory = new GeometryFactory();
    final LineString lineString1 = factory.createLineString( new Coordinate[] { c1, c2 } );

    final Point point = JTSUtilities.pointOnLinePercent( lineString1, 50 );

    final LineString lineString2 = factory.createLineString( new Coordinate[] { c1, point.getCoordinate(), c2 } );
    return JtsVectorUtilities.getOrthogonalVector( lineString2, point );
  }
}
