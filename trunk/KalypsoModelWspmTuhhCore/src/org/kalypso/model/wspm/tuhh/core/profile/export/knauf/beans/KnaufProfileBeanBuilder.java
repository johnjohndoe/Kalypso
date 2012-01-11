/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans;

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
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.jts.JtsVectorUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.base.FillMissingProfileGeocoordinatesRunnable;
import org.kalypso.model.wspm.core.profil.base.MoveProfileRunnable;
import org.kalypso.model.wspm.core.profil.wrappers.ProfilePointWrapper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.profile.utils.TuhhProfiles;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Dirk Kuch
 */
public final class KnaufProfileBeanBuilder implements ICoreRunnableWithProgress
{
  private final KnaufReach m_reach;

  private final KnaufProfileWrapper m_profile;

  private final Set<AbstractKnaufProjectBean> m_beans = new LinkedHashSet<>();

  public KnaufProfileBeanBuilder( final KnaufReach reach, final KnaufProfileWrapper profile )
  {
    m_reach = reach;
    m_profile = profile;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final Set<IStatus> stati = new LinkedHashSet<>();

    final IProfil profile = m_profile.getProfile();
    final IProfileObject[] objects = profile.getProfileObjects();

    final FillMissingProfileGeocoordinatesRunnable runnable = new FillMissingProfileGeocoordinatesRunnable( m_profile );
    stati.add( runnable.execute( monitor ) );

    // TODO handle sinousit�t

    final IProfileBuilding building = findBuilding( objects );
    if( Objects.isNull( building ) )
      Collections.addAll( stati, buildDefaultBeans( m_reach, m_profile ) );
    else if( building instanceof BuildingBruecke )
      Collections.addAll( stati, buildBridgeBeans( (BuildingBruecke) building ) );
    else if( Objects.isNotNull( building ) )
    {
      final String message = String.format( "Achtung! Bauwerk am Profile %.3f wurde �bersprungen. %s", m_profile.getStation(), building.getClass().getName() );
      final Status status = new Status( IStatus.WARNING, KalypsoModelWspmTuhhCorePlugin.getID(), message );
      stati.add( status );

      Collections.addAll( stati, buildDefaultBeans( m_reach, m_profile ) );
    }

    return StatusUtilities.createStatus( stati, "Knauf-Profilexport Bean-Generierung" );
  }

  private IProfileBuilding findBuilding( final IProfileObject[] objects )
  {
    for( final IProfileObject object : objects )
    {
      if( object instanceof IProfileBuilding )
        return (IProfileBuilding) object;
    }

    return null;
  }

  private IStatus[] buildBridgeBeans( final BuildingBruecke bridge )
  {
    final Set<IStatus> stati = new LinkedHashSet<>();

    final Coordinate vector = getBaseVector();
    final double distance = bridge.getWidth();

    // FIXME direction / unterwasser / oberwasser

    final KnaufProfileWrapper oberwasser = buildBridgeProfile( vector, distance, -1 );
    final KnaufProfileWrapper unterwasser = buildBridgeProfile( vector, distance, 1 );
    m_reach.addProfiles( oberwasser, unterwasser );

    final double deltaH = getDeltaH( bridge, unterwasser );
    unterwasser.accept( new ChangeProfilePointHeight( deltaH ), 1 );

    Collections.addAll( stati, buildDefaultBeans( m_reach, oberwasser ) );
    Collections.addAll( stati, buildDefaultBeans( m_reach, m_profile ) ); // FIXME bridge profile!!!!
    Collections.addAll( stati, buildDefaultBeans( m_reach, unterwasser ) );

    return new IStatus[] { StatusUtilities.createStatus( stati, KalypsoModelWspmTuhhCorePlugin.getID(), "Export of KalypsoWspm Bridge Profile" ) };
  }

  private double getDeltaH( final BuildingBruecke bridge, final KnaufProfileWrapper profile )
  {
    final ProfilePointWrapper point = org.kalypso.model.wspm.tuhh.core.util.WspmProfileHelper.getSohlpunktPoint( profile );
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
    final KnaufProfileWrapper profile = new KnaufProfileWrapper( m_reach, TuhhProfiles.clone( m_profile.getProfile() ) );
    new MoveProfileRunnable( profile.getProfile(), vector, distance, direction ).execute( new NullProgressMonitor() );
    profile.getProfile().setStation( m_profile.getStation() + distance * direction );

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

    final ProfilePointWrapper[] points = m_profile.getPoints();
    if( Arrays.isEmpty( points ) || ArrayUtils.getLength( points ) < 2 )
      return null;

    return toVector( points[0], points[ArrayUtils.getLength( points ) - 1] );
  }

  private Coordinate toVector( final ProfilePointWrapper... markers )
  {
    if( Arrays.isEmpty( markers ) || ArrayUtils.getLength( markers ) < 2 )
      return null;

    final ProfilePointWrapper marker1 = markers[0];
    final ProfilePointWrapper marker2 = markers[ArrayUtils.getLength( markers ) - 1];

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

  private IStatus[] buildDefaultBeans( final KnaufReach reach, final KnaufProfileWrapper profile )
  {
    m_beans.add( new KnaufSA20Bean( reach, profile ) );
    m_beans.add( new KnaufSA21Bean( reach, profile ) );

    final ProfilePointWrapper[] points = profile.getPoints();
    for( final ProfilePointWrapper point : points )
    {
      m_beans.add( new KnaufSA30Bean( profile, point ) );
    }

    final Status status = new Status( IStatus.OK, KalypsoModelWspmTuhhCorePlugin.getID(), "Default Knauf Profilexport Bean-Generierung erfolgreich" );
    return new IStatus[] { status };
  }

  public AbstractKnaufProjectBean[] getBeans( )
  {
    return m_beans.toArray( new AbstractKnaufProjectBean[] {} );
  }

}
