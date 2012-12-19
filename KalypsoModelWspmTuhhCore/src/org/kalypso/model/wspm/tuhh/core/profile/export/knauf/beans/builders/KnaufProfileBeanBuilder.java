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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.base.interpolation.FillMissingProfileGeocoordinatesRunnable;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;

/**
 * @author Dirk Kuch
 */
public final class KnaufProfileBeanBuilder extends AbstractKnaufProfileBeanBuilder
{

  private final KnaufProfileWrapper m_profile;

  public KnaufProfileBeanBuilder( final KnaufProfileWrapper profile )
  {
    m_profile = profile;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final Set<IStatus> stati = new LinkedHashSet<>();

    final IProfile profile = m_profile.getProfile();
    final IProfileObject[] objects = profile.getProfileObjects();

    final FillMissingProfileGeocoordinatesRunnable runnable = new FillMissingProfileGeocoordinatesRunnable( profile );
    stati.add( runnable.execute( monitor ) );

    // TODO handle sinousit‰t

    final IProfileBuilding building = findBuilding( objects );
    if( Objects.isNull( building ) )
      Collections.addAll( stati, buildDefaultBeans( m_profile ) );
    else if( building instanceof BuildingBruecke )
    {
      final KnaufBridgeProfileBuilder builder = new KnaufBridgeProfileBuilder( m_profile, (BuildingBruecke) building );
      final IStatus status = builder.execute( new SubProgressMonitor( monitor, 1 ) );

      addBeans( builder.getBeans() );
      stati.add( status );
    }
    else if( Objects.isNotNull( building ) )
    {
      final String message = String.format( Messages.getString( "KnaufProfileBeanBuilder_0" ), m_profile.getStation(), building.getTypeLabel() ); //$NON-NLS-1$
      final Status status = new Status( IStatus.WARNING, KalypsoModelWspmTuhhCorePlugin.getID(), message );
      stati.add( status );

      Collections.addAll( stati, buildDefaultBeans( m_profile ) );
    }

    return StatusUtilities.createStatus( stati, String.format( Messages.getString( "KnaufProfileBeanBuilder_1" ), m_profile.getStation() ) ); //$NON-NLS-1$
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

}
