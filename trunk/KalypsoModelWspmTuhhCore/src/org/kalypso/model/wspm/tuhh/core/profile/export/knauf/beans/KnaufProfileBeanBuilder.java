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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.wrappers.ProfilePointWrapper;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;

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

    // TODO handle sinousit�t

    final IProfileBuilding building = findBuilding( objects );
    if( Objects.isNull( building ) )
      Collections.addAll( stati, buildDefaultBeans( m_reach, m_profile ) );
    else if( building instanceof BuildingBruecke )
      Collections.addAll( stati, buildBridgeBeans() );
    else if( Objects.isNotNull( building ) )
    {
      final String message = String.format( "Achtung! Bauwerk am Profile %.3f wurde �bersprungen. %s", m_profile.getStation(), building.getClass().getName() );
      final Status status = new Status( IStatus.WARNING, KalypsoModelWspmTuhhCorePlugin.getID(), message );
      stati.add( status );

      Collections.addAll( stati, buildDefaultBeans( m_reach, m_profile ) );
    }

    return StatusUtilities.createStatus( stati, "Knauf-Profilexport Bean-Generierung" );
  }

  private static IProfileBuilding findBuilding( final IProfileObject[] objects )
  {
    for( final IProfileObject object : objects )
    {
      if( object instanceof IProfileBuilding )
        return (IProfileBuilding) object;
    }

    return null;
  }

  private static IStatus[] buildBridgeBeans( )
  {
    // TODO Auto-generated method stub
    return new IStatus[] {};
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
