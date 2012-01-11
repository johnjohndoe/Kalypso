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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.wrappers.ProfilePointWrapper;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.init.KnaufBeanInitializer;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractKnaufProfileBeanBuilder implements ICoreRunnableWithProgress
{
  private final Set<AbstractKnaufProjectBean> m_beans = new LinkedHashSet<>();

  protected IStatus[] buildDefaultBeans( final KnaufProfileWrapper profile )
  {
    final KnaufSA20Bean sa20 = new KnaufSA20Bean( profile );
    final KnaufSA21Bean sa21 = new KnaufSA21Bean( profile );

    KnaufBeanInitializer.doInitialize( sa20 );
    KnaufBeanInitializer.doInitialize( sa21 );

    addBeans( sa20 );
    addBeans( sa21 );

    if( isBridgeProfile( profile ) )
    {
      final KnaufSA29Bean sa29 = new KnaufSA29Bean( profile );
      KnaufBeanInitializer.doInitialize( sa29 );

      addBeans( sa29 );
    }

    final ProfilePointWrapper[] points = profile.getPoints();
    for( final ProfilePointWrapper point : points )
    {
      addBeans( new KnaufSA30Bean( profile, point ) );
    }

    final Status status = new Status( IStatus.OK, KalypsoModelWspmTuhhCorePlugin.getID(), "Default Knauf Profilexport Bean-Generierung erfolgreich" );
    return new IStatus[] { status };
  }

  private boolean isBridgeProfile( final KnaufProfileWrapper profile )
  {
    final IProfileObject[] objects = profile.getProfile().getProfileObjects();
    for( final IProfileObject object : objects )
    {
      if( object instanceof BuildingBruecke )
      {
        return true;
      }
    }

    return false;
  }

  protected void addBeans( final AbstractKnaufProjectBean... beans )
  {
    Collections.addAll( m_beans, beans );
  }

  public AbstractKnaufProjectBean[] getBeans( )
  {
    return m_beans.toArray( new AbstractKnaufProjectBean[] {} );
  }

}
