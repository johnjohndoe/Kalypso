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

import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA14Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA15Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA16Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA40Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufReachBeanBuilder extends AbstractKnaufBeanBuilder
{

  private final KnaufReach m_reach;

  public KnaufReachBeanBuilder( final KnaufReach reach )
  {
    m_reach = reach;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final Set<IStatus> stati = new LinkedHashSet<>();

    addBeans( new KnaufSA14Bean( m_reach ) );
    addBeans( new KnaufSA15Bean( m_reach ) );
    addBeans( new KnaufSA16Bean( m_reach ) );

    final KnaufProfileWrapper[] profiles = m_reach.getProfiles();
    for( final KnaufProfileWrapper profile : profiles )
    {
      monitor.setTaskName( String.format( Messages.getString( "KnaufReachBeanBuilder_0" ), profile.getStation() ) ); //$NON-NLS-1$

      final KnaufProfileBeanBuilder builder = new KnaufProfileBeanBuilder( profile );
      stati.add( builder.execute( new NullProgressMonitor() ) );

      addBeans( builder.getBeans() );

      monitor.worked( 1 );
    }

    addBeans( new KnaufSA40Bean( m_reach ) );

    return StatusUtilities.createStatus( stati, Messages.getString( "KnaufReachBeanBuilder_1" ) ); //$NON-NLS-1$
  }
}
