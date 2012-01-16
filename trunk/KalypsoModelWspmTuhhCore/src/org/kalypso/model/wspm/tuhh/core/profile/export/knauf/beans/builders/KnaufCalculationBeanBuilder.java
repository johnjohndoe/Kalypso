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

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufCalculation;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA10Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA11Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA12Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA13Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA91Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA94Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA99Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufCalculationBeanBuilder extends AbstractKnaufBeanBuilder
{
  private final KnaufCalculation m_calculation;

  public KnaufCalculationBeanBuilder( final KnaufCalculation calculation )
  {
    m_calculation = calculation;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final Set<IStatus> stati = new LinkedHashSet<>();
    stati.add( buildHeader() );

    final KnaufReach[] reaches = m_calculation.getReaches();

    monitor.beginTask( Messages.getString( "KnaufCalculationBeanBuilder_0" ), ArrayUtils.getLength( reaches ) ); //$NON-NLS-1$

    for( final KnaufReach reach : reaches )
    {
      final KnaufReachBeanBuilder builder = new KnaufReachBeanBuilder( reach );
      stati.add( builder.execute( new SubProgressMonitor( monitor, ArrayUtils.getLength( reach.getProfiles() ) ) ) );

      addBeans( builder.getBeans() );

      monitor.worked( 1 );
    }

    stati.add( buildFooter() );

    return StatusUtilities.createStatus( stati, Messages.getString( "KnaufCalculationBeanBuilder_1" ) ); //$NON-NLS-1$
  }

  private IStatus buildHeader( )
  {
    addBeans( new KnaufSA10Bean() );
    addBeans( new KnaufSA11Bean( m_calculation ) );
    addBeans( new KnaufSA12Bean( m_calculation ) );
    addBeans( new KnaufSA13Bean() );

    return new Status( IStatus.OK, KalypsoModelWspmTuhhCorePlugin.getID(), Messages.getString( "KnaufCalculationBeanBuilder_2" ) ); //$NON-NLS-1$

  }

  private IStatus buildFooter( )
  {
    addBeans( new KnaufSA91Bean( m_calculation ) );
    addBeans( new KnaufSA94Bean( m_calculation ) );
    addBeans( new KnaufSA99Bean() );

    return new Status( IStatus.OK, KalypsoModelWspmTuhhCorePlugin.getID(), Messages.getString( "KnaufCalculationBeanBuilder_3" ) ); //$NON-NLS-1$
  }

}
