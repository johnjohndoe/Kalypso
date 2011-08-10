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
package org.kalypso.model.wspm.pdb.internal.wspm;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;

/**
 * @author Gernot Belger
 */
public class CheckoutPdbOperation implements ICoreRunnableWithProgress
{
  private final CheckoutDataMapping m_mapping;

  public CheckoutPdbOperation( final CheckoutDataMapping mapping )
  {
    m_mapping = mapping;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Loading data from cross section database", 100 );

    // FIXME: overwrite existing data ->
    // First: create/update all water bodies
    // Second: delete all existing states
    // third: delete all existing water levels
    // last: download data as before

    final CheckoutWaterBodyWorker waterBodyWorker = new CheckoutWaterBodyWorker( m_mapping );
    waterBodyWorker.execute( new SubProgressMonitor( monitor, 10 ) );

    final CheckoutStateWorker stateWorker = new CheckoutStateWorker( m_mapping );
    stateWorker.execute( new SubProgressMonitor( monitor, 10 ) );

    final CheckoutCrossSectionsWorker crossSectionsWorker = new CheckoutCrossSectionsWorker( m_mapping );
    crossSectionsWorker.execute( new SubProgressMonitor( monitor, 45 ) );

    final CheckoutWaterlevelWorker waterlevelWorker = new CheckoutWaterlevelWorker( m_mapping );
    waterlevelWorker.execute( new SubProgressMonitor( monitor, 45 ) );

    m_mapping.fireEvents( new SubProgressMonitor( monitor, 5 ) );

    ProgressUtilities.done( monitor );

    return Status.OK_STATUS;
  }


}