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
package org.kalypso.model.wspm.pdb.ui.preferences.internal;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;

/**
 * @author Gernot Belger
 */
public class ValidateSettingsOperation implements ICoreRunnableWithProgress
{
  private final IPdbSettings m_settings;

  private IStatus m_result;

  public ValidateSettingsOperation( final IPdbSettings settings )
  {
    m_settings = settings;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InterruptedException
  {
    monitor.beginTask( "Testing connection...", IProgressMonitor.UNKNOWN );

    final ConnectJob job = new ConnectJob( m_settings );

    final IJobChangeListener listener = new JobChangeAdapter()
    {
      @Override
      public void done( final IJobChangeEvent event )
      {
        handleDone( job.getConnectionStatus() );
      }
    };
    job.addJobChangeListener( listener );
    job.setUser( false );
    job.setSystem( true );
    job.schedule( 0 );

    while( !monitor.isCanceled() && m_result == null )
    {
      monitor.worked( 1 );

      Thread.sleep( 100 );
    }

    if( monitor.isCanceled() )
    {
      job.cancel();
      return Status.CANCEL_STATUS;
    }

    return m_result;
  }

  protected void handleDone( final IStatus result )
  {
    m_result = result;
  }
}