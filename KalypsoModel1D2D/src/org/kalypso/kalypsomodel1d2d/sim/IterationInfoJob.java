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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.IOException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;

/**
 * Monitors the .itr file and updates the iteration info.<br>
 * The job runs until it is cancelled.
 * 
 * @author Gernot Belger
 * 
 */
public class IterationInfoJob extends Job
{
  private IterationInfo m_iterationInfo = null;

  private final IProgressMonitor m_monitor;

  private final IControlModel1D2D m_controlModel;

  public IterationInfoJob( final IterationInfo info, final IControlModel1D2D model, final IProgressMonitor monitor )
  {
    super( "IterationInfoJob" ); //$NON-NLS-1$

    m_monitor = SubMonitor.convert( monitor );
    m_controlModel = model;
    m_iterationInfo = info;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    while( !monitor.isCanceled() )
    {
      try
      {
        updateIteration();
        Thread.sleep( 500 );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        return StatusUtilities.statusFromThrowable( e );
      }
    }
    return Status.OK_STATUS;
  }

  /**
   * Will be called while the rma10s process is running.<br>
   * Updates the calculation progress monitor and reads the Output.itr.
   */
  protected void updateIteration( ) throws IOException
  {
    final int oldStepNr = m_iterationInfo.getStepNr();

    m_iterationInfo.readIterFile();

    final int stepNr = m_iterationInfo.getStepNr();
    if( oldStepNr != stepNr )
    {
      String msg = ""; //$NON-NLS-1$
      if( stepNr == 0 )
        msg =  Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfoJob.2"  ); //$NON-NLS-1$
      else
        msg =  Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfoJob.3" , stepNr, m_controlModel.getNCYC() ); //$NON-NLS-1$

      m_monitor.subTask( msg );
      m_monitor.worked( stepNr - oldStepNr );
    }
  }

  public void finish( ) throws IOException
  {
    if( m_iterationInfo != null )
    {
      updateIteration(); // update one last time
      m_iterationInfo.finish(); // save the last observation
    }

    cancel();
  }

}
