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
package org.kalypso.services.observation.server;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.repository.RepositoryException;
import org.kalypso.services.observation.sei.IObservationService;

/**
 * This job initializes the observation service and sets it to the observation service wrapper.
 * 
 * @author Holger Albert
 */
public class ObservationServiceJob extends Job
{
  /**
   * This wrapper also implements the {@link IObservationService}-Interface. It uses this job for reloading the
   * observation service.
   */
  private final ObservationServiceImpl m_observationServiceWrapper;

  public ObservationServiceJob( final ObservationServiceImpl observationServiceWrapper )
  {
    super( "ObservationServiceJob" );

    m_observationServiceWrapper = observationServiceWrapper;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      /* Monitor. */
      if( monitor.isCanceled() )
        return Status.CANCEL_STATUS;

      /* Create the observation service. */
      final IObservationService observationService = new ObservationServiceDelegate();

      /* Monitor. */
      if( monitor.isCanceled() )
        return Status.CANCEL_STATUS;

      /* Dispose the old delegate. */
      final ObservationServiceDelegate delegate = (ObservationServiceDelegate) m_observationServiceWrapper.getDelegateInternal();
      if( delegate != null )
      {
        // System.out.println( "Dispose the old observation service delegate (" + delegate.toString() + ") ... " +
        // DateFormat.getDateTimeInstance().format( Calendar.getInstance().getTime() ) );
        delegate.dispose();
      }

      /* Store the result. */
      // System.out.println( "Set the new observation service delegate (" + observationService.toString() + ") ... " +
      // DateFormat.getDateTimeInstance().format( Calendar.getInstance().getTime() ) );
      m_observationServiceWrapper.setDelegate( observationService );

      return Status.OK_STATUS;
    }
    catch( final RepositoryException ex )
    {
      return StatusUtilities.statusFromThrowable( ex );
    }
  }
}