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
package org.kalypso.ui.rrm.internal.simulations.jobs;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

/**
 * This job validates the simulation and provides the validation status.
 * 
 * @author Holger Albert
 */
public class ValidateSimulationJob extends Job
{
  /**
   * The na control.
   */
  private final NAControl m_control;

  /**
   * The validation status.
   */
  private IStatus m_validationStatus;

  /**
   * The constructor.
   * 
   * @param control
   *          The na control.
   */
  public ValidateSimulationJob( final NAControl control )
  {
    super( "ValidateSimulationJob" );

    m_control = control;
    m_validationStatus = new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Not available." );
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( "Validating the simulation...", 1000 );
      monitor.subTask( "Validating the simulation..." );

      /* Validate the simulation. */
      m_validationStatus = validateSimulation();

      /* Monitor. */
      monitor.worked( 1000 );

      return Status.OK_STATUS;
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function returns the validation status.
   * 
   * @return The validation status.
   */
  public IStatus getValidationStatus( )
  {
    return m_validationStatus;
  }

  /**
   * This function validates the simulation and returns the validation status.
   * 
   * @return The validation status.
   */
  private IStatus validateSimulation( )
  {
    try
    {
      final boolean outdated = m_control.isOutdated();
      if( outdated )
        return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "The results are outdated." );

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The results are uptodate." );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Validation has failed.", ex );
    }
  }
}