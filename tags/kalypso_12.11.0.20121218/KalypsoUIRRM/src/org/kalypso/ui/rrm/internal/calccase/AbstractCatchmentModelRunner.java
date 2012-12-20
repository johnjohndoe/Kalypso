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
package org.kalypso.ui.rrm.internal.calccase;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.model.hydrology.project.RrmSimulation;

/**
 * This class executes a catchment model.
 * 
 * @author Holger Albert
 */
public abstract class AbstractCatchmentModelRunner
{
  /**
   * The constructor.
   */
  public AbstractCatchmentModelRunner( )
  {
    super();
  }

  /**
   * This function executes the catchment model.
   * 
   * @param info
   *          The catchment model info.
   * @param monitor
   *          A progress monitor.
   */
  public abstract void executeCatchmentModel( ICatchmentModelInfo info, final IProgressMonitor monitor ) throws CoreException;

  /**
   * This function refreshs the simulation folder. It does not throw an exception.
   * 
   * @param simulation
   *          The simulation.
   */
  protected void refresh( final RrmSimulation simulation )
  {
    try
    {
      /* Refresh the simulation folder. */
      simulation.getSimulationFolder().refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
    }
    catch( final Exception e )
    {
      /* REMARK: give priority to other exception, so we just system out it. */
      e.printStackTrace();
    }
  }
}