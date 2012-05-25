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
package org.kalypso.ui.rrm.internal.simulations;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.simulations.dialogs.CalculateSimulationDialog;
import org.kalypso.ui.rrm.internal.simulations.runnables.CalculateSimulationRunnable;

/**
 * The simulations handler.
 * 
 * @author Holger Albert
 */
public class SimulationHandler
{
  /**
   * The constructor.
   */
  public SimulationHandler( )
  {
  }

  /**
   * This function checks all simulations for duplicates.
   * 
   * @param allSimulations
   *          All simulations.
   * @return The first name of the simulation, which is a duplicate or null.
   */
  public String findDuplicates( final NAControl[] allSimulations )
  {
    final Set<String> allNames = new HashSet<>();

    for( final NAControl simulation : allSimulations )
    {
      final String name = simulation.getDescription();
      if( allNames.contains( name ) )
        return name;

      allNames.add( name );
    }

    return null;
  }

  /**
   * This function checks the sanity of the simulations (e.g. if they has emtpy names).
   * 
   * @param simulations
   *          The simulations to calculate.
   * @return A OK status if everything is ok.
   */
  public IStatus checkSanity( final NAControl[] simulations )
  {
    /* Check names (= calc case folder names) for empty names. */
    if( hasEmptyNames( simulations ) )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "Simulations with empty name detected. Please rename the simulations." );

    /* Check sanity of selected simulations -> status of simulation. */
    // TODO

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "OK" );
  }

  private boolean hasEmptyNames( final NAControl[] simulations )
  {
    for( final NAControl simulation : simulations )
    {
      if( StringUtils.isEmpty( simulation.getDescription() ) )
        return true;
    }

    return false;
  }

  /**
   * This function calculates the simulations. It will open a dialog and asks for confirmation before starting the
   * calculation.
   * 
   * @param shell
   *          The shell.
   * @param simulations
   *          The simulations to calculate.
   * @return The status of the operation.
   */
  public IStatus calculateSimulation( final Shell shell, final NAControl[] simulations )
  {
    /* Ask the user if he wants to start the calculation. */
    final CalculateSimulationDialog dialog = new CalculateSimulationDialog( shell, simulations );
    if( dialog.open() != Window.OK )
      return new Status( IStatus.CANCEL, KalypsoUIRRMPlugin.getID(), "The calculation was cancled." );

    /* Create the operation. */
    final CalculateSimulationRunnable operation = new CalculateSimulationRunnable( simulations, dialog.isCalculateCatchmentModels(), dialog.isCalculateStartConditions() );

    /* Execute the operation. */
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow workbenchWindow = workbench.getActiveWorkbenchWindow();
    final IWorkbenchPage page = workbenchWindow.getActivePage();
    final IWorkbenchPart part = page.getActivePart();
    final IWorkbenchPartSite site = part.getSite();
    final IProgressService progressService = (IProgressService) site.getService( IProgressService.class );
    final IStatus status = RunnableContextHelper.execute( progressService, true, true, operation );
    if( !status.isOK() )
    {
      /* Log the error message. */
      KalypsoUIRRMPlugin.getDefault().getLog().log( status );

      /* Show an error, if the operation has failed. */
      ErrorDialog.openError( shell, "Calculate Simulations", "Calculation of the simulation has failed...", status );
    }

    return status;
  }
}