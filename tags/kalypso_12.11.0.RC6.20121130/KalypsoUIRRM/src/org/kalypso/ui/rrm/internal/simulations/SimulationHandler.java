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
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.simulations.dialogs.CalculateSimulationDialog;
import org.kalypso.ui.rrm.internal.simulations.dialogs.SimulationProgressMonitorDialog;
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
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "SimulationHandler_0" ) ); //$NON-NLS-1$

    /* Check sanity of selected simulations -> status of simulation. */
    // TODO

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "SimulationHandler_1" ) ); //$NON-NLS-1$
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
      return new Status( IStatus.CANCEL, KalypsoUIRRMPlugin.getID(), Messages.getString( "SimulationHandler_2" ) ); //$NON-NLS-1$

    /* Create the operation. */
    final CalculateSimulationRunnable operation = new CalculateSimulationRunnable( simulations, dialog.isCalculateCatchmentModels(), dialog.isCalculateStartConditions() );

    /* Execute the operation. */
    final SimulationProgressMonitorDialog progressDialog = new SimulationProgressMonitorDialog( shell );
    final IStatus status = RunnableContextHelper.execute( progressDialog, true, true, operation );

    /* If only one simulation was calculated, descend on level. */
    IStatus statusToSet = status;
    if( status.isMultiStatus() )
    {
      final IStatus[] children = status.getChildren();
      if( children.length == 1 )
        statusToSet = children[0];
    }

    /* Always show dialog. */
    final StatusDialog statusDialog = new StatusDialog( shell, statusToSet, Messages.getString( "SimulationHandler_3" ) ); //$NON-NLS-1$
    statusDialog.open();

    return status;
  }
}