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
package org.kalypso.ui.rrm.internal.simulations.worker;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Holger Albert
 */
public class CreateSimulationWorker implements ICoreRunnableWithProgress
{
  /**
   * The rrm simulation.
   */
  private final RrmSimulation m_rrmSimulation;

  /**
   * The constructor.
   * 
   * @param rrmSimulation
   *          The rrm simulation.
   */
  public CreateSimulationWorker( final RrmSimulation rrmSimulation )
  {
    m_rrmSimulation = rrmSimulation;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    /* If no monitor is given, take a null progress monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString("CreateSimulationWorker_0"), 200 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString("CreateSimulationWorker_1") ); //$NON-NLS-1$

      /* Create the simulation folder. */
      final IFolder simulationFolder = m_rrmSimulation.getSimulationFolder();
      simulationFolder.create( false, true, null );

      /* Copy template. */
      final RrmProject project = m_rrmSimulation.getProject();
      final IFolder calcCaseTemplateFolder = project.getCalcCaseTemplateFolder();
      copyCalcCaseTemplate( calcCaseTemplateFolder, simulationFolder );

      /* Monitor. */
      monitor.worked( 200 );

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("CreateSimulationWorker_2") ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("CreateSimulationWorker_3"), ex ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function copies the contents of the calc case template folder into the simulation folder.
   * 
   * @param calcCaseTemplateFolder
   *          The calc case template folder.
   * @param simulationFolder
   *          The simulation folder.
   */
  private void copyCalcCaseTemplate( final IFolder calcCaseTemplateFolder, final IFolder simulationFolder ) throws CoreException
  {
    final IResource[] members = calcCaseTemplateFolder.members();
    for( final IResource member : members )
    {
      final IPath target = simulationFolder.getFullPath().append( member.getName() );
      member.copy( target, false, null );
    }
  }
}