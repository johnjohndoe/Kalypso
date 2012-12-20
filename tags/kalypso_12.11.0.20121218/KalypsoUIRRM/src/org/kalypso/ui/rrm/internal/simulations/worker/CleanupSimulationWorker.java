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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Holger Albert
 */
public class CleanupSimulationWorker implements ICoreRunnableWithProgress
{
  /**
   * The rrm simulation.
   */
  private final RrmSimulation m_rrmSimulation;

  /**
   * True, if the catchment models should be calculated.
   */
  private final boolean m_calculateCatchmentModels;

  /**
   * The constructor.
   * 
   * @param rrmSimulation
   *          The rrm simulation.
   * @param calculateCatchmentModels
   *          True, if the catchment models should be calculated.
   */
  public CleanupSimulationWorker( final RrmSimulation rrmSimulation, final boolean calculateCatchmentModels )
  {
    m_rrmSimulation = rrmSimulation;
    m_calculateCatchmentModels = calculateCatchmentModels;
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
      monitor.beginTask( Messages.getString("CleanupSimulationWorker_0"), 300 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString("CleanupSimulationWorker_1") ); //$NON-NLS-1$

      /* Delete the calculation gml. */
      final IFile calculationGml = m_rrmSimulation.getCalculationGml();
      if( calculationGml.exists() )
        calculationGml.delete( false, new SubProgressMonitor( monitor, 100 ) );

      /* Delete the model.gml (only if the cms should be calculated). */
      // TODO Implement the condition for the case the cms should not be calculated...
      final IFile modelGml = m_rrmSimulation.getModelGml();
      if( modelGml.exists() )
        modelGml.delete( false, new SubProgressMonitor( monitor, 100 ) );

      /* Delete the timeseries results (only if the cms should be calculated). */
      // TODO Implement the condition for the case the cms should not be calculated...
      final IFolder[] folders = m_rrmSimulation.getTimeseriesFolders();
      for( final IFolder folder : folders )
      {
        /* Empty the folder. */
        emptyFolder( folder );

        /* Monitor. */
        monitor.worked( 100 / folders.length );
      }

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("CleanupSimulationWorker_2") ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("CleanupSimulationWorker_3"), ex ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private void emptyFolder( final IFolder folder ) throws CoreException
  {
    if( folder == null || !folder.exists() )
      return;

    final IResource[] members = folder.members();
    for( final IResource member : members )
      member.delete( false, null );
  }
}