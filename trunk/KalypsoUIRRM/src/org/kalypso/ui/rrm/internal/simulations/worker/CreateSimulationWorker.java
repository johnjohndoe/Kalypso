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
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Holger Albert
 */
public class CreateSimulationWorker implements ICoreRunnableWithProgress
{
  /**
   * The simulation.
   */
  private final NAControl m_simulation;

  /**
   * The rrm simulation.
   */
  private final RrmSimulation m_rrmSimulation;

  /**
   * The model.
   */
  private NaModell m_model;

  /**
   * The constructor.
   * 
   * @param simulation
   *          The simulation.
   * @param rrmSimulation
   *          The rrm simulation.
   */
  public CreateSimulationWorker( final NAControl simulation, final RrmSimulation rrmSimulation )
  {
    m_simulation = simulation;
    m_rrmSimulation = rrmSimulation;
    m_model = null;
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
      monitor.beginTask( "Creating simulation...", 200 );
      monitor.subTask( "Creating simulation..." );

      /* Create the simulation folder. */
      final IFolder simulationFolder = m_rrmSimulation.getSimulationFolder();
      simulationFolder.create( false, true, null );

      /* Copy template. */
      final RrmProject project = m_rrmSimulation.getProject();
      final IFolder calcCaseTemplateFolder = project.getCalcCaseTemplateFolder();
      copyCalcCaseTemplate( calcCaseTemplateFolder, simulationFolder );

      /* Save the calculation.gml. */
      final IFile calculationGml = m_rrmSimulation.getCalculationGml();
      final GMLWorkspace simulationWorkspace = FeatureFactory.createGMLWorkspace( m_simulation.getFeatureType(), null, null );
      final Feature simulationFeature = simulationWorkspace.getRootFeature();
      FeatureHelper.copyData( m_simulation, simulationFeature );
      GmlSerializer.saveWorkspace( simulationWorkspace, calculationGml );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( "Loading model.gml..." );

      /* Load the model.gml. */
      final RrmScenario scenario = m_rrmSimulation.getScenario();
      final IFile modelFile = scenario.getModelFile();
      final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( modelFile );
      m_model = (NaModell) modelWorkspace.getRootFeature();

      /* Monitor. */
      monitor.worked( 100 );

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "Creation of the simulation was successfull." );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Error during creation of the simulation.", ex );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function returns model.
   * 
   * @return The model or null.
   */
  public NaModell getModel( )
  {
    return m_model;
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
      member.copy( simulationFolder.getFullPath(), false, null );
  }
}