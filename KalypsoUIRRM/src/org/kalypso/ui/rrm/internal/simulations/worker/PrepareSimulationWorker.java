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

import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Holger Albert
 */
public class PrepareSimulationWorker implements ICoreRunnableWithProgress
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
  private final NaModell m_model;

  /**
   * True, if the start conditions should be calculated.
   */
  private final boolean m_calculateStartConditions;

  /**
   * The constructor.
   * 
   * @param simulation
   *          The simulation.
   * @param rrmSimulation
   *          The rrm simulation.
   * @param model
   *          The model.
   * @param calculateStartConditions
   *          True, if the start conditions should be calculated.
   */
  public PrepareSimulationWorker( final NAControl simulation, final RrmSimulation rrmSimulation, final NaModell model, final boolean calculateStartConditions )
  {
    m_simulation = simulation;
    m_rrmSimulation = rrmSimulation;
    m_model = model;
    m_calculateStartConditions = calculateStartConditions;
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

    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* Monitor. */
      monitor.beginTask( "Preparing simulation...", 200 );
      monitor.subTask( "Preparing simulation..." );

      /* Determine, if it is a longterm simulation. */
      final boolean isLongterm = isLongterm( m_simulation );
      if( isLongterm )
      {
        /* Update status. */
        collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "This is a longterm simulation." ) );

        /* If the start conditions should be calculated. */
        if( m_calculateStartConditions )
        {
          /* Update status. */
          collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Start conditions should be calculated." ) );

          /* We need to manipulate the model.gml, activating all result flags. */
          final IFeatureBindingCollection<Node> nodes = m_model.getNodes();
          for( final Node node : nodes )
            node.setGenerateResults( true );

          /* Copy expertcontrol.gml. */
          // TODO

          /* Set list of start condition times of the referencing shortterm simulations (first time there). */
          // TODO
        }
        else
        {
          /* Update status. */
          collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Start conditions should not be calculated." ) );

          /* HINT: Longterm simulations without calculation of the start conditions */
          /* HINT: will use the global expertcontrol.gml later. */
        }
      }
      else
      {
        /* Update status. */
        collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "This is a shortterm simulation." ) );

        /* If a longterm simulation is referenced. */
        final String initialValueSource = m_simulation.getInitialValueSource();
        if( initialValueSource != null && initialValueSource.length() > 0 )
        {
          /* Update status. */
          collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "It does reference a longterm simulation." ) );

          /* Copy the lzsim data. */
          copyInitialCondition( m_simulation, m_rrmSimulation );
        }
        else
        {
          /* Update status. */
          collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "It does not reference a longterm simulation." ) );

          /* HINT: In this case the lzsim data is not copied. */
        }

        /* HINT: Shortterm simulations will use the global expertcontrol.gml later. */
      }

      /* Monitor. */
      monitor.worked( 200 );

      return collector.asMultiStatus( "Peparation of the simulation was successfull." );
    }
    catch( final Exception ex )
    {
      /* Add the exception to the log. */
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );

      return collector.asMultiStatus( "Error during preparation of the simulation." );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function returns true, if the simulation is a longterm simulation.
   * 
   * @param simulation
   *          The simulation.
   * @return True, if the simulation is a longterm simulation.
   */
  private boolean isLongterm( final NAControl simulation )
  {
    final Integer timestep = simulation.getMinutesOfTimestep();
    if( timestep != null && timestep.intValue() == 1440 )
      return true;

    return false;
  }

  private IStatus copyInitialCondition( final NAControl control, final RrmSimulation rrmSimulation )
  {
    final String calcCaseNameSource = control.getInitialValueSource();
    if( StringUtils.isBlank( calcCaseNameSource ) )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "No longterm simulation is set." );

    final RrmScenario scenario = rrmSimulation.getScenario();
    final IFolder folderCalcCases = scenario.getSimulationsFolder();
    final RrmSimulation sourceCalcCase = new RrmSimulation( folderCalcCases.getFolder( new Path( calcCaseNameSource ) ) );
    if( !sourceCalcCase.exists() )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( "Failed to copy initial values from simulation '%s': Simulation does not exist.", sourceCalcCase.getName() ) );

    final IFolder currentSourceFolder = sourceCalcCase.getCurrentResultsFolder();
    if( !currentSourceFolder.exists() )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( "Failed to copy initial values from simulation '%s': No results available.", sourceCalcCase.getName() ) );

    final IFolder initialValuesSourceFolder = sourceCalcCase.getCurrentLzimResultFolder();
    final Date startDate = control.getSimulationStart();
    final String initialValuesSourceFilename = new SimpleDateFormat( "yyyyMMdd'.gml'" ).format( startDate );
    final IFile initialValuesSourceFile = initialValuesSourceFolder.getFile( initialValuesSourceFilename );
    if( !initialValuesSourceFile.exists() )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( "Failed to copy initial values from simulation '%s': Initial values missing (%s).", sourceCalcCase.getName(), initialValuesSourceFilename ) );

    try
    {
      final IFile initialValuesTargetFile = rrmSimulation.getLzsimGml();
      FileUtils.copyFile( initialValuesSourceFile.getLocation().toFile(), initialValuesTargetFile.getLocation().toFile() );
      initialValuesSourceFile.getParent().refreshLocal( IResource.DEPTH_INFINITE, null );

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( "Initial values were copied from simulation '%s'.", sourceCalcCase.getName() ) );
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "Failed to copy initial values from simulation '%s'.", sourceCalcCase.getName() ), e );
    }
  }
}