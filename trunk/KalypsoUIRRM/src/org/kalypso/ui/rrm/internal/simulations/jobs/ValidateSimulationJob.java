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

import java.util.Date;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.utils.ParameterTypeUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.StatusCollection;

/**
 * This job validates the simulation and provides the validation status.
 * 
 * @author Holger Albert
 */
public class ValidateSimulationJob extends Job
{
  /**
   * The simulation.
   */
  private final RrmSimulation m_simulation;

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
   * @param simulation
   *          The simulation.
   * @param control
   *          The na control.
   */
  public ValidateSimulationJob( final RrmSimulation simulation, final NAControl control )
  {
    super( "ValidateSimulationJob" );

    m_simulation = simulation;
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

      /* Get the calculation status gml. */
      final IFile calculationStatusGml = m_simulation.getCalculationStatusGml();
      if( calculationStatusGml.exists() )
      {
        /* The last modified timestamp of the results of the simulation. */
        final long lastModifiedResults = getLastModifiedResults( calculationStatusGml );

        /* Validate the simulation. */
        m_validationStatus = validateSimulation( lastModifiedResults, monitor );
      }
      else
        m_validationStatus = new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "The simulation was not calculated." );

      return Status.OK_STATUS;
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Reading the calculation status has failed.", ex );
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

  private long getLastModifiedResults( final IFile calculationStatusGml ) throws Exception
  {
    /* Load the workspace. */
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( calculationStatusGml );

    /* Get the root feature. */
    final Feature rootFeature = workspace.getRootFeature();

    /* Cast to status collection. */
    final StatusCollection statusCollection = (StatusCollection) rootFeature;
    final IFeatureBindingCollection<IGeoStatus> stati = statusCollection.getStati();
    final IGeoStatus status = stati.get( stati.size() - 1 );
    final Date time = status.getTime();
    if( time != null )
      return time.getTime();

    return calculationStatusGml.getLocalTimeStamp();
  }

  /**
   * This function validates the simulation and returns the validation status.
   * 
   * @param lastModifiedResults
   *          The last modified timestamp of the results of the simulation.
   * @param monitor
   *          A progress monitor.
   * @return The validation status.
   */
  private IStatus validateSimulation( final long lastModifiedResults, final IProgressMonitor monitor )
  {
    try
    {
      final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

      collector.add( validateGenerator( m_control.getGeneratorN(), lastModifiedResults ) );
      monitor.worked( 250 );

      collector.add( validateGenerator( m_control.getGeneratorT(), lastModifiedResults ) );
      monitor.worked( 250 );

      collector.add( validateGenerator( m_control.getGeneratorE(), lastModifiedResults ) );
      monitor.worked( 250 );

      collector.add( validateSimulation( m_control, lastModifiedResults ) );
      monitor.worked( 250 );

      return collector.asMultiStatusOrOK( "Results are outdated.", "Results are up to date." );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Validation has failed.", ex );
    }
  }

  private IStatus validateGenerator( final IRainfallGenerator generator, final long lastModifiedResults )
  {
    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final long lastModified = generator.getLastModified();
    if( lastModified > lastModifiedResults )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "Some values of the generator has changed." ) );
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The values are unchanged." ) );

    if( generator instanceof ILinearSumGenerator )
    {
      final ILinearSumGenerator linearGenerator = (ILinearSumGenerator) generator;

      final long lastModifiedTimeseries = linearGenerator.getLastModifiedTimeseries();
      if( lastModifiedTimeseries > lastModifiedResults )
        collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "Some timeseries of the linear sum generator has changed." ) );
      else
        collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The timeseries are unchanged." ) );

      final long lastModifiedCatchments = linearGenerator.getLastModifiedCatchments();
      if( lastModifiedCatchments > lastModifiedResults )
        collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "Some catchments of the linear sum generator has changed." ) );
      else
        collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The catchments are unchanged." ) );
    }

    if( generator instanceof IMultiGenerator )
    {
      final IMultiGenerator multiGenerator = (IMultiGenerator) generator;

      final long lastModifiedSubGenerators = multiGenerator.getLastModifiedSubGenerators();
      if( lastModifiedSubGenerators > lastModifiedResults )
        collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "Some sub generators of the multi generator has changed." ) );
      else
        collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The sub generators are unchanged." ) );
    }

    return collector.asMultiStatus( String.format( "Catchment Model %s (%s)", generator.getDescription(), ParameterTypeUtils.formatParameterType( generator.getParameterType() ) ) );
  }

  private IStatus validateSimulation( final NAControl control, final long lastModifiedResults )
  {
    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final long lastModified = control.getLastModified();
    if( lastModified > lastModifiedResults )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "Some values of the simulation has changed." ) );
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The values are unchanged." ) );

    final long lastModifiedGenerators = control.getLastModifiedGenerators();
    if( lastModifiedGenerators > lastModifiedResults )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "Some generators of the simulation has changed." ) );
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The generators are unchanged." ) );

    final long lastModifiedInputData = control.getLastModifiedInputData();
    if( lastModifiedInputData > lastModifiedResults )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "Some input data of the simulation has changed." ) );
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The input data is unchanged." ) );

    return collector.asMultiStatus( String.format( "Simulation %s", control.getDescription() ) );
  }
}