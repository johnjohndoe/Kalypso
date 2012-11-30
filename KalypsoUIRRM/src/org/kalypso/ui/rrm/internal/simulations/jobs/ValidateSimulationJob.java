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
import org.kalypso.ui.rrm.internal.i18n.Messages;
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
    super( "ValidateSimulationJob" ); //$NON-NLS-1$

    m_simulation = simulation;
    m_control = control;
    m_validationStatus = new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_1" ) ); //$NON-NLS-1$
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
      monitor.beginTask( Messages.getString( "ValidateSimulationJob_2" ), 1000 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "ValidateSimulationJob_3" ) ); //$NON-NLS-1$

      /* Get the calculation status gml. */
      final IFile calculationStatusGml = m_simulation.getCurrentCalculationResult().getCalculationStatusGml();
      if( calculationStatusGml.exists() )
      {
        /* The last modified timestamp of the results of the simulation. */
        final long lastModifiedResults = getLastModifiedResults( calculationStatusGml );

        /* Validate the simulation. */
        m_validationStatus = validateSimulation( lastModifiedResults, monitor );
      }
      else
        m_validationStatus = new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_4" ) ); //$NON-NLS-1$

      return Status.OK_STATUS;
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_5" ), ex ); //$NON-NLS-1$
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

      return collector.asMultiStatusOrOK( Messages.getString( "ValidateSimulationJob_6" ), Messages.getString( "ValidateSimulationJob_7" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_8" ), ex ); //$NON-NLS-1$
    }
  }

  private IStatus validateGenerator( final IRainfallGenerator generator, final long lastModifiedResults )
  {
    if( generator == null )
      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("ValidateSimulationJob.0") ); //$NON-NLS-1$

    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final Date lastModified = generator.getLastModified();
    if( lastModified != null && lastModified.getTime() > lastModifiedResults )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_9" ) ) ); //$NON-NLS-1$
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_10" ) ) ); //$NON-NLS-1$

    if( generator instanceof ILinearSumGenerator )
    {
      final ILinearSumGenerator linearGenerator = (ILinearSumGenerator) generator;

      final long lastModifiedTimeseries = linearGenerator.getLastModifiedTimeseries();
      if( lastModifiedTimeseries > lastModifiedResults )
        collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_11" ) ) ); //$NON-NLS-1$
      else
        collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_12" ) ) ); //$NON-NLS-1$

      final long lastModifiedCatchments = linearGenerator.getLastModifiedCatchments();
      if( lastModifiedCatchments > lastModifiedResults )
        collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_13" ) ) ); //$NON-NLS-1$
      else
        collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_14" ) ) ); //$NON-NLS-1$
    }

    if( generator instanceof IMultiGenerator )
    {
      final IMultiGenerator multiGenerator = (IMultiGenerator) generator;

      final long lastModifiedSubGenerators = multiGenerator.getLastModifiedSubGenerators();
      if( lastModifiedSubGenerators > lastModifiedResults )
        collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_15" ) ) ); //$NON-NLS-1$
      else
        collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_16" ) ) ); //$NON-NLS-1$
    }

    return collector.asMultiStatus( String.format( Messages.getString( "ValidateSimulationJob_17" ), generator.getDescription(), ParameterTypeUtils.formatParameterType( generator.getParameterType() ) ) ); //$NON-NLS-1$
  }

  private IStatus validateSimulation( final NAControl control, final long lastModifiedResults )
  {
    if( control == null )
      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("ValidateSimulationJob.1") ); //$NON-NLS-1$

    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final Date lastModified = control.getLastModified();
    if( lastModified != null && lastModified.getTime() > lastModifiedResults )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_18" ) ) ); //$NON-NLS-1$
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_19" ) ) ); //$NON-NLS-1$

    final long lastModifiedGenerators = control.getLastModifiedGenerators();
    if( lastModifiedGenerators > lastModifiedResults )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_20" ) ) ); //$NON-NLS-1$
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_21" ) ) ); //$NON-NLS-1$

    final long lastModifiedInputData = control.getLastModifiedInputData();
    if( lastModifiedInputData > lastModifiedResults )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_22" ) ) ); //$NON-NLS-1$
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "ValidateSimulationJob_23" ) ) ); //$NON-NLS-1$

    return collector.asMultiStatus( String.format( Messages.getString( "ValidateSimulationJob_24" ), control.getDescription() ) ); //$NON-NLS-1$
  }
}