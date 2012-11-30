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

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.calccase.AbstractCatchmentModelRunner;
import org.kalypso.ui.rrm.internal.calccase.ICatchmentModelInfo;
import org.kalypso.ui.rrm.internal.calccase.LinearSumCatchmentModelInfo;
import org.kalypso.ui.rrm.internal.calccase.LinearSumCatchmentModelRunner;
import org.kalypso.ui.rrm.internal.calccase.MultiCatchmentModelInfo;
import org.kalypso.ui.rrm.internal.calccase.MultiCatchmentModelRunner;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Holger Albert
 */
public class CalculateCatchmentModelsWorker implements ICoreRunnableWithProgress
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
   * The simulation data.
   */
  private final INaSimulationData m_simulationData;

  /**
   * The constructor.
   * 
   * @param rrmSimulation
   *          The rrm simulation.
   * @param calculateCatchmentModels
   *          True, if the catchment models should be calculated.
   * @param simulationData
   *          The simulation data.
   */
  public CalculateCatchmentModelsWorker( final RrmSimulation rrmSimulation, final boolean calculateCatchmentModels, final INaSimulationData simulationData )
  {
    m_rrmSimulation = rrmSimulation;
    m_calculateCatchmentModels = calculateCatchmentModels;
    m_simulationData = simulationData;
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
      monitor.beginTask( Messages.getString( "CalculateCatchmentModelsWorker_0" ), 600 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "CalculateCatchmentModelsWorker_1" ) ); //$NON-NLS-1$

      /* Get the some data of the simulation data. */
      final NaModell naModel = m_simulationData.getNaModel();
      final NAControl simulation = m_simulationData.getMetaControl();

      // TODO Copy links of eventually old model.gml, only if cms should not be calculated (no yet!)...
      // TODO Implement condition for not calculating the cm (not yet!)...

      /* Rainfall. */
      final ICatchmentModelInfo infoN = getCatchmentModelInfo( m_rrmSimulation, simulation, naModel, simulation.getGeneratorN(), Catchment.PROP_PRECIPITATION_LINK, ITimeseriesConstants.TYPE_RAINFALL );
      final AbstractCatchmentModelRunner runnerN = getCatchmentModelRunner( infoN );
      if( infoN != null )
        runnerN.executeCatchmentModel( infoN, new SubProgressMonitor( monitor, 100 ) );

      /* Temperature. */
      final ICatchmentModelInfo infoT = getCatchmentModelInfo( m_rrmSimulation, simulation, naModel, simulation.getGeneratorT(), Catchment.PROP_TEMPERATURE_LINK, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE );
      final AbstractCatchmentModelRunner runnerT = getCatchmentModelRunner( infoT );
      if( infoT != null )
        runnerT.executeCatchmentModel( infoT, new SubProgressMonitor( monitor, 100 ) );

      /* Evaporation. */
      final ICatchmentModelInfo infoE = getCatchmentModelInfo( m_rrmSimulation, simulation, naModel, simulation.getGeneratorE(), Catchment.PROP_EVAPORATION_LINK, ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED );
      final AbstractCatchmentModelRunner runnerE = getCatchmentModelRunner( infoE );
      if( infoE != null )
        runnerE.executeCatchmentModel( infoE, new SubProgressMonitor( monitor, 100 ) );

      /* Gauge measurement. */
      final TimeseriesMappingRunner mappingGaugeRunner = new TimeseriesMappingRunner( m_rrmSimulation, simulation, TimeseriesMappingType.gaugeMeasurement );
      mappingGaugeRunner.execute( new SubProgressMonitor( monitor, 100 ) );

      /* Node inflow. */
      final TimeseriesMappingRunner mappingInflowRunner = new TimeseriesMappingRunner( m_rrmSimulation, simulation, TimeseriesMappingType.nodeInflow );
      mappingInflowRunner.execute( new SubProgressMonitor( monitor, 100 ) );

      /* Sea evaporation for storages. */
      final TimeseriesMappingRunner mappingStorageEvaporationRunner = new TimeseriesMappingRunner( m_rrmSimulation, simulation, TimeseriesMappingType.storageEvaporation );
      mappingStorageEvaporationRunner.execute( new SubProgressMonitor( monitor, 100 ) );

      return collector.asMultiStatus( Messages.getString( "CalculateCatchmentModelsWorker_2" ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      /* Add the exception to the log. */
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );

      return collector.asMultiStatus( Messages.getString( "CalculateCatchmentModelsWorker_3" ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private ICatchmentModelInfo getCatchmentModelInfo( final RrmSimulation simulation, final NAControl control, final NaModell model, final IRainfallGenerator generator, final QName targetLink, final String parameterType )
  {
    if( generator == null )
      return null;

    if( generator instanceof ILinearSumGenerator )
      return new LinearSumCatchmentModelInfo( simulation, control, model, (ILinearSumGenerator) generator, targetLink, parameterType );

    if( generator instanceof IMultiGenerator )
      return new MultiCatchmentModelInfo( simulation, control, model, (IMultiGenerator) generator, targetLink, parameterType );

    throw new IllegalArgumentException( Messages.getString( "CalculateCatchmentModelsWorker_4" ) ); // $NON-NLS-1$ //$NON-NLS-1$
  }

  private AbstractCatchmentModelRunner getCatchmentModelRunner( final ICatchmentModelInfo info )
  {
    if( info == null )
      return null;

    if( info instanceof LinearSumCatchmentModelInfo )
      return new LinearSumCatchmentModelRunner( null );

    if( info instanceof MultiCatchmentModelInfo )
      return new MultiCatchmentModelRunner();

    throw new IllegalArgumentException( Messages.getString( "CalculateCatchmentModelsWorker_5" ) ); // $NON-NLS-1$ //$NON-NLS-1$
  }
}