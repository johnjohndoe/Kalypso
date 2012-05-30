/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.binding.InitialValue;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.control.SimulationCollection;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.simulations.SimulationUtilities;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Holger Albert
 */
public class PrepareLongtermSimulationWorker implements ICoreRunnableWithProgress
{
  /**
   * True, if the start conditions should be calculated.
   */
  private final boolean m_calculateStartConditions;

  /**
   * The simulation data.
   */
  private final INaSimulationData m_simulationData;

  /**
   * The constructor.
   * 
   * @param calculateStartConditions
   *          True, if the start conditions should be calculated.
   * @param simulationData
   *          The simulation data.
   */
  public PrepareLongtermSimulationWorker( final boolean calculateStartConditions, final INaSimulationData simulationData )
  {
    m_calculateStartConditions = calculateStartConditions;
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
      monitor.beginTask( "Preparing simulation...", 200 );
      monitor.subTask( "Preparing simulation..." );

      /* Get the some data of the simulation data. */
      final NaModell naModel = m_simulationData.getNaModel();
      final NAControl simulation = m_simulationData.getMetaControl();

      /* Determine, if it is a longterm simulation. */
      final boolean isLongterm = SimulationUtilities.isLongterm( simulation );
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
          final IFeatureBindingCollection<Node> nodes = naModel.getNodes();
          for( final Node node : nodes )
            node.setGenerateResults( true );

          /* Change expertControl.gml. */
          final NAModellControl naControl = m_simulationData.getNaControl();
          final IFeatureBindingCollection<InitialValue> initialValues = naControl.getInitialValues();

          /* Set list of start condition times of the referencing shortterm simulations (first time there). */
          final NAControl[] referencingSimulations = findReferencingShortTermSimulations( simulation );
          for( final NAControl referencingSimulation : referencingSimulations )
          {
            final Date simulationStart = referencingSimulation.getSimulationStart();
            final InitialValue initialValue = initialValues.addNew( InitialValue.FEATURE_INITIAL_VALUE );
            initialValue.setActive( true );
            initialValue.setInitialDate( simulationStart );
          }
        }
        else
        {
          /* Update status. */
          collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Start conditions should not be calculated." ) );

          /* HINT: Longterm simulations without calculation of the start conditions */
          /* HINT: will use an unchanged expertControl.gml later. */
        }
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

  private NAControl[] findReferencingShortTermSimulations( final NAControl simulation )
  {
    final Collection<NAControl> results = new ArrayList<NAControl>();

    final String description = simulation.getDescription();
    final SimulationCollection owner = (SimulationCollection) simulation.getOwner();
    final IFeatureBindingCollection<NAControl> allSimulations = owner.getSimulations();
    for( final NAControl oneSimulation : allSimulations )
    {
      if( SimulationUtilities.isLongterm( oneSimulation ) )
        continue;

      final String initialValueSource = oneSimulation.getInitialValueSource();
      if( initialValueSource == null || initialValueSource.length() == 0 )
        continue;

      if( initialValueSource.equals( description ) )
        results.add( oneSimulation );
    }

    return results.toArray( new NAControl[results.size()] );
  }
}