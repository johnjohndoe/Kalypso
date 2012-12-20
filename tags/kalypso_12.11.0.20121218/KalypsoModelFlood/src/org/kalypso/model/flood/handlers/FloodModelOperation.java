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
package org.kalypso.model.flood.handlers;

import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.core.SimulationKalypsoFlood;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.model.flood.util.FloodModelHelper;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.simulation.core.refactoring.ISimulationRunner;
import org.kalypso.simulation.core.refactoring.SimulationRunnerFactory;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.core.util.SimulationUtilitites;

/**
 * @author Gernot Belger
 */
public final class FloodModelOperation implements ICoreRunnableWithProgress
{
  private final IFolder m_scenarioFolder;

  private final Shell m_shell;

  private final IRunoffEvent[] m_eventsToProcess;

  private final IKalypsoCascadingTheme m_wspTheme;

  public FloodModelOperation( final IFolder scenarioFolder, final Shell shell, final IRunoffEvent[] eventsToProcess, final IKalypsoCascadingTheme wspTheme )
  {
    m_scenarioFolder = scenarioFolder;
    m_shell = shell;
    m_eventsToProcess = eventsToProcess;
    m_wspTheme = wspTheme;
  }

  final Modeldata getModeldata( )
  {
    final Map<String, String> inputs = new HashMap<>();
    inputs.put( SimulationKalypsoFlood.INPUT_FLOOD_MODEL, "models/flood.gml" ); //$NON-NLS-1$
    inputs.put( SimulationKalypsoFlood.INPUT_GRID_FOLDER, "grids" ); //$NON-NLS-1$

    final Map<String, String> outputs = new HashMap<>();
    outputs.put( SimulationKalypsoFlood.OUTPUT_FLOOD_MODEL, "models/flood.gml" ); //$NON-NLS-1$
    outputs.put( SimulationKalypsoFlood.OUTPUT_EVENTS_BASE_FOLDER, "events" ); //$NON-NLS-1$

    return SimulationUtilitites.createModelData( SimulationKalypsoFlood.TYPEID, inputs, true, outputs, true );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final Modeldata modeldata = getModeldata();

    final URL scenarioURL = ResourceUtilities.createQuietURL( m_scenarioFolder );

    final Map<String, Object> inputs = SimulationRunnerFactory.resolveInputs( modeldata.getInput() );
    final List<String> outputs = SimulationRunnerFactory.resolveOutputs( modeldata.getOutput() );

    final ISimulationRunner runner = SimulationRunnerFactory.createRunner( modeldata, scenarioURL );
    final IStatus status = runner.run( inputs, outputs, monitor );
    if( !status.isOK() )
      return status;

    // handle results if job is successful
    // add all themes to map
    for( final IRunoffEvent runoffEvent : m_eventsToProcess )
    {
      final int index = FloodModelHelper.findWspTheme( runoffEvent, m_wspTheme );
      try
      {
        FloodModelHelper.addResultTheme( runoffEvent, m_wspTheme, index );
      }
      catch( final Exception e )
      {
        final String message = Messages.getString( "org.kalypso.model.flood.handlers.ProcessFloodModelHandler.14" ) + e.getLocalizedMessage(); //$NON-NLS-1$
        ProcessFloodModelHandler.showErrorDialog( m_shell, message ); //$NON-NLS-1$
        return new Status( IStatus.ERROR, KalypsoModelFloodPlugin.PLUGIN_ID, message, e );
      }
    }

    return new Status( IStatus.OK, KalypsoModelFloodPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.model.flood.handlers.ProcessFloodModelHandler.15" ) ); //$NON-NLS-1$
  }
}