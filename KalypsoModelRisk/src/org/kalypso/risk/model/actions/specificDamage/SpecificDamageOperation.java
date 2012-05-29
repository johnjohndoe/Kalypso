/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.model.actions.specificDamage;

import java.net.URL;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.simulation.ISimulationSpecKalypsoRisk.SIMULATION_KALYPSORISK_TYPEID;
import org.kalypso.risk.model.simulation.SimulationKalypsoRiskModelspecHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.simulation.core.refactoring.ISimulationRunner;
import org.kalypso.simulation.core.refactoring.SimulationRunnerFactory;
import org.kalypso.simulation.core.simspec.Modeldata;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class SpecificDamageOperation implements ICoreRunnableWithProgress
{
  private final IScenarioDataProvider m_scenarioDataProvider;

  private final IFolder m_scenarioFolder;

  public SpecificDamageOperation( final IScenarioDataProvider scenarioDataProvider, final IFolder scenarioFolder )
  {
    m_scenarioDataProvider = scenarioDataProvider;
    m_scenarioFolder = scenarioFolder;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final IRasterizationControlModel rasterizationControlModel = m_scenarioDataProvider.getModel( IRasterizationControlModel.MODEL_ID );

    if( rasterizationControlModel.getAssetValueClassesList().size() == 0 )
      return new Status( IStatus.WARNING, KalypsoRiskPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.risk.model.actions.specificDamage.DamagePotentialCalculationHandler.8" ) ); //$NON-NLS-1$

    final Modeldata modeldata = SimulationKalypsoRiskModelspecHelper.getModeldata( SIMULATION_KALYPSORISK_TYPEID.SPECIFIC_DAMAGE_CALCULATION );

    final URL scenarioURL = ResourceUtilities.createQuietURL( m_scenarioFolder );

    final Map<String, Object> inputs = SimulationRunnerFactory.resolveInputs( modeldata.getInput() );
    final List<String> outputs = SimulationRunnerFactory.resolveOutputs( modeldata.getOutput() );

    final ISimulationRunner runner = SimulationRunnerFactory.createRunner( modeldata, scenarioURL );
    return runner.run( inputs, outputs, monitor );
  }
}