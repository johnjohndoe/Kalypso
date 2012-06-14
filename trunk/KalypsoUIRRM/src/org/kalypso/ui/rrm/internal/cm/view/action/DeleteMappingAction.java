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
package org.kalypso.ui.rrm.internal.cm.view.action;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.SimulationCollection;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class DeleteMappingAction extends Action
{
  private final ITimeseriesMapping m_mapping;

  public DeleteMappingAction( final ITimeseriesMapping mapping )
  {
    m_mapping = mapping;

    setText( Messages.getString("DeleteMappingAction_0") ); //$NON-NLS-1$
    setToolTipText( Messages.getString("DeleteMappingAction_1") ); //$NON-NLS-1$

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.DELETE ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    /* Get the shell. */
    final Shell shell = event.widget.getDisplay().getActiveShell();

    try
    {
      /* Check, if the generator is used somewhere. */
      if( areUsed() )
      {
        final String errorMessage = getErrorMessage();
        MessageDialog.openWarning( shell, getText(), errorMessage );
        return;
      }

      /* Get the delete message. */
      final String deleteMessage = getDeleteMessage();
      if( !MessageDialog.openConfirm( shell, getText(), deleteMessage ) )
        return;

      /* Delete the selected mappingss. */
      final DeleteFeatureCommand deleteCommand = new DeleteFeatureCommand( m_mapping );

      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final CommandableWorkspace mappingWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_TIMESERIES_MAPPINGS );

      mappingWorkspace.postCommand( deleteCommand );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to delete mapping(s)", e ); //$NON-NLS-1$
      StatusDialog.open( shell, status, getText() );
    }
  }

  private boolean areUsed( ) throws CoreException
  {
    /* Get the data provider. */
    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    /* Get the workspace of the catchment models and the simulations. */
    final CommandableWorkspace simulationsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_SIMULATIONS );

    /* Get the root features. */
    final SimulationCollection simulationCollection = (SimulationCollection) simulationsWorkspace.getRootFeature();

    /* Get the generators and the simulations. */
    final IFeatureBindingCollection<NAControl> simulations = simulationCollection.getSimulations();

    return isUsed( m_mapping, simulations );
  }

  private boolean isUsed( final ITimeseriesMapping mapping, final IFeatureBindingCollection<NAControl> allSimulations )
  {
    // FIXME: implement, if simulation has reference to mapping

    for( final NAControl oneSimulation : allSimulations )
    {
// final IRainfallGenerator generatorN = oneSimulation.getGeneratorN();
// final IRainfallGenerator generatorE = oneSimulation.getGeneratorE();
// final IRainfallGenerator generatorT = oneSimulation.getGeneratorT();

// if( generatorN != null && generatorN.getId().equals( generator.getId() ) )
// return true;
//
// if( generatorE != null && generatorE.getId().equals( generator.getId() ) )
// return true;
//
// if( generatorT != null && generatorT.getId().equals( generator.getId() ) )
// return true;
    }

    return false;
  }

  private String getErrorMessage( )
  {
// if( m_generators.length > 1 )
// return
// "Some of the selected catchment models are used either in a multi catchment model or a simulation. These cannot be deleted.";

    return String.format( Messages.getString("DeleteMappingAction_2"), m_mapping.getDescription() ); //$NON-NLS-1$
  }

  private String getDeleteMessage( )
  {
//    if( m_generators.length > 1 )
//      return Messages.getString( "DeleteGeneratorAction_3" ); //$NON-NLS-1$
//
    return String.format( "Delete catchment model '%s'? This operation cannot made undone!", m_mapping.getDescription() ); //$NON-NLS-1$
  }
}
