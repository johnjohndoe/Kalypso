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
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.kalypso.afgui.model.IModel;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Gernot Belger
 */
public class ImportWSPMHandler extends AbstractHandler
{
  /**
   * @see org.kalypso.ui.command.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  @SuppressWarnings("unchecked")
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ICaseDataProvider<IModel> modelProvider = (ICaseDataProvider<IModel>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

    ITerrainModel terrainModel;
    try
    {
      terrainModel = modelProvider.getModel( ITerrainModel.class );
      final IFEDiscretisationModel1d2d discModel = modelProvider.getModel( IFEDiscretisationModel1d2d.class );
      final IFlowRelationshipModel flowRelationModel = modelProvider.getModel( IFlowRelationshipModel.class );

      if( discModel == null )
        return Status.CANCEL_STATUS;

      /* Import Reach into Terrain-Model */
      final IRiverProfileNetworkCollection networkModel = terrainModel.getRiverProfileNetworkCollection();

      final ImportWspmWizard importWizard = new ImportWspmWizard( discModel, networkModel, flowRelationModel );
      importWizard.setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModel1D2DPlugin.getDefault(), getClass().getName() ) );

      final WizardDialog2 dialog = new WizardDialog2( shell, importWizard );
      dialog.setRememberSize( true );
      if( dialog.open() != Window.OK )
        return Status.CANCEL_STATUS;

      try
      {
        /* post empty command(s) in order to make pool dirty. */
        ((SzenarioDataProvider) modelProvider).postCommand( ITerrainModel.class, new EmptyCommand( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportWSPMHandler.0" ), false ) ); //$NON-NLS-1$
        ((SzenarioDataProvider) modelProvider).postCommand( IFEDiscretisationModel1d2d.class, new EmptyCommand( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportWSPMHandler.1" ), false ) ); //$NON-NLS-1$
        ((SzenarioDataProvider) modelProvider).postCommand( IFlowRelationshipModel.class, new EmptyCommand( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportWSPMHandler.2" ), false ) ); //$NON-NLS-1$
      }
      catch( final Exception e )
      {
        // will never happen?
        e.printStackTrace();
      }

      /* post empty command(s) in order to make pool dirty. */
      final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );

      // TODO: add a new layer containing the new profiles in the profile-network map?
      /* Zoom to new elements in fe-map? */
      if( mapView != null )
      {
        final Feature[] newFEFeatures = importWizard.getDiscretisationModelAdds();
        final GM_Envelope envelope = FeatureHelper.getEnvelope( newFEFeatures );
        if( envelope != null )
          mapView.postCommand( new ChangeExtentCommand( mapView.getMapPanel(), envelope ), null );
      }
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportWSPMHandler.3" ), e ); //$NON-NLS-1$
    }

    return Status.OK_STATUS;
  }

}
