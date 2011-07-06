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
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippleWizard;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Thomas Jung
 */
public class ImportProfileHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  @SuppressWarnings("unchecked")
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ICaseDataProvider<IModel> modelProvider = (ICaseDataProvider<IModel>) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

    ITerrainModel terrainModel;
    try
    {
      terrainModel = modelProvider.getModel( ITerrainModel.class );
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportProfileHandler.0" ), e ); //$NON-NLS-1$
    }

    /* Import Reach into Terrain-Model */
    final IRiverProfileNetworkCollection networkModel = terrainModel.getRiverProfileNetworkCollection();

    final ImportTrippleWizard importWizard = new ImportTrippleWizard( networkModel );

    importWizard.setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModel1D2DPlugin.getDefault(), getClass().getName() ) );

    final WizardDialog2 dialog = new WizardDialog2( shell, importWizard );
    dialog.setRememberSize( true );
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    try
    {
      /* post empty command(s) in order to make pool dirty. */
      ((SzenarioDataProvider) modelProvider).postCommand( ITerrainModel.class, new EmptyCommand( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportProfileHandler.1" ), false ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      // will never happen?
      e.printStackTrace();
    }

    /* Add new layer to profile-collection-map */
    final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView != null )
    {
      final Feature[] newFEFeatures = importWizard.getTerrainModelAdds();

      if( newFEFeatures.length > 0 )
      {
        final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();

        final IRiverProfileNetwork network = importWizard.getAddedRiverNetwork();
        final FeaturePath networkPath = new FeaturePath( network.getFeature() );
        final FeaturePath profilesPath = new FeaturePath( networkPath, IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE.getLocalPart() );
        final String source = terrainModel.getFeature().getWorkspace().getContext().toString();
        // TODO: aktivates the theme, is this ok?
        final AddThemeCommand command = new AddThemeCommand( mapModell, network.getName(), "gml", profilesPath.toString(), source ); //$NON-NLS-1$
        mapView.postCommand( command, null );

        /* Zoom to new profiles in fe-map? */
        final GM_Envelope envelope = FeatureHelper.getEnvelope( newFEFeatures );
        if( envelope != null )
          mapView.postCommand( new ChangeExtentCommand( mapView.getMapPanel(), envelope ), null );
      }
    }
    else
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportProfileHandler.3" ) ); //$NON-NLS-1$

    return Status.OK_STATUS;
  }

}
