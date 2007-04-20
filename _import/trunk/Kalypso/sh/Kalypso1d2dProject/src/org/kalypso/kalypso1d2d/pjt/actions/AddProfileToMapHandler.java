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
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

import de.renew.workflow.cases.ICaseDataProvider;

/**
 * @author Gernot Belger
 */
public class AddProfileToMapHandler extends AbstractHandler
{
  /**
   * @see de.renew.workflow.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ICaseDataProvider<IFeatureWrapper2> modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );

    /* Get the map */
    final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
      throw new ExecutionException( "Kartenansicht nicht geˆffnet." );
    final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();

    /* Get network collection */
    final ITerrainModel terrainModel;
    try
    {
      terrainModel = modelProvider.getModel( ITerrainModel.class );
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( "Es kˆnnen keine Themen hinzugef¸gt werden.", e );
    }

    final IRiverProfileNetworkCollection riverProfileNetworkCollection = terrainModel.getRiverProfileNetworkCollection();

    /* ask user and add everything to map */
    final Object[] result = showNetworksDialog( shell, riverProfileNetworkCollection );
    if( result != null )
    {
      for( final Object object : result )
      {
        final IRiverProfileNetwork network = (IRiverProfileNetwork) object;

        final FeaturePath networkPath = new FeaturePath( network.getWrappedFeature() );
        final FeaturePath profilesPath = new FeaturePath( networkPath, IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE.getLocalPart() );
        final String source = terrainModel.getWrappedFeature().getWorkspace().getContext().toString();
        // TODO: aktivates the theme, is this ok?
        final AddThemeCommand command = new AddThemeCommand( mapModell, network.getName(), "gml", profilesPath.toString(), source );
        mapView.postCommand( command, null );
      }
    }

    return Status.OK_STATUS;
  }

  private Object[] showNetworksDialog( final Shell shell, final IRiverProfileNetworkCollection riverProfileNetworkCollection )
  {
    final ListDialog dialog = new ListDialog( shell );
    dialog.setTitle( "Profile in Karte anzeigen" );
    dialog.setMessage( "W‰hlen Sie die Profilnetzwerke aus, welche Sie als Themen in die Karte ¸bernehmen mˆchten:" );
    dialog.setContentProvider( new ArrayContentProvider() );
    dialog.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IRiverProfileNetwork network = (IRiverProfileNetwork) element;
        return "'" + network.getName() + "' - " + network.getDescription();
      }
    } );

    dialog.setInput( riverProfileNetworkCollection );

    if( riverProfileNetworkCollection.size() > 0 )
      dialog.setInitialSelections( new Object[] { riverProfileNetworkCollection.get( 0 ) } );

    dialog.open();
    // Stefan: this does not make sense to me, throwing an exception when the user cancelled
    // if(dialog.open() != Window.OK )
    // throw new CoreException( Status.CANCEL_STATUS );

    return dialog.getResult();
  }

}
