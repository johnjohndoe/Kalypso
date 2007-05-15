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
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.SoureAndPathThemePredicate;
import org.kalypso.ogc.gml.command.ActivateThemeCommand;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
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
    if( mapModell == null )
      throw new ExecutionException( "Kartenansicht nicht initialisiert, versuchen Sie es noch einmal." );

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
    if( result == null )
      return Status.CANCEL_STATUS;

    for( final Object object : result )
    {
      final IRiverProfileNetwork network = (IRiverProfileNetwork) object;

      final FeaturePath networkPath = new FeaturePath( network.getWrappedFeature() );
      final FeaturePath profilesFeaturePath = new FeaturePath( networkPath, IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE.getLocalPart() );
      final String profilesPath = profilesFeaturePath.toString();
      final String source = terrainModel.getWrappedFeature().getWorkspace().getContext().toString();

      /* Check if this theme is already present, if true, just activate it */
      final IKalypsoThemePredicate predicate = new SoureAndPathThemePredicate( source, profilesPath );
      final KalypsoThemeVisitor visitor = new KalypsoThemeVisitor( predicate );
      mapModell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
      final IKalypsoTheme[] foundThemes = visitor.getFoundThemes();
      final ICommand command;
      if( foundThemes.length == 0 )
        command = new AddThemeCommand( mapModell, network.getName(), "gml", profilesPath, source );
      else
        command = new ActivateThemeCommand( mapModell, foundThemes[0] );
      mapView.postCommand( command, null );
    }

    return Status.OK_STATUS;
  }

  private Object[] showNetworksDialog( final Shell shell, final IRiverProfileNetworkCollection riverProfileNetworkCollection )
  {
    final ListDialog dialog = new ListDialog( shell );
    dialog.setTitle( "Profile in Karte anzeigen" );
    dialog.setMessage( "W‰hlen Sie das Profilthema aus, welches Sie in der Karte anzeigen mˆchten:" );
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

    if( dialog.open() != Window.OK )
      return null;

    return dialog.getResult();
  }

}
