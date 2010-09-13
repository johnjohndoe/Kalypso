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

import java.net.URL;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.SoureAndPathThemePredicate;
import org.kalypso.ogc.gml.command.ActivateThemeCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.EnableThemeCommand;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;

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
    final ICaseDataProvider<IFeatureWrapper2> modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

    /* Get the map */
    final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.0" ) ); //$NON-NLS-1$

    final IMapModell orgMapModell = mapView.getMapPanel().getMapModell();
    if( !(orgMapModell instanceof GisTemplateMapModell) )
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.1" ) ); //$NON-NLS-1$

    final GisTemplateMapModell mapModell = (GisTemplateMapModell) orgMapModell;

    /* Get network collection */
    final ITerrainModel terrainModel;
    try
    {
      terrainModel = modelProvider.getModel( ITerrainModel.class.getName(), ITerrainModel.class );
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.2" ), e ); //$NON-NLS-1$
    }

    final IRiverProfileNetworkCollection riverProfileNetworkCollection = terrainModel.getRiverProfileNetworkCollection();

    /* ask user and add everything to map */
    final Object[] result = showNetworksDialog( shell, riverProfileNetworkCollection );
    if( result == null )
      return Status.CANCEL_STATUS;

    // REMARK: quite complicated way to get the map-file relative path to the terrain model.
    final URL mapContext = mapModell.getContext();
    final IFile mapContextFile = ResourceUtilities.findFileFromURL( mapContext );
    final URL terrainModelContext = terrainModel.getFeature().getWorkspace().getContext();
    final IFile terrainModelFile = ResourceUtilities.findFileFromURL( terrainModelContext );
    final IPath relativeTerrainModelPath = ResourceUtilities.makeRelativ( mapContextFile, terrainModelFile );
    final String source = relativeTerrainModelPath.toPortableString();

    for( final Object object : result )
    {
      final IRiverProfileNetwork network = (IRiverProfileNetwork) object;

      final FeaturePath networkPath = new FeaturePath( network.getFeature() );
      final FeaturePath profilesFeaturePath = new FeaturePath( networkPath, IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE.getLocalPart() );
      final String profilesPath = profilesFeaturePath.toString();

      /* Check if this theme is already present, if true, just activate it */
      final IKalypsoThemePredicate predicate = new SoureAndPathThemePredicate( source, profilesPath );
      final KalypsoThemeVisitor visitor = new KalypsoThemeVisitor( predicate );
      mapModell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
      final IKalypsoTheme[] foundThemes = visitor.getFoundThemes();
      final ICommand command;
      if( foundThemes.length == 0 )
        command = new AddThemeCommand( mapModell, network.getName(), "gml", profilesPath, source ); //$NON-NLS-1$
      else
      {
        final IKalypsoTheme themeToActivate = foundThemes[0];
        final CompositeCommand compositeCommand = new CompositeCommand( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.9" ) ); //$NON-NLS-1$
        compositeCommand.addCommand( new EnableThemeCommand( themeToActivate, true ) );
        compositeCommand.addCommand( new ActivateThemeCommand( mapModell, themeToActivate ) );
        command = compositeCommand;
      }
      mapView.postCommand( command, null );
    }

    return Status.OK_STATUS;
  }

  private Object[] showNetworksDialog( final Shell shell, final IRiverProfileNetworkCollection riverProfileNetworkCollection )
  {
    if( riverProfileNetworkCollection.size() == 0 )
    {
      MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.5" ), Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.22" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return null;
    }

    final ListDialog dialog = new ListDialog( shell );
    dialog.setTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.5" ) ); //$NON-NLS-1$
    dialog.setMessage( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.6" ) ); //$NON-NLS-1$
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
        return "'" + network.getName() + "' - " + network.getDescription(); //$NON-NLS-1$ //$NON-NLS-2$
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
