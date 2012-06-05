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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.SoureAndPathThemePredicate;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ui.action.AddCascadingThemeCommand;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.action.IThemeCommand.ADD_THEME_POSITION;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class AddProfileToMapHandler extends AbstractHandler
{
  /**
   * themeID of the cascading theme containing the cross section themes.
   */
  private static final String CONTAINER_THEME_ID = "crossSections"; //$NON-NLS-1$

  @Override
  public Object execute( final ExecutionEvent event )
  {
    try
    {
      final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
      final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

      final ITerrainModel terrainModel = getTerrainModel( context );
      final IRiverProfileNetworkCollection riverProfileNetworkCollection = terrainModel.getRiverProfileNetworkCollection();

      /* ask user and add everything to map */
      final Object[] result = showNetworksDialog( shell, riverProfileNetworkCollection );
      if( result == null )
        return Status.CANCEL_STATUS;

      final IRiverProfileNetwork network = (IRiverProfileNetwork) result[0];

      /* Add new layer to profile-collection-map and remove existing one with same path and source */
      final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
      if( mapView == null )
        throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportProfileHandler.3" ) ); //$NON-NLS-1$

      final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();

      final FeaturePath networkPath = new FeaturePath( network );
      final FeaturePath profilesPath = new FeaturePath( networkPath, IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE.getLocalPart() );

      final URL terrainModelLocation = terrainModel.getWorkspace().getContext();

      final String absoluteTerrainPath = terrainModelLocation.toString();
      final String relativeTerrainPath = createRelativeTerrainPath( mapModell.getContext(), terrainModelLocation );

      /* Remove themes with same path in map */
      removeExistingThemes( mapView, mapModell, profilesPath, absoluteTerrainPath, relativeTerrainPath );

      /* Add as new theme */
      addNewTheme( mapView, mapModell, network, profilesPath, relativeTerrainPath );

      /* Zoom to new profiles in fe-map? */
      final GM_Envelope envelope = network.getEnvelope();
      if( envelope != null )
        mapView.postCommand( new ChangeExtentCommand( mapView.getMapPanel(), envelope ), null );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e );
    }
  }

  private void addNewTheme( final MapView mapView, final GisTemplateMapModell mapModell, final IRiverProfileNetwork network, final FeaturePath profilesPath, final String relativeTerrainPath )
  {
    final IKalypsoCascadingTheme containerTheme = CascadingThemeHelper.getCascadingThemeByProperty( mapModell, CONTAINER_THEME_ID );
    if( containerTheme != null )
    {
      /* add new theme into existing cascading theme */
      final AddThemeCommand command = new AddThemeCommand( containerTheme, network.getName(), "gml", profilesPath.toString(), relativeTerrainPath ); //$NON-NLS-1$
      mapView.postCommand( command, null );
      return;
    }

    /* Container theme does not yet exist (needs special handling, as commands are posted in jobs) */
    final String name = Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportProfileHandler.4" ); //$NON-NLS-1$

    final AddCascadingThemeCommand cascadingCommand = new AddCascadingThemeCommand( mapModell, name, ADD_THEME_POSITION.eFront );

    final AddThemeCommand command = new AddThemeCommand( mapModell, network.getName(), "gml", profilesPath.toString(), relativeTerrainPath ); //$NON-NLS-1$
    cascadingCommand.addCommand( command );

    final Map<String, String> properties = new HashMap<String, String>();
    properties.put( CascadingThemeHelper.PROPERTY_THEME_ID, CONTAINER_THEME_ID );
    cascadingCommand.addProperties( properties );

    mapView.postCommand( cascadingCommand, null );
  }

  protected ITerrainModel getTerrainModel( final IEvaluationContext context ) throws ExecutionException
  {
    final IScenarioDataProvider modelProvider = (IScenarioDataProvider) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    try
    {
      return modelProvider.getModel( ITerrainModel.class.getName() );
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.AddProfileToMapHandler.2" ), e ); //$NON-NLS-1$
    }
  }

  protected void removeExistingThemes( final MapView mapView, final GisTemplateMapModell mapModell, final FeaturePath profilesPath, final String absoluteTerrainPath, final String relativeTerrainPath )
  {
    final IKalypsoTheme[] foundThemes = findExistingThemes( mapModell, profilesPath, absoluteTerrainPath, relativeTerrainPath );
    if( foundThemes.length > 0 )
    {
      for( final IKalypsoTheme themeToRemove : foundThemes )
      {
        final RemoveThemeCommand commandRemove = new RemoveThemeCommand( mapModell, themeToRemove, true ); //$NON-NLS-1$
        mapView.postCommand( commandRemove, null );
      }
    }
  }

  private String createRelativeTerrainPath( final URL mapLocation, final URL terrainLocation )
  {
    final IPath mapPath = ResourceUtilities.findPathFromURL( mapLocation );
    final IPath terrainPath = ResourceUtilities.findPathFromURL( terrainLocation );

    final IPath mapFolderPath = mapPath.removeLastSegments( 1 );

    final IPath relativePath = terrainPath.makeRelativeTo( mapFolderPath );
    return relativePath.toPortableString();
  }

  protected IKalypsoTheme[] findExistingThemes( final GisTemplateMapModell mapModell, final FeaturePath profilesPath, final String absolutePath, final String relativePath )
  {
    final Collection<IKalypsoTheme> allThemes = new ArrayList<IKalypsoTheme>();

    /* Find all themes with absolute path to terrain model */
    // REMARK: this is for backwards compatibility; the path was absolute in the beginning, so they might still be
    // projects out there with an absolute path
    final IKalypsoThemePredicate absolutePredicate = new SoureAndPathThemePredicate( absolutePath, profilesPath.toString() );
    final KalypsoThemeVisitor absoluteVisitor = new KalypsoThemeVisitor( absolutePredicate );
    mapModell.accept( absoluteVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );

    final IKalypsoTheme[] absoluteThemes = absoluteVisitor.getFoundThemes();
    allThemes.addAll( Arrays.asList( absoluteThemes ) );

    /* Find all themes with relative path to terrain model */
    final IKalypsoThemePredicate relativePredicate = new SoureAndPathThemePredicate( relativePath, profilesPath.toString() );
    final KalypsoThemeVisitor relativeVisitor = new KalypsoThemeVisitor( relativePredicate );
    mapModell.accept( relativeVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );

    final IKalypsoTheme[] relativeThemes = relativeVisitor.getFoundThemes();
    allThemes.addAll( Arrays.asList( relativeThemes ) );

    return allThemes.toArray( new IKalypsoTheme[allThemes.size()] );
  }

  private Object[] showNetworksDialog( final Shell shell, final IRiverProfileNetworkCollection riverProfileNetworkCollection )
  {
    final IFeatureBindingCollection<IRiverProfileNetwork> collection = riverProfileNetworkCollection.getRiverProfileNetworks();
    if( collection.size() == 0 )
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
      @Override
      public String getText( final Object element )
      {
        final IRiverProfileNetwork network = (IRiverProfileNetwork) element;
        return "'" + network.getName() + "' - " + network.getDescription(); //$NON-NLS-1$ //$NON-NLS-2$
      }
    } );

    dialog.setInput( riverProfileNetworkCollection );

    if( collection.size() > 0 )
      dialog.setInitialSelections( new Object[] { collection.get( 0 ) } );

    if( dialog.open() != Window.OK )
      return null;

    return dialog.getResult();
  }
}
