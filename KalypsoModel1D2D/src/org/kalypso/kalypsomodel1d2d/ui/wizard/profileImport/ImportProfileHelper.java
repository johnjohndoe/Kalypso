/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.SoureAndPathThemePredicate;
import org.kalypso.ogc.gml.command.ActivateThemeCommand;
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

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Holger Albert
 */
public class ImportProfileHelper
{
  /**
   * Theme id of the cascading theme containing the cross section themes.
   */
  private static final String CONTAINER_THEME_ID = "crossSections"; //$NON-NLS-1$

  private ImportProfileHelper( )
  {
  }

  public static ITerrainModel getTerrainModel( ) throws ExecutionException
  {
    try
    {
      final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      return modelProvider.getModel( ITerrainModel.class.getName() );
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( Messages.getString( "ImportProfileHelper.0" ), e ); //$NON-NLS-1$
    }
  }

  public static void addTheme( final MapView mapView, final ITerrainModel terrainModel, final IRiverProfileNetwork[] networks )
  {
    final GisTemplateMapModell mapModel = (GisTemplateMapModell)mapView.getMapPanel().getMapModell();

    for( final IRiverProfileNetwork network : networks )
    {
      final FeaturePath networkPath = new FeaturePath( network );
      final FeaturePath profilesPath = new FeaturePath( networkPath, IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE.getLocalPart() );
      final URL terrainModelLocation = terrainModel.getWorkspace().getContext();
      final String absoluteTerrainPath = terrainModelLocation.toString();
      final String relativeTerrainPath = ImportProfileHelper.createRelativeTerrainPath( mapModel.getContext(), terrainModelLocation );

      /* Remove themes with same path in map. */
      removeExistingThemes( mapView, mapModel, profilesPath, absoluteTerrainPath, relativeTerrainPath );

      /* Add as new theme. */
      addNewTheme( mapView, mapModel, network, profilesPath, relativeTerrainPath );
    }

    /* Zoom to the networks. */
    zoomToNetworks( mapView, networks );
  }

  private static String createRelativeTerrainPath( final URL mapLocation, final URL terrainLocation )
  {
    final IPath mapPath = ResourceUtilities.findPathFromURL( mapLocation );
    final IPath terrainPath = ResourceUtilities.findPathFromURL( terrainLocation );

    final IPath mapFolderPath = mapPath.removeLastSegments( 1 );
    final IPath relativePath = terrainPath.makeRelativeTo( mapFolderPath );

    return relativePath.toPortableString();
  }

  private static void removeExistingThemes( final MapView mapView, final GisTemplateMapModell mapModell, final FeaturePath profilesPath, final String absoluteTerrainPath, final String relativeTerrainPath )
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

  private static IKalypsoTheme[] findExistingThemes( final GisTemplateMapModell mapModell, final FeaturePath profilesPath, final String absolutePath, final String relativePath )
  {
    final Collection<IKalypsoTheme> allThemes = new ArrayList<>();

    /* Find all themes with absolute path to terrain model. */
    // REMARK: This is for backwards compatibility.
    // REMARK: The path was absolute in the beginning, so they might still be projects out there with an absolute path.
    final IKalypsoThemePredicate absolutePredicate = new SoureAndPathThemePredicate( absolutePath, profilesPath.toString() );
    final KalypsoThemeVisitor absoluteVisitor = new KalypsoThemeVisitor( absolutePredicate );
    mapModell.accept( absoluteVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );

    final IKalypsoTheme[] absoluteThemes = absoluteVisitor.getFoundThemes();
    allThemes.addAll( Arrays.asList( absoluteThemes ) );

    /* Find all themes with relative path to terrain model. */
    final IKalypsoThemePredicate relativePredicate = new SoureAndPathThemePredicate( relativePath, profilesPath.toString() );
    final KalypsoThemeVisitor relativeVisitor = new KalypsoThemeVisitor( relativePredicate );
    mapModell.accept( relativeVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );

    final IKalypsoTheme[] relativeThemes = relativeVisitor.getFoundThemes();
    allThemes.addAll( Arrays.asList( relativeThemes ) );

    return allThemes.toArray( new IKalypsoTheme[allThemes.size()] );
  }

  private static void addNewTheme( final MapView mapView, final GisTemplateMapModell mapModell, final IRiverProfileNetwork network, final FeaturePath profilesPath, final String relativeTerrainPath )
  {
    final IKalypsoCascadingTheme containerTheme = CascadingThemeHelper.getCascadingThemeByProperty( mapModell, CONTAINER_THEME_ID );
    if( containerTheme != null )
    {
      /* Add new theme into existing cascading theme. */
      final AddThemeCommand command = new AddThemeCommand( containerTheme, network.getName(), "gml", profilesPath.toString(), relativeTerrainPath ); //$NON-NLS-1$
      mapView.postCommand( command, null );
      return;
    }

    /* Container theme does not yet exist (needs special handling, as commands are posted in jobs). */
    final String name = Messages.getString( "ImportProfileHelper.1" ); //$NON-NLS-1$

    final AddCascadingThemeCommand cascadingCommand = new AddCascadingThemeCommand( mapModell, name, ADD_THEME_POSITION.eFront );

    final AddThemeCommand command = new AddThemeCommand( mapModell, network.getName(), "gml", profilesPath.toString(), relativeTerrainPath ); //$NON-NLS-1$
    cascadingCommand.addCommand( command );

    final Map<String, String> properties = new HashMap<>();
    properties.put( CascadingThemeHelper.PROPERTY_THEME_ID, CONTAINER_THEME_ID );
    cascadingCommand.addProperties( properties );

    final Runnable runnable = new Runnable()
    {
      @Override
      public void run( )
      {
        /* find profile theme */
        final IKalypsoCascadingTheme newContainerTheme = CascadingThemeHelper.getCascadingThemeByProperty( mapModell, CONTAINER_THEME_ID );
        if( newContainerTheme == null )
          return;

        final IKalypsoTheme[] allThemes = newContainerTheme.getAllThemes();
        if( ArrayUtils.isEmpty( allThemes ) )
          return;

        // REMARK: there should be exactly one now, as we just created it
        final IKalypsoTheme newProfileTheme = allThemes[0];
        if( newProfileTheme == null )
          return;

        /* activate it */
        final ICommand activaProfileThemeCommand = new ActivateThemeCommand( mapModell, newProfileTheme );
        mapView.postCommand( activaProfileThemeCommand, null );
      }
    };

    mapView.postCommand( cascadingCommand, runnable );
  }

  private static void zoomToNetworks( final MapView mapView, final IRiverProfileNetwork[] networks )
  {
    GM_Envelope mergedEnvelope = null;
    for( final IRiverProfileNetwork network : networks )
    {
      final IFeatureBindingCollection<IProfileFeature> profiles = network.getProfiles();
      final GM_Envelope envelope = profiles.getBoundingBox();
      if( mergedEnvelope == null )
      {
        mergedEnvelope = envelope;
        continue;
      }

      mergedEnvelope = mergedEnvelope.getMerged( envelope );
    }

    if( mergedEnvelope != null )
      mapView.postCommand( new ChangeExtentCommand( mapView.getMapPanel(), mergedEnvelope ), null );
  }
}