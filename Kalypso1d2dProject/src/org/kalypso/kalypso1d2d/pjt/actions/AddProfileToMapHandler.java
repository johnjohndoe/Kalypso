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
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.SoureAndPathThemePredicate;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.command.RemoveThemeCommand;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;
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
  public Object execute( final ExecutionEvent event )
  {
    try
    {
      final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
      final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
      final ICaseDataProvider<IFeatureWrapper2> modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

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
      
      final IRiverProfileNetwork network = (IRiverProfileNetwork) result[ 0 ];

      /* Add new layer to profile-collection-map and remove existing one with same path and source*/
      final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
      if( mapView != null )
      { 
          final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();

          final FeaturePath networkPath = new FeaturePath( network.getFeature() );
          final FeaturePath profilesPath = new FeaturePath( networkPath, IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE.getLocalPart() );
          final String source = terrainModel.getFeature().getWorkspace().getContext().toString();
         
          final IKalypsoThemePredicate predicate = new SoureAndPathThemePredicate( source, profilesPath.toString() );
          final KalypsoThemeVisitor visitor = new KalypsoThemeVisitor( predicate );
          mapModell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
          IKalypsoTheme[] foundThemes = visitor.getFoundThemes();
          if( foundThemes.length > 0 ){
            for( final IKalypsoTheme themeToRemove: foundThemes ){
              final RemoveThemeCommand commandRemove = new RemoveThemeCommand( mapModell, themeToRemove, true ); //$NON-NLS-1$
              mapView.postCommand( commandRemove, null );
            }
          }
          final AddThemeCommand command = new AddThemeCommand( mapModell, network.getName(), "gml", profilesPath.toString(), source ); //$NON-NLS-1$
          mapView.postCommand( command, null );

          /* Zoom to new profiles in fe-map? */
          final GM_Envelope envelope = network.getWrappedList().getBoundingBox();
          if( envelope != null )
            mapView.postCommand( new ChangeExtentCommand( mapView.getMapPanel(), envelope ), null );
      }
      else
        throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ImportProfileHandler.3" ) ); //$NON-NLS-1$
      
      return Status.OK_STATUS;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e );
    }
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
