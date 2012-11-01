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
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportProfileHelper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class AddProfileToMapHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event )
  {
    try
    {
      final Shell shell = HandlerUtil.getActiveShell( event );

      final ITerrainModel terrainModel = ImportProfileHelper.getTerrainModel();
      final IRiverProfileNetworkCollection riverProfileNetworkCollection = terrainModel.getRiverProfileNetworkCollection();

      /* Ask user and add everything to map. */
      final Object[] result = showNetworksDialog( shell, riverProfileNetworkCollection );
      if( result == null )
        return null;

      final IRiverProfileNetwork network = (IRiverProfileNetwork)result[0];

      /* Add new layer to profile-collection-map and remove existing one with same path and source. */
      final MapView mapView = findMapView();
      ImportProfileHelper.addTheme( mapView, terrainModel, new IRiverProfileNetwork[] { network } );

      return null;
    }
    catch( final Exception e )
    {
      // FIXME: Error handling...
      e.printStackTrace();
      return null;
    }
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
        final IRiverProfileNetwork network = (IRiverProfileNetwork)element;
        return "'" + network.getName() + "' - " + network.getDescription(); //$NON-NLS-1$ //$NON-NLS-2$
      }
    } );

    dialog.setInput( collection );

    if( collection.size() > 0 )
      dialog.setInitialSelections( new Object[] { collection.get( 0 ) } );

    if( dialog.open() != Window.OK )
      return null;

    return dialog.getResult();
  }

  private MapView findMapView( ) throws ExecutionException
  {
    final MapView mapView = (MapView)PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
      throw new ExecutionException( Messages.getString("AddProfileToMapHandler.0") ); //$NON-NLS-1$

    return mapView;
  }
}