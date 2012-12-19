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
package org.kalypso.ui.rrm.internal.hydrotops;

import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.KeyInfo;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.model.hydrology.binding.GeologyCollection;
import org.kalypso.model.hydrology.binding.LanduseCollection;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.OverlayCollection;
import org.kalypso.model.hydrology.binding.SoilTypeCollection;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.operation.hydrotope.HydrotopeCreationOperation;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Gernot Belger
 * @author Dejan Antanaskovic
 */
public class RRMCreateHydrotopsHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final IStructuredSelection selection = (IStructuredSelection) HandlerUtil.getCurrentSelectionChecked( event );

    final CommandableWorkspace workspace = findHydrotopes( selection );
    final HydrotopeCollection hydrotopes = (HydrotopeCollection) workspace.getRootFeature();

    final RrmScenario scenario = RrmScenario.forAnyModelGml( workspace );
    if( scenario == null )
      throw new ExecutionException( "Invalid project structure" ); //$NON-NLS-1$

    final LanduseCollection landuse = findData( hydrotopes, scenario.getLanduseFile().getName(), LanduseCollection.class );
    final SoilTypeCollection pedology = findData( hydrotopes, scenario.getPedologyFile().getName(), SoilTypeCollection.class );
    final GeologyCollection geology = findData( hydrotopes, scenario.getGeologyFile().getName(), GeologyCollection.class );
    final OverlayCollection overlay = findData( hydrotopes, scenario.getOverlayFile().getName(), OverlayCollection.class );
    final NaModell naModel = findData( hydrotopes, scenario.getModelFile().getName(), NaModell.class );

    final String windowTitle = HandlerUtils.getCommandName( event );
    if( !MessageDialog.openConfirm( shell, windowTitle, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.5" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return null;

    /* Clear old list */
    hydrotopes.getHydrotopes().clear();

    final HydrotopeCreationOperation operation = new HydrotopeCreationOperation( naModel, landuse, pedology, geology, overlay, hydrotopes, null );

    final IStatus status = ProgressUtilities.busyCursorWhile( operation );

    /* Undo changes if an error happens */
    handleResult( workspace, status );

    /* Show result to user */
    final String commandName = HandlerUtils.getCommandName( event );
    StatusDialog.open( shell, status, commandName );

    return null;
  }

  private void handleResult( final CommandableWorkspace workspace, final IStatus status )
  {
    if( status.matches( IStatus.CANCEL ) )
    {
      final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
      final KeyInfo info = pool.getInfo( workspace );
      if( info != null )
        info.reload( true );
    }
    else
    {
      try
      {
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, workspace.getRootFeature(), (Feature[]) null, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
        workspace.postCommand( new EmptyCommand( StringUtils.EMPTY, false ) );
      }
      catch( final Exception e )
      {
        // should never happen
        e.printStackTrace();
      }
    }
  }

  private <T extends Feature> T findData( final HydrotopeCollection context, final String gmlFileName, final Class<T> type ) throws ExecutionException
  {
    try
    {
      final URL contextLocation = context.getWorkspace().getContext();
      final IPoolableObjectType poolKey = new PoolableObjectType( "gml", gmlFileName, contextLocation ); //$NON-NLS-1$

      final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
      final CommandableWorkspace dataWorkspace = (CommandableWorkspace) pool.getObject( poolKey );
      final Feature rootFeature = dataWorkspace.getRootFeature();

      if( type.isInstance( rootFeature ) )
        return type.cast( rootFeature );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    throw new ExecutionException( String.format( Messages.getString("RRMCreateHydrotopsHandler.0"), type.getName() ) ); //$NON-NLS-1$
  }

  private CommandableWorkspace findHydrotopes( final IStructuredSelection selection ) throws ExecutionException
  {
    final IKalypsoTheme[] themes = MapHandlerUtils.getSelectedThemes( selection );
    for( final IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
        final FeatureList featureList = featureTheme.getFeatureList();
        if( featureList != null )
        {
          final Feature owner = featureList.getOwner();
          if( owner instanceof HydrotopeCollection )
            return featureTheme.getWorkspace();
        }
      }
    }

    // Should not happen because of handler enablement
    throw new ExecutionException( "Failed to find hydrotopes" ); //$NON-NLS-1$
  }
}