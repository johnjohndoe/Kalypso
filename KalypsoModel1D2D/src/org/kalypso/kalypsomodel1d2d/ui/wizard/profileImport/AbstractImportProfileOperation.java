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

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Holger Albert
 */
public abstract class AbstractImportProfileOperation implements ICoreRunnableWithProgress
{
  private final ImportProfileData m_data;

  public AbstractImportProfileOperation( final ImportProfileData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString("AbstractImportProfileOperation.0"), 1000 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString("AbstractImportProfileOperation.1") ); //$NON-NLS-1$

      /* Execute the operation. */
      final IStatus executeStatus = execute( m_data, new SubProgressMonitor( monitor, 800 ) );
      if( !executeStatus.isOK() )
        return executeStatus;

      /* Monitor. */
      monitor.subTask( Messages.getString("AbstractImportProfileOperation.2") ); //$NON-NLS-1$

      /* Make the pool dirty. */
      makeDirty();

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString("AbstractImportProfileOperation.3") ); //$NON-NLS-1$

      /* Add theme. */
      addTheme( m_data.getMapView() );

      /* Monitor. */
      monitor.worked( 100 );

      return executeStatus;
    }
    catch( final CoreException ex )
    {
      return ex.getStatus();
    }
    catch( final ExecutionException | InvocationTargetException ex )
    {
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, ex.getLocalizedMessage(), ex );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  protected abstract IStatus execute( ImportProfileData data, IProgressMonitor monitor ) throws CoreException, InvocationTargetException;

  private void makeDirty( )
  {
    try
    {
      /* Fire change events. */
      final IRiverProfileNetwork[] networks = getAddedRiverNetworks();
      if( networks.length > 0 )
      {
        final GMLWorkspace workspace = networks[0].getWorkspace();
        final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace, networks[0].getOwner(), networks, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
        workspace.fireModellEvent( event );
      }

      /* Post empty command in order to make pool dirty. */
      final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      modelProvider.postCommand( ITerrainModel.class.getName(), new EmptyCommand( Messages.getString("AbstractImportProfileOperation.4"), false ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      /* Will never happen? */
      e.printStackTrace();
    }
  }

  private void addTheme( final MapView mapView ) throws ExecutionException
  {
    if( mapView == null )
      throw new ExecutionException( Messages.getString("AbstractImportProfileOperation.5") ); //$NON-NLS-1$

    final ITerrainModel terrainModel = ImportProfileHelper.getTerrainModel();
    if( terrainModel == null )
      return;

    final IRiverProfileNetwork[] networks = getAddedRiverNetworks();
    if( networks == null || networks.length == 0 )
      return;

    ImportProfileHelper.addTheme( mapView, terrainModel, networks );
  }

  public abstract IRiverProfileNetwork[] getAddedRiverNetworks( );
}