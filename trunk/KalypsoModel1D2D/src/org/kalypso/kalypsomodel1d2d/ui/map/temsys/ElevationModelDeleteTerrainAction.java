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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.DeleteNativeTerrainElevationWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
class ElevationModelDeleteTerrainAction extends Action implements IUpdateable
{
  private final ApplyElevationWidgetDataModel m_dataModel;

  private final TableViewer m_elevationViewer;

  ElevationModelDeleteTerrainAction( final TableViewer elevationViewer, final ApplyElevationWidgetDataModel dataModel )
  {
    m_elevationViewer = elevationViewer;
    m_dataModel = dataModel;

    setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.12" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_DELETE ) );

  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();
    deleteElevationModel( shell );
    m_dataModel.getMapPanel().repaintMap();
  }

  private final void deleteElevationModel( final Shell shell )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_elevationViewer.getSelection();
    if( selection.isEmpty() )
      return;

    if( !MessageDialog.openConfirm( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.17" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.2" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return;

    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getDiscretisationModel();
    if( model1d2d == null )
    {
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.16" ) ); //$NON-NLS-1$
    }

    final IKalypsoFeatureTheme elevationTheme = m_dataModel.getElevationTheme();
    if( elevationTheme == null )
      return;

    final CommandableWorkspace workspace = elevationTheme.getWorkspace();
    if( workspace == null )
      return;

    final ITerrainElevationModelSystem modelSystem = m_dataModel.getElevationModelSystem();
    final ChangeTerrainElevationSystemCommand compositeCommand = new ChangeTerrainElevationSystemCommand( workspace, model1d2d, modelSystem );

    for( final Object selected : selection.toList() )
    {
      if( selected instanceof INativeTerrainElevationModelWrapper )
      {
        final INativeTerrainElevationModelWrapper nativeEleModel = (INativeTerrainElevationModelWrapper) selected;

        final DeleteNativeTerrainElevationWrapper delCmd = new DeleteNativeTerrainElevationWrapper( modelSystem, nativeEleModel, true );
        compositeCommand.addCommand( delCmd, nativeEleModel.getSourceFile() );
      }
    }

    m_elevationViewer.setSelection( new StructuredSelection() );

    final TableViewer elevationListTableViewer = m_elevationViewer;
    /* Also refresh table AFTER models have been deleted */
    elevationTheme.postCommand( compositeCommand, new Runnable()
    {
      @Override
      public void run( )
      {
        ViewerUtilities.refresh( elevationListTableViewer, true );
      }
    } );

    final MultiStatus deleteFiles = new MultiStatus( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), IStatus.OK, "", null ); //$NON-NLS-1$
    if( deleteFiles.isOK() )
    {
      try
      {
        m_dataModel.saveModels();
        deleteFiles.add( compositeCommand.deleteFiles() );
      }
      catch( final Exception e )
      {
        deleteFiles.add( new MultiStatus( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), 1, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand.4" ), null ) ); //$NON-NLS-1$
      }
    }

    ErrorDialog.openError( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.3" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.4" ), deleteFiles ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void update( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_elevationViewer.getSelection();
    setEnabled( !selection.isEmpty() );
  }
}