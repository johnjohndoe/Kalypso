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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class ElevationModelSystemEditorComponent extends Composite
{
  private final Collection<IAction> m_actions = new ArrayList<>();

  private TableViewer m_elevationViewer;

  private final ApplyElevationWidgetDataModel m_dataModel;

  private ToolBarManager m_toolbar;

  public ElevationModelSystemEditorComponent( final FormToolkit toolkit, final Composite parent, final ApplyElevationWidgetDataModel dataModel )
  {
    super( parent, SWT.NONE );

    m_dataModel = dataModel;

    toolkit.adapt( this );

    ControlUtils.addDisposeListener( this );

    GridLayoutFactory.fillDefaults().numColumns( 2 ).spacing( 0, 0 ).applyTo( this );

    final Control elevationControl = createElevationViewer( toolkit, this );
    /* Exactly as hight as the toolbar */
    final GridData tableData = new GridData( SWT.FILL, SWT.FILL, true, false );
    tableData.minimumWidth = tableData.widthHint = 50;
    elevationControl.setLayoutData( tableData );

    createToolbar( toolkit, this ).setLayoutData( new GridData( SWT.CENTER, SWT.FILL, false, true ) );
  }

  @Override
  public void dispose( )
  {
    m_toolbar.dispose();

    super.dispose();
  }

  private Control createElevationViewer( final FormToolkit toolkit, final Composite parent )
  {
    final Table elevationTable = toolkit.createTable( parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );
    elevationTable.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    elevationTable.setLinesVisible( true );
    elevationTable.setHeaderVisible( true );
    elevationTable.addControlListener( new ColumnsResizeControlListener() );

    m_elevationViewer = new TableViewer( elevationTable );

    m_elevationViewer.setContentProvider( new ArrayContentProvider() );

    createNameColumn( m_elevationViewer );
    createDescriptionColumn( m_elevationViewer );

    m_elevationViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection)event.getSelection() );
      }
    } );

    final ITerrainElevationModelSystem elevationModelSystem = m_dataModel.getElevationModelSystem();
    if( elevationModelSystem != null )
    {
      final IFeatureBindingCollection<ITerrainElevationModel> terrainElevationModels = elevationModelSystem.getTerrainElevationModels();
      m_elevationViewer.setInput( terrainElevationModels );
    }

    return elevationTable;
  }

  private void createNameColumn( final TableViewer elevationViewer )
  {
    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( elevationViewer, SWT.LEFT );
    final ViewerColumnItem column = new ViewerColumnItem( nameColumn );

    column.setResizable( false );
    column.setText( Messages.getString( "ElevationModelSystemEditorComponent_0" ) ); //$NON-NLS-1$
    ColumnsResizeControlListener.setMinimumPackWidth( column.getColumn() );
    nameColumn.setLabelProvider( new ElevationListNameProvider() );
    ColumnViewerSorter.registerSorter( nameColumn, new ViewerComparator() );
  }

  private void createDescriptionColumn( final TableViewer elevationViewer )
  {
    final ViewerColumn descriptionColumn = ColumnViewerUtil.createViewerColumn( elevationViewer, SWT.LEFT );
    final ViewerColumnItem column = new ViewerColumnItem( descriptionColumn );

    column.setResizable( false );
    column.setText( Messages.getString( "ElevationModelSystemEditorComponent_1" ) ); //$NON-NLS-1$
    ColumnsResizeControlListener.setMinimumPackWidth( column.getColumn() );
    descriptionColumn.setLabelProvider( new ElevationListDescriptionProvider() );
    ColumnViewerSorter.registerSorter( descriptionColumn, new ViewerComparator() );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    if( selection.isEmpty() )
      m_dataModel.setElevationModels( null );
    else
    {
      final List< ? > list = selection.toList();

      final Collection<ITerrainElevationModel> elevationModels = new ArrayList<>( list.size() );
      for( final Object object : list )
      {
        if( object instanceof ITerrainElevationModel )
          elevationModels.add( (ITerrainElevationModel)object );
      }

      m_dataModel.setElevationModels( elevationModels.toArray( new ITerrainElevationModel[elevationModels.size()] ) );
    }

    updateActions();
  }

  private void updateActions( )
  {
    for( final IAction action : m_actions )
    {
      if( action instanceof IUpdateable )
        ((IUpdateable)action).update();
    }
  }

  private Control createToolbar( final FormToolkit toolkit, final Composite parent )
  {
    final ToolBar tb = new ToolBar( parent, SWT.VERTICAL | SWT.FLAT );
    toolkit.adapt( tb );

    m_toolbar = new ToolBarManager( tb );

    createActions();

    m_toolbar.update( true );
    updateActions();

    return tb;
  }

  private void createActions( )
  {
    final Action moveUpAction = new ElevationModelMoveSelectionAction( m_elevationViewer, m_dataModel, -1, KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_MOVE_UP );
    m_toolbar.add( moveUpAction );
    m_actions.add( moveUpAction );

    final Action moveDownAction = new ElevationModelMoveSelectionAction( m_elevationViewer, m_dataModel, 1, KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_MOVE_DOWN );
    m_toolbar.add( moveDownAction );
    m_actions.add( moveDownAction );

    m_toolbar.add( new ElevationModelJumpToTerrainAction( m_dataModel ) );

    final Action deleteAction = new ElevationModelDeleteTerrainAction( m_elevationViewer, m_dataModel );
    m_toolbar.add( deleteAction );
    m_actions.add( deleteAction );
  }
}