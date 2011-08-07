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

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.resource.ImageDescriptor;
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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class ElevationModelSystemEditorComponent extends Composite
{
  private TableViewer m_elevationViewer;

  private final ApplyElevationWidgetDataModel m_dataModel;

  private Label m_descriptionText;

  private ToolBarManager m_toolbar;

  public ElevationModelSystemEditorComponent( final FormToolkit toolkit, final Composite parent, final ApplyElevationWidgetDataModel dataModel )
  {
    super( parent, SWT.NONE );

    m_dataModel = dataModel;

    toolkit.adapt( this );

    ControlUtils.addDisposeListener( this );

    GridLayoutFactory.swtDefaults().numColumns( 3 ).spacing( 0, 0 ).applyTo( this );

    final Control elevationControl = createElevationViewer( toolkit, this );
    /* Exactly as hight as the toolbar */
    final GridData tableData = new GridData( SWT.FILL, SWT.FILL, true, false );
    elevationControl.setLayoutData( tableData );

    createToolbar( toolkit, this ).setLayoutData( new GridData( SWT.CENTER, SWT.FILL, false, true ) );

    final Control descriptionControl = createDescriptionPanel( toolkit, this );
    /* Exactly as hight as the toolbar */
    final GridData descriptionData = new GridData( SWT.FILL, SWT.FILL, true, false );
    descriptionControl.setLayoutData( descriptionData );
  }

  @Override
  public void dispose( )
  {
    m_toolbar.dispose();

    super.dispose();
  }

  private Control createElevationViewer( final FormToolkit toolkit, final Composite parent )
  {
    final Table elevationTable = toolkit.createTable( parent, SWT.FILL | SWT.BORDER );
    elevationTable.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    elevationTable.setLinesVisible( true );
    elevationTable.addControlListener( new ColumnsResizeControlListener() );

    m_elevationViewer = new TableViewer( elevationTable );

    m_elevationViewer.setContentProvider( new ArrayContentProvider() );

    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( m_elevationViewer, SWT.LEFT );
    final ViewerColumnItem column = new ViewerColumnItem( nameColumn );

    column.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( column.getColumn() );
    nameColumn.setLabelProvider( new ElevationListLabelProvider() );
    ColumnViewerSorter.registerSorter( nameColumn, new ViewerComparator() );

    m_elevationViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection) event.getSelection() );
      }
    } );

    final ITerrainElevationModelSystem elevationModelSystem = m_dataModel.getElevationModelSystem();
    final IFeatureWrapperCollection<ITerrainElevationModel> terrainElevationModels = elevationModelSystem.getTerrainElevationModels();
    m_elevationViewer.setInput( terrainElevationModels );

    return elevationTable;
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    if( selection.isEmpty() )
    {
      m_dataModel.setElevationModel( null );
      m_descriptionText.setText( StringUtils.EMPTY );
    }
    else
    {
      if( selection.getFirstElement() instanceof ITerrainElevationModel )
      {
        final ITerrainElevationModel firstElement = (ITerrainElevationModel) selection.getFirstElement();
        m_dataModel.setElevationModel( firstElement );
        m_descriptionText.setText( firstElement.getDescription() );
      }
    }
  }

  private Control createToolbar( final FormToolkit toolkit, final Composite parent )
  {
    final ToolBar tb = new ToolBar( parent, SWT.VERTICAL | SWT.FLAT );
    toolkit.adapt( tb );

    m_toolbar = new ToolBarManager( tb );

    createActions();

    m_toolbar.update( true );

    return tb;
  }

  private void createActions( )
  {
    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();

    final Action moveUpAction = new ElevationModelMoveSelectionAction( m_elevationViewer, m_dataModel, -1 );
    final ImageDescriptor moveUpImage = imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_MOVE_UP );
    moveUpAction.setImageDescriptor( moveUpImage );
    m_toolbar.add( moveUpAction );

    final Action moveDownAction = new ElevationModelMoveSelectionAction( m_elevationViewer, m_dataModel, 1 );
    final ImageDescriptor moveDownImage = imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_MOVE_DOWN );
    moveDownAction.setImageDescriptor( moveDownImage );
    m_toolbar.add( moveDownAction );

    m_toolbar.add( new ElevationModelJumToTerrainAction( m_dataModel ) );

    final Action deleteAction = new ElevationModelDeleteTerrainAction( m_elevationViewer, m_dataModel );
    m_toolbar.add( deleteAction );
  }

  private Control createDescriptionPanel( final FormToolkit toolkit, final Composite parent )
  {
    final Group descriptionGroup = new Group( parent, SWT.NONE );
    toolkit.adapt( descriptionGroup );
    descriptionGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.14" ) ); //$NON-NLS-1$
    GridLayoutFactory.swtDefaults().applyTo( descriptionGroup );

    final String initialMessage = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.15" ); //$NON-NLS-1$
    m_descriptionText = toolkit.createLabel( descriptionGroup, initialMessage, SWT.WRAP );
    m_descriptionText.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    return descriptionGroup;
  }
}
