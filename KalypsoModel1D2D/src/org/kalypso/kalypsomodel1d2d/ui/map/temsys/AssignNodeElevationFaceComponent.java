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
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class AssignNodeElevationFaceComponent extends Composite
{
  private final ApplyElevationWidgetDataModel m_dataModel;

  private Text m_inputText;

  private final List<IFE1D2DNode> m_selectionNodeList = new ArrayList<>();

  private TableViewer m_nodeElevationViewer;

  /**
   * Listen to node selection and fill the selection list
   */
  private final ISelectionChangedListener nodeSelectionListener = new ISelectionChangedListener()
  {
    @Override
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleSelectionChanged( (IStructuredSelection)event.getSelection() );
    }
  };

  private final KeyBasedDataModelChangeListener m_modelChangeListener = new KeyBasedDataModelChangeListener()
  {
    @Override
    public void dataChanged( final String key, final Object newValue )
    {
      handleDataChanged( key, newValue );
    }
  };

  public AssignNodeElevationFaceComponent( final FormToolkit toolkit, final Composite parent, final ApplyElevationWidgetDataModel dataModel )
  {
    super( parent, SWT.NONE );

    Assert.isNotNull( dataModel );
    Assert.isNotNull( toolkit );

    m_dataModel = dataModel;

    GridLayoutFactory.swtDefaults().applyTo( this );
    toolkit.adapt( this );
    createControls( toolkit, this );

    m_dataModel.addKeyBasedDataChangeListener( m_modelChangeListener );
  }

  private void createControls( final FormToolkit toolkit, final Composite parent )
  {
    createDtmLabel( toolkit, parent ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createNoElevationSelector( toolkit, parent ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createNodeTable( toolkit, parent ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  private Control createDtmLabel( final FormToolkit toolkit, final Composite parent )
  {
    final Composite panel = toolkit.createComposite( parent );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( panel );

    toolkit.createLabel( panel, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.14" ) ); //$NON-NLS-1$

    m_inputText = toolkit.createText( panel, StringUtils.EMPTY );
    m_inputText.setMessage( Messages.getString( "AssignNodeElevationFaceComponent.1" ) ); //$NON-NLS-1$
    m_inputText.setEditable( false );
    m_inputText.setEnabled( false );
    m_inputText.setTextLimit( 100 );
    m_inputText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    return panel;
  }

  private Control createNoElevationSelector( final FormToolkit toolkit, final Composite parent )
  {
    final Composite panel = toolkit.createComposite( parent );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( panel );

    toolkit.createLabel( panel, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.17" ) ); //$NON-NLS-1$

    final Button selectNoElevationButton = toolkit.createButton( panel, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.18" ), SWT.PUSH ); //$NON-NLS-1$

    selectNoElevationButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        handleNoElevationButtonPressed();
      }
    } );

    return panel;
  }

  void handleNoElevationButtonPressed( )
  {
    final IFE1D2DNode[] allNonElevationNodes = ApplyElevationHelper.getAllNonElevationNodes( m_dataModel );
    m_nodeElevationViewer.setInput( allNonElevationNodes );
    m_nodeElevationViewer.refresh();
  }

  private Control createNodeTable( final FormToolkit toolkit, final Composite parent )
  {
    final Composite panel = toolkit.createComposite( parent );
    GridLayoutFactory.fillDefaults().numColumns( 1 ).applyTo( panel );

    final Table table = toolkit.createTable( panel, SWT.FULL_SELECTION | SWT.BORDER | SWT.MULTI );
    final GridData tableData = new GridData( SWT.FILL, SWT.FILL, true, true );
    tableData.minimumHeight = 100;
    tableData.heightHint = 100;
    tableData.minimumWidth = 2;
    tableData.widthHint = 2;
    table.setLayoutData( tableData );
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    table.addControlListener( new ColumnsResizeControlListener() );

    m_nodeElevationViewer = new TableViewer( table );
    m_nodeElevationViewer.setUseHashlookup( true );
    m_nodeElevationViewer.setColumnProperties( new String[] { "Node", "Elevation" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_nodeElevationViewer.setContentProvider( new ArrayContentProvider() );

    ColumnViewerUtil.createEmptyColumn( m_nodeElevationViewer );
    createElevationColumn( m_nodeElevationViewer );
    createNameColumn( m_nodeElevationViewer );

    final List<IFE1D2DNode> selectedNode = m_dataModel.getSelectedNode();
    if( selectedNode == null )
      m_nodeElevationViewer.setInput( new IFE1D2DNode[] {} );
    else
      m_nodeElevationViewer.setInput( selectedNode.toArray( new IFE1D2DNode[] {} ) );

    m_nodeElevationViewer.addSelectionChangedListener( nodeSelectionListener );

    final Composite buttonPanel = toolkit.createComposite( panel );
    buttonPanel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    GridLayoutFactory.fillDefaults().numColumns( 3 ).equalWidth( true ).applyTo( buttonPanel );

    final Button selectAll = toolkit.createButton( buttonPanel, WorkbenchMessages.SelectionDialog_selectLabel, SWT.PUSH );
    selectAll.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    selectAll.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        table.selectAll();
      }
    } );

    final Button selectAllWithoutElevation = toolkit.createButton( buttonPanel, Messages.getString( "AssignNodeElevationFaceComponent.2" ), SWT.PUSH ); //$NON-NLS-1$
    selectAllWithoutElevation.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    selectAllWithoutElevation.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        selectAllWithoutElevation();
      }
    } );

    final Button deSelectAll = toolkit.createButton( buttonPanel, WorkbenchMessages.SelectionDialog_deselectLabel, SWT.PUSH );
    deSelectAll.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    deSelectAll.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        table.deselectAll();
      }
    } );

    final Button applySelected = toolkit.createButton( buttonPanel, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.26" ), SWT.PUSH ); //$NON-NLS-1$
    applySelected.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    applySelected.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        applyElevation();
      }
    } );

    return panel;
  }

  protected void selectAllWithoutElevation( )
  {
    final List<IFE1D2DNode> nodesWithoutElevation = new ArrayList<>();

    final IFE1D2DNode[] allNodes = (IFE1D2DNode[])m_nodeElevationViewer.getInput();
    for( final IFE1D2DNode node : allNodes )
    {
      final GM_Point point = node.getPoint();
      if( point != null && Double.isNaN( point.getZ() ) )
        nodesWithoutElevation.add( node );
    }

    final ISelection selection = new StructuredSelection( nodesWithoutElevation );
    m_nodeElevationViewer.setSelection( selection );
  }

  private void createNameColumn( final TableViewer viewer )
  {
    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    final ViewerColumnItem column = new ViewerColumnItem( nameColumn );

    column.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.22" ) ); //$NON-NLS-1$
    column.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( column.getColumn() );
    nameColumn.setLabelProvider( new FENodeNameProvider() );
    ColumnViewerSorter.registerSorter( nameColumn, new FENodeNameComparator() );

    nameColumn.setEditingSupport( new FENodeNameEditingSupport( viewer ) );

    column.setWidth( 100 );
  }

  private void createElevationColumn( final TableViewer viewer )
  {
    final ViewerColumn heightColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.RIGHT );
    final ViewerColumnItem column = new ViewerColumnItem( heightColumn );

    column.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.23" ) ); //$NON-NLS-1$
    column.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( column.getColumn() );
    heightColumn.setLabelProvider( new FENodeHeightProvider() );
    ColumnViewerSorter.registerSorter( heightColumn, new FENodeHeightComparator() );

    heightColumn.setEditingSupport( new FENodeHeightEditingSupport( viewer, m_dataModel ) );

    column.setWidth( 100 );
  }

  protected final void applyElevation( )
  {
    try
    {
      final ISelection selection = m_nodeElevationViewer.getSelection();
      if( !(selection instanceof IStructuredSelection) )
        return;

      final List<IFE1D2DNode> nodeList = new ArrayList<>();

      for( final Object selected : ((IStructuredSelection)selection).toList() )
      {
        if( selected instanceof IFE1D2DNode )
          nodeList.add( (IFE1D2DNode)selected );
      }

      ApplyElevationHelper.assignElevationToSelectedNodes( m_dataModel, nodeList );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    m_nodeElevationViewer.refresh();
  }

  public TableViewer getTableViewer( )
  {
    return m_nodeElevationViewer;
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    m_selectionNodeList.clear();
    m_selectionNodeList.addAll( selection.toList() );

    m_dataModel.setSelectedNodeList( m_selectionNodeList );

    final IMapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel != null )
    {
      mapPanel.repaintMap();

      /* center on selected point */
      centerMapOnSelection( mapPanel, selection );
    }
  }

  private void centerMapOnSelection( final IMapPanel mapPanel, final IStructuredSelection selection )
  {
    if( selection.size() != 1 )
      return;

    final Object firstElement = selection.getFirstElement();
    if( !(firstElement instanceof IFE1D2DNode) )
      return;

    final IFE1D2DNode selectedNode = (IFE1D2DNode)firstElement;
    final GM_Point point = selectedNode.getPoint();
    if( point == null )
      return;

    final GM_Envelope boundingBox = mapPanel.getBoundingBox();
    if( boundingBox == null )
      return;

    final GM_Envelope panedBBox = mapPanel.getBoundingBox().getPaned( point );
    mapPanel.setBoundingBox( panedBBox );
  }

  protected void handleDataChanged( final String key, final Object newValue )
  {
    final Runnable todo = new Runnable()
    {
      @Override
      public void run( )
      {
        updateInput( key, newValue );
      }
    };
    final Control control = m_nodeElevationViewer.getControl();
    if( control != null && !control.isDisposed() )
      control.getDisplay().syncExec( todo );
  }

  protected void updateInput( final String key, final Object newValue )
  {
    if( ITerrainElevationModel.class.toString().equals( key ) )
    {
      final ITerrainElevationModel[] selection = (ITerrainElevationModel[])newValue;
      String name = StringUtils.EMPTY;
      if( selection != null )
      {
        final String[] names = new String[selection.length];
        for( int i = 0; i < names.length; i++ )
        {
          names[i] = selection[i].getName();
          if( StringUtils.isBlank( names[i] ) )
            names[i] = ((INativeTerrainElevationModelWrapper)newValue).getId();
        }

        name = StringUtils.join( names, ", " ); //$NON-NLS-1$
      }
      else
        name = Messages.getString( "AssignNodeElevationFaceComponent.3" ); //$NON-NLS-1$

      m_inputText.setText( name );
    }
    else if( ApplyElevationWidgetDataModel.SELECTED_NODE_KEY.equals( key ) )
    {
      if( m_nodeElevationViewer == null )
        return;

      m_nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
      m_nodeElevationViewer.setInput( newValue );
    }
  }
}