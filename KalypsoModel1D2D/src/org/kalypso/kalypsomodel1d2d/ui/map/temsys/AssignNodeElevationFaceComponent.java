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

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.model.Util;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeIFeatureWrapper2NameCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class AssignNodeElevationFaceComponent
{
  /**
   * @author Thomas Jung
   * 
   */
  private final class ICellModifierImplementation implements ICellModifier
  {
    @Override
    public boolean canModify( final Object element, final String property )
    {
      // Find the index of the column
      final Object[] properties = m_nodeElevationViewer.getColumnProperties();
      return property.equals( properties[0] ) || property.equals( properties[1] );
    }

    @Override
    public Object getValue( final Object element, final String property )
    {
      if( property.equals( m_nodeElevationViewer.getColumnProperties()[1] ) )
        return FENodeLabelProvider.getElevationString( (IFE1D2DNode) element );
      else if( property.equals( m_nodeElevationViewer.getColumnProperties()[0] ) )
        return FENodeLabelProvider.getNameOrID( (IFE1D2DNode) element );
      else
        return null;
    }

    @Override
    public void modify( final Object element, final String property, final Object value )
    {
      final IFE1D2DNode node;
      if( element instanceof TableItem )
      {
        final Object data = ((TableItem) element).getData();
        node = (data instanceof IFE1D2DNode) ? (IFE1D2DNode) data : null;
      }
      else
        return;

      if( property.equals( m_nodeElevationViewer.getColumnProperties()[1] ) )
      {
        if( FENodeLabelProvider.getElevationString( node ).equals( value ) )
          return;

        final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getDiscretisationModel();
        if( model1d2d == null )
          return;

        double newElevation;

        try
        {
          newElevation = Double.parseDouble( (String) value );

        }
        catch( final Throwable th )
        {
          // TODO Patrice show the user a message
          th.printStackTrace();
          return;
        }

        // return FENodeLabelProvider.getElevationString( (IFE1D2DNode) element );
        final IKalypsoFeatureTheme nodeTheme = (IKalypsoFeatureTheme) m_dataModel.getData( ApplyElevationWidgetDataModel.NODE_THEME );
        if( nodeTheme == null )
          System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.3" ) ); //$NON-NLS-1$

        final CommandableWorkspace workspace = Util.getCommandableWorkspace( IFEDiscretisationModel1d2d.class );// nodeTheme.getWorkspace();

        final Runnable updateTable = new Runnable()
        {
          @Override
          public void run( )
          {
            try
            {
              final IMapPanel mapPanel = m_dataModel.getMapPanel();
              mapPanel.invalidateMap();
              m_nodeElevationViewer.refresh( node, true );

            }
            catch( final Throwable th )
            {
              th.printStackTrace();
            }

          }
        };

        final ChangeNodePositionCommand command = new ChangeNodePositionCommand( model1d2d, node, newElevation, false )
        {
          /**
           * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand#process()
           */
          @Override
          public void process( ) throws Exception
          {
            super.process();

            final Display display = m_table.getDisplay();
            display.asyncExec( updateTable );
          }
        };
        try
        {
          workspace.postCommand( command );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
      else if( property.equals( m_nodeElevationViewer.getColumnProperties()[0] ) )
      {
        if( FENodeLabelProvider.getNameOrID( node ).equals( value ) )
        {
          System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.4" ) ); //$NON-NLS-1$
          return;
        }
        final CommandableWorkspace workspace = Util.getCommandableWorkspace( IFEDiscretisationModel1d2d.class );

        final Runnable updateTable = new Runnable()
        {
          @Override
          public void run( )
          {
            try
            {
              m_nodeElevationViewer.refresh( node, true );

            }
            catch( final Throwable th )
            {
              th.printStackTrace();
            }

          }
        };

        final ChangeIFeatureWrapper2NameCmd cmd = new ChangeIFeatureWrapper2NameCmd( node, (String) value )
        {
          /**
           * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeIFeatureWrapper2NameCmd#process()
           */
          @Override
          public void process( ) throws Exception
          {
            super.process();
            final Display display = m_table.getDisplay();
            display.asyncExec( updateTable );
          }
        };
        try
        {
          workspace.postCommand( cmd );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
      else
        System.out.println( "BAD property:" + property ); //$NON-NLS-1$
    }
  }

  private ApplyElevationWidgetDataModel m_dataModel;

  private Text m_inputText;

  private Table m_table;

  protected final List<IFE1D2DNode> m_selectionNodeList = new ArrayList<IFE1D2DNode>();

  protected TableViewer m_nodeElevationViewer;

  private final ICellModifier m_cellModifier = new ICellModifierImplementation();

  /**
   * Listen to node selection and fill the selection list
   */
  private final ISelectionChangedListener nodeSelectionListener = new ISelectionChangedListener()
  {
    @Override
    public void selectionChanged( final SelectionChangedEvent event )
    {
      final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
      m_selectionNodeList.clear();
      final List tableSelection = selection.toList();
      m_selectionNodeList.addAll( tableSelection );

      m_dataModel.setSelectedNodeList( m_selectionNodeList );
      final IMapPanel mapPanel = m_dataModel.getMapPanel();
      if( mapPanel != null )
        mapPanel.repaintMap();
    }

  };

  final KeyBasedDataModelChangeListener modelChangeListener = new KeyBasedDataModelChangeListener()
  {
    @Override
    public void dataChanged( final String key, final Object newValue )
    {
      if( ITerrainElevationModel.class.toString().equals( key ) )
      {
        if( newValue == null )
          m_inputText.setText( " " ); //$NON-NLS-1$
        else
        {
          String name = ((INativeTerrainElevationModelWrapper) newValue).getName();
          if( name == null )
            name = ((INativeTerrainElevationModelWrapper) newValue).getGmlID();
          m_inputText.setText( name );
        }
      }
      else if( ApplyElevationWidgetDataModel.SELECTED_NODE_KEY.equals( key ) )
      {
        if( m_nodeElevationViewer == null )
          return;
        final Runnable todo = new Runnable()
        {
          @Override
          public void run( )
          {
            m_nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
            m_nodeElevationViewer.setInput( newValue );
          }
        };
        final Control control = m_nodeElevationViewer.getControl();
        if( control != null && !control.isDisposed() )
          control.getDisplay().syncExec( todo );
      }
    }
  };

  private final ICellEditorValidator doubleValidator = new ICellEditorValidator()
  {

    @Override
    public String isValid( final Object value )
    {
      try
      {
        Double.parseDouble( (String) value );
        return null;
      }
      catch( final Throwable th )
      {
        return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.11" ); //$NON-NLS-1$
      }

    }

  };

  private Label selectNoElevationLabel;

  private Button selectNoElevationButton;

  private Group nodeViewerGroup;

  private Group noElevationGroup;

  AssignNodeElevationFaceComponent( )
  {

  }

  public void createControl( final ApplyElevationWidgetDataModel dataModel, final FormToolkit toolKit, final Composite parent )
  {
    Assert.throwIAEOnNullParam( dataModel, "dataModel" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( toolKit, "toolKit" ); //$NON-NLS-1$
    m_dataModel = dataModel;
    guiCreateSelectRegion( parent );
    m_dataModel.addKeyBasedDataChangeListener( modelChangeListener );
  }

  private void guiCreateSelectRegion( final Composite cComposite )
  {
    FormData regionFormData;
    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 5 );
    regionFormData.top = new FormAttachment( 0, 5 );
    final Label infoLabel = new Label( cComposite, SWT.FLAT );
    infoLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.14" )/* "Selected Terrain Model" */); //$NON-NLS-1$
    infoLabel.setLayoutData( regionFormData );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( infoLabel, 10 );
    regionFormData.top = new FormAttachment( 0, 5 );
    this.m_inputText = new Text( cComposite, SWT.FLAT | SWT.BORDER );
    m_inputText.setEditable( false );
    m_inputText.setText( "" ); //$NON-NLS-1$
    m_inputText.setTextLimit( 100 );
    m_inputText.setLayoutData( regionFormData );

    // The No Elevation Group
    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 5 );
    regionFormData.top = new FormAttachment( infoLabel, 10 );
    // regionFormData.right = new FormAttachment( 100, 0 );
    noElevationGroup = new Group( cComposite, SWT.NONE );
    noElevationGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.16" ) );// Modelle Knoten Suchen //$NON-NLS-1$
    // //$NON-NLS-1$
    noElevationGroup.setLayoutData( regionFormData );
    noElevationGroup.setLayout( new GridLayout( 2, false ) );

    selectNoElevationLabel = new Label( noElevationGroup, SWT.FLAT );
    selectNoElevationLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.17" ) ); //$NON-NLS-1$

    selectNoElevationButton = new Button( noElevationGroup, SWT.PUSH );
    selectNoElevationButton.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.18" ) ); //$NON-NLS-1$
    selectNoElevationButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        final IFE1D2DNode[] allNonElevationNodes = ApplyElevationHelper.getAllNonElevationNodes( m_dataModel );
        m_nodeElevationViewer.setInput( allNonElevationNodes );
        m_nodeElevationViewer.refresh();
      }
    } );

    // The Node Viewer Group
    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 5 );
    regionFormData.top = new FormAttachment( noElevationGroup, 10 );
    // regionFormData.right = new FormAttachment( 100, 0 );
    nodeViewerGroup = new Group( cComposite, SWT.NONE );
    nodeViewerGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.19" ) ); //$NON-NLS-1$
    nodeViewerGroup.setLayoutData( regionFormData );
    nodeViewerGroup.setLayout( new FormLayout() );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 10 );
    regionFormData.top = new FormAttachment( 0, 10 );
    regionFormData.bottom = new FormAttachment( 100, -10 );
    regionFormData.height = 70;

    m_nodeElevationViewer = new TableViewer( nodeViewerGroup, SWT.FULL_SELECTION | SWT.BORDER | SWT.MULTI );
    m_nodeElevationViewer.setUseHashlookup( true );
    m_nodeElevationViewer.setColumnProperties( new String[] { "Node", "Elevation" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_nodeElevationViewer.setLabelProvider( new FENodeLabelProvider() );
    m_nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
    // nodeElevationViewer.setSorter( new FENodeViewerSorter() );

    m_table = m_nodeElevationViewer.getTable();
    m_table.setLayoutData( regionFormData );
    m_table.setHeaderVisible( true );
    m_table.setLinesVisible( true );

    final TableColumn lineColumn = new TableColumn( m_table, SWT.LEFT );
    lineColumn.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.22" )/* "Node" */); //$NON-NLS-1$
    lineColumn.setWidth( 100 / 1 );
    lineColumn.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        m_nodeElevationViewer.setSorter( new FENodeViewerSorter( lineColumn ) );
        m_nodeElevationViewer.refresh();
      }
    } );

    final TableColumn actualPointNum = new TableColumn( m_table, SWT.LEFT );
    actualPointNum.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.23" )/* "Elevation" */); //$NON-NLS-1$
    actualPointNum.setWidth( 100 / 2 );
    actualPointNum.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        m_nodeElevationViewer.setSorter( new FENodeViewerSorter( actualPointNum ) );
        m_nodeElevationViewer.refresh();
      }
    } );

    final List<IFE1D2DNode> selectedNode = m_dataModel.getSelectedNode();
    if( selectedNode == null )
      m_nodeElevationViewer.setInput( new IFE1D2DNode[] {} );
    else
      m_nodeElevationViewer.setInput( selectedNode.toArray( new IFE1D2DNode[] {} ) );

    m_nodeElevationViewer.addSelectionChangedListener( nodeSelectionListener );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( m_table, 5 );
    regionFormData.top = new FormAttachment( 0, 10 );
    regionFormData.right = new FormAttachment( 100, -2 );
    final Button selectAll = new Button( nodeViewerGroup, SWT.PUSH );
    selectAll.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.24" )/* "Select All" */); //$NON-NLS-1$
    selectAll.setLayoutData( regionFormData );
    selectAll.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        m_table.selectAll();
      }
    } );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( m_table, 5 );
    regionFormData.top = new FormAttachment( selectAll, 5 );
    regionFormData.right = new FormAttachment( 100, -2 );
    final Button deSelectAll = new Button( nodeViewerGroup, SWT.PUSH );
    deSelectAll.setLayoutData( regionFormData );
    deSelectAll.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.25" )/* "DeSelect All" */); //$NON-NLS-1$

    deSelectAll.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        m_table.deselectAll();
      }

    } );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( m_table, 5 );
    regionFormData.top = new FormAttachment( deSelectAll, 5 );
    regionFormData.right = new FormAttachment( 100, -2 );
    final Button applySelected = new Button( nodeViewerGroup, SWT.PUSH );
    applySelected.setLayoutData( regionFormData );
    applySelected.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.26" )/* "Apply Selected" */); //$NON-NLS-1$
    applySelected.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        try
        {
          applyElevation();
          m_nodeElevationViewer.refresh();
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }

    } );

    final TextCellEditor textCellEditor = new TextCellEditor( m_table );
    final TextCellEditor eleCellEditor = new TextCellEditor( m_table );
    ((Text) eleCellEditor.getControl()).setTextLimit( 15 );
    eleCellEditor.setValidator( this.doubleValidator );

    final CellEditor[] editors = new CellEditor[] { textCellEditor, eleCellEditor };
    m_nodeElevationViewer.setCellEditors( editors );
    m_nodeElevationViewer.setCellModifier( m_cellModifier );
  }

  private final void applyElevation( ) throws Exception
  {
    final ISelection selection = m_nodeElevationViewer.getSelection();
    if( !(selection instanceof IStructuredSelection) )
      return;

    final List<IFE1D2DNode> nodeList = new ArrayList<IFE1D2DNode>();
    for( final Object selected : ((IStructuredSelection) selection).toList() )
    {
      if( selected instanceof IFE1D2DNode )
      {
        nodeList.add( (IFE1D2DNode) selected );
      }
    }

    ApplyElevationHelper.assignElevationToSelectedNodes( m_dataModel, nodeList );
  }

  public TableViewer getTableViewer( )
  {
    return m_nodeElevationViewer;
  }

}
