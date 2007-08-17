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
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.ops.NodeOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeIFeatureWrapper2NameCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class AssignNodeElevationFaceComponent
{
  ApplyElevationWidgetDataModel m_dataModel;

  private Text inputText;

  private Table table;

  private final List<IFE1D2DNode> selectionNodeList = new ArrayList<IFE1D2DNode>();

  private TableViewer nodeElevationViewer;

  private final ICellModifier modifier = new ICellModifier()
  {

    public boolean canModify( Object element, String property )
    {
      System.out.println( "CanmOdiy=" + property ); //$NON-NLS-1$
      // Find the index of the column
      if( property.equals( nodeElevationViewer.getColumnProperties()[1] ) )
      {
        return true;
      }
      else if( property.equals( nodeElevationViewer.getColumnProperties()[0] ) )
      {
        return true;
      }
      else
      {
        return false;
      }
    }

    public Object getValue( Object element, String property )
    {
      if( property.equals( nodeElevationViewer.getColumnProperties()[1] ) )
      {
        return FENodeLabelProvider.getElevationString( (IFE1D2DNode) element );
      }
      else if( property.equals( nodeElevationViewer.getColumnProperties()[0] ) )
      {
        return FENodeLabelProvider.getNameOrID( (IFE1D2DNode) element );
      }
      else
      {
        return null;
      }
    }

    public void modify( Object element, String property, Object value )
    {

      final IFE1D2DNode node;
      if( element instanceof TableItem )
      {
        Object data = ((TableItem) element).getData();
        node = (data instanceof IFE1D2DNode) ? (IFE1D2DNode) data : null;
      }
      else
      {
        return;
      }

      if( property.equals( nodeElevationViewer.getColumnProperties()[1] ) )
      {
        if( FENodeLabelProvider.getElevationString( node ).equals( value ) )
        {
          System.out.println( Messages.getString("AssignNodeElevationFaceComponent.1") ); //$NON-NLS-1$
          return;
        }

        final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getDiscretisationModel();
        if( model1d2d == null )
        {
          System.out.println( Messages.getString("AssignNodeElevationFaceComponent.2") ); //$NON-NLS-1$
          return;
        }
        double newElevation;

        try
        {
          newElevation = Double.parseDouble( (String) value );
        }
        catch( Throwable th )
        {
          // TODO Patrice show the user a message
          th.printStackTrace();
          return;
        }

        // return FENodeLabelProvider.getElevationString( (IFE1D2DNode) element );
        final IKalypsoFeatureTheme nodeTheme = (IKalypsoFeatureTheme) m_dataModel.getData( ApplyElevationWidgetDataModel.NODE_THEME );
        if( nodeTheme == null )
        {
          System.out.println( Messages.getString("AssignNodeElevationFaceComponent.3") ); //$NON-NLS-1$
        }

        final CommandableWorkspace workspace = Util.getCommandableWorkspace( IFEDiscretisationModel1d2d.class );// nodeTheme.getWorkspace();

        final Runnable updateTable = new Runnable()
        {
          public void run( )
          {
            try
            {
              MapPanel mapPanel = m_dataModel.getMapPanel();
              mapPanel.invalidateMap();
              nodeElevationViewer.refresh( node, true );

            }
            catch( Throwable th )
            {
              th.printStackTrace();
            }

          }
        };

        final ChangeNodePositionCommand command = new ChangeNodePositionCommand( model1d2d, node, newElevation )
        {
          /**
           * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand#process()
           */
          @Override
          public void process( ) throws Exception
          {
            super.process();

            // get notification fired for the nodes
            FeatureStructureChangeModellEvent modellEvent = new FeatureStructureChangeModellEvent( workspace, model1d2d.getWrappedFeature(), new Feature[] { getMovedNode().getWrappedFeature(),
                model1d2d.getWrappedFeature() },// changedFeaturesArray,
            FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
            workspace.fireModellEvent( modellEvent );
            Display display = table.getDisplay();
            display.asyncExec( updateTable );
          }
        };
        try
        {
          workspace.postCommand( command );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
      else if( property.equals( nodeElevationViewer.getColumnProperties()[0] ) )
      {
        if( FENodeLabelProvider.getNameOrID( node ).equals( value ) )
        {
          System.out.println( Messages.getString("AssignNodeElevationFaceComponent.4") ); //$NON-NLS-1$
          return;
        }
        final CommandableWorkspace workspace = Util.getCommandableWorkspace( IFEDiscretisationModel1d2d.class );

        final Runnable updateTable = new Runnable()
        {
          public void run( )
          {
            try
            {
              nodeElevationViewer.refresh( node, true );

            }
            catch( Throwable th )
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
            Display display = table.getDisplay();
            display.asyncExec( updateTable );
          }
        };
        try
        {
          workspace.postCommand( cmd );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
        // node.setName( (String) value );
        // Feature nodeFeature = node.getWrappedFeature();
        //
        //
        // Feature nodeParent = nodeFeature.getParent();
        // final FeatureStructureChangeModellEvent modellEvent =
        // new FeatureStructureChangeModellEvent(
        // workspace,
        // nodeParent,
        // new Feature[] { nodeFeature, nodeParent },
        // FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
        // workspace.fireModellEvent( modellEvent );
        // nodeElevationViewer.refresh();
      }
      else
      {
        System.out.println( "BAD property:" + property ); //$NON-NLS-1$
      }
    }

  };

  MouseListener mouseListener = new MouseListener()
  {

    public void mouseDoubleClick( MouseEvent e )
    {
      System.out.println( "mouse klick" ); //$NON-NLS-1$
      m_dataModel.setIgnoreMapSelection( true );
    }

    public void mouseDown( MouseEvent e )
    {
      m_dataModel.setIgnoreMapSelection( true );
      System.out.println( "mouse down" ); //$NON-NLS-1$

    }

    public void mouseUp( MouseEvent e )
    {
      System.out.println( "mouse up" ); //$NON-NLS-1$
    }

  };

  /**
   * Listen to node selection and fill the selection list
   */
  private final ISelectionChangedListener nodeSelectionListener = new ISelectionChangedListener()
  {

    public void selectionChanged( SelectionChangedEvent event )
    {

      // if(nodeElevationViewer.getControl().getParent().isFocusControl())
      // {
      // dataModel.setIgnoreMapSelection( true );

      IStructuredSelection selection = (IStructuredSelection) event.getSelection();
      selectionNodeList.clear();
      List tableSelection = selection.toList();
      selectionNodeList.addAll( tableSelection );
      // to easy feature wrapper
      EasyFeatureWrapper featureWrappers[] = new EasyFeatureWrapper[tableSelection.size()];
      CommandableWorkspace workspace = null;
      Feature parentFeature = m_dataModel.getDiscretisationModel().getWrappedFeature();
      IRelationType property = null;

      for( int i = featureWrappers.length - 1; i >= 0; i-- )
      {

        featureWrappers[i] = new EasyFeatureWrapper( workspace, ((IFE1D2DNode) tableSelection.get( i )).getWrappedFeature(), parentFeature, property );
      }
      m_dataModel.getMapPanel().getSelectionManager().setSelection( featureWrappers );
      // }
      // else
      // {
      // // dataModel.setIgnoreMapSelection( false );
      // }
    }

  };

  KeyBasedDataModelChangeListener modelChangeListener = new KeyBasedDataModelChangeListener()
  {

    public void dataChanged( String key, final Object newValue )
    {
      System.out.println( "Key=" + key ); //$NON-NLS-1$
      if( ITerrainElevationModel.class.toString().equals( key ) )
      {
        if( newValue == null )
        {
          inputText.setText( " " ); //$NON-NLS-1$
        }
        else
        {
          String name = ((INativeTerrainElevationModelWrapper) newValue).getName();
          if( name == null )
          {
            name = ((INativeTerrainElevationModelWrapper) newValue).getGmlID();

          }
          inputText.setText( name );
        }
      }
      else if( ApplyElevationWidgetDataModel.SELECTED_NODE_KEY.equals( key ) )
      {

        if( nodeElevationViewer == null )
        {
          return;
        }
        Runnable todo = new Runnable()
        {

          public void run( )
          {
            nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
            nodeElevationViewer.setInput( newValue );
          }

        };
        nodeElevationViewer.getControl().getDisplay().syncExec( todo );
      }
    }

  };

  private final ICellEditorValidator doubleValidator = new ICellEditorValidator()
  {

    public String isValid( Object value )
    {
      try
      {
        Double.parseDouble( (String) value );
        return null;
      }
      catch( Throwable th )
      {
        return Messages.getString("AssignNodeElevationFaceComponent.11"); //$NON-NLS-1$
      }

    }

  };

  private Label selectNoElevationLabel;

  private Button selectNoElevationButton;

  private SelectionListener noElevationBtnAdapter;

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
    infoLabel.setText( Messages.getString("AssignNodeElevationFaceComponent.14")/* "Selected Terrain Model" */); //$NON-NLS-1$
    infoLabel.setLayoutData( regionFormData );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( infoLabel, 10 );
    regionFormData.top = new FormAttachment( 0, 5 );
    this.inputText = new Text( cComposite, SWT.FLAT | SWT.BORDER );
    inputText.setEditable( false );
    inputText.setText( "" ); //$NON-NLS-1$
    inputText.setTextLimit( 100 );
    inputText.setLayoutData( regionFormData );

    // The No Elevation Group
    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 5 );
    regionFormData.top = new FormAttachment( infoLabel, 10 );
    // regionFormData.right = new FormAttachment( 100, 0 );
    noElevationGroup = new Group( cComposite, SWT.NONE );
    noElevationGroup.setText( Messages.getString("AssignNodeElevationFaceComponent.16") );// Modelle Knoten Suchen //$NON-NLS-1$
    noElevationGroup.setLayoutData( regionFormData );
    noElevationGroup.setLayout( new GridLayout( 2, false ) );

    selectNoElevationLabel = new Label( noElevationGroup, SWT.FLAT );
    selectNoElevationLabel.setText( Messages.getString("AssignNodeElevationFaceComponent.17") ); //$NON-NLS-1$

    selectNoElevationButton = new Button( noElevationGroup, SWT.PUSH );
    selectNoElevationButton.setText( Messages.getString("AssignNodeElevationFaceComponent.18") ); //$NON-NLS-1$
    selectNoElevationButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        getAllNonElevationNodes();
      }
    } );

    // The Node Viewer Group
    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 5 );
    regionFormData.top = new FormAttachment( noElevationGroup, 10 );
    // regionFormData.right = new FormAttachment( 100, 0 );
    nodeViewerGroup = new Group( cComposite, SWT.NONE );
    nodeViewerGroup.setText( Messages.getString("AssignNodeElevationFaceComponent.19") ); //$NON-NLS-1$
    nodeViewerGroup.setLayoutData( regionFormData );
    nodeViewerGroup.setLayout( new FormLayout() );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 10 );
    regionFormData.top = new FormAttachment( 0, 10 );
    regionFormData.bottom = new FormAttachment( 100, -10 );
    regionFormData.height = 70;
    nodeElevationViewer = new TableViewer( nodeViewerGroup, SWT.FULL_SELECTION | SWT.BORDER | SWT.MULTI );
    nodeElevationViewer.setUseHashlookup( true );
    nodeElevationViewer.setColumnProperties( new String[] { "Node", "Elevation" } ); //$NON-NLS-1$ //$NON-NLS-2$
    nodeElevationViewer.setLabelProvider( new FENodeLabelProvider() );
    nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
    // nodeElevationViewer.setSorter( new FENodeViewerSorter() );

    this.table = nodeElevationViewer.getTable();
    table.setLayoutData( regionFormData );
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    final TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
    lineColumn.setText( Messages.getString("AssignNodeElevationFaceComponent.22")/* "Node" */); //$NON-NLS-1$
    lineColumn.setWidth( 100 / 1 );
    lineColumn.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        nodeElevationViewer.setSorter( new FENodeViewerSorter( lineColumn ) );
        nodeElevationViewer.refresh();
        // System.out.println("fired 0");
      }
    } );

    final TableColumn actualPointNum = new TableColumn( table, SWT.LEFT );
    actualPointNum.setText( Messages.getString("AssignNodeElevationFaceComponent.23")/* "Elevation" */); //$NON-NLS-1$
    actualPointNum.setWidth( 100 / 2 );
    actualPointNum.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        nodeElevationViewer.setSorter( new FENodeViewerSorter( actualPointNum ) );
        nodeElevationViewer.refresh();
        // System.out.println("fired 1");
      }
    } );

    final List<IFE1D2DNode> selectedNode = m_dataModel.getSelectedNode();
    if( selectedNode == null )
    {
      nodeElevationViewer.setInput( new IFE1D2DNode[] {} );
    }
    else
    {
      nodeElevationViewer.setInput( selectedNode.toArray( new IFE1D2DNode[] {} ) );
    }

    nodeElevationViewer.addSelectionChangedListener( nodeSelectionListener );
    // nodeElevationViewer.getControl().addFocusListener( focusListener );
    nodeElevationViewer.getTable().addMouseListener( mouseListener );
    // nodeElevationViewer.getControl().addMouseListener( mouseListener );

    // actualPointNum.addListener(SWT.Selection, new Listener() {
    // public void handleEvent(Event e) {
    // // sort column 2
    // TableItem[] items = table.getItems();
    // Collator collator = Collator.getInstance(Locale.getDefault());
    // for (int i = 1; i < items.length; i++) {
    // String value1 = items[i].getText(1);
    // for (int j = 0; j < i; j++) {
    // String value2 = items[j].getText(1);
    // if (collator.compare(value1, value2) < 0) {
    // String[] values = { items[i].getText(0),
    // items[i].getText(1) };
    // items[i].dispose();
    // TableItem item = new TableItem(table, SWT.NONE, j);
    // item.setText(values);
    // items = table.getItems();
    // break;
    // }
    // }
    // }
    // }
    // });

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( table, 5 );
    regionFormData.top = new FormAttachment( 0, 10 );
    regionFormData.right = new FormAttachment( 100, -2 );
    final Button selectAll = new Button( nodeViewerGroup, SWT.PUSH );
    selectAll.setText( Messages.getString("AssignNodeElevationFaceComponent.24")/* "Select All" */); //$NON-NLS-1$
    selectAll.setLayoutData( regionFormData );
    selectAll.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        table.selectAll();
      }
    } );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( table, 5 );
    regionFormData.top = new FormAttachment( selectAll, 5 );
    regionFormData.right = new FormAttachment( 100, -2 );
    final Button deSelectAll = new Button( nodeViewerGroup, SWT.PUSH );
    deSelectAll.setLayoutData( regionFormData );
    deSelectAll.setText( Messages.getString("AssignNodeElevationFaceComponent.25")/* "DeSelect All" */); //$NON-NLS-1$

    deSelectAll.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        table.deselectAll();
      }

    } );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( table, 5 );
    regionFormData.top = new FormAttachment( deSelectAll, 5 );
    regionFormData.right = new FormAttachment( 100, -2 );
    final Button applySelected = new Button( nodeViewerGroup, SWT.PUSH );
    applySelected.setLayoutData( regionFormData );
    applySelected.setText( Messages.getString("AssignNodeElevationFaceComponent.26")/* "Apply Selected" */); //$NON-NLS-1$
    applySelected.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        try
        {
          applyElevation();
          nodeElevationViewer.refresh();
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }

    } );

    // noElevationBtnAdapter =

    final TextCellEditor textCellEditor = new TextCellEditor( table );
    final TextCellEditor eleCellEditor = new TextCellEditor( table );
    ((Text) eleCellEditor.getControl()).setTextLimit( 15 );
    eleCellEditor.setValidator( this.doubleValidator );

    final CellEditor[] editors = new CellEditor[] { textCellEditor, eleCellEditor };
    nodeElevationViewer.setCellEditors( editors );
    nodeElevationViewer.setCellModifier( modifier );

  }

  void getAllNonElevationNodes( )
  {
    final List<IFE1D2DNode> allNodes = m_dataModel.getDiscretisationModel().getNodes();
    final List<IFE1D2DNode> noElevationNodes = new ArrayList<IFE1D2DNode>();

    for( int i = 0; i < allNodes.size(); i++ )
    {

      try
      {
        if( !NodeOps.hasElevation( allNodes.get( i ) ) )
        {
          noElevationNodes.add( allNodes.get( i ) );
        }
      }
      catch( final RuntimeException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }

    nodeElevationViewer.setInput( noElevationNodes.toArray( new IFE1D2DNode[] {} ) );
    nodeElevationViewer.refresh();
    System.out.println( Messages.getString("AssignNodeElevationFaceComponent.27") + allNodes.size() ); //$NON-NLS-1$
    System.out.println( Messages.getString("AssignNodeElevationFaceComponent.28") + noElevationNodes.size() ); //$NON-NLS-1$
  }

  private final void applyElevation( ) throws Exception
  {
    // System.out.println( "List of Elements Selected " + selectionNodeList.size() );
    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getDiscretisationModel();
    if( model1d2d == null )
    {
      System.out.println( Messages.getString("AssignNodeElevationFaceComponent.29") ); //$NON-NLS-1$
    }

    final IKalypsoFeatureTheme elevationTheme = m_dataModel.getElevationTheme();
    if( elevationTheme == null )
    {
      return;
    }
    // CommandableWorkspace workspace = elevationTheme.getWorkspace();
    final CommandableWorkspace workspace = m_dataModel.getDiscretisationModelWorkspace();// Util.getCommandableWorkspace(
    // IFEDiscretisationModel1d2d.class
    // );//
    // nodeTheme.getWorkspace();

    if( workspace == null )
    {
      return;
    }

    IElevationProvider elevationProvider = m_dataModel.getElevationModel();
    if( elevationProvider == null )
    {
      elevationProvider = m_dataModel.getElevationModelSystem();
      if( elevationProvider == null )
      {
        return;
      }
    }

    final ChangeTerrainElevationSystemCommand compositeCommand = new ChangeTerrainElevationSystemCommand( workspace, model1d2d );
    ChangeNodePositionCommand changePosCmd;
    double elevation;

    final ISelection selection = nodeElevationViewer.getSelection();
    if( !(selection instanceof IStructuredSelection) )
    {
      return;
    }
    for( final Object selected : ((IStructuredSelection) selection).toList() )
    {
      if( selected instanceof IFE1D2DNode )
      {
        final IFE1D2DNode node = (IFE1D2DNode) selected;
        elevation = elevationProvider.getElevation( node.getPoint() );
        changePosCmd = new ChangeNodePositionCommand( model1d2d, node, elevation );
        changePosCmd.process();
        compositeCommand.addCommand( changePosCmd, null );
      }
    }

    // just reveal the command to the undo framework
    elevationTheme.postCommand( compositeCommand, null );
    m_dataModel.getDiscretisationModelWorkspace().getCommandManager().postCommand( compositeCommand );
  }
}
