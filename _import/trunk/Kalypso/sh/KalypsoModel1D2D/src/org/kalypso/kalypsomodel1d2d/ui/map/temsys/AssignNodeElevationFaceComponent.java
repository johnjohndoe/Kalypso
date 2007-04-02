/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.ops.NodeOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class AssignNodeElevationFaceComponent
{
  ApplyElevationWidgetDataModel dataModel;

  private FormToolkit toolKit;

  private Text inputText;

  private Table table;

  private List<IFE1D2DNode> selectionNodeList = new ArrayList<IFE1D2DNode>();

  private TableViewer nodeElevationViewer;

  private ICellModifier modifier = new ICellModifier()
  {

    public boolean canModify( Object element, String property )
    {
      System.out.println( "CanmOdiy=" + property );
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

      IFE1D2DNode node = null;
      if( element instanceof TableItem )
      {
        Object data = ((TableItem) element).getData();
        if( data instanceof IFE1D2DNode )
        {
          node = (IFE1D2DNode) data;
        }

      }
      if( property.equals( nodeElevationViewer.getColumnProperties()[1] ) )
      {
        if( FENodeLabelProvider.getElevationString( node ).equals( value ) )
        {
          System.out.println( "No change in the elevation!" );
          return;
        }

        IFEDiscretisationModel1d2d model1d2d = dataModel.getDiscretisationModel();
        if( model1d2d != null )
        {
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
          ChangeNodePositionCommand command = new ChangeNodePositionCommand( model1d2d, node, newElevation );
          IKalypsoFeatureTheme nodeTheme = (IKalypsoFeatureTheme) dataModel.getData( ApplyElevationWidgetDataModel.NODE_THEME );
          if( nodeTheme == null )
          {
            return;
          }
          try
          {
            // manual execute of the command needed because otherwise update done
            // before the command execution by the framework sothat the table does
            // not display the right label
            command.process();
            nodeTheme.postCommand( command, null );
            // command.process();

          }
          catch( Throwable th )
          {
            th.printStackTrace();
          }
          // nodeElevationViewer.up
          nodeElevationViewer.refresh( node, true );
          // nodeElevationViewer.refresh( element, true );
          System.out.println( "adadad" + node.getPoint() );
        }
      }
      else if( property.equals( nodeElevationViewer.getColumnProperties()[0] ) )
      {
        if( FENodeLabelProvider.getNameOrID( node ).equals( value ) )
        {
          System.out.println( "No name change!" );
          return;
        }
        node.setName( (String) value );
        nodeElevationViewer.refresh();// true);//, new String[]{property});
      }
      else
      {
        System.out.println( "BAD property:" + property );
      }
    }

  };

  // FocusListener focusListener = new FocusListener()
  // {
  //
  // public void focusGained( FocusEvent e )
  // {
  // dataModel.setIgnoreMapSelection( true );
  // }
  //
  // public void focusLost( FocusEvent e )
  // {
  // dataModel.setIgnoreMapSelection( false );
  // }
  //    
  // };

  MouseListener mouseListener = new MouseListener()
  {

    public void mouseDoubleClick( MouseEvent e )
    {
      System.out.println( "mouse klick" );
      dataModel.setIgnoreMapSelection( true );
    }

    public void mouseDown( MouseEvent e )
    {
      dataModel.setIgnoreMapSelection( true );
      System.out.println( "mouse down" );

    }

    public void mouseUp( MouseEvent e )
    {
      System.out.println( "mouse up" );
    }

  };

  /**
   * Listen to node selection and fill the selection list
   */
  private ISelectionChangedListener nodeSelectionListener = new ISelectionChangedListener()
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
      Feature parentFeature = dataModel.getDiscretisationModel().getWrappedFeature();
      IRelationType property = null;

      for( int i = featureWrappers.length - 1; i >= 0; i-- )
      {

        featureWrappers[i] = new EasyFeatureWrapper( workspace, ((IFE1D2DNode) tableSelection.get( i )).getWrappedFeature(), parentFeature, property );
      }
      dataModel.getMapPanel().getSelectionManager().setSelection( featureWrappers );
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
      System.out.println( "Key=" + key );
      if( ITerrainElevationModel.class.toString().equals( key ) )
      {
        if( newValue == null )
        {
          inputText.setText( " " );
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

  private ICellEditorValidator doubleValidator = new ICellEditorValidator()
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
        return "Geben Sie eine Zahl ein";
      }

    }

  };

  private Label selectNoElevationLabel;

  private Button selectNoElevationButton;

  private SelectionListener noElevationBtnAdapter;

  AssignNodeElevationFaceComponent( )
  {

  }

  public void createControl( ApplyElevationWidgetDataModel dataModel, FormToolkit toolKit, Composite parent )
  {
    Assert.throwIAEOnNullParam( dataModel, "dataModel" );
    Assert.throwIAEOnNullParam( toolKit, "toolKit" );
    this.dataModel = dataModel;
    this.toolKit = toolKit;
    guiCreateSelectRegion( parent );
    dataModel.addKeyBasedDataChangeListener( modelChangeListener );
  }

  private void guiCreateSelectRegion( Composite cComposite )
  {
    FormData regionFormData;
    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 5 );
    regionFormData.top = new FormAttachment( 0, 5 );
    Label infoLabel = new Label( cComposite, SWT.FLAT );
    infoLabel.setText( "Aktives Höhenmodell"/* "Selected Terrain Model" */);
    infoLabel.setLayoutData( regionFormData );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( infoLabel, 10 );
    regionFormData.top = new FormAttachment( 0, 5 );
    this.inputText = new Text( cComposite, SWT.FLAT | SWT.BORDER );
    inputText.setEditable( false );
    inputText.setText( "" );
    inputText.setLayoutData( regionFormData );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 5 );
    regionFormData.top = new FormAttachment( infoLabel, 10 );
    selectNoElevationLabel = new Label( cComposite, SWT.FLAT );
    selectNoElevationLabel.setText( "Select Nodes with NoElevation" );
    selectNoElevationLabel.setLayoutData( regionFormData );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( selectNoElevationLabel, 5 );
    regionFormData.top = new FormAttachment( infoLabel, 10 );
    selectNoElevationButton = new Button( cComposite, SWT.PUSH );
    selectNoElevationButton.setText( "Nodes" );
    selectNoElevationButton.setLayoutData( regionFormData );
    selectNoElevationButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent event )
      {
        getAllNonElevationNodes();
      }
    });
    
    // new SelectionAdapter()
    // {
    // public void widgetSelected( SelectionEvent event )
    // {
    // table.selectAll();
    // }
    //
    // } );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 5 );
    regionFormData.top = new FormAttachment( selectNoElevationLabel, 5 );
    // regionFormData.height = 70;
    Label areaSelectLabel = new Label( cComposite, SWT.FLAT );
    areaSelectLabel.setText( "Editierbare Knoten"/* "Select Area" */);
    areaSelectLabel.setLayoutData( regionFormData );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( 0, 10 );
    regionFormData.top = new FormAttachment( selectNoElevationButton, 10 );
    regionFormData.height = 70;
    nodeElevationViewer = new TableViewer( cComposite, SWT.FULL_SELECTION | SWT.BORDER | SWT.MULTI );
    this.table = nodeElevationViewer.getTable();
    table.setLayoutData( regionFormData );

    TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
    lineColumn.setText( "Knoten"/* "Node" */);
    lineColumn.setWidth( 100 / 1 );
    TableColumn actualPointNum = new TableColumn( table, SWT.LEFT );
    actualPointNum.setText( "Höhe   "/* "Elevation" */);
    actualPointNum.setWidth( 100 / 2 );

    nodeElevationViewer.setUseHashlookup( true );
    nodeElevationViewer.setColumnProperties( new String[] { "Node", "Elevation" } );

    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    nodeElevationViewer.setLabelProvider( new FENodeLabelProvider() );
    nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
    List<IFE1D2DNode> selectedNode = dataModel.getSelectedNode();
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

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( table, 5 );
    regionFormData.top = new FormAttachment( selectNoElevationButton, 10 );
    Button selectAll = new Button( cComposite, SWT.PUSH );
    selectAll.setText( "Alles selektieren"/* "Select All" */);
    selectAll.setLayoutData( regionFormData );
    selectAll.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
        table.selectAll();
      }

    } );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( table, 5 );
    regionFormData.top = new FormAttachment( selectAll, 5 );
    Button deSelectAll = new Button( cComposite, SWT.PUSH );
    deSelectAll.setLayoutData( regionFormData );
    deSelectAll.setText( "Alles deselektieren"/* "DeSelect All" */);

    deSelectAll.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
        table.deselectAll();
      }

    } );

    regionFormData = new FormData();
    regionFormData.left = new FormAttachment( table, 5 );
    regionFormData.top = new FormAttachment( deSelectAll, 5 );
    Button applySelected = new Button( cComposite, SWT.PUSH );
    applySelected.setLayoutData( regionFormData );
    applySelected.setText( "Höhen zuweisen"/* "Apply Selected" */);
    applySelected.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
        try
        {
          applyElevation();
          nodeElevationViewer.refresh();
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }

    } );

   // noElevationBtnAdapter = 

    TextCellEditor textCellEditor = new TextCellEditor( table );
    TextCellEditor eleCellEditor = new TextCellEditor( table );
    ((Text) eleCellEditor.getControl()).setTextLimit( 15 );
    eleCellEditor.setValidator( this.doubleValidator );

    CellEditor[] editors = new CellEditor[] { textCellEditor, eleCellEditor };
    nodeElevationViewer.setCellEditors( editors );
    nodeElevationViewer.setCellModifier( modifier );

  }

  void getAllNonElevationNodes( )
  {
    List<IFE1D2DNode> allNodes = dataModel.getDiscretisationModel().getNodes();
    List<IFE1D2DNode> noElevationNodes = new ArrayList<IFE1D2DNode>();
    
    for(int i = 0; i<allNodes.size();i++){
      
      try
      {
        if (!NodeOps.hasElevation(allNodes.get(i))){
          noElevationNodes.add(allNodes.get(i));
        }
      }
      catch( RuntimeException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      
      
    }
    System.out.println("Number of Nodes :"+allNodes.size());
    System.out.println("Nodes With No Elevation:"+noElevationNodes.size());
  }
  private final void applyElevation( ) throws Exception
  {
    // System.out.println( "List of Elements Selected " + selectionNodeList.size() );
    IFEDiscretisationModel1d2d model1d2d = dataModel.getDiscretisationModel();
    if( model1d2d == null )
    {
      System.out.println( "model  is null" );
    }

    IKalypsoFeatureTheme elevationTheme = dataModel.getElevationTheme();
    if( elevationTheme == null )
    {
      return;
    }
    CommandableWorkspace workspace = elevationTheme.getWorkspace();
    if( workspace == null )
    {
      return;
    }

    IElevationProvider elevationProvider = dataModel.getElevationModel();
    if( elevationProvider == null )
    {
      elevationProvider = dataModel.getElevationModelSystem();
      if( elevationProvider == null )
      {
        return;
      }
    }

    ChangeTerrainElevationSystemCommand compositeCommand = new ChangeTerrainElevationSystemCommand( workspace, model1d2d );
    ChangeNodePositionCommand changePosCmd;
    double elevation;

    ISelection selection = nodeElevationViewer.getSelection();
    if( !(selection instanceof IStructuredSelection) )
    {
      return;
    }
    IFE1D2DNode node;
    for( Object selected : ((IStructuredSelection) selection).toList() )
    {
      if( selected instanceof IFE1D2DNode )
      {
        node = (IFE1D2DNode) selected;
        elevation = elevationProvider.getElevation( node.getPoint() );
        changePosCmd = new ChangeNodePositionCommand( model1d2d, node, elevation );
        changePosCmd.process();
        compositeCommand.addCommand( changePosCmd );
      }
    }

    // just reveal the command to the undo framework
    elevationTheme.postCommand( compositeCommand, null );
  }
}
