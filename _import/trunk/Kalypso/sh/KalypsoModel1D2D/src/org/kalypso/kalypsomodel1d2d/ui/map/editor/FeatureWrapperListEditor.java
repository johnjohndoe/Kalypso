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
package org.kalypso.kalypsomodel1d2d.ui.map.editor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
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
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeIFeatureWrapper2NameCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.sort.IEnvelopeProvider;

/**
 * 
 * @author Madanagopal
 */
@SuppressWarnings({"synthetic-access", "hiding", "unchecked"})
public class FeatureWrapperListEditor implements IButtonConstants
{
  /* ======================================================================== */
  private TableViewer tableViewer;

  private KeyBasedDataModel dataModel;

  private Image image;

  private Image imageDown;

  private Image imageUp;

  private FormToolkit toolkit;

  private Composite parent;
  
  private ICellModifier modifier = new ICellModifier()
  {

    public boolean canModify( Object element, String property )
    {
      System.out.println( "CanmOdiy=" + property );
      // Find the index of the column
      if( property.equals( tableViewer.getColumnProperties()[0] ) )
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
      System.out.println("getting prop="+property);
      if( property.equals( tableViewer.getColumnProperties()[0] ) )
      {
        if( element instanceof IFeatureWrapper2 )
        {
          
          return ((IFeatureWrapper2)element).getName();
        }
        else
        {
          throw new RuntimeException(
              "Only IFeatureWrapper2 are accepted:"+element);
        }
      }
      else
      {
        return null;
      }
    }

    public void modify( Object element, String property, Object value )
    {

      IFeatureWrapper2 featureWrapper = null;
      if( element instanceof TableItem )
      {
        Object data = ((TableItem) element).getData();
        if( data instanceof IFeatureWrapper2 )
        {
          featureWrapper = (IFeatureWrapper2) data;
        }

      }

      if( property.equals( tableViewer.getColumnProperties()[0] ) )
      {
        final String oldName = featureWrapper.getName(); 
        if( value == null )
        {
          System.out.println("new Name is null");
        }
        else if( value.equals( oldName ) )
        {
          System.out.println( "No name change!" );
          return;
        }
        
        featureWrapper.setName( (String) value );
        ChangeIFeatureWrapper2NameCmd renameCommand =
            new ChangeIFeatureWrapper2NameCmd( featureWrapper, (String)value)
        {
          /**
           * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeIFeatureWrapper2NameCmd#process()
           */
          @Override
          public void process( ) throws Exception
          {
            super.process();
            refreshTableView();
          }
        };
        KeyBasedDataModelUtil.postCommand( dataModel, renameCommand );
      }
      else
      {
        System.out.println( "BAD property:" + property );
      }
    }

  };

  /**
   * The id for the selection in the data model
   */
  private final String idSselection;

  /**
   * The id for the input in the data model
   */
  private final String idInput;

  private final String idMapPanel;

  private FeatureWrapperListInputProvider inputProvider;

  private IEnvelopeProvider selectionEnvelopeProvider;

  //final String mainGroupTitle = "Bitte Höhenmodell auswählen";

  final String bTextMaximizeSelected = "Geländemodell anzeigen und maximieren";

  final String deleteSelected = "Geländemodell löschen";
  
  final String calculateSelected = "Run Calculation";

  final String defaultTestDecription = "Wählen Sie ein Modell aus.";
  
  final String saveToolTip = "Deskription Sichern";

  final String titleDescriptionGroup = "Beschreibung";

  final private SelectionListener moveUpListener = new SelectionAdapter()
  {
    public void widgetSelected( SelectionEvent event )
    {
      System.out.println( "MoveUp:" + tableViewer.getSelection() );
      moveSelection( -1 );
      tableViewer.refresh();
    }
  };

  final private SelectionListener moveDownListener = new SelectionAdapter()
  {
    public void widgetSelected( SelectionEvent event )
    {
      moveSelection( 1 );
      tableViewer.refresh();
    }
  };

  final private ISelectionChangedListener elevationModelSelectListener = new ISelectionChangedListener()
  {
    @SuppressWarnings("synthetic-access")
    public void selectionChanged( SelectionChangedEvent event )
    {
      try
      {
        IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        if( selection == null )
        {
          System.out.println( "Selection is null" );
          return;
        }
        Object firstElement = selection.getFirstElement();
        if( firstElement == null )
        {
          return;//throw new NullPointerException( "Null Value while selection.getFirstElement() :" + firstElement );
        }
        else
        {
          if( firstElement instanceof IFeatureWrapper2 )
          {
            IFeatureWrapper2 firstElementWrapper = (IFeatureWrapper2) firstElement;
//            dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, firstElement );
//            descriptionText.setText( firstElement.getDescription() );
//            descriptionText.redraw();
            setCurrentSelection(firstElementWrapper);
          }
        }
      }
      catch( Throwable th )
      {
        th.printStackTrace();
      }
    }

  };

  private Label descriptionLabel;

  private Group descriptionGroupText;

  private Text descriptionText;

  private String[] buttonsList;

  private Button saveButton;

//  private IFeatureWrapper2 currentElementSelection;

  private KeyBasedDataModelChangeListener dataModelListener = 
      new KeyBasedDataModelChangeListener()
      {

        @SuppressWarnings("synthetic-access")
        public void dataChanged( String key, Object newValue )
        {
          if( ICommonKeys.KEY_FEATURE_WRAPPER_LIST.equals( key ) )
          {
//            tableViewer.setInput( newValue );
            updateOnNewInput( newValue );
          }
          else if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
          {
//            IStructuredSelection selection =
//              new StructuredSelection(
//                  newValue==null ? new Object[]{}:new Object[]{newValue} );
//            
//            tableViewer.setSelection( selection );
            updateOnNewSelection( newValue );
          }
          else
          {
            //uninteresting key
          }
        }
    
      };

  public FeatureWrapperListEditor( String selectionID, String inputID, String mapPanelID )
  {
    this.idSselection = selectionID;
    this.idMapPanel = mapPanelID;
    this.idInput = inputID;
  }

  public void createControl( KeyBasedDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    guiSelectFromList( parent );
    dataModel.addKeyBasedDataChangeListener( this.dataModelListener );
  }

  private void guiSelectFromList( Composite parent )
  {
    FormData formData;

    formData = new FormData();
    formData.left = new FormAttachment( 0, 10 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment(100,0);

    tableViewer = new TableViewer( parent, SWT.FILL | SWT.BORDER );
    Table table = tableViewer.getTable();
    tableViewer.setContentProvider( new ArrayContentProvider() );
    tableViewer.setLabelProvider( getLabelProvider(parent.getDisplay()) );
    table.setLinesVisible( true );
    table.setLayoutData( formData );

    final TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
    lineColumn.setWidth( 50 );
   
    Object inputData = 
      dataModel.getData( 
        ICommonKeys.KEY_FEATURE_WRAPPER_LIST );

    if (inputData == null)
    {
      inputData = new ArrayList<IFeatureWrapper2>();
    }
    tableViewer.setInput((List<ICalculationUnit>)inputData );
    tableViewer.addSelectionChangedListener( this.elevationModelSelectListener );

    formData = new FormData();
    formData.left = new FormAttachment(table, 5);
    formData.bottom = new FormAttachment( 100, 0 );
    formData.top = new FormAttachment( 0, 5 );
    
    Composite btnComposite = new Composite(parent,SWT.NONE);
    btnComposite.setLayout( new GridLayout(1,false));
    btnComposite.setLayoutData( formData );   
    if (searchForThisString( IButtonConstants.BTN_MOVE_UP ))
    {        
      Button moveUpBtn = new Button( btnComposite, SWT.PUSH );
      imageUp = new Image( btnComposite.getDisplay(), 
          KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
              PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
              "icons/elcl16/list_up.gif" ).getImageData() );
      moveUpBtn.setImage( imageUp );
      moveUpBtn.addSelectionListener( this.moveUpListener );
    }
    
    if (searchForThisString( IButtonConstants.BTN_MOVE_DOWN ))
      {
        Button moveDownBtn = new Button( btnComposite, SWT.PUSH );
        imageDown = new Image( btnComposite.getDisplay(),
            KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
                PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                "icons/elcl16/list_down.gif" ).getImageData() );
        moveDownBtn.setImage( imageDown );
        moveDownBtn.addSelectionListener( this.moveDownListener );
      }
    
    if (searchForThisString( IButtonConstants.BTN_CLICK_TO_RUN ))
      {
        Button showTerrain = new Button( btnComposite, SWT.PUSH );
        showTerrain.setToolTipText( bTextMaximizeSelected );
        image = new Image( btnComposite.getDisplay(),
            KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
                PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                "icons/elcl16/goTo_Terrain.gif" ).getImageData() );
        showTerrain.setImage( image );
        showTerrain.addSelectionListener( new SelectionAdapter()
        {
          public void widgetSelected( SelectionEvent event )
          {            
            maximizeSelected();
          }        
        } );      
      }
    
    if (searchForThisString( IButtonConstants.BTN_REMOVE ))
      {
       Button deleteButton = new Button( btnComposite, SWT.PUSH );
        deleteButton.setToolTipText( deleteSelected );
        image = new Image( btnComposite.getDisplay(),
            KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
                PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                "icons/elcl16/remove.gif" ).getImageData() );
        deleteButton.setImage( image );
        deleteButton.addSelectionListener( new SelectionAdapter()
        {
          public void widgetSelected( SelectionEvent event )
          {
            try
            {
              deleteSelected();
              tableViewer.refresh();
            }
            catch( Throwable th )
            {
              th.printStackTrace();
            }
          }
        } );     
      }
    
    if (searchForThisString( IButtonConstants.BTN_ADD ))
    {
      Button addButton = new Button( btnComposite, SWT.PUSH );
      addButton.setToolTipText( deleteSelected );
      image = new Image( btnComposite.getDisplay(),
          KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
              PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
              "icons/elcl16/add.gif" ).getImageData() );
      addButton.setImage( image );
      addButton.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent event )
        {
          try
          {
          createFeatureWrapper();
          }
          catch( Throwable th )
          {
            th.printStackTrace();
          }
        }
      } );           
    }
    
    if (searchForThisString( IButtonConstants.BTN_CLICK_TO_CALCULATE ))
    {
      Button calculateButton = new Button( btnComposite, SWT.PUSH );
      calculateButton.setToolTipText( calculateSelected );
      image = new Image( btnComposite.getDisplay(),
          KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
              PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
              "icons/elcl16/add.gif" ).getImageData() );
      calculateButton.setImage( image );
      calculateButton.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent event )
        {
          try
          {
          System.out.println("Calculate Selected");
          }
          catch( Throwable th )
          {
            th.printStackTrace();
          }
        }
      } );           
    }
    
    descriptionGroupText = new Group( parent, SWT.NONE );
    descriptionGroupText.setText( titleDescriptionGroup );
    formData = new FormData();
    formData.left = new FormAttachment( btnComposite, 5 );
    formData.top = new FormAttachment( 0, 10 );
    formData.bottom = new FormAttachment( 100, 0 );
    descriptionGroupText.setLayoutData( formData );

    FormLayout formDescription = new FormLayout();
    descriptionGroupText.setLayout( formDescription );
    
    descriptionText = new Text( descriptionGroupText, SWT.MULTI | SWT.WRAP );
    descriptionText.setText( defaultTestDecription );
    
    FormData formDescripData = new FormData();
    formDescripData.left = new FormAttachment( 0, 0 );
    formDescripData.right = new FormAttachment( 100, 0 );
    formDescripData.top = new FormAttachment( 0, 0 );
    descriptionText.setLayoutData( formDescripData );
    
    saveButton = new Button (descriptionGroupText,SWT.PUSH);
    saveButton.setText( "Sichern" );
    saveButton.setToolTipText( saveToolTip );
    image = new Image( descriptionGroupText.getDisplay(),
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
            "icons/elcl16/save.gif" ).getImageData() );
    saveButton.setImage( image );
    formData = new FormData();
    //formData.left = new FormAttachment(descriptionGroupText,5);
    formData.right = new FormAttachment(100,0);
    formData.bottom = new FormAttachment(100,0);
    saveButton.setLayoutData( formData ); 
    saveButton.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
          if (getCurrentSelection()!= null)
          {
            getCurrentSelection().setDescription( descriptionText.getText() );          
          }        
      }
    } );  
    
    //setup cell editing
    TextCellEditor textCellEditor = new TextCellEditor( table );
    CellEditor[] editors = 
        new CellEditor[] { textCellEditor };
    tableViewer.setCellEditors( editors );
    tableViewer.setCellModifier( modifier );
    tableViewer.setColumnProperties( new String[]{"Name"} );
  }


  protected IBaseLabelProvider getLabelProvider(Display display)
  {
    
    return null;
  }

  protected void deleteSelected( ) throws Exception
  {
  }

  protected void moveSelection( int delta )
  {
  }

  protected void maximizeSelected( )
  {
  }

  public void setRequiredButtons( String... buttonsList )
  {
    this.buttonsList = buttonsList;
  }
  
  public boolean searchForThisString(String searchStr)
  {
    for (String i:buttonsList){
      if (i.compareTo( searchStr )== 0)
        return true;      
    }
    return false;
  }
  
  private void setCurrentSelection( IFeatureWrapper2 firstElement )
  {
    dataModel.setData(
        ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, firstElement );
  }
  
  
  private IFeatureWrapper2 getCurrentSelection(){    
    return (IFeatureWrapper2) 
        dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
  }
  
  public void createFeatureWrapper()
  {
  }
  
  
  public KeyBasedDataModel getDataModel()
  {
    return dataModel;
  }
  
  /**
   * Update the gui components to reflect the table new input
   */
  final void updateOnNewInput( final Object input )
  {
    Runnable changeInputRunnable = new Runnable()
    {
      /**
       * @see java.lang.Runnable#run()
       */
      public void run( )
      {
        if( input == null )
        {
          tableViewer.setInput( new Object[]{} );
        }
        else
        {
          tableViewer.setInput( input );
        }
        IFeatureWrapper2 currentSelection = getCurrentSelection();
        //    final IStructuredSelection selection;
        //    if( currentSelection != null )
        //    {
        //      selection = 
        //        new StructuredSelection( new Object[]{ currentSelection } );
        //    }
        //    else
        //    {
        //      selection = 
        //        new StructuredSelection( new Object[]{ } );
        //    }
        //    tableViewer.setSelection( selection );
        updateOnNewSelection( currentSelection );
      }
    };
    Display display = parent.getDisplay();
    
    display.syncExec( changeInputRunnable );
  }
  
  final void updateOnNewSelection( final Object currentSelection  )
  {
    final Runnable runnable = new Runnable()
    {

      public void run( )
      {
        final Object cachedCurrentSelect = currentSelection; 
        final String desc;
        if( cachedCurrentSelect instanceof IFeatureWrapper2 )
        {
            desc = ((IFeatureWrapper2)cachedCurrentSelect).getDescription();
        }  
        else if ( cachedCurrentSelect == null )
        {
          desc = "";
        }
        else
        {
          throw new IllegalArgumentException(
              "IfeatureWrapper2 expected but got:"+currentSelection);
        }
        descriptionText.setText( desc );
        descriptionText.redraw();
      }
      
    };
    
    
    Display display = parent.getDisplay();
    
    display.syncExec( runnable );
  }
  
  final void refreshTableView( )
  {
    final Runnable runnable = new Runnable()
    {

      public void run( )
      {
        tableViewer.refresh();
      }
      
    };
    
    
    Display display = parent.getDisplay();
    
    display.syncExec( runnable );
  }
  
  public void refreshOtherSections(){
    
  }
  
}
