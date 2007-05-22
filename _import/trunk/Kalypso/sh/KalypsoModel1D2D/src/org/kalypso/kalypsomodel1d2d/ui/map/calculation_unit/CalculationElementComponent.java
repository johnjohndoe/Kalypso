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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.ui.map.editor.FeatureWrapperListInputProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.editor.IButtonConstants;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.sort.IEnvelopeProvider;

/**
 * @author Madanagopsl
 *
 */
public class CalculationElementComponent
{


  class ListLabelProvider extends LabelProvider
  {
    public Image getImage( Object element )
    {
      return null;
    }

    public String getText( Object element )
    {
      if( element instanceof IFeatureWrapper2 )
      {

        String name = ((IFeatureWrapper2) element).getName();
        if( name != null )
        {
          return name;
        }
        else
        {
          return ((IFeatureWrapper2) element).getGmlID();
        }
      }
      else
      {
        throw new RuntimeException( "Only IFeatureWrapper2 is supported:" + "but got \n\tclass=" + (element == null ? null : element.getClass()) + "\n\t value=" + element );
      }
    }
  }

  
  /* ======================================================================== */
  private TableViewer tableViewer;

  private KeyBasedDataModel dataModel;

  private Image image;

  private Image imageDown;

  private Image imageUp;

  private FormToolkit toolkit;

  private Composite parent;

  /**
   * The id for the selection in the data model
   */
  

  /**
   * The id for the input in the data model
   */


  final String mainGroupTitle = "Bitte Höhenmodell auswählen";

  final String bTextMaximizeSelected = "Geländemodell anzeigen und maximieren";

  final String deleteSelected = "Geländemodell löschen";

  final String defaultTestDecription = "Wählen Sie ein Modell aus.";
  
  final String saveToolTip = "Deskription Sichern";

  final String titleDescriptionGroup = "Beschreibung";


  private Label descriptionLabel;

  private Group descriptionGroupText;

  private Text descriptionText;

  private String[] buttonsList;

  private Button saveButton;

  private IFeatureWrapper2 currentElementSelection;

  private Text _2dElementField;

  private Text _1dElementField;

  private GridData data;

  public void createControl( KeyBasedDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    guiSelectFromList( parent );
  }
  
  private void guiSelectFromList( Composite parent )
  {
    Composite optionsComposite = new Composite(parent,SWT.FLAT);
    optionsComposite.setLayout( new GridLayout(2,false));
    
    Label _1dElementLabel = new Label(optionsComposite, SWT.RIGHT);
    _1dElementLabel.setText("1D Element: ");
    
    _1dElementField = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    data = new GridData(GridData.FILL_HORIZONTAL);
    _1dElementField.setLayoutData(data);
    
    Label _2dELementLabel = new Label(optionsComposite, SWT.RIGHT);
    _2dELementLabel.setText("2D Element: ");
    
    _2dElementField = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    data = new GridData(GridData.FILL_HORIZONTAL);
    _2dElementField.setLayoutData(data);
    
    Label BoundaryUpLabel = new Label(optionsComposite, SWT.RIGHT);
    BoundaryUpLabel.setText("Boundary Up: ");
    
    //TODO Have the Image here. 
      
    Label BoundaryDownLabel = new Label(optionsComposite, SWT.RIGHT);
    BoundaryDownLabel.setText("Boundary Down: ");
    
    
    // @TODO Madan Continue the GUI Work
    
    
    
 
//    FormData formData;
//    formData = new FormData();
//    formData.left = new FormAttachment( 0, 5 );
//    formData.top = new FormAttachment( 0, 5 );
//    Label terrainModelLabel = new Label( parent, SWT.NONE );
//    terrainModelLabel.setText( mainGroupTitle );
//    terrainModelLabel.setLayoutData( formData );
//
//    formData = new FormData();
//    formData.left = new FormAttachment( 0, 10 );
//    formData.top = new FormAttachment( terrainModelLabel, 5 );
//    formData.bottom = new FormAttachment(100,0);
//
//    tableViewer = new TableViewer( parent, SWT.FILL | SWT.BORDER );
//    Table table = tableViewer.getTable();
//    tableViewer.setContentProvider( new ArrayContentProvider() );
//    tableViewer.setLabelProvider( new ListLabelProvider() );
//    table.setLinesVisible( true );
//    table.setLayoutData( formData );
//
//   
//    Object inputData = dataModel.getData( CalculationUnitDataModel.CALCULATION_UNITS );
//
//    if (inputData == null)
//    {
//      inputData = new ArrayList<IFeatureWrapper2>();
//    }
//    tableViewer.setInput((List<ICalculationUnit>)inputData );
//    tableViewer.addSelectionChangedListener( this.elevationModelSelectListener );
//
//    formData = new FormData();
//    formData.left = new FormAttachment(table, 5);
//    formData.bottom = new FormAttachment( 100, 0 );
//    formData.top = new FormAttachment( terrainModelLabel, 5 );
//    
//    Composite btnComposite = new Composite(parent,SWT.NONE);
//    btnComposite.setLayout( new GridLayout(1,false));
//    btnComposite.setLayoutData( formData );   
//    if (searchForThisString( IButtonConstants.BTN_MOVE_UP ))
//    {        
//      Button moveUpBtn = new Button( btnComposite, SWT.PUSH );
//      imageUp = new Image( btnComposite.getDisplay(), 
//          KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
//              PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
//              "icons/elcl16/list_up.gif" ).getImageData() );
//      moveUpBtn.setImage( imageUp );
//      moveUpBtn.addSelectionListener( this.moveUpListener );
//    }
//    
//    if (searchForThisString( IButtonConstants.BTN_MOVE_DOWN ))
//      {
//        Button moveDownBtn = new Button( btnComposite, SWT.PUSH );
//        imageDown = new Image( btnComposite.getDisplay(),
//            KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
//                PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
//                "icons/elcl16/list_down.gif" ).getImageData() );
//        moveDownBtn.setImage( imageDown );
//        moveDownBtn.addSelectionListener( this.moveDownListener );
//      }
//    
//    if (searchForThisString( IButtonConstants.BTN_CLICK_TO_RUN ))
//      {
//        Button showTerrain = new Button( btnComposite, SWT.PUSH );
//        showTerrain.setToolTipText( bTextMaximizeSelected );
//        image = new Image( btnComposite.getDisplay(),
//            KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
//                PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
//                "icons/elcl16/goTo_Terrain.gif" ).getImageData() );
//        showTerrain.setImage( image );
//        showTerrain.addSelectionListener( new SelectionAdapter()
//        {
//          public void widgetSelected( SelectionEvent event )
//          {            
//            maximizeSelected();
//          }        
//        } );      
//      }
//    
//    if (searchForThisString( IButtonConstants.BTN_REMOVE ))
//      {
//       Button deleteButton = new Button( btnComposite, SWT.PUSH );
//        deleteButton.setToolTipText( deleteSelected );
//        image = new Image( btnComposite.getDisplay(),
//            KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
//                PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
//                "icons/elcl16/remove.gif" ).getImageData() );
//        deleteButton.setImage( image );
//        deleteButton.addSelectionListener( new SelectionAdapter()
//        {
//          public void widgetSelected( SelectionEvent event )
//          {
//            try
//            {
//            }
//            catch( Throwable th )
//            {
//              th.printStackTrace();
//            }
//          }
//        } );     
//      }
//    
//    if (searchForThisString( IButtonConstants.BTN_ADD ))
//    {
//      Button addButton = new Button( btnComposite, SWT.PUSH );
//      addButton.setToolTipText( deleteSelected );
//      image = new Image( btnComposite.getDisplay(),
//          KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
//              PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
//              "icons/elcl16/add.gif" ).getImageData() );
//      addButton.setImage( image );
//      addButton.addSelectionListener( new SelectionAdapter()
//      {
//        public void widgetSelected( SelectionEvent event )
//        {
//
//        }
//      } );           
//    }
//    
//    descriptionGroupText = new Group( parent, SWT.NONE );
//    descriptionGroupText.setText( titleDescriptionGroup );
//    formData = new FormData();
//    formData.left = new FormAttachment( btnComposite, 5 );
//    formData.top = new FormAttachment( terrainModelLabel, 10 );
//    formData.bottom = new FormAttachment( 100, 0 );
//    descriptionGroupText.setLayoutData( formData );
//
//    FormLayout formDescription = new FormLayout();
//    descriptionGroupText.setLayout( formDescription );
//    
//    descriptionText = new Text( descriptionGroupText, SWT.MULTI | SWT.WRAP );
//    descriptionText.setText( defaultTestDecription );
//    
//    FormData formDescripData = new FormData();
//    formDescripData.left = new FormAttachment( 0, 0 );
//    formDescripData.right = new FormAttachment( 100, 0 );
//    formDescripData.top = new FormAttachment( 0, 0 );
//    //formDescripData.bottom = new FormAttachment( 100, 0 );
//    descriptionText.setLayoutData( formDescripData );
//    
//    saveButton = new Button (descriptionGroupText,SWT.PUSH);
//    saveButton.setText( "Sichern" );
//    saveButton.setToolTipText( saveToolTip );
//    image = new Image( descriptionGroupText.getDisplay(),
//        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
//            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
//            "icons/elcl16/save.gif" ).getImageData() );
//    saveButton.setImage( image );
//    formData = new FormData();
//    //formData.left = new FormAttachment(descriptionGroupText,5);
//    formData.right = new FormAttachment(100,0);
//    formData.bottom = new FormAttachment(100,0);
//    saveButton.setLayoutData( formData ); 
//    saveButton.addSelectionListener( new SelectionAdapter()
//    {
//      public void widgetSelected( SelectionEvent event )
//      {
//      }
//    } );        
  }

}
