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

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ops.*;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * @author Madanagopsl
 *
 */
public class CalculationElementComponent
{


  private static final String SUB_CALCULATION_UNIT_Title = "Sub Calculation Unit";

  /* ======================================================================== */
  TableViewer tableViewer;
  private Image imageBoundaryUp;
  final String mainGroupTitle = "Bitte Höhenmodell auswählen";
  final String bTextMaximizeSelected = "Geländemodell anzeigen und maximieren";
  final String deleteSelected = "Geländemodell löschen";
  final String defaultTestDecription = "Wählen Sie ein Modell aus.";
  final String saveToolTip = "Deskription Sichern";
  final String titleDescriptionGroup = "Beschreibung";
  private Group descriptionGroupText;
  Text descriptionText;
  private Text _2dElementField;
  private Text _1dElementField;
  private GridData data;
  private Image imageBoundaryDown;
  private FormToolkit toolkit;
  Composite parent;
  private KeyBasedDataModel dataModel;
  private int num1DElement;
  private int num2DElement;

  KeyBasedDataModelChangeListener newKeyListener = new KeyBasedDataModelChangeListener(){
    public void dataChanged( final String key, final Object newValue )
    {
      Display display = parent.getDisplay();
      final Runnable runnable = new Runnable()
      {
        public void run( )
        {
          if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) ){
            if (newValue != null){
            updateThisSection( newValue );
            }            
          }
        }
      };
      display.syncExec( runnable );      
    }    
  };

  private Composite rootComposite;

  private Label _1dElementLabel;

  private Label _2dElementLabel;

  private Button boundaryUpBtn;

  private Image imageBoundaryUp_Ok;

  private Image imageBoundaryUp_Error;

  private Image imageBoundaryDown_Ok;

  private Image imageBoundaryDown_Error;

  private Button boundaryDownBtn;

  private Composite subCalculationComposite;

  private Feature wrappedFeature;

  private Label labelName;

  private Text typeField;

  private Label titleSubCalculation;

  Table table;

  private Image image_;
  public void createControl( KeyBasedDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    guiSelectFromList( parent );
    dataModel.addKeyBasedDataChangeListener( newKeyListener );
  }
  
  protected void updateThisSection( Object newValue )
  {
    Assert.throwIAEOnNullParam( newValue, "newValue" );    
    if (newValue instanceof ICalculationUnit)
    {
        if (newValue instanceof ICalculationUnit1D){
          typeField.setText( "ICal 1D" );          
        } else if (newValue instanceof ICalculationUnit2D){
          typeField.setText( "ICal 2D" );
        } else if (newValue instanceof ICalculationUnit1D2D){
          typeField.setText( "ICal 1D/2D" );
        } 
        else
        {
          typeField.setText( "No Data" );
        }
        
        num1DElement = CalUnitOps.getNum1DElement((ICalculationUnit) newValue);
        if (newValue instanceof ICalculationUnit1D)
        {
          _1dElementLabel.setEnabled( true );
          _1dElementField.setEnabled( true );
          _1dElementField.setText( num1DElement+"");
        }
        else 
        {
          _1dElementField.setEnabled( false );
          _1dElementLabel.setEnabled( false );
        }
        
        num2DElement = CalUnitOps.getNum2DElement((ICalculationUnit) newValue);
        if (newValue instanceof ICalculationUnit2D)
        {
          _2dElementLabel.setEnabled( true );
          _2dElementField.setEnabled( true );        
          _2dElementField.setText( num2DElement+"");
        }
        else
        {
          _2dElementField.setEnabled( false );
          _2dElementLabel.setEnabled( false );
        }
        
        
        if (CalUnitOps.hasUpBoundary((ICalculationUnit) newValue))
        {
          boundaryUpBtn.setImage( imageBoundaryUp_Ok );
        }
        else
        {
          boundaryUpBtn.setImage( imageBoundaryUp_Error );
        } 
        
        if (CalUnitOps.hasDownBoundary((ICalculationUnit) newValue))
        {
          boundaryDownBtn.setImage( imageBoundaryDown_Ok );
        }
        else
        {
          boundaryDownBtn.setImage( imageBoundaryDown_Error );
        }
        
        if (!(newValue instanceof ICalculationUnit1D2D))
        {
          titleSubCalculation.setEnabled( false );
          table.setEnabled( false );
          descriptionGroupText.setEnabled( false );
          descriptionText.setEnabled( false );          
        }
        else{
          titleSubCalculation.setEnabled( true );
          table.setEnabled(true);
          descriptionGroupText.setEnabled( true );
          descriptionText.setEnabled( true );
        }
        if (newValue instanceof ICalculationUnit1D2D){
         
          tableViewer.setInput( ((ICalculationUnit1D2D)newValue).getSubUnits().toArray());
        }
        
    }
    else
    {
      // Disable the Section
      _1dElementField.setEnabled( false );
      _1dElementLabel.setEnabled( false );
      _2dElementField.setEnabled( false );
      _2dElementLabel.setEnabled( false );
      boundaryUpBtn.setImage( imageBoundaryUp_Error );
      boundaryDownBtn.setImage( imageBoundaryDown_Error );
    }
  }

  private void guiSelectFromList( Composite parent )
  {
    rootComposite = new Composite (parent,SWT.FLAT);
    rootComposite.setLayout( new GridLayout(1,false) );
    
    Composite optionsComposite = new Composite(rootComposite,SWT.FLAT);
    optionsComposite.setLayout( new GridLayout(2,false));
    
    labelName = new Label(optionsComposite,SWT.RIGHT);
    labelName.setText( "Type of Element :" );
    
    typeField = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    typeField.setEditable( false );
    data = new GridData(GridData.FILL_HORIZONTAL);
    typeField.setLayoutData(data);
    
    _1dElementLabel = new Label(optionsComposite, SWT.RIGHT);
    _1dElementLabel.setText("1D Element: ");
    
    _1dElementField = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    _1dElementField.setEditable( false );
    data = new GridData(GridData.FILL_HORIZONTAL);
    _1dElementField.setLayoutData(data);    
        
    _2dElementLabel = new Label(optionsComposite, SWT.RIGHT);
    _2dElementLabel.setText("2D Element: ");
    
    _2dElementField = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    _2dElementField.setEditable( false );
    data = new GridData(GridData.FILL_HORIZONTAL);
    _2dElementField.setLayoutData(data);
    
    image_ = new Image( optionsComposite.getDisplay(), 
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
        "icons/elcl16/alert.gif" ).getImageData() );      
    
    Label boundaryUpLabel = new Label(optionsComposite, SWT.RIGHT);
    boundaryUpLabel.setText("Boundary Up: ");
    
    boundaryUpBtn = new Button(optionsComposite,SWT.CENTER|SWT.PUSH);
    
    boundaryUpBtn.setImage( image_ );    
    imageBoundaryUp_Ok = new Image( optionsComposite.getDisplay(), 
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
        "icons/elcl16/inputOk.gif" ).getImageData() );
    
    imageBoundaryUp_Error = new Image( optionsComposite.getDisplay(), 
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
        "icons/elcl16/inputError.gif" ).getImageData());
    
    
    Label boundaryDownLabel = new Label(optionsComposite, SWT.RIGHT);
    boundaryDownLabel.setText("Boundary Down: ");
    
    boundaryDownBtn = new Button(optionsComposite,SWT.CENTER|SWT.PUSH);
    
    
    imageBoundaryDown_Ok = new Image( optionsComposite.getDisplay(), 
          KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
              PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
              "icons/elcl16/inputOk.gif" ).getImageData() );


    imageBoundaryDown_Error = new Image( optionsComposite.getDisplay(), 
          KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
              PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
              "icons/elcl16/inputError.gif" ).getImageData() );      

  
    boundaryDownBtn.setImage( image_ );
    
    subCalculationComposite = new Composite(rootComposite,SWT.FLAT);
    subCalculationComposite.setLayout( new FormLayout() );
    
    titleSubCalculation = new Label(subCalculationComposite,SWT.NONE);
    titleSubCalculation.setText(SUB_CALCULATION_UNIT_Title);
    FormData formData = new FormData();
    formData.top = new FormAttachment(optionsComposite,5);
    titleSubCalculation.setLayoutData( formData );    
    
    tableViewer = new TableViewer( subCalculationComposite, SWT.FILL | SWT.BORDER );
    table = tableViewer.getTable();
    table.addSelectionListener( new SelectionAdapter(){
      private IStructuredSelection sel;

      public void widgetSelected( SelectionEvent e )
      {
        
        if (tableViewer.getSelection() != null)
        {
          
          try
          {
            if(tableViewer.getSelection() instanceof IStructuredSelection){
              sel = (IStructuredSelection)(tableViewer.getSelection());
              String description = ((ICalculationUnit)sel.getFirstElement()).getDescription();
              descriptionText.setText( description );
            }
          }
          catch( RuntimeException e1 )
          {
            // TODO Auto-generated catch block
            e1.printStackTrace();
          }
          
        }
        else
        {
          descriptionText.setText( defaultTestDecription );
        }         
        
     }
      
    });
    tableViewer.setContentProvider( new ArrayContentProvider() );
    tableViewer.setLabelProvider( new ListLabelProvider(this) );
    //tableViewer.addSelectionChangedListener( listener )
    table.setLinesVisible( true );
    
    
    
    formData = new FormData();
    formData.top = new FormAttachment(titleSubCalculation,5);
    formData.left = new FormAttachment(0,5);
    formData.bottom = new FormAttachment(100,0);
    table.setLayoutData( formData );
    
    descriptionGroupText = new Group( subCalculationComposite, SWT.NONE );
    descriptionGroupText.setText( titleDescriptionGroup );
    formData = new FormData();
    formData.left = new FormAttachment( table, 5 );
    formData.top = new FormAttachment( titleSubCalculation, 10 );
    formData.right = new FormAttachment(100,0);
    formData.bottom = new FormAttachment( 100, 0 );
    descriptionGroupText.setLayoutData( formData );

    FormLayout formDescription = new FormLayout();
    descriptionGroupText.setLayout( formDescription );
    
    descriptionText = new Text( descriptionGroupText, SWT.MULTI|SWT.WRAP );
     
    
    descriptionText.setEditable( false );
    descriptionText.setText( defaultTestDecription );
    
    FormData formDescripData = new FormData();
    formDescripData.left = new FormAttachment( 0, 0 );
    formDescripData.right = new FormAttachment( 100, 0 );
    descriptionText.setLayoutData(formDescripData);
  }
  
  private ICalculationUnit getSelectedCalculationUnit()
  { 
      if (dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER )!= null)
        return (ICalculationUnit)dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
      else 
        return null;
  }
}
