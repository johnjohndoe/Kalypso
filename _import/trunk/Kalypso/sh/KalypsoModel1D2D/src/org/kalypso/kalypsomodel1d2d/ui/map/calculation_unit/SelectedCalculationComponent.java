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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Madanagopsl
 *
 */
public class SelectedCalculationComponent
{


  private static final String SUB_CALCULATION_UNIT_Title = "Sub Calculation Unit";

  /* ======================================================================== */
  private TableViewer tableViewer;
  private final String defaultTestDecription = "W‰hlen Sie ein Modell aus.";
  private final String titleDescriptionGroup = "Beschreibung";
  private Group descriptionGroupText;
  private Text descriptionText;
  private Text element2D;
  private Text element1D;
  /** 
   * text field that holds the number of boundary condition 
   * assigned to the currently shown calculation unit
   */
  
  private Text textCountBC;
  /**
   * Label for the boundary condition text field
   */
  private Label boundaryConditionsLabel;
  
  private Text bLineText;
  private GridData data;
  private FormToolkit toolkit;
  private Composite parent;
  private KeyBasedDataModel dataModel;

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

  private Label element1DLabel;

  private Label element2DLabel;

  private Composite subCalculationComposite;

  private Feature wrappedFeature;

  private Label labelName;

  private Text typeField;

  private Label titleSubCalculation;

  Table table;

  private Image image_alert;

  private Label selectedProjectName;

  private Text selectedProjField;
  public void createControl( KeyBasedDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    guiSelectFromList( parent );
    dataModel.addKeyBasedDataChangeListener( newKeyListener );
  }
  
  @SuppressWarnings("unchecked")
  protected void updateThisSection( Object newValue )
  {
    Assert.throwIAEOnNullParam( newValue, "newValue" );    
    if (newValue instanceof ICalculationUnit)
    {
        if (newValue instanceof ICalculationUnit)
        {
          ICalculationUnit selUnit = (ICalculationUnit) newValue;
          selectedProjField.setText(selUnit.getName());          
        }
        
        if (newValue instanceof ICalculationUnit1D){
          typeField.setText( "1D" );          
        } else if (newValue instanceof ICalculationUnit2D){
          typeField.setText( "2D" );
        } else if (newValue instanceof ICalculationUnit1D2D){
          typeField.setText( "1D/2D" );
        } 
        else
        {
          typeField.setText( "Type unbekannt" );
        }
        
        if (newValue instanceof ICalculationUnit1D)
        {
          int num1DElement = 
                CalUnitOps.getNum1DElement((ICalculationUnit) newValue);
          element1DLabel.setEnabled( true );
          element1D.setEnabled( true );
          element1D.setText( 
              String.valueOf( num1DElement ) );
          bLineText.setText( 
              String.valueOf( 
                  CalUnitOps.getNumBoundaryLine( (ICalculationUnit)newValue  )) );
        }
        else 
        {
          element1D.setText( "" );
          element1D.setEnabled( false );
          element1DLabel.setEnabled( false );
        }
        
        if (newValue instanceof ICalculationUnit2D)
        {
          int num2DElement = CalUnitOps.getNum2DElement((ICalculationUnit) newValue);
          element2DLabel.setEnabled( true );
          element2D.setEnabled( true );        
          element2D.setText( 
              String.valueOf( num2DElement ) );
          bLineText.setText( 
              String.valueOf( 
                  CalUnitOps.getNumBoundaryLine( (ICalculationUnit)newValue  )) );
        }
        else
        {
          element2D.setEnabled( false );
          element2D.setText( "" );
          element2DLabel.setEnabled( false );
        }
        
        if ( newValue instanceof ICalculationUnit1D2D )
        {
          titleSubCalculation.setEnabled( true );
          table.setEnabled(true);
          descriptionGroupText.setEnabled( true );
          descriptionText.setEnabled( true );
          bLineText.setText( 
              String.valueOf( 
                  CalUnitOps.getNumBoundaryLine( (ICalculationUnit)newValue  )) );
          tableViewer.setInput( ((ICalculationUnit1D2D)newValue).getSubUnits().toArray());
        }
        else
        {
          titleSubCalculation.setEnabled( false );
          table.clearAll();
          table.setEnabled( false );
          descriptionGroupText.setEnabled( false );
          descriptionText.setEnabled( false );          
        }
        //get and set the number of boundary conditions
        IGrabDistanceProvider grabDistanceProvider = dataModel.getData( IGrabDistanceProvider.class, ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER );
        
        final double grabDistance = grabDistanceProvider.getGrabDistance();
        
        int bcCount = CalUnitOps.countAssignedBoundaryConditions( 
            getBoundaryConditions(), 
            (ICalculationUnit)newValue, 
             grabDistance );
//        int bcCont1 =
//          CalUnitOps.getBoundaryConditions( 
//                          getBoundaryConditions(), 
//                          (ICalculationUnit)newValue, 
//                          grabDistance ).size();
        textCountBC.setText(
            String.valueOf( bcCount ));
    }
    else
    {
      // Disable the Section
      element1D.setEnabled( false );
      element1DLabel.setEnabled( false );
      element2D.setEnabled( false );
      element2DLabel.setEnabled( false );
      bLineText.setEnabled( false );
      textCountBC.setEnabled( false );
    }
  }

  public List<IBoundaryCondition> getBoundaryConditions()
  {
    final CommandableWorkspace workspace =
      KeyBasedDataModelUtil.getBCWorkSpace( dataModel );//TODO check bc workspace remove
//      dataModel.getData(
//        CommandableWorkspace.class, 
//        ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE );
    final Feature bcHolderFeature = workspace.getRootFeature();//bcTheme.getFeatureList().getParentFeature();
    //TODO Patrice replace with operational model
    IFlowRelationshipModel flowRelationship =
      (IFlowRelationshipModel) bcHolderFeature.getAdapter( IFlowRelationshipModel.class );
    List<IBoundaryCondition> conditions =
        new ArrayList<IBoundaryCondition>((List)flowRelationship);
    return conditions;
  }
  
  private void guiSelectFromList( Composite parent )
  {
    rootComposite = new Composite (parent,SWT.FLAT);
    rootComposite.setLayout( new GridLayout(1,false) );
    
    Composite optionsComposite = new Composite(rootComposite,SWT.FLAT);
    optionsComposite.setLayout( new GridLayout(2,false));
    
    selectedProjectName = new Label(optionsComposite,SWT.RIGHT);
    selectedProjectName.setText("Berechnungseinheit: " );
    
    selectedProjField = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    selectedProjField.setEditable( false );
    data = new GridData(GridData.FILL_HORIZONTAL);
    selectedProjField.setLayoutData(data);
    
    labelName = new Label(optionsComposite,SWT.RIGHT);
    labelName.setText( "Type: " );
    
    typeField = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    typeField.setEditable( false );
    data = new GridData(GridData.FILL_HORIZONTAL);
    typeField.setLayoutData(data);
    
    element1DLabel = new Label(optionsComposite, SWT.RIGHT);
    element1DLabel.setText("1D Elemente: ");
    
    element1D = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    element1D.setEditable( false );
    data = new GridData(GridData.FILL_HORIZONTAL);
    element1D.setLayoutData(data);    
        
    element2DLabel = new Label(optionsComposite, SWT.RIGHT);
    element2DLabel.setText("2D Elemente: ");
    
    element2D = new Text(optionsComposite, SWT.SINGLE|SWT.BORDER);
    element2D.setEditable( false );
    data = new GridData(GridData.FILL_HORIZONTAL);
    element2D.setLayoutData(data);
    
    image_alert = new Image( optionsComposite.getDisplay(), 
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
        "icons/elcl16/alert.gif" ).getImageData() );      
    
    Label boundaryUpLabel = new Label(optionsComposite, SWT.RIGHT);
    boundaryUpLabel.setText("Randlinien: ");    
    bLineText = toolkit.createText( optionsComposite, "", SWT.SINGLE|SWT.BORDER );
    
    boundaryConditionsLabel = new Label(optionsComposite, SWT.RIGHT);
    boundaryConditionsLabel.setText("Randbedingungen: ");    
    textCountBC = toolkit.createText( optionsComposite, "", SWT.SINGLE|SWT.BORDER );
    

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
    tableViewer.setLabelProvider( new CalculationUnitViewerLabelProvider(subCalculationComposite.getDisplay()));//ListLabelProvider(this) );    
    table.setLinesVisible( true );   
    
    formData = new FormData();
    formData.top = new FormAttachment(titleSubCalculation,5);
    formData.left = new FormAttachment(0,5);
    formData.bottom = new FormAttachment(100,0);
    formData.width = 100;
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
    
    descriptionText = new Text( descriptionGroupText,SWT.MULTI|SWT.WRAP );     
    
    descriptionText.setEditable( false );
    descriptionText.setText( defaultTestDecription );
    
    FormData formDescripData = new FormData();
    formDescripData.left = new FormAttachment( 0, 0 );
    formDescripData.right = new FormAttachment( 100, 0 );
    descriptionText.setLayoutData(formDescripData);
  }

}
