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

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ArrayContentProvider;
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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddSubCalcUnitsToCalcUnit1D2D;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * @author Madanagopal
 *
 */
public class CreateSubCalculationUnitCopyDialog extends Dialog
{
  
  private static final int RESET_ID = IDialogConstants.NO_TO_ALL_ID + 1;
  public static final int OK_APPLIED = IDialogConstants.OK_ID;

 
  private Composite parent;
  private Combo typeCombo;
  private CalculationUnitDataModel dataModel;

//  KeyBasedDataModelChangeListener newKeyListener = new KeyBasedDataModelChangeListener(){
//    public void dataChanged( final String key, final Object newValue )
//    {
//      Display display = parent.getDisplay();
//      final Runnable runnable = new Runnable()
//      {
//        public void run( )
//        {
////          if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) ){
////            //updateThisSection( newValue );
////          }
//        }
//      };
//      display.syncExec( runnable );      
//    }    
//  };
  private ArrayList<ICalculationUnit> inputListWithNo1D2D;
private ArrayList<ICalculationUnit> inputListCalSubUnits;
private TableViewer subCalculationUnits;
private Table subCalcUnitsTable;
private ICalculationUnit1D2D calculation1D2D;
 
  protected CreateSubCalculationUnitCopyDialog( Shell parentShell, CalculationUnitDataModel dataModel )
  {
    super( parentShell );    
    this.parent = parentShell.getParent();
    this.dataModel = dataModel;
    //this.dataModel.addKeyBasedDataChangeListener( newKeyListener  );
  }

  protected Control createDialogArea(Composite parent)
  {
    Composite comp = (Composite)super.createDialogArea(parent);
    
    FormLayout layout = new FormLayout();
    comp.setLayout( layout );
    
    FormData formData = new FormData();
    formData.left = new FormAttachment(0,5);
    formData.top = new FormAttachment(0,5);
    
    Group fromCalculationUnitGroup = new Group( comp, SWT.NONE );
    fromCalculationUnitGroup.setText( "Select Calculation Units" );//Modelle Knoten Suchen    
    fromCalculationUnitGroup.setLayoutData( formData );
    fromCalculationUnitGroup.setLayout( new GridLayout(1,false) );
    
    final TableViewer calculationUnits = new TableViewer(fromCalculationUnitGroup);
    calculationUnits.setContentProvider(new ArrayContentProvider());
    calculationUnits.setLabelProvider( new CalculationUnit1D2DLabelProvider() );
    System.out.println(dataModel.getData(ICommonKeys.KEY_FEATURE_WRAPPER_LIST ));
    
    Object inputData = dataModel.getData(ICommonKeys.KEY_FEATURE_WRAPPER_LIST );

    inputListWithNo1D2D = new ArrayList<ICalculationUnit>();
    
    if (inputData == null)
    {
      inputData = new ArrayList<IFeatureWrapper2>();
    }
    else
    {      
      for (ICalculationUnit cUnit : (List<ICalculationUnit>)inputData){
        if ((cUnit instanceof ICalculationUnit1D) || 
              (cUnit instanceof ICalculationUnit2D))
        {
          inputListWithNo1D2D.add(cUnit);
        }
        
      }
    }
    calculationUnits.setInput(inputListWithNo1D2D );
    
    final Table calcUnitsTable = calculationUnits.getTable();
    calcUnitsTable.setLinesVisible( true );
    calcUnitsTable.setLayoutData( new GridData(GridData.FILL_BOTH) );   
    
    Button addButton = new Button (comp, SWT.PUSH);
    addButton.setText( "ADD");
    Image addImage = new Image( comp.getDisplay(), 
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
        "icons/elcl16/forward.gif" ).getImageData() );  
    addButton.setImage( addImage );
    formData = new FormData();
    formData.left = new FormAttachment(fromCalculationUnitGroup,5);
    formData.top = new FormAttachment(35);
    addButton.setLayoutData( formData );
    
    inputListCalSubUnits= new ArrayList<ICalculationUnit>();
    
    addButton.addSelectionListener( new SelectionAdapter(){
      public void widgetSelected( SelectionEvent e )
      {
        inputListCalSubUnits.add(inputListWithNo1D2D.get( calcUnitsTable.getSelectionIndex()));
        inputListWithNo1D2D.remove(calcUnitsTable.getSelectionIndex());
        subCalculationUnits.refresh();
        calculationUnits.refresh();
      }
      
    });
    
    Button removeButton = new Button (comp, SWT.PUSH);
    removeButton.setText( "MOVE");
    Image removeImage = new Image( comp.getDisplay(), 
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
        "icons/elcl16/backward.gif" ).getImageData() );  
    removeButton.setImage( removeImage );
    formData = new FormData();
    formData.left = new FormAttachment(fromCalculationUnitGroup,5);
    formData.top = new FormAttachment(addButton,10);
    removeButton.setLayoutData( formData );
    removeButton.addSelectionListener( new SelectionAdapter(){
      public void widgetSelected( SelectionEvent e )
      {
        inputListWithNo1D2D.add(inputListCalSubUnits.get( subCalcUnitsTable.getSelectionIndex()));
        inputListCalSubUnits.remove( subCalcUnitsTable.getSelectionIndex() );
        calculationUnits.refresh();
        subCalculationUnits.refresh();
      }      
    });
    
    formData = new FormData();
    formData.left = new FormAttachment(removeButton,5);
    formData.top = new FormAttachment(0,5);
    formData.right = new FormAttachment(100,-5);
    
    Group fromCalculationSubUnitGroup = new Group( comp, SWT.NONE );
    fromCalculationSubUnitGroup.setText( "Sub-Calculation Units" );//Modelle Knoten Suchen    
    fromCalculationSubUnitGroup.setLayoutData( formData );
    fromCalculationSubUnitGroup.setLayout( new GridLayout(1,false) );

    subCalculationUnits = new TableViewer(fromCalculationSubUnitGroup);
    subCalculationUnits.setContentProvider( new ArrayContentProvider() );
    subCalculationUnits.setLabelProvider( new CalculationUnit1D2DLabelProvider());
    subCalculationUnits.setInput(inputListCalSubUnits);
    
    subCalcUnitsTable = subCalculationUnits.getTable();
    subCalcUnitsTable.setLinesVisible( true );
    subCalcUnitsTable.setLayoutData( new GridData(GridData.FILL_BOTH) );    
    return comp;  
  }

  protected void createButtonsForButtonBar(Composite parent)
  {
    super.createButtonsForButtonBar(parent);
    createButton(parent, RESET_ID, "Reset All", false);
  }
  
  protected void buttonPressed(int buttonId)
  {
    calculation1D2D = (ICalculationUnit1D2D) dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER);
    if(buttonId == RESET_ID)
    {
//      nameField.setText("");
//      typeCombo.select( 0 );        
//      descriptionText.setText(defaultDescriptionText);
    }
    if (buttonId == OK_APPLIED)
    {
      
      AddSubCalcUnitsToCalcUnit1D2D cmd 
          = new AddSubCalcUnitsToCalcUnit1D2D(inputListCalSubUnits,
                  calculation1D2D,
                  Util.getModel( IFEDiscretisationModel1d2d.class )){
        
          @Override
          public void process(){
            try
            {
              super.process();
            }
            catch( Exception e )
            {
              // TODO Auto-generated catch block
              e.printStackTrace();
            }
            dataModel.setData(
                ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER,
                dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER));
          }
      };

      ICommandTarget cmdTarget =
        (ICommandTarget) dataModel.getData( ICommonKeys.KEY_COMMAND_TARGET );
      if( cmdTarget == null )
      {
        throw new RuntimeException(
            "Could not found command target; not set in the data model" );
      }
      cmdTarget.postCommand( cmd, null );
      super.okPressed();   
    }
    else
    {
      super.buttonPressed(buttonId);
    }
  }   
  

}
