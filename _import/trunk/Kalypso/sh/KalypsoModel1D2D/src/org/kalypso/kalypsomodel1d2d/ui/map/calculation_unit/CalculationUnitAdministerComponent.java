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

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy;
import org.kalypso.kalypsomodel1d2d.ui.map.cline.RouteLineElementWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * @author Madanagopal
 *
 */
public class CalculationUnitAdministerComponent 
{

  private FormToolkit toolkit;
  private Composite parent;
  private CalculationUnitDataModel dataModel;
  private Composite rootComposite;
  private Combo actionsCombo;
  private Combo elementsCombo;
  private Button goButton;
  private Image goImage;
  private static final String ACTION_KEY_ADMINISTER = "Verwalten";
  private static final String ACTION_KEY_REMOVE = "REMOVE";
  private static final String ACTION_KEY_DRAW = "New Zeichnen";
  
  private static final String ELEMENTS_KEY_ELEMENTS = "Elemente";
  private static final String ELEMENTS_KEY_SUBUNITS = "Sub-Einheiten";
  private static final String ELEMENTS_KEY_BOUNDARY_UP = "RandLinien";

//  private KeyBasedDataModelChangeListener settingsKeyListener = new KeyBasedDataModelChangeListener(){
//    public void dataChanged( String key, Object newValue )
//    {
//    }  
//  };
  
  
  public void createControl( CalculationUnitDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    guiComboSelections( parent );
  //  dataModel.addKeyBasedDataChangeListener( settingsKeyListener );
  }

  private void guiComboSelections( Composite parentComposite )
  {
    rootComposite = new Composite(parentComposite,SWT.FLAT);
    rootComposite.setLayout( new GridLayout(3,false) );
        
    actionsCombo = new Combo(rootComposite, SWT.RIGHT|SWT.READ_ONLY|SWT.BORDER);
    actionsCombo.add( ACTION_KEY_ADMINISTER );
   // actionsCombo.add( ACTION_KEY_REMOVE );
    actionsCombo.add( ACTION_KEY_DRAW );
    GridData data = new GridData(GridData.FILL_HORIZONTAL);
    actionsCombo.setLayoutData( data );
    
    elementsCombo = new Combo(rootComposite, SWT.RIGHT|SWT.READ_ONLY|SWT.BORDER);
    elementsCombo.add( ELEMENTS_KEY_ELEMENTS );
    elementsCombo.add( ELEMENTS_KEY_SUBUNITS );
    elementsCombo.add( ELEMENTS_KEY_BOUNDARY_UP );
   
    data = new GridData(GridData.FILL_HORIZONTAL);
    elementsCombo.setLayoutData( data );
    
    goButton = new Button(rootComposite,SWT.PUSH);
    goButton.setToolTipText( "Funktion aktivieren" );
//    goButton.setText( "GO");
    
    goImage = new Image( rootComposite.getDisplay(), 
        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin(
            PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
        "icons/elcl16/nav_go.gif" ).getImageData() );  
    goButton.setImage( goImage );    
    final SelectionListener goButtonListener = new SelectionListener()
    {

      public void widgetDefaultSelected( SelectionEvent e )
      {
                
      }

      public void widgetSelected( SelectionEvent e )
      {
        changeStategy();
      }     
    };
    goButton.addSelectionListener( goButtonListener  );
  }

  /**
   * 
   */
  public void changeStategy()
  {
    final String selectedType = elementsCombo.getText();
    final String selectedAction = actionsCombo.getText();
    IWidgetWithStrategy widgetWithStrategy = 
      (IWidgetWithStrategy) dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );
    
    final Object selectedWrapper = dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    
    IWidget strategy = null;
    
    if( ACTION_KEY_ADMINISTER.equals( selectedAction) )
    {
      if( ELEMENTS_KEY_BOUNDARY_UP.equals( selectedType )  )
      {
        strategy = new AlterCalUnitBorderWidget(dataModel);
      }
      else if( ELEMENTS_KEY_ELEMENTS.equals( selectedType ))
      {
        strategy= new AddRemoveElementToCalUnitWidget(dataModel);        
      }
      else if( ELEMENTS_KEY_SUBUNITS.equals( selectedType )  )
      {
        final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        CreateSubCalculationUnitCopyDialog calculationDialog = 
                      new CreateSubCalculationUnitCopyDialog( shell, dataModel );
        int answer = calculationDialog.open();
        if( answer == Window.OK )
        {
          // Do Nothing
        }
      }      
    }
//    else if (ACTION_KEY_REMOVE.equals( selectedAction ))
//    {
//      if( ELEMENTS_KEY_BOUNDARY_UP.equals( selectedType )){
//        
//      }
//      else if (ELEMENTS_KEY_ELEMENTS.equals( selectedType ))
//      {
//        strategy = new RemoveElementFromCalUnitWidget(dataModel);
//      }
//      else if( ELEMENTS_KEY_SUBUNITS.equals( selectedType ))
//      {
//        
//      }
//
//    }
    else if( ACTION_KEY_DRAW.equals( selectedAction) )
    {
      if( ELEMENTS_KEY_BOUNDARY_UP.equals( selectedType )  )
      {
        strategy = 
          new RouteLineElementWidget<IBoundaryLine>(
                  "Route boundary line",
                  "Route boundary line",
                  IBoundaryLine.class,
                  Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE);
      }
      else if (ELEMENTS_KEY_ELEMENTS.equals( selectedType ))
      {
        if (selectedWrapper instanceof ICalculationUnit1D)
          strategy = new CreateFEElement1DWidget();
        if (selectedWrapper instanceof ICalculationUnit2D)
          strategy = new CreateFE2DElementWidget();
      }
      else
      {
        System.out.println("Drawing not supported for:"+selectedType);
      }
      
    }
    widgetWithStrategy.setStrategy( strategy );
  }
}
