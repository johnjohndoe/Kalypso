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

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * @author Madanagopal
 *
 */
class CreateCalculationUnitDialog extends Dialog{
  
  private static final int RESET_ID = IDialogConstants.NO_TO_ALL_ID + 1;
  public static final int OK_APPLIED = IDialogConstants.OK_ID;
  
  private Text nameField;
  private Text typeField;
  private String defaultDescriptionText = "Geben Sie eine Deskription fur das Complex Element";
  private Combo typeCombo;
  private Text descriptionText;

  protected CreateCalculationUnitDialog( Shell parentShell )
  {
    
    super( parentShell );
    // TODO Auto-generated constructor stub
  }
  
  protected Control createDialogArea(Composite parent)
  {
    Composite comp = (Composite)super.createDialogArea(parent);
    
    GridLayout layout = (GridLayout)comp.getLayout();
    layout.numColumns = 2;
    
    Label nameLabel = new Label(comp, SWT.RIGHT);
    nameLabel.setText("Name: ");
    
    nameField = new Text(comp, SWT.SINGLE|SWT.BORDER);
    GridData data = new GridData(GridData.FILL_HORIZONTAL);
    nameField.setLayoutData(data);
    
    Label typeLabel = new Label(comp, SWT.RIGHT);
    typeLabel.setText("Type: ");
    
    //@TODO A Combo Field
    
    typeCombo = new Combo(comp, SWT.RIGHT|SWT.READ_ONLY|SWT.BORDER);
    typeCombo.add( "1D" );
    typeCombo.add( "2D" );
    typeCombo.add("1d/2D");
    data = new GridData(GridData.FILL_HORIZONTAL);
    typeCombo.setLayoutData( data );      
    
    Label DescriptionLabel = new Label(comp, SWT.RIGHT);
    DescriptionLabel.setText("Description: ");
    
    descriptionText = new Text(comp, SWT.BORDER | SWT.MULTI );
    descriptionText.setText( defaultDescriptionText );
    data = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ); 
    data.heightHint = 100;
    descriptionText.setLayoutData( data );
    
    return comp;
  }
  protected void createButtonsForButtonBar(Composite parent)
  {
    super.createButtonsForButtonBar(parent);
    createButton(parent, RESET_ID, "Reset All", false);
  }
  
  protected void buttonPressed(int buttonId)
  {
    if(buttonId == RESET_ID)
    {
      nameField.setText("");
      typeCombo.select( 0 );        
      descriptionText.setText(defaultDescriptionText);
    }
    if (buttonId == OK_APPLIED)
    {
      System.out.println(nameField.getText()+","+typeCombo.getText()+","+descriptionText.getText());
      nameField.getText();
      typeCombo.getText();
      descriptionText.getText();
    //@TODO Add to the discretisation Model 1d2d
      super.okPressed();       
    }
    else
    {
      super.buttonPressed(buttonId);
    }
  }    
}  
