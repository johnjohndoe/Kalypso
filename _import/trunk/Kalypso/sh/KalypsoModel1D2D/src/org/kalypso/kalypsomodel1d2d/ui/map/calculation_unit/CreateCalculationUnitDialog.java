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

import java.util.List;

import javax.xml.namespace.QName;

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
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.Util;

/**
 * @author Madanagopal
 * 
 */
@SuppressWarnings( { "unchecked", "hiding", "synthetic-access" }) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
class CreateCalculationUnitDialog extends Dialog
{

  private static final String QNAME_KEY_1D2D = Messages.getString("CreateCalculationUnitDialog.3"); //$NON-NLS-1$

  private static final String QNAME_KEY_2D = Messages.getString("CreateCalculationUnitDialog.4"); //$NON-NLS-1$

  private static final String QNAME_KEY_1D = Messages.getString("CreateCalculationUnitDialog.5"); //$NON-NLS-1$

  private static final int RESET_ID = IDialogConstants.NO_TO_ALL_ID + 1;

  public static final int OK_APPLIED = IDialogConstants.OK_ID;

  private Text nameField;

  private Text typeField;

  private final String defaultDescriptionText = Messages.getString("CreateCalculationUnitDialog.6"); //$NON-NLS-1$

  private Combo typeCombo;

  private Text descriptionText;

  private final KeyBasedDataModel dataModel;

  private ICalculationUnit createdCalculationUnit;

  protected CreateCalculationUnitDialog( final Shell parentShell, final KeyBasedDataModel dataModel )
  {
    super( parentShell );
    parentShell.setText( Messages.getString("CreateCalculationUnitDialog.7") ); //$NON-NLS-1$
    this.dataModel = dataModel;
  }

  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite comp = (Composite) super.createDialogArea( parent );

    final GridLayout layout = (GridLayout) comp.getLayout();
    layout.numColumns = 2;

    final Label nameLabel = new Label( comp, SWT.RIGHT );
    nameLabel.setText( Messages.getString("CreateCalculationUnitDialog.8") ); //$NON-NLS-1$

    nameField = new Text( comp, SWT.SINGLE | SWT.BORDER );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    nameField.setLayoutData( data );
    // nameField.addModifyListener( new ModifyListener()
    // {
    // public void modifyText( ModifyEvent e )
    // {
    // String numberPattern = "\\d+";
    // if( !nameField.getText().matches( numberPattern ) )
    // {
    // setMessage( null );
    // setErrorMessage( "Falsch Parameter" );
    // setPageComplete( false );
    // }
    // else {
    //       
    // setMessage( null );
    // setErrorMessage(null);
    // setPageComplete( true );
    // }
    // }
    // } );

    // nameField.addModifyListener( new ModifyListener(){
    //
    // public void modifyText( ModifyEvent e )
    // {
    // if (nameField.getText() == null){
    // setMessage( null );
    // setErrorMessage( "Startdatum nach Enddatum nicht möglich." );
    // setPageComplete(false);
    // }
    //        
    // }
    //      
    // } );

    final Label typeLabel = new Label( comp, SWT.RIGHT );
    typeLabel.setText( Messages.getString("CreateCalculationUnitDialog.9") ); //$NON-NLS-1$

    // @TODO A Combo Field

    typeCombo = new Combo( comp, SWT.RIGHT | SWT.READ_ONLY | SWT.BORDER );
    typeCombo.add( QNAME_KEY_1D );
    typeCombo.add( QNAME_KEY_2D );
    typeCombo.add( QNAME_KEY_1D2D );
    data = new GridData( GridData.FILL_HORIZONTAL );
    typeCombo.setLayoutData( data );

    final Label DescriptionLabel = new Label( comp, SWT.RIGHT );
    DescriptionLabel.setText( Messages.getString("CreateCalculationUnitDialog.10") ); //$NON-NLS-1$

    descriptionText = new Text( comp, SWT.BORDER | SWT.MULTI );
    descriptionText.setText( defaultDescriptionText );
    data = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING | GridData.FILL_BOTH );
    data.heightHint = 100;
    descriptionText.setLayoutData( data );
    return comp;
  }

  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    super.createButtonsForButtonBar( parent );
    createButton( parent, RESET_ID, "Reset All", false ); //$NON-NLS-1$
  }

  @Override
  protected void buttonPressed( final int buttonId )
  {
    if( buttonId == RESET_ID )
    {
      nameField.setText( "" ); //$NON-NLS-1$
      typeCombo.select( 0 );
      descriptionText.setText( defaultDescriptionText );
    }
    if( buttonId == OK_APPLIED )
    {
      if( typeCombo.getText() == null )
      {

      }
      System.out.println( nameField.getText() + "," + typeCombo.getText() + "," + descriptionText.getText() ); //$NON-NLS-1$ //$NON-NLS-2$
      final String name = nameField.getText();
      final String qNameKey = typeCombo.getText();
      final String desc = descriptionText.getText();

      // model is taken from the current context
      final CreateCalculationUnitCmd cmd = new CreateCalculationUnitCmd( getCUnitQName( qNameKey ), Util.getModel( IFEDiscretisationModel1d2d.class ), name, desc )
      {
        /**
         * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd#process()
         */
        @SuppressWarnings( { "unchecked", "synthetic-access" }) //$NON-NLS-1$ //$NON-NLS-2$
        @Override
        public void process( ) throws Exception
        {
          super.process();
          // reset list of calculation units
          IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d) dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
          List<ICalculationUnit> calUnits = CalcUnitOps.getModelCalculationUnits( model1d2d );
          dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, calUnits );

          // set the create unit as selected
          dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, getCreatedCalculationUnit() );

        }
      };
      KeyBasedDataModelUtil.postCommand( dataModel, cmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );

      super.okPressed();
      this.createdCalculationUnit = cmd.getCreatedCalculationUnit();
    }
    else
    {
      super.buttonPressed( buttonId );
    }
  }

  /**
   * Return the QNane associated with the given key
   * 
   * @param qNameKey
   *            the q-name key
   * @return return the QName for the given key
   * @throws RuntimeException
   *             if qNameKey is an unknown key
   */
  private static final QName getCUnitQName( final String qNameKey ) throws RuntimeException
  {
    if( QNAME_KEY_1D.equals( qNameKey ) )
    {
      return Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D;
    }
    else if( QNAME_KEY_2D.equals( qNameKey ) )
    {

      return Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_2D;
    }
    else if( QNAME_KEY_1D2D.equals( qNameKey ) )
    {
      return Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D2D;
    }
    else
    {
      throw new RuntimeException( "Unknown qNameKey:" + qNameKey ); //$NON-NLS-1$
    }
  }
}
