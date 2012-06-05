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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;

public class CloneCalculationUnitWizardPage extends WizardPage
{
  Text m_calcUnitName;

  final CalculationUnitDataModel m_dataModel;

  public CloneCalculationUnitWizardPage( final String name, final CalculationUnitDataModel calcUnitDataModel )
  {
    super( name );
    setTitle( name );
    m_dataModel = calcUnitDataModel;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 2;
    composite.setLayout( gridLayout );
    setControl( composite );

    final Label nameLabel = new Label( composite, SWT.RIGHT );
    nameLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CreateCalculationUnitWizardPage.3" ) ); //$NON-NLS-1$
    m_calcUnitName = new Text( composite, SWT.SINGLE | SWT.BORDER );
    m_calcUnitName.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_calcUnitName.addModifyListener( new ModifyListener()
    {
      // @SuppressWarnings("synthetic-access")
      @Override
      public void modifyText( ModifyEvent e )
      {
        setPageComplete( isNameValid() ); //$NON-NLS-1$
        getContainer().updateButtons();
      }

    } );

    setPageComplete( false );
  }

  boolean isNameValid( )
  {
    if( m_calcUnitName.getText() == null || "".equals( m_calcUnitName.getText().trim() ) )
    {
      return false;
    }
    final ICalculationUnit[] calculationUnits = m_dataModel.getCalculationUnits();
    for( int i = 0; i < calculationUnits.length; i++ )
    {
      if( m_calcUnitName.getText().equals( calculationUnits[i].getName() ) )
      {
        return false;
      }
    }
    return true;
  }

  public String getCalculationUnitName( )
  {
    return m_calcUnitName.getText();
  }

  public String getCalculationUnitDescription( )
  {
    return ""; //$NON-NLS-1$
  }

}
