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

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

public class CreateCalculationUnitWizardPage extends WizardPage
{
  private Text m_calcUnitName;

  private Combo m_calcUnitType;

  private static final String QNAME_KEY_1D2D = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CreateCalculationUnitWizardPage.0" ); //$NON-NLS-1$

  private static final String QNAME_KEY_2D = "2D Berechnungseinheit"; //$NON-NLS-1$

  private static final String QNAME_KEY_1D = "1D Berechnungseinheit"; //$NON-NLS-1$

  public CreateCalculationUnitWizardPage( final String name, final String description )
  {
    super( name );
    setTitle( name );
    setDescription( description );
  }

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
      @Override
      public void modifyText( final ModifyEvent e )
      {
        handleNameModified();
      }
    } );

    final Label typeLabel = new Label( composite, SWT.RIGHT );
    typeLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CreateCalculationUnitWizardPage.5" ) ); //$NON-NLS-1$

    m_calcUnitType = new Combo( composite, SWT.RIGHT | SWT.READ_ONLY | SWT.BORDER );
    m_calcUnitType.add( QNAME_KEY_1D2D );
    m_calcUnitType.add( QNAME_KEY_1D );
    m_calcUnitType.add( QNAME_KEY_2D );
    m_calcUnitType.select( 0 );
    m_calcUnitType.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    setPageComplete( false );
  }

  void handleNameModified( )
  {
    setPageComplete( !m_calcUnitName.getText().trim().equals( "" ) ); //$NON-NLS-1$
    getContainer().updateButtons();
  }

  public String getCalculationUnitName( )
  {
    return m_calcUnitName.getText();
  }

  public QName getCalculationUnitType( )
  {
    final String qNameKey = m_calcUnitType.getText();
    if( QNAME_KEY_1D.equals( qNameKey ) )
      return ICalculationUnit1D.QNAME;
    else if( QNAME_KEY_2D.equals( qNameKey ) )
      return ICalculationUnit2D.QNAME;
    else if( QNAME_KEY_1D2D.equals( qNameKey ) )
      return ICalculationUnit1D2D.QNAME;
    else
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CreateCalculationUnitWizardPage.6" ) + qNameKey ); //$NON-NLS-1$
  }

  public String getCalculationUnitDescription( )
  {
    return StringUtils.EMPTY;
  }
}