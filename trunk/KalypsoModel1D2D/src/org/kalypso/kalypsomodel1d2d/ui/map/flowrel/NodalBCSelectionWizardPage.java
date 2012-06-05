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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class NodalBCSelectionWizardPage extends WizardPage
{
  protected final Button[] m_radioBtnGroup;

  private final IBoundaryConditionDescriptor[] m_descriptors;

  private final NodalBCDescriptorPage m_descriptorPage;

  private Text m_bcValue;
  
  private String m_strBCValueTmp = null;

  protected NodalBCSelectionWizardPage( final String pageName, final IBoundaryConditionDescriptor[] descriptors, final NodalBCDescriptorPage descriptorPage )
  {
    super( pageName );

    m_descriptorPage = descriptorPage;

    setTitle( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizardPage.0") ); //$NON-NLS-1$
    setDescription( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizardPage.1") ); //$NON-NLS-1$
    m_descriptors = descriptors;
    m_radioBtnGroup = new Button[m_descriptors.length];
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public synchronized void createControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 2;
    container.setLayout( gridLayout );
    setControl( container );

    new Label( container, SWT.NONE ).setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizardPage.2") ); //$NON-NLS-1$
    m_bcValue = new Text( container, SWT.BORDER );
    m_bcValue.setText( "20.0" ); //$NON-NLS-1$
    m_bcValue.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    // bcValue.addKeyListener( new KeyListener()
    // {
    //
    // public void keyPressed( KeyEvent e )
    // {
    // // Empty
    // }
    //
    // public void keyReleased( KeyEvent e )
    // {
    // cacheNewName();
    // if( updateListener != null )
    // {
    // updateListener.update();
    // }
    // }
    //
    // };
    // ) )
    final GridData radioGroupGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    radioGroupGridData.horizontalSpan = 2;
    radioGroupGridData.verticalIndent = 15;
    final Group radioGroup = new Group( container, SWT.NONE );
    radioGroup.setLayoutData( radioGroupGridData );
    radioGroup.setLayout( (new GridLayout( 1, false )) );
    radioGroup.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizardPage.4") ); //$NON-NLS-1$
    for( int i = 0; i < m_radioBtnGroup.length; i++ )
    {
      final GridData radioButtonGridData = new GridData( SWT.FILL, SWT.BEGINNING, true, false );
      radioButtonGridData.horizontalSpan = 1;
      radioButtonGridData.horizontalIndent = 10;
      radioButtonGridData.verticalIndent = 5;
      final Button radio = new Button( radioGroup, SWT.RADIO );
      radio.setText( m_descriptors[i].getName() );
      radio.setLayoutData( radioButtonGridData );
      m_radioBtnGroup[i] = radio;

      final IBoundaryConditionDescriptor selectedDescriptor = m_descriptors[i];

      radio.addSelectionListener( new SelectionAdapter()
      {
        /**
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        @Override
        public void widgetSelected( final SelectionEvent e )
        {
          setDescriptor( selectedDescriptor );
          if( selectedDescriptor instanceof WaveStepDescriptor ){
            saveDefaultBCValue();
            setWaveDefaultValue();
          }
          else{
            restoreDefaultBCValue();
          }
        }
      } );
    }
    // m_radioBtnGroup[0].setSelection( true );
    // m_descriptorPage.setDescriptor( m_descriptors[0] );
    // m_selectionPage.setDescriptor( m_descriptors[0] );
  }

  protected void restoreDefaultBCValue( )
  {
    if( m_strBCValueTmp != null ){
      String lStrBCValueTmp = m_bcValue.getText();
      m_bcValue.setText( m_strBCValueTmp );
      m_strBCValueTmp = lStrBCValueTmp;
    }
    else{
      m_bcValue.setText( "20.0" ); //$NON-NLS-1$
    }
  }

  protected void setWaveDefaultValue( )
  {
    m_bcValue.setText( WaveStepDescriptor.DEFAULT_VALUE );
  }

  protected void saveDefaultBCValue( )
  {
    if( m_strBCValueTmp == null )
      m_strBCValueTmp = m_bcValue.getText();
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
   */
  @Override
  public boolean isPageComplete( )
  {
    for( final Button element : m_radioBtnGroup )
      if( element.getSelection() == true )
        return true;
    return false;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  @Override
  public boolean canFlipToNextPage( )
  {
    return isPageComplete();
  }

  protected void setDescriptor( final IBoundaryConditionDescriptor selectedDescriptor )
  {
    m_descriptorPage.setDescriptor( selectedDescriptor );

    getContainer().updateButtons();
  }

  protected String getSteadyValue( )
  {
    return m_bcValue.getText();
//    return Double.parseDouble( m_bcValue.getText() );
  }
}
