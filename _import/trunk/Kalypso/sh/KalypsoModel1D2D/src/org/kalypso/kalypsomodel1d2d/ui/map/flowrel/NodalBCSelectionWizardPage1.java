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

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateNodalBCFlowrelationWidget.TimeserieTypeDescription;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class NodalBCSelectionWizardPage1 extends WizardPage
{
  private Button[] m_radioBtnGroup;

  private TimeserieTypeDescription[] m_descriptions;

  protected NodalBCSelectionWizardPage1( final TimeserieTypeDescription[] descriptions )
  {
    super( "Super title" );
    setTitle( "Art der Randbedingung" );
    setDescription( "Geben sie auf dieser Seite die Art der Randbedingung ein." );
    m_descriptions = descriptions;
  }

  public void init( ISelection selection )
  {
    m_radioBtnGroup = new Button[m_descriptions.length + 1];
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    for( int i = 0; i < m_radioBtnGroup.length - 1; i++ )
    {
      final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
      gridData.horizontalSpan = 3;
      m_radioBtnGroup[i] = new Button( container, SWT.RADIO );
      m_radioBtnGroup[i].setText( m_descriptions[i].getName() );
      m_radioBtnGroup[i].setLayoutData( gridData );
    }
    m_radioBtnGroup[m_radioBtnGroup.length - 1] = new Button( container, SWT.RADIO );
    m_radioBtnGroup[m_radioBtnGroup.length - 1].setText( "Zeitreihe aus Repository" );
    m_radioBtnGroup[m_radioBtnGroup.length - 1].addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        getWizard().getContainer().updateButtons();
      }
    } );

    final GridData gridData2 = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    gridData2.horizontalSpan = 2;
    m_radioBtnGroup[m_radioBtnGroup.length - 1].setLayoutData( gridData2 );
    // Button button = new Button( container, SWT.PUSH );
    // button.setText( "Durchsuchen..." );
    // button.addSelectionListener( new SelectionAdapter()
    // {
    // @Override
    // public void widgetSelected( SelectionEvent e )
    // {
    // for(int i=0; i<m_radioBtnGroup.length - 1; i++)
    // m_radioBtnGroup[i].setSelection( false );
    // m_radioBtnGroup[m_radioBtnGroup.length - 1].setSelection( true );
    // blahBlah();
    //        
    // }
    // } );

    m_radioBtnGroup[0].setFocus();
  }

  public int getSelectedChoice( )
  {
    for( int i = 0; i < m_radioBtnGroup.length; i++ )
    {
      if( m_radioBtnGroup[i].getSelection() )
        return i;
    }
    // TODO what if nothing is selected (if possible)
    return 0;
  }

  public boolean isChoiceTimeseries( )
  {
    return getSelectedChoice() == m_radioBtnGroup.length - 1;
  }
}
