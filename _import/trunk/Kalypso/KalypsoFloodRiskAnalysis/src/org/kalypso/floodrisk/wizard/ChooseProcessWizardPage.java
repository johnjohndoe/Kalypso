/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.floodrisk.wizard;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.floodrisk.process.ProcessExtension;

/**
 * ChooseCalculationsWizardPage
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (13.05.2005)
 */
public class ChooseProcessWizardPage extends WizardPage
{

  private ProcessExtension[] m_processes;

  private Composite m_topLevel;

  protected boolean check = false;

  public ChooseProcessWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setPageComplete( true );
  }

  public void setProcesses( ProcessExtension[] processes )
  {
    m_processes = processes;
  }

  public void createControl( Composite parent )
  {
    initializeDialogUnits( parent );
    m_topLevel = new Composite( parent, SWT.NONE );

    GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 2;
    m_topLevel.setLayout( gridLayout );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    m_topLevel.setLayoutData( data );

    for( int i = 0; i < m_processes.length; i++ )
    {
      final ProcessExtension processExt = m_processes[i];

      final Button checkButton = new Button( m_topLevel, SWT.CHECK );
      checkButton.setSelection( check );
      checkButton.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent e )
        {
          check = checkButton.getSelection();
          if( checkButton.getSelection() )
          {
            processExt.setState( true );
            System.out.println( processExt.getName() + "added" );
          }
          else
          {
            processExt.setState( false );
            System.out.println( processExt.getName() + "removed" );
          }
        }
      } );

      Label landusePropLabel = new Label( m_topLevel, SWT.NONE );
      landusePropLabel.setText( processExt.getName() );
    }

    setControl( m_topLevel );
  }

  public ProcessExtension[] getProcesses()
  {
    return m_processes;
  }
}