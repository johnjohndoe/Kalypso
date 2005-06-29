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
package org.kalypso.floodrisk.wizard;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
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
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.floodrisk.process.ProcessExtension;

/**
 * ProcessInputWizardPage
 * <p>
 * WizardPage, which asks for the location of the inputdata
 * 
 * created by
 * 
 * @author Nadja Peiler (18.05.2005)
 */
public class ProcessInputWizardPage extends WizardPage
{

  private ProcessExtension m_processExt;

  private Composite m_topLevel;

  private Text m_textModelData;

  private IProject m_project;

  /**
   * Constructor
   * 
   * @param pageName
   * @param title
   * @param titleImage
   */
  protected ProcessInputWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setPageComplete( false );
  }

  /**
   * sets the process for which inputdata is needed
   * 
   * @param processExt
   *  
   */
  public void setProcessExtension( ProcessExtension processExt )
  {
    m_processExt = processExt;
  }

  /**
   * 
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    initializeDialogUnits( parent );
    m_topLevel = new Composite( parent, SWT.NONE );

    GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    m_topLevel.setLayout( gridLayout );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    m_topLevel.setLayoutData( data );

    Label label = new Label( m_topLevel, SWT.NONE );
    label.setText( "ModelData: " );

    m_textModelData = new Text( m_topLevel, SWT.BORDER );
    m_textModelData.setEditable( false );
    GridData data1 = new GridData();
    data1.horizontalAlignment = GridData.FILL;
    data1.grabExcessHorizontalSpace = true;
    m_textModelData.setLayoutData( data1 );

    Button button = new Button( m_topLevel, SWT.PUSH );
    button.setText( "Auswahl ..." );
    GridData data2 = new GridData();
    data2.horizontalAlignment = GridData.END;
    button.setLayoutData( data2 );

    button.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        try
        {
          KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[]
          { "xml" } );
          dialog.open();
          Object[] result = dialog.getResult();
          if( result != null )
          {
            Path resultPath = (Path)result[0];
            m_textModelData.setText( resultPath.toString() );
            m_processExt.setModelDataPath( resultPath );
          }
          validate();

        }
        catch( Exception e1 )
        {
          e1.printStackTrace();
        }
      }
    } );

    setControl( m_topLevel );
  }

  KalypsoResourceSelectionDialog createResourceDialog( String[] fileResourceExtensions )
  {
    return new KalypsoResourceSelectionDialog( getShell(), m_project, "Select modelData", fileResourceExtensions,
        m_project );
  }

  /**
   * sets the selected project
   * 
   * @param project
   *  
   */
  public void setProject( IProject project )
  {
    m_project = project;
  }

  /**
   * validates the user input
   * 
   *  
   */
  void validate()
  {
    setErrorMessage( null );
    boolean pageComplete = true;

    // modelData
    if( m_textModelData.getText() != null && m_textModelData.getText().length() > 0 )
    {
      //ok
    }
    else
    {
      setErrorMessage( "Bitte Modelldaten ausw‰hlen!" );
      pageComplete = false;
    }
    setPageComplete( pageComplete );
  }

}