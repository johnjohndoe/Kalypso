/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.wizards.export2d;

import java.io.File;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.ImageProvider;

public class FileSelectWizardPage extends WizardPage
{
  private Text m_destinationFileField;

  private String[] m_filenameFilters;

  private Button m_btnExportMiddleNodes;

  private Button m_btnExportRoughness;

  public FileSelectWizardPage( final String pageName, final String[] fileExtensions )
  {
    super( pageName, "", ImageProvider.IMAGE_UTIL_UPLOAD_WIZ );
    setTitle( "FE Netz exportieren" );
    setDescription( "Bitte w�hlen Sie eine Datei aus, welche Sie als FE Netz exportieren m�chten." );
    m_filenameFilters = fileExtensions;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    final Label label_1 = new Label( container, SWT.NONE );
    final GridData gridData_1 = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    label_1.setLayoutData( gridData_1 );
    label_1.setText( "Destination file name:" );

    m_destinationFileField = new Text( container, SWT.BORDER );
    m_destinationFileField.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    m_destinationFileField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Button button = new Button( container, SWT.NONE );
    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        browseForFile();
      }
    } );
    button.setText( "Browse" );
    final Label label_2 = new Label( container, SWT.NONE );
    label_2.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING) );
    label_2.setText( " " );
    m_btnExportMiddleNodes = new Button( container, SWT.CHECK );
    m_btnExportMiddleNodes.setText( "Export middle nodes" );
    m_btnExportMiddleNodes.setSelection( true );
    final Label label_3 = new Label( container, SWT.NONE );
    label_3.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING) );
    label_3.setText( " " );

    final Label label_4 = new Label( container, SWT.NONE );
    label_4.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING) );
    label_4.setText( " " );
    m_btnExportRoughness = new Button( container, SWT.CHECK );
    m_btnExportRoughness.setText( "Export roughness data" );
    m_btnExportRoughness.setSelection( false );
    
    GridData gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.widthHint = 75;

    setPageComplete( false );
    button.setFocus();
  }

  /**
   * Update the current page complete state based on the field content.
   */
  protected void updatePageComplete( )
  {
    setPageComplete( false );
    final File file = new File( getFilePath() );
    boolean regularExtension = false;
    for( int i = 0; i < m_filenameFilters.length; i++ )
      if( file.getName().endsWith( m_filenameFilters[i].substring( 1 ) ) )
      {
        regularExtension = true;
        break;
      }
    if( !regularExtension )
    {
      setMessage( null );
      setErrorMessage( "Irregular file extension" );
      return;
    }
    setMessage( null );
    setErrorMessage( null );
    setPageComplete( true );
  }

  protected void browseForFile( )
  {
    m_destinationFileField.setText( browse( getFilePath() ) );
  }

  private String browse( final String path )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.OPEN );
    dialog.setFilterExtensions( m_filenameFilters );
    if( path != "" )
      dialog.setFileName( path );
    return dialog.open();
  }

  public String getFilePath( )
  {
    return m_destinationFileField.getText().trim();
  }
  
  public boolean isSelectedExportMiddleNodes(){
    return m_btnExportMiddleNodes.getSelection();
  }
  
  public boolean isSelectedExportRoughessData(){
    return m_btnExportRoughness.getSelection();
  }
}
