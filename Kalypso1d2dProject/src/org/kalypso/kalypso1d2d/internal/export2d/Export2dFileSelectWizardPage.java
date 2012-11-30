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
package org.kalypso.kalypso1d2d.internal.export2d;

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
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.MeshConverterFactory;
import org.kalypso.ui.ImageProvider;

class Export2dFileSelectWizardPage extends WizardPage
{
  private Text m_destinationFileField;

  private final String[] m_filenameExtensions;

  private final String[] m_fileTypes;

  private String m_selectedExtension;

  private Button m_btnExportMiddleNodes;

  private Button m_btnExportRoughness;

  public Export2dFileSelectWizardPage( final String pageName, final String[] fileNameExtensions, final String[] fileTypes )
  {
    super( pageName, "", ImageProvider.IMAGE_UTIL_UPLOAD_WIZ ); //$NON-NLS-1$

    setTitle( Messages.getString( "org.kalypso.wizards.export2d.Export2dFileSelectWizardPage.1" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.wizards.export2d.Export2dFileSelectWizardPage.2" ) ); //$NON-NLS-1$

    m_filenameExtensions = fileNameExtensions;
    m_fileTypes = fileTypes;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    final Label label_1 = new Label( container, SWT.NONE );
    final GridData gridData_1 = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    label_1.setLayoutData( gridData_1 );
    label_1.setText( Messages.getString( "org.kalypso.wizards.export2d.Export2dFileSelectWizardPage.3" ) ); //$NON-NLS-1$

    m_destinationFileField = new Text( container, SWT.BORDER );
    m_destinationFileField.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    m_destinationFileField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Button button = new Button( container, SWT.NONE );
    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        browseForFile();
      }
    } );
    button.setText( Messages.getString( "org.kalypso.wizards.export2d.Export2dFileSelectWizardPage.4" ) ); //$NON-NLS-1$
    final Label label_2 = new Label( container, SWT.NONE );
    label_2.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    label_2.setText( " " ); //$NON-NLS-1$
    m_btnExportMiddleNodes = new Button( container, SWT.CHECK );
    m_btnExportMiddleNodes.setText( Messages.getString( "org.kalypso.wizards.export2d.Export2dFileSelectWizardPage.6" ) ); //$NON-NLS-1$
    m_btnExportMiddleNodes.setSelection( false );
    final Label label_3 = new Label( container, SWT.NONE );
    label_3.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    label_3.setText( " " ); //$NON-NLS-1$

    final Label label_4 = new Label( container, SWT.NONE );
    label_4.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    label_4.setText( " " ); //$NON-NLS-1$
    m_btnExportRoughness = new Button( container, SWT.CHECK );
    m_btnExportRoughness.setText( Messages.getString( "org.kalypso.wizards.export2d.Export2dFileSelectWizardPage.9" ) ); //$NON-NLS-1$
    m_btnExportRoughness.setSelection( false );

    final GridData gd = new GridData();
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
    for( final String m_filenameExtension : m_filenameExtensions )
      if( file.getName().endsWith( m_filenameExtension.substring( 1 ) ) )
      {
        regularExtension = true;
        m_selectedExtension = m_filenameExtension;
        break;
      }

    m_btnExportRoughness.setEnabled( MeshConverterFactory.supportFlowResistanceClasses( m_selectedExtension ) );
    m_btnExportMiddleNodes.setEnabled( MeshConverterFactory.supportMidSideNodes( m_selectedExtension ) );

    if( !regularExtension )
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "org.kalypso.wizards.export2d.Export2dFileSelectWizardPage.10" ) ); //$NON-NLS-1$
      m_selectedExtension = null;
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
    final FileDialog dialog = new FileDialog( getShell(), SWT.SAVE );
    dialog.setFilterExtensions( m_filenameExtensions );
    dialog.setFilterNames( m_fileTypes );
    if( path != "" ) //$NON-NLS-1$
      dialog.setFileName( path );
    final String fileName = dialog.open();

    if( fileName == null )
      return ""; //$NON-NLS-1$

    final String[] filterExtensions = dialog.getFilterExtensions();
    boolean regularExtension = false;
    for( final String filterExtension : filterExtensions )
      if( fileName.endsWith( filterExtension.substring( 1 ) ) )
      {
        regularExtension = true;
        break;
      }
    if( !regularExtension && filterExtensions.length == 1 )
      return fileName + filterExtensions[0].substring( 1 );
    return fileName;
  }

  public String getFilePath( )
  {
    return m_destinationFileField.getText().trim();
  }

  public boolean isSelectedExportMiddleNodes( )
  {
    return m_btnExportMiddleNodes.getSelection();
  }

  public boolean isSelectedExportRoughessData( )
  {
    return m_btnExportRoughness.getSelection();
  }

  public String getSelectedExtension( )
  {
    return m_selectedExtension;
  }

  public void setEnabledExportMiddleNodes( final boolean enable )
  {
    m_btnExportMiddleNodes.setEnabled( enable );
  }

  public void setEnabledExportRoughessData( final boolean enable )
  {
    m_btnExportRoughness.setEnabled( enable );
  }
}
