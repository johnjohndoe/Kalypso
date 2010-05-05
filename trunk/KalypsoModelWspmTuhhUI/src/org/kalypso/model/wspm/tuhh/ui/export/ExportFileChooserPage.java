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
package org.kalypso.model.wspm.tuhh.ui.export;

import java.io.File;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.IFileChooserDelegate;
import org.kalypso.contribs.eclipse.ui.forms.IMessageReceiver;

/**
 * @author kimwerner
 */
public class ExportFileChooserPage extends WizardPage implements IWizardPage, IMessageReceiver
{
  private FileChooserGroup m_fileChooserGroup;

  private File m_file;

  private final IFileChooserDelegate m_fileChooser;

  private final String m_extension;

  private String m_fileGroupText = "";

  public ExportFileChooserPage( final IFileChooserDelegate fileChooser, final String extension )
  {
    super( "exportProfileFileChooserPage" );

    m_fileChooser = fileChooser;
    m_extension = extension;
  }

  public void setFileGroupText( final String fileGroupText )
  {
    m_fileGroupText = fileGroupText;
  }

  protected IFileChooserDelegate getFileChooserDelegate( )
  {
    return m_fileChooser;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite comp = new Composite( parent, SWT.NONE );
    comp.setLayout( new GridLayout() );

    createPageContent( comp );

    setControl( comp );

    final IMessageProvider message = validatePage();
    setPageComplete( message == null || message.getMessageType() != IMessageProvider.ERROR );
  }

  /**
   * Creates the contents of this page, intended to be overwritten by clients.<br>
   * Clients should call the super implementation of this method, before adding its own contents.
   * 
   * @param parent
   *          A composite with a grid layout.
   */
  protected void createPageContent( final Composite parent )
  {
    createFileChooser( parent );
  }

  private void createFileChooser( final Composite comp )
  {
    m_fileChooserGroup = new FileChooserGroup( m_fileChooser );
    m_fileChooserGroup.addFileChangedListener( new FileChooserGroup.FileChangedListener()
    {
      public void fileChanged( final File file )
      {
        setFile( file );
      }
    } );

    m_fileChooserGroup.setShowLabel( false );
    final Group group = m_fileChooserGroup.createControl( comp, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    group.setText( m_fileGroupText );

    final IDialogSettings dialogSettings = getDialogSettings();
    m_fileChooserGroup.setDialogSettings( dialogSettings );

    final File file = m_fileChooserGroup.getFile();
    if( file != null )
    {
      final String newFileName = enforceSuffix( file );
      setFile( new File( newFileName ) );
    }
  }

  public String enforceSuffix( final File file )
  {
    final String fileName = file.getAbsolutePath();
    if( m_extension == null )
      return fileName;

    return FileChooserGroup.setSuffix( fileName, m_extension );
  }

  protected void setFile( final File file )
  {
    m_file = file;

    updateMessage();
  }

  public void updateMessage( )
  {
    final IMessageProvider validate = validatePage();
    setMessage( validate );
  }

  /**
   * Validates this page. Intended to be overwritten by clients.
   */
  protected IMessageProvider validatePage( )
  {
    return m_fileChooser.validate( m_file );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.forms.IMessageReceiver#setMessage(org.eclipse.jface.dialogs.IMessageProvider)
   */
  @Override
  public void setMessage( final IMessageProvider message )
  {
    if( message == null )
      setMessage( (String) null );
    else
      setMessage( message.getMessage(), message.getMessageType() );

    setPageComplete( message == null || message.getMessageType() != IMessageProvider.ERROR );
  }

  public File getFile( )
  {
    return m_file;
  }
}
