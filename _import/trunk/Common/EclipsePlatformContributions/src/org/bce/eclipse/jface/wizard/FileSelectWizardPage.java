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
package org.bce.eclipse.jface.wizard;

import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * FileSelectWizardPage contains a simple FileFieldEditor that allows the user to enter the file name directly as a
 * string or to use the FileChooserDialog.
 * 
 * @author schlienger
 */
public class FileSelectWizardPage extends WizardPage
{
  private final String[] m_fileExtensions;

  private final PreferenceStore m_store;

  private FileFieldEditor m_ffe;

  /**
   * @param pageName
   *          the name of this page
   * @param fileName
   *          the default name of the file, can be empty
   * @param fileExtensions
   *          [optional, can be null] the list of allowed file extensions
   */
  public FileSelectWizardPage( final String pageName, final String fileName, final String[] fileExtensions )
  {
    super( pageName );

    m_fileExtensions = fileExtensions;

    m_store = new PreferenceStore();
    m_store.setDefault( "FILE", fileName );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    if( m_ffe != null )
      m_ffe.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    final Composite sub = new Composite( parent, SWT.FILL );

    m_ffe = new FileFieldEditor( "FILE", "Datei:", sub );
    m_ffe.setPreferenceStore( m_store );
    m_ffe.loadDefault();
    m_ffe.setEmptyStringAllowed( false );
    m_ffe.setFileExtensions( m_fileExtensions );

    if( m_ffe.getStringValue() == null || m_ffe.getStringValue().length() == 0 )
    {
      final String fileName = getDialogSettings().get( "FILE" );
      if( fileName != null )
      {
        m_store.setDefault( "FILE", fileName );
        m_ffe.loadDefault();
      }
    }

    final GridLayout gridLayout = new GridLayout( 4, true );
    sub.setLayout( gridLayout );
    sub.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_ffe.fillIntoGrid( sub, 4 );

    setControl( sub );

    setDescription( "Geben Sie einen Dateiname ein" );
  }

  public String getFilePath()
  {
    if( m_ffe == null )
      return null;

    m_ffe.store();
    final String fileName = m_store.getString( "FILE" );
    getDialogSettings().put( "FILE", fileName );

    return fileName;
  }
}
