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
package org.kalypso.ui.repository.wizard;

import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * FileSelectWizardPage
 * 
 * @author schlienger
 */
public class FileSelectWizardPage extends WizardPage
{
  private final PreferenceStore m_store;
  private FileFieldEditor m_ffe;

  protected FileSelectWizardPage( final String pageName, final String fileName )
  {
    super( pageName );
    
    m_store = new PreferenceStore();
    m_store.setDefault( "FILE", fileName );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose( )
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
    
    final GridLayout gridLayout = new GridLayout( 4, true );
    sub.setLayout( gridLayout );
    sub.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_ffe.fillIntoGrid( sub, 4 );
    
    setControl( sub );
  }
  
  public String getFilePath()
  {
    if( m_ffe == null )
      return null;
    
    m_ffe.store();
    
    return m_store.getString( "FILE" );
  }
}
