/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.metadoc.ui;

import org.apache.commons.configuration.Configuration;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * A simple wizard page which takes its contents from a configuration object
 * 
 * @author schlienger
 */
public class ConfigurationWizardPage extends WizardPage
{
  private final Configuration m_config;
  private final String[] m_editableKeys;
  private final String[] m_labels;

  public ConfigurationWizardPage( final String pageName, final String title, final ImageDescriptor titleImage,
      final Configuration config, final String[] editableKeys, final String[] labels )
  {
    super( pageName, title, titleImage );

    if( editableKeys.length != labels.length )
      throw new IllegalArgumentException( "Label and key arrays do not have the same length" );

    m_config = config;
    m_editableKeys = editableKeys;
    m_labels = labels;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 2, false ) );

    for( int i = 0; i < m_editableKeys.length; i++ )
    {
      final String key = m_editableKeys[i];
      
      final Label label = new Label( composite, SWT.LEAD );
      label.setText( m_labels[i] );

      final Text text = new Text( composite, SWT.LEAD );
      text.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL ) );
      text.setText( m_config.getString( key, "" ) );
      text.addModifyListener( new ModifyListener()
      {
        public void modifyText( ModifyEvent e )
        {
          m_config.setProperty( key, text.getText() );
        }
      } );
    }
    
    setControl( composite );
  }
}
