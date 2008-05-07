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

package org.kalypso.ui.editor.obstableeditor;

import java.util.Arrays;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.i18n.Messages;

/**
 * @author schlienger
 */
public class TablePropertiesDialog extends TitleAreaDialog
{
  protected String m_tz;

  public TablePropertiesDialog( final Shell parentShell, final String timezoneName )
  {
    super( parentShell );

    m_tz = timezoneName;
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( Composite parent )
  {
    setTitle( Messages.getString("org.kalypso.ui.editor.obstableeditor.TablePropertiesDialog.0") ); //$NON-NLS-1$

    final Composite cmp = new Composite( parent, SWT.FILL );
    cmp.setLayout( new GridLayout( 2, false ) );
    cmp.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Label lblTz = new Label( cmp, SWT.LEFT );
    lblTz.setText( Messages.getString("org.kalypso.ui.editor.obstableeditor.TablePropertiesDialog.1") ); //$NON-NLS-1$
    final Combo cmbTz = new Combo( cmp, SWT.DROP_DOWN );
    cmbTz.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final String[] tz = TimeZone.getAvailableIDs();
    Arrays.sort( tz );
    cmbTz.setItems( tz );
    cmbTz.setText( m_tz );
    
    cmbTz.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        m_tz = cmbTz.getText();
      }
    } );
    
    return cmp;
  }

  public String getTimezoneName( )
  {
    return m_tz;
  }
}
