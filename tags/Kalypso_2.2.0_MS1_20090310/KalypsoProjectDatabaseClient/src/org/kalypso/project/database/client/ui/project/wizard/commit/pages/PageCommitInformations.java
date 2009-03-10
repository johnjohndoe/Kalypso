/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.project.database.client.ui.project.wizard.commit.pages;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

/**
 * @author Dirk Kuch
 */
public class PageCommitInformations extends WizardPage
{
  protected String m_sFile;

  protected String m_changes = "";

  public PageCommitInformations( )
  {
    super( "pageCommitInformations" ); //$NON-NLS-1$

    setTitle( "Projektaktualisierung" );
    setDescription( "Beschreiben Sie die �nderungen, die am Projekt durchgef�hrt wurden." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout() );
    setControl( container );

    final Group group = new Group( container, SWT.NONE );
    group.setLayout( new GridLayout() );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    group.setText( "�nderungen" );

    final Text changes = new Text( group, SWT.MULTI | SWT.WRAP | SWT.BORDER );
    changes.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    changes.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        m_changes = changes.getText();
      }
    } );

    setPageComplete( true );
  }

  public String getChanges( )
  {
    return m_changes;
  }

}
