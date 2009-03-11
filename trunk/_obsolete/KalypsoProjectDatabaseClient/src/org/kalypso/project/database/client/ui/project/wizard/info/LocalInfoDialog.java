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
package org.kalypso.project.database.client.ui.project.wizard.info;

import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.i18n.Messages;

/**
 * @author kuch
 */
public class LocalInfoDialog extends TitleAreaDialog
{

  protected final ILocalProject m_handler;

  public LocalInfoDialog( final ILocalProject handler, final Shell parentShell )
  {
    super( parentShell );
    m_handler = handler;

    setBlockOnOpen( true );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {
    final Control contents = super.createContents( parent );

    setTitle( Messages.getString("org.kalypso.project.database.client.ui.project.wizard.info.LocalInfoDialog.0") ); //$NON-NLS-1$
    setMessage( null );

    return contents;
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite composite = (Composite) super.createDialogArea( parent );
    composite.setLayout( new GridLayout() );
    final GridData data = new GridData( GridData.FILL, GridData.FILL, true, true );
    data.heightHint = 350;
    data.widthHint = 400;
    composite.setLayoutData( data );

    final Composite body = new Composite( composite, SWT.NULL );
    body.setLayout( new GridLayout() );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    renderProjectInfo( body );

    return composite;
  }

  private void renderProjectInfo( final Composite parent )
  {
    /* name */
    new Label( parent, SWT.NULL ).setText( Messages.getString("org.kalypso.project.database.client.ui.project.wizard.info.LocalInfoDialog.1") ); //$NON-NLS-1$

    final Text name = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    name.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* description */
    final Label labelDescription = new Label( parent, SWT.TOP );
    labelDescription.setText( Messages.getString("org.kalypso.project.database.client.ui.project.wizard.info.LocalInfoDialog.2") ); //$NON-NLS-1$
    labelDescription.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, false ) );

    final Text description = new Text( parent, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    description.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    description.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        try
        {
          final IProjectDescription project = m_handler.getProject().getDescription();
          project.setComment( description.getText() );

          m_handler.getProject().setDescription( project, new NullProgressMonitor() );
        }
        catch( final CoreException e1 )
        {
          KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
        }
      }
    } );

    try
    {
      final IProjectDescription project = m_handler.getProject().getDescription();

      name.setText( project.getName() );
      description.setText( project.getComment() );
    }
    catch( final CoreException e )
    {
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    // create OK and Cancel buttons by default
    createButton( parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true );
// createButton(parent, IDialogConstants.CANCEL_ID,
// IDialogConstants.CANCEL_LABEL, false);
  }
}
