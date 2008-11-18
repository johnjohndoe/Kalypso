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
package org.kalypso.project.database.client.ui.project.status;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.ProjectDatabaseModel;
import org.kalypso.project.database.client.core.model.remote.IRemoteProjectsListener;
import org.kalypso.util.swt.StatusDialog;

/**
 * Composite which displays the current project model database server state
 * 
 * @author Dirk Kuch
 */
public class ProjectDatabaseServerStatusComposite extends Composite implements IRemoteProjectsListener
{
  private static final Image IMG_SERVER_WAITING = new Image( null, ProjectDatabaseServerStatusComposite.class.getResourceAsStream( "icons/server_refresh.gif" ) );

  private static final Image IMG_SERVER_OK = new Image( null, ProjectDatabaseServerStatusComposite.class.getResourceAsStream( "icons/server_okay.gif" ) );

  private static final Image IMG_SERVER_ERROR = new Image( null, ProjectDatabaseServerStatusComposite.class.getResourceAsStream( "icons/server_error.gif" ) );

  private final FormToolkit m_toolkit;

  private Composite m_body;

  public ProjectDatabaseServerStatusComposite( final Composite parent, final FormToolkit toolkit )
  {
    super( parent, SWT.NONE );
    m_toolkit = toolkit;

    final ProjectDatabaseModel model = KalypsoProjectDatabaseClient.getDefault().getProjectDatabaseModel();
    model.addRemoteListener( this );

    final GridLayout layout = new GridLayout();
    layout.verticalSpacing = layout.marginWidth = 0;
    this.setLayout( layout );

    update( model.getRemoteConnectionState() );
  }

  protected void update( final IStatus connectionState )
  {
    if( this.isDisposed() )
      return;

    if( m_body != null && !m_body.isDisposed() )
    {
      m_body.dispose();
      m_body = null;
    }

    m_body = m_toolkit.createComposite( this );
    m_body.setLayout( new GridLayout() );
    m_body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, false ) );

    final ImageHyperlink img = m_toolkit.createImageHyperlink( m_body, SWT.RIGHT | SWT.BEGINNING );
    img.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    img.setUnderlined( false );

    if( connectionState != null && connectionState.getSeverity() == IStatus.OK )
    {
      img.setText( "Server Status: online" );
      img.setImage( IMG_SERVER_OK );

    }
    else if( connectionState.getSeverity() == IStatus.WARNING )
    {
      img.setText( "Server Status: aktualisiere" );
      img.setImage( IMG_SERVER_WAITING );
    }
    else
    {
      img.setText( "Server Status: offline" );
      img.setImage( IMG_SERVER_ERROR );
    }

    if( connectionState != null )
    {
      img.addHyperlinkListener( new HyperlinkAdapter()
      {
        /**
         * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
         */
        @Override
        public void linkActivated( final HyperlinkEvent e1 )
        {
          final StatusDialog dialog = new StatusDialog( img.getShell(), connectionState, "Verbindungsstatus" );
          dialog.open();
        }
      } );
    }

    m_toolkit.adapt( this );
    this.layout();
  }

  /**
   * @see org.kalypso.project.database.client.core.model.remote.IRemoteProjectsListener#remoteConnectionChanged(boolean)
   */
  @Override
  public void remoteConnectionChanged( final IStatus connectionState )
  {
    new UIJob( "" )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        update( connectionState );
        return Status.OK_STATUS;
      }
    }.schedule();
  }

  /**
   * @see org.kalypso.project.database.client.core.model.remote.IRemoteProjectsListener#remoteWorkspaceChanged()
   */
  @Override
  public void remoteWorkspaceChanged( )
  {
  }

}
