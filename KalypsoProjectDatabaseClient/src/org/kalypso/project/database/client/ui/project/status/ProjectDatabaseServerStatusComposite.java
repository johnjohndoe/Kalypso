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

import javax.xml.ws.WebServiceException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.sei.IProjectDatabase;

/**
 * Composite which displays the current project model database server state
 * 
 * @author Dirk Kuch
 */
public class ProjectDatabaseServerStatusComposite extends Composite
{
  private static final int SCHEDULE_TIME = 1000;

  private final Image IMG_SERVER_OK = new Image( null, ProjectDatabaseServerStatusComposite.class.getResourceAsStream( "icons/server_okay.gif" ) );

  private final Image IMG_SERVER_ERROR = new Image( null, ProjectDatabaseServerStatusComposite.class.getResourceAsStream( "icons/server_error.gif" ) );

  private final FormToolkit m_toolkit;

  private Composite m_body;

  public ProjectDatabaseServerStatusComposite( final Composite parent, final FormToolkit toolkit )
  {
    super( parent, SWT.NONE );
    m_toolkit = toolkit;

    update();

    final UIJob job = new UIJob( "" )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        update();
        return Status.OK_STATUS;
      }
    };

    job.addJobChangeListener( new JobChangeAdapter()
    {
      /**
       * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
       */
      @Override
      public void done( final IJobChangeEvent event )
      {
        job.schedule( SCHEDULE_TIME );
      }
    } );

    job.schedule( SCHEDULE_TIME );
  }

  /**
   * @see org.eclipse.swt.widgets.Control#update()
   */
  @Override
  public void update( )
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

    try
    {
      final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();
      service.ping();

      final ImageHyperlink img = m_toolkit.createImageHyperlink( m_body, SWT.RIGHT );
      img.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
      img.setText( "Server Status: online" );
      img.setImage( IMG_SERVER_OK );
      img.setEnabled( false );
      img.setUnderlined( false );
    }
    catch( final WebServiceException e )
    {
      final ImageHyperlink img = m_toolkit.createImageHyperlink( m_body, SWT.RIGHT );
      img.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
      img.setText( "Server Status: offline" );
      img.setImage( IMG_SERVER_ERROR );
      img.setEnabled( false );
      img.setUnderlined( false );
    }

    m_toolkit.adapt( this );
    this.layout();
  }

}
