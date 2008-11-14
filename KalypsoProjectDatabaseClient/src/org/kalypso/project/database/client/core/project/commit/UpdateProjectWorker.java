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
package org.kalypso.project.database.client.core.project.commit;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManager;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.project.database.client.IProjectDataBaseClientConstant;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.project.export.ProjectExportHandler;
import org.kalypso.project.database.common.model.ProjectHandler;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.common.utils.ProjectModelUrlResolver;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class UpdateProjectWorker implements ICoreRunnableWithProgress
{
  private final ProjectHandler m_handler;

  public UpdateProjectWorker( final ProjectHandler handler )
  {
    m_handler = handler;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Aktualisiere Projekt", 4 );

    // remove local project lock
    final IRemoteProjectPreferences preferences = m_handler.getRemotePreferences();
    final String ticket = preferences.getEditTicket();
    preferences.setEditTicket( "" );

    final File urlTempDir = new File( System.getProperty( "java.io.tmpdir" ) );
    final File src = new File( urlTempDir, "update.zip" );

    monitor.worked( 1 );

    try
    {
      monitor.subTask( "Exportiere Projekt in lokales, tempor‰res Verzeichnis" );
      final ProjectExportHandler worker = new ProjectExportHandler( m_handler.getProject(), src );
      final IStatus status = worker.execute( monitor );
      monitor.worked( 1 );

      if( !status.isOK() )
        throw new CoreException( StatusUtilities.createErrorStatus( "Creating archive of project failed." ) );

      final FileSystemManager manager = VFSUtilities.getManager();
      final FileObject source = manager.resolveFile( src.getAbsolutePath() );

      final String urlDestination = ProjectModelUrlResolver.getUrlAsWebdav( new ProjectModelUrlResolver.IResolverInterface()
      {
        @Override
        public String getPath( )
        {
          return System.getProperty( IProjectDataBaseClientConstant.CLIENT_WRITEABLE_PATH );
        }

      }, "update.zip" );

      monitor.subTask( "‹bertrage Projekt auf Server" );
      final FileObject destination = manager.resolveFile( urlDestination );
      VFSUtilities.copy( source, destination );

      final URL myDestinationUrl = ProjectModelUrlResolver.getUrlAsHttp( new ProjectModelUrlResolver.IResolverInterface()
      {
        @Override
        public String getPath( )
        {
          return System.getProperty( IProjectDataBaseClientConstant.CLIENT_READABLE_PATH );
        }

      }, "update.zip" );

      final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();
      final KalypsoProjectBean bean = service.udpateProject( m_handler.getBean(), myDestinationUrl );
      preferences.setVersion( bean.getProjectVersion() );

      destination.close();
      destination.delete();
    }
    catch( final FileSystemException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    catch( final IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      // add local project lock
      preferences.setEditTicket( ticket );
      src.delete();

      monitor.done();
    }

    return Status.OK_STATUS;
  }
}
