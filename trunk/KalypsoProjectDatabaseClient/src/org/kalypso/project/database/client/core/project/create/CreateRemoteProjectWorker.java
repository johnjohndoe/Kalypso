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
package org.kalypso.project.database.client.core.project.create;

import java.io.File;
import java.net.URL;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.project.database.client.IProjectDataBaseClientConstant;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.core.project.export.ProjectExportHandler;
import org.kalypso.project.database.client.i18n.Messages;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.common.nature.RemoteProjectNature;
import org.kalypso.project.database.common.utils.ProjectModelUrlResolver;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class CreateRemoteProjectWorker implements ICoreRunnableWithProgress
{

  private final ILocalProject m_handler;

  public CreateRemoteProjectWorker( final ILocalProject handler )
  {
    m_handler = handler;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final File urlTempDir = new File( System.getProperty( "java.io.tmpdir" ) ); //$NON-NLS-1$
    final File src = new File( urlTempDir, "update.zip" ); //$NON-NLS-1$

    try
    {
      final IProject project = m_handler.getProject();
      final ProjectExportHandler worker = new ProjectExportHandler( project, src );
      final IStatus status = worker.execute( monitor );

      if( !status.isOK() )
      {
        throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString("org.kalypso.project.database.client.core.project.create.CreateRemoteProjectWorker.2") ) ); //$NON-NLS-1$
      }

      final FileSystemManager manager = VFSUtilities.getManager();
      final FileObject source = manager.resolveFile( src.getAbsolutePath() );

      final String urlDestination = ProjectModelUrlResolver.getUrlAsWebdav( new ProjectModelUrlResolver.IResolverInterface()
      {
        @Override
        public String getPath( )
        {
          return System.getProperty( IProjectDataBaseClientConstant.CLIENT_WRITEABLE_PATH );
        }

      }, "update.zip" ); //$NON-NLS-1$

      final FileObject destination = manager.resolveFile( urlDestination );
      VFSUtilities.copy( source, destination );

      final URL myDestinationUrl = ProjectModelUrlResolver.getUrlAsHttp( new ProjectModelUrlResolver.IResolverInterface()
      {
        @Override
        public String getPath( )
        {
          return System.getProperty( IProjectDataBaseClientConstant.CLIENT_READABLE_PATH );
        }

      }, "update.zip" ); //$NON-NLS-1$

      final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();
      final IProjectNature nature = project.getNature( RemoteProjectNature.NATURE_ID );
      if( nature instanceof RemoteProjectNature )
      {
        final RemoteProjectNature remote = (RemoteProjectNature) nature;
        final IRemoteProjectPreferences preferences = remote.getRemotePreferences( project, null );

        final KalypsoProjectBean bean = new KalypsoProjectBean();
        bean.setName( project.getName() );
        bean.setDescription( project.getName() );
        bean.setUnixName( project.getName() ); // TODO generate unixName
        bean.setProjectVersion( 0 );
        bean.setProjectType( preferences.getProjectType() );

        service.createProject( bean, myDestinationUrl );

        preferences.setVersion( 0 );
        preferences.setIsOnServer( true );
      }

      // bad @hack if the client has committed a large file, it can happen, that the client looses the http connection.
      // file.close() reestablish this http-connection
      destination.close();
      destination.delete();
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      src.delete();
    }

    return Status.OK_STATUS;
  }
}
