package org.kalypso.project.database.client.core.model.remote;

import java.util.LinkedHashSet;
import java.util.Set;

import javax.xml.ws.WebServiceException;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * Data model of remote projects in the {@link org.kalypso.project.database.server.ProjectDatabase}
 * 
 * @author Dirk Kuch
 */
public class RemoteWorkspaceModel
{
  // 300 000 = 5 min
  private static final int JOB_DELAY = 5000;

  protected KalypsoProjectBean[] m_beans = new KalypsoProjectBean[] {};

  protected IStatus m_connectionState = StatusUtilities.createInfoStatus( "aktualisiere" );

  protected Set<IRemoteProjectsListener> m_listener = new LinkedHashSet<IRemoteProjectsListener>();

  protected final WorkspaceJob UPDATE_JOB;

  public RemoteWorkspaceModel( )
  {
    init();

    UPDATE_JOB = new WorkspaceJob( "updating status of remote projects" )
    {
      @Override
      public IStatus runInWorkspace( final IProgressMonitor monitor )
      {
        try
        {
          final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();
          if( service == null )
          {
            return Status.CANCEL_STATUS;
          }

          final KalypsoProjectBean[] remote = service.getAllProjectHeads();
          if( m_connectionState == null || m_connectionState.getSeverity() != IStatus.OK )
          {
            m_connectionState = StatusUtilities.createOkStatus( "Verbindung OK" );
            fireConnectionStatusChanged();
          }

          if( remote.length != m_beans.length )
          {
            m_beans = remote;
            fireWorkspaceChanged();

            return Status.OK_STATUS;
          }
          else
          {
            for( final KalypsoProjectBean bean : remote )
            {
              if( !ArrayUtils.contains( m_beans, bean ) )
              {
                m_beans = remote;
                fireWorkspaceChanged();

                return Status.OK_STATUS;
              }
            }
          }

        }
        catch( final WebServiceException e )
        {
          if( m_connectionState == null || m_connectionState.getSeverity() == IStatus.OK )
          {
            m_connectionState = StatusUtilities.statusFromThrowable( e );
            fireConnectionStatusChanged();
          }

          if( m_beans.length != 0 )
          {
            m_beans = new KalypsoProjectBean[] {};
            fireWorkspaceChanged();
          }

          KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
        }

        return Status.OK_STATUS;
      }

    };
    UPDATE_JOB.addJobChangeListener( new JobChangeAdapter()
    {
      /**
       * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
       */
      @Override
      public void done( final IJobChangeEvent event )
      {

        UPDATE_JOB.schedule( JOB_DELAY );
      }
    } );

    UPDATE_JOB.schedule();
  }

  protected void fireWorkspaceChanged( )
  {
    for( final IRemoteProjectsListener listener : m_listener )
    {
      listener.remoteWorkspaceChanged();
    }
  }

  protected void fireConnectionStatusChanged( )
  {
    for( final IRemoteProjectsListener listener : m_listener )
    {
      listener.remoteConnectionChanged( m_connectionState );
    }
  }

  private void init( )
  {
    try
    {
      final IProjectDatabase service = KalypsoProjectDatabaseClient.getServiceUnblocking();
      if( service == null )
      {
        m_beans = new KalypsoProjectBean[] {};
      }
      else
      {
        m_beans = service.getAllProjectHeads();
      }
    }
    catch( final WebServiceException e )
    {
      m_beans = new KalypsoProjectBean[] {};
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }

  }

  public KalypsoProjectBean[] getBeans( )
  {
    return m_beans;
  }

  public void addListener( final IRemoteProjectsListener listener )
  {
    m_listener.add( listener );
  }

  public void removeListener( final IRemoteProjectsListener listener )
  {
    m_listener.remove( listener );
  }

  public void dispose( )
  {
    m_listener = null;
    m_beans = null;
  }

  public void setDirty( )
  {
    UPDATE_JOB.schedule();
  }

  public IStatus getRemoteConnectionState( )
  {
    return m_connectionState;
  }
}
