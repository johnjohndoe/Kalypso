package org.kalypso.project.database.client.core.model.remote;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.ws.WebServiceException;

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
  // 300000 = 5 min
  private static final int JOB_DELAY = 10000;

  protected Set<KalypsoProjectBean> m_beans = new TreeSet<KalypsoProjectBean>();

  protected Set<IRemoteWorkspaceListener> m_listener = new LinkedHashSet<IRemoteWorkspaceListener>();

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
          final KalypsoProjectBean[] beans = service.getAllProjectHeads();

          final Set<KalypsoProjectBean> myBeans = new TreeSet<KalypsoProjectBean>();
          for( final KalypsoProjectBean bean : beans )
          {
            myBeans.add( bean );
          }

          if( !myBeans.equals( m_beans ) )
          {
            m_beans = myBeans;

            for( final IRemoteWorkspaceListener listener : m_listener )
            {
              listener.remoteWorkspaceChanged();
            }
          }
        }
        catch( final WebServiceException e )
        {
          m_beans = new HashSet<KalypsoProjectBean>();

          for( final IRemoteWorkspaceListener listener : m_listener )
          {
            listener.remoteWorkspaceChanged();
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
    UPDATE_JOB.schedule( JOB_DELAY );
  }

  private void init( )
  {
    try
    {
      final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();

      final KalypsoProjectBean[] beans = service.getAllProjectHeads();
      for( final KalypsoProjectBean bean : beans )
      {
        m_beans.add( bean );
      }
    }
    catch( final WebServiceException e )
    {
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }

  }

  public KalypsoProjectBean[] getBeans( )
  {
    return m_beans.toArray( new KalypsoProjectBean[] {} );
  }

  public void addListener( final IRemoteWorkspaceListener listener )
  {
    m_listener.add( listener );
  }

  public void removeListener( final IRemoteWorkspaceListener listener )
  {
    m_listener.remove( listener );
  }

  public void dispose( )
  {
    m_listener = null;
    m_beans = null;
  }
}
