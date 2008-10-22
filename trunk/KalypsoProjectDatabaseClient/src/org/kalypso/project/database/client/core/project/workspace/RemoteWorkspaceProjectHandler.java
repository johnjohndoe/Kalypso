package org.kalypso.project.database.client.core.project.workspace;

import java.lang.reflect.InvocationTargetException;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class RemoteWorkspaceProjectHandler implements ICoreRunnableWithProgress
{
  private final String[] m_remote;

  private KalypsoProjectBean[] m_beans;

  /**
   * @param natures
   *          handle these natures!
   */
  public RemoteWorkspaceProjectHandler( final String[] remote )
  {
    m_remote = remote;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
  {
    final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();

    final Set<KalypsoProjectBean> myBeans = new TreeSet<KalypsoProjectBean>();

    for( final String id : m_remote )
    {
      final KalypsoProjectBean[] beans = service.getProjectHeads( id );
      for( final KalypsoProjectBean bean : beans )
      {
        myBeans.add( bean );
      }
    }

    m_beans = myBeans.toArray( new KalypsoProjectBean[] {} );

    return Status.OK_STATUS;
  }

  public KalypsoProjectBean[] getBeans( )
  {
    return m_beans;
  }

}
