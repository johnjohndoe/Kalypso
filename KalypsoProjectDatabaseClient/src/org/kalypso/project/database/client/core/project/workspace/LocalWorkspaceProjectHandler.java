package org.kalypso.project.database.client.core.project.workspace;

import java.lang.reflect.InvocationTargetException;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;

/**
 * @author Dirk Kuch
 */
public class LocalWorkspaceProjectHandler implements ICoreRunnableWithProgress
{

  private final String[] m_natures;

  /**
   * @param natures
   *          handle these natures!
   */
  public LocalWorkspaceProjectHandler( final String[] natures )
  {
    m_natures = natures;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
  {
    final Set<IProject> myProjects = new LinkedHashSet<IProject>();
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    final IProject[] projects = root.getProjects();
    for( final IProject project : projects )
    {
      for( final String natureId : m_natures )
      {
        final IProjectNature nature = project.getNature( natureId );
        if( nature != null )
        {
          myProjects.add( project );
          break;
        }
      }
    }

    return Status.OK_STATUS;
  }
}
// private ProjectWrapper[] m_projects;
//
// @Override
// public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException,
// InterruptedException
// {
// try
// {
// final IProject[] localProjects = getWorkspaceProjects();
//
// final RemoteProjectWorker remote = new RemoteProjectWorker(
// PlanerClientConstants.PLANER_CLIENT_MODEL_DATA_SERVICE_PROJECT_ID );
// remote.execute( monitor );
// final KalypsoProjectBean[] remoteProjects = remote.getProjects();
//
// m_projects = mergeProjects( localProjects, remoteProjects );
//
// return Status.OK_STATUS;
// }
// catch( final Exception e )
// {
// throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
// }
// }
//
// public ProjectWrapper[] getProjects( )
// {
// return m_projects;
// }
//
// private ProjectWrapper[] mergeProjects( final IProject[] localProjects, final KalypsoProjectBean[] remoteProjects )
// {
//
// final Map<String, ProjectWrapper> projects = new TreeMap<String, ProjectWrapper>();
//
// /* local projects overwrites remote projects! */
// for( final KalypsoProjectBean bean : remoteProjects )
// {
// final String name = bean.getName();
// final ProjectWrapper handler = new ProjectWrapper( bean );
//
// projects.put( name, handler );
// }
//
// for( final IProject local : localProjects )
// {
// final String name = local.getName();
// final ProjectWrapper handler = new ProjectWrapper( local );
//
// projects.put( name, handler );
// }
//
// return projects.values().toArray( new ProjectWrapper[] {} );
// }
//
// private IProject[] getWorkspaceProjects( )
// {
// final Set<IProject> myProjects = new LinkedHashSet<IProject>();
//
// final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
// final IProject[] projects = root.getProjects();
//
// for( final IProject project : projects )
// {
// try
// {
// final IProjectNature nature = project.getNature( PlanerClientProjectNature.NATURE_ID );
// if( nature != null )
// myProjects.add( project );
// }
// catch( final CoreException e )
// {
// PlanerClientBase.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
// }
// }
//
// // TODO sort
// return myProjects.toArray( new IProject[] {} );
//
// }

