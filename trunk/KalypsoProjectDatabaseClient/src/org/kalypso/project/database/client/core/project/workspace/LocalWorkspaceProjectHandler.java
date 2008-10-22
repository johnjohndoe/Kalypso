package org.kalypso.project.database.client.core.project.workspace;

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

  private IProject[] m_projects;

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
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final Set<IProject> myProjects = new LinkedHashSet<IProject>();
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    final IProject[] projects = root.getProjects();
    for( final IProject project : projects )
    {
      if( !project.isAccessible() || !project.isOpen() )
        continue;

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

    m_projects = myProjects.toArray( new IProject[] {} );

    return Status.OK_STATUS;
  }

  public IProject[] getProjects( )
  {
    return m_projects;
  }
}
