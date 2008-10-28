package org.kalypso.project.database.client.core.model.local;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;

/**
 * @author Dirk Kuch
 */
public class LocalWorkspaceModel
{

  // TODO add IPreferenceChangeListener for nature preferences

  private final IResourceChangeListener RESOURCE_LISTENER = new IResourceChangeListener()
  {
    @Override
    public void resourceChanged( final IResourceChangeEvent event )
    {
      // TODO add newly created projects
      // TODO remove removed projects

      System.out.print( "TODO LocalWorkspaceListener" );
      final int adsfasfd = 0;
    }
  };

  /**
   * if m_natures == null -> handle (return) all local projects
   */
  private final Set<IProject> m_projects = new HashSet<IProject>();

  private final Set<ILocalWorkspaceListener> m_listener = new HashSet<ILocalWorkspaceListener>();

  public LocalWorkspaceModel( )
  {

  }

  public void dispose( )
  {
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.removeResourceChangeListener( RESOURCE_LISTENER );

    m_projects.clear();
    m_listener.clear();
  }

  public void init( )
  {
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.addResourceChangeListener( RESOURCE_LISTENER );

    final IWorkspaceRoot root = workspace.getRoot();

    final IProject[] projects = root.getProjects();
    for( final IProject project : projects )
    {
      if( !project.isAccessible() || !project.isOpen() )
        continue;

      m_projects.add( project );
    }
  }

  public void addListener( final ILocalWorkspaceListener listener )
  {
    m_listener.add( listener );
  }

  public void removeListener( final ILocalWorkspaceListener listener )
  {
    m_listener.remove( listener );
  }

  public IProject[] getProjects( )
  {
    return m_projects.toArray( new IProject[] {} );
  }
}
