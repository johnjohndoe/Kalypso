package org.kalypso.project.database.client.core.model;

import org.eclipse.core.resources.IProject;
import org.kalypso.afgui.extension.IProjectHandler;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.core.model.interfaces.IRemoteProject;

public abstract class AbstractProjectHandler implements Comparable<IProjectHandler>, IProjectHandler
{
  public boolean isLocal( )
  {
    return this instanceof ILocalProject;
  }

  public boolean isRemote( )
  {
    return this instanceof IRemoteProject;
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final IProjectHandler handler )
  {
    return getName().compareTo( handler.getName() );
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof IProjectHandler )
    {
      final IProjectHandler handler = (IProjectHandler) obj;
      return getUniqueName().equals( handler.getUniqueName() );
    }
    else if( obj instanceof IProject )
    {
      final IProject other = (IProject) obj;
      return getUniqueName().equals( other.getName() );
    }

    return super.equals( obj );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return getUniqueName().hashCode();
  }

}
