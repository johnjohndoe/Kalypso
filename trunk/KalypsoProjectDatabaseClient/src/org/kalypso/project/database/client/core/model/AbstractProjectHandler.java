package org.kalypso.project.database.client.core.model;

import org.kalypso.afgui.extension.IProjectHandler;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.core.model.interfaces.IRemoteProject;

public abstract class AbstractProjectHandler implements Comparable<IProjectHandler>, IProjectHandler
{
// private final KalypsoProjectBean m_bean = null;

// private final ILocalProject m_local = null;

  public boolean isLocal( )
  {
    return this instanceof ILocalProject;
  }

  public boolean isRemote( )
  {
    return this instanceof IRemoteProject;
  }

// public KalypsoProjectBean getBean( )
// {
// Assert.isNotNull( m_bean );
//
// return m_bean;
// }
//
// public IProject getProject( )
// {
// Assert.isNotNull( m_local );
//
// return m_local.getProject();
// }
//
// public void setProject( final ILocalProject project )
// {
// m_local = project;
// }
//
// public void setBean( final KalypsoProjectBean bean )
// {
// m_bean = bean;
// }

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

// public boolean isLocalRemoteProject( )
// {
// if( m_bean != null && m_local != null )
// {
// return true;
// }
//
// // server offline? local project has "local"remote nature?
// if( isLocal() )
// {
// try
// {
// final IProjectNature nature = getProject().getNature( RemoteProjectNature.NATURE_ID );
// if( nature == null )
// {
// return false;
// }
//
// return true;
// }
// catch( final CoreException e )
// {
// return false;
// }
// }
//
// return false;
// }

// public IRemoteProjectPreferences getRemotePreferences( ) throws CoreException
// {
// return m_local.getRemotePreferences();
// }

}
