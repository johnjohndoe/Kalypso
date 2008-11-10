package org.kalypso.project.database.client.core.model;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.local.ILocalProject;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.common.nature.RemoteProjectNature;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

public class ProjectHandler implements Comparable<ProjectHandler>
{
  private KalypsoProjectBean m_bean = null;

  private ILocalProject m_local = null;

  public boolean isLocal( )
  {
    if( m_local != null )
      return true;

    return false;
  }

  public boolean isRemote( )
  {
    if( m_bean != null )
      return true;

    return false;
  }

  public KalypsoProjectBean getBean( )
  {
    Assert.isNotNull( m_bean );

    return m_bean;
  }

  public IProject getProject( )
  {
    Assert.isNotNull( m_local );

    return m_local.getProject();
  }

  public void setProject( final ILocalProject project )
  {
    m_local = project;
  }

  public void setBean( final KalypsoProjectBean bean )
  {
    m_bean = bean;
  }

  public String getName( )
  {
    if( isRemote() )
      return m_bean.getName();
    else
    {
      try
      {
        final IProjectDescription description = getProject().getDescription();

        return description.getName();
      }
      catch( final CoreException e )
      {
        KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );

        return getProject().getName();
      }
    }
  }

  public String getUnixName( )
  {
    if( isRemote() )
      return m_bean.getUnixName();
    else
      return getProject().getName();
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final ProjectHandler o )
  {
    return getName().compareTo( o.getName() );
  }

  public boolean isLocalRemoteProject( )
  {
    if( m_bean != null && m_local != null )
      return true;

    // server offline? local project has "local"remote nature?
    if( isLocal() )
    {
      try
      {
        final IProjectNature nature = getProject().getNature( RemoteProjectNature.NATURE_ID );
        if( nature == null )
          return false;

        return true;
      }
      catch( final CoreException e )
      {
        return false;
      }
    }

    return false;
  }

  public IRemoteProjectPreferences getRemotePreferences( ) throws CoreException
  {
    return m_local.getRemotePreferences();
  }

}
