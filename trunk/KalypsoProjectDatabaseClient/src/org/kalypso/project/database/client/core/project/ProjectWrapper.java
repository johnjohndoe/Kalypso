package org.kalypso.project.database.client.core.project;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

public class ProjectWrapper
{
  private KalypsoProjectBean m_bean = null;

  private IProject m_local = null;

  public ProjectWrapper( final KalypsoProjectBean bean )
  {
    m_bean = bean;
  }

  public ProjectWrapper( final IProject local )
  {
    m_local = local;
  }

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

    return m_local;
  }

  public void setProject( final IProject project )
  {
    m_local = project;
  }

  public void setBean( final KalypsoProjectBean bean )
  {
    m_bean = bean;
  }
}
