package org.kalypso.util.repository;

import java.util.List;

/**
 * @author schlienger
 *
 */
public interface IRepositoryContainer
{
  public List getRepositories();
  
  public void addRepository( IRepository rep );
  public void removeRepository( IRepository rep );
  
  public void addRepositoryContainerListener( IRepositoryContainerListener l );
  public void removeRepositoryContainerListener( IRepositoryContainerListener l );
}
