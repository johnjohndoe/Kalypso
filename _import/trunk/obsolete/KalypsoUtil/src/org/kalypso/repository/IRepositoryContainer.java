package org.kalypso.repository;

import java.util.List;

/**
 * Algemeine Interface für Repositories Container:
 * <p>- Liste von Repositories
 * <p>- Handlung von Listeners
 * 
 * @author schlienger
 */
public interface IRepositoryContainer
{
  public List getRepositories();

  public void addRepository( IRepository rep );

  public void removeRepository( IRepository rep );

  public void addRepositoryContainerListener( IRepositoryContainerListener l );

  public void removeRepositoryContainerListener( IRepositoryContainerListener l );
}