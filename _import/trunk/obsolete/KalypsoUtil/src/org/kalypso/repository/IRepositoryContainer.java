package org.kalypso.repository;

import java.util.List;

/**
 * Allgemeine Interface für Repositories Container:
 * <p>- Liste von Repositories
 * <p>- Handlung von Listeners
 * 
 * @author schlienger
 */
public interface IRepositoryContainer
{
  public List getRepositories();
  
  public void addRepository( final IRepository rep );

  public void removeRepository( final IRepository rep );

  public void addRepositoryContainerListener( final IRepositoryContainerListener l );

  public void removeRepositoryContainerListener( final IRepositoryContainerListener l );
}