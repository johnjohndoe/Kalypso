package org.kalypso.repository;

import java.util.List;
import java.util.NoSuchElementException;

/**
 * Allgemeine Interface für Repositories Container:
 * <p>- Liste von Repositories
 * <p>- Handlung von Listeners
 * 
 * @author schlienger
 */
public interface IRepositoryContainer
{
  /**
   * @return the list of repositories
   */
  public List getRepositories();

  /**
   * Tries to find the item within the repository list.
   * 
   * @param id
   * @return item if found
   * 
   * @throws NoSuchElementException when item could not be found. 
   */
  public IRepositoryItem findItem( final String id ) throws NoSuchElementException;
  
  /**
   * Adds a repository.
   * 
   * @param rep the one to add
   */
  public void addRepository( final IRepository rep );

  public void removeRepository( final IRepository rep );

  public void addRepositoryContainerListener( final IRepositoryContainerListener l );

  public void removeRepositoryContainerListener( final IRepositoryContainerListener l );
}