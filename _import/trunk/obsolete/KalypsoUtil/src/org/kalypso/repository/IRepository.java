package org.kalypso.repository;


/**
 * Eingangspunkt zu einem Repository. Es liefert z.B. die ersten Items. Damit
 * kann eine Struktur aufgebaut werden.
 * 
 * @author schlienger
 */
public interface IRepository extends IRepositoryItem
{
  /**
   * Returns the location of the repository. In some cases this might not apply.
   * For instance the location of a File repository is the path of the root.
   */
  public String getLocation();

  /**
   * Returns true when this repository is in readonly mode. What this really means, 
   * depends on the client implementation. Some repositories might only be viewed or
   * browsed, while some others might be modified.
   */
  public boolean isReadOnly();
  
  /**
   * Finds the item that has the given id.
   * 
   * @throws RepositoryException
   *           if item could not be found
   */
  public IRepositoryItem findItem( final String id ) throws RepositoryException;

  /**
   * Forces the reload of the whole Repository structure.
   */
  public void reload() throws RepositoryException;

  public void addRepositoryListener( final IRepositoryListener l );

  public void removeRepositoryListener( final IRepositoryListener l );

  public void fireRepositoryStructureChanged();
}