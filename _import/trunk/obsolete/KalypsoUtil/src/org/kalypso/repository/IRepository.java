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
   * @return location
   */
  public String getLocation();

  /**
   * Returns true when this repository is in readonly mode. What this really means, 
   * depends on the client implementation. Some repositories might only be viewed or
   * browsed, while some others might be modified.
   * @return readonly flag
   */
  public boolean isReadOnly();
  
  /**
   * Finds the item that has the given id.
   * @param id
   * @return item
   * 
   * @throws RepositoryException
   *           if item could not be found
   */
  public IRepositoryItem findItem( final String id ) throws RepositoryException;

  /**
   * Sets a property for this repository. Properties can be used internally, by the
   * repository itself, or by its items.
   * @param name
   * @param value
   */
  public void setProperty( final String name, final String value );
  
  /**
   * Returns the value of the given property or null if not set yet.
   * @param name
   * @return property string
   */
  public String getProperty( final String name );
  
  /**
   * Returns the value of the given property, or defaultValue if no value is set yet.
   * @param name
   * @param defaultValue
   * @return property string
   */
  public String getProperty( final String name, final String defaultValue );

  /**
   * Returns the factory that created this repository.
   * @return factory
   */
  public IRepositoryFactory getFactory();
  
  /**
   * Forces the reload of the whole Repository structure.
   * @throws RepositoryException
   */
  public void reload() throws RepositoryException;

  public void addRepositoryListener( final IRepositoryListener l );

  public void removeRepositoryListener( final IRepositoryListener l );

  public void fireRepositoryStructureChanged();
}