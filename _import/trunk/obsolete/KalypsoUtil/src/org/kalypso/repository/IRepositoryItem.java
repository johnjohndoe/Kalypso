package org.kalypso.repository;

import org.kalypso.util.adapter.IAdaptable;

/**
 * Ein Item innerhalb des Repository. Die Items sind hierarchisch organisiert.
 * 
 * @author schlienger
 */
public interface IRepositoryItem extends IAdaptable
{
  /**
   * Returns the item's name. The name is used to display the item in the
   * repository for instance.
   */
  public String getName();

  /**
   * Returns a unique identifier for this item. The identifier should be build
   * using the following rule (URL oriented):
   * <p>
   * 
   * <pre>id(item) = rep-id:/item-id</pre>
   * 
   * <p>
   * Thus, the item's id is made of the id of the item's repository id plus its
   * own id. The identifier should be build according to the URL specification.
   */
  public String getIdentifier();

  /** returns the parent item to which this one belongs */
  public IRepositoryItem getParent() throws RepositoryException;

  /** returns true when this item has children */
  public boolean hasChildren() throws RepositoryException;

  /** returns the children of this item */
  public IRepositoryItem[] getChildren() throws RepositoryException;

  /** returns the repository into which this item exists */
  public IRepository getRepository();
}