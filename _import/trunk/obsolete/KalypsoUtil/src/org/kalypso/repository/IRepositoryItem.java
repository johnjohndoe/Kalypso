package org.kalypso.repository;

import org.kalypso.util.adapter.IAdaptable;

/**
 * Ein Item innerhalb des Repository. Die Items sind hierarchisch organisiert. 
 * 
 * @author schlienger
 */
public interface IRepositoryItem extends IAdaptable
{
  public String getName();
  
  /**
   * Returns a unique identifier.
   */
  public String getIdentifier();
  
  /** returns the parent item to which this one belongs */
  public IRepositoryItem getParent();
  
  /** returns true when this item has children */
  public boolean hasChildren();
  
  /** returns the children of this item */
  public IRepositoryItem[] getChildren();
  
  /** returns the repository into which this item exists */
  public IRepository getRepository();
}
