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
  
  public IRepositoryItem getParent();
  
  public boolean hasChildren();
  
  public IRepositoryItem[] getChildren();
}
