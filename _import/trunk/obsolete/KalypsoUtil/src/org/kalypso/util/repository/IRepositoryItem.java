package org.kalypso.util.repository;

/**
 * Ein Item innerhalb des Repository. Die Items sind hierarchisch organisiert. 
 * 
 * @author schlienger
 */
public interface IRepositoryItem
{
  public String getName();
  
  public IRepositoryItem getParent();
  
  public boolean hasChildren();
  
  public IRepositoryItem[] getChildren();
}
