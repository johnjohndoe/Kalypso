package org.kalypso.repository.virtual;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

/**
 * VirtualRepositoryItem
 * 
 * @author schlienger
 */
public class VirtualRepositoryItem implements IRepositoryItem
{

  /**
   * 
   */
  public VirtualRepositoryItem( )
  {
    super();
    // TODO Auto-generated constructor stub
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent( ) throws RepositoryException
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( ) throws RepositoryException
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class anotherClass )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
