package org.kalypso.dcadapter;

import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import com.bce.datacenter.db.IngresDatabase;

/**
 * DataCenterRepository
 * 
 * @author marc
 */
public class DataCenterRepository extends AbstractRepository
{
  private final IngresDatabase m_database;

  /**
   * @param fac
   * @param url
   * @param password
   * @param userName
   */
  public DataCenterRepository( final DataCenterRepositoryFactory fac, final String url, final String userName, final String password )
  {
    super( fac );
    
    m_database = new IngresDatabase( url, userName, password );
  }

  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( final String id ) throws RepositoryException
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public void reload( ) throws RepositoryException
  {
    // TODO Auto-generated method stub
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
}
