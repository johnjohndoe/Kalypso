package org.kalypso.dcadapter;

import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;

/**
 * DataCenterRepositoryFactory
 * 
 * @author marc
 */
public class DataCenterRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository( ) throws RepositoryException
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository( ) throws RepositoryException
  {
    final String url = "";
    final String userName = "";
    final String password = "";
    
    return new DataCenterRepository( this, url, userName, password );
  }
}
