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
    final String url = "jdbc:edbc://LOCALHOST:II7/vnode::db_kalypso/INGRES";
    final String userName = "ingres";
    final String password = "ingres";
    
    return new DataCenterRepository( this, url, userName, password );
  }
}
