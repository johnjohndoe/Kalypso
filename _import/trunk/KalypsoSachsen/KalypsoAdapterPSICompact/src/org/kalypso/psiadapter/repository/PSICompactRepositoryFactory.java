package org.kalypso.psiadapter.repository;

import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;

/**
 * @author schlienger
 *
 */
public class PSICompactRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * Does nothing.
   * 
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository( )
  {
    return true;
  }

  /**
   * @throws RepositoryException
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository() throws RepositoryException
  {
    return PSICompactFactory.getRepository( );
  }
}
