package org.kalypso.psiadapter.repository;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.RepositoryException;

/**
 * @author schlienger
 *
 */
public class PSICompactRepositoryFactory implements IRepositoryFactory
{
  /**
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository(org.kalypso.util.repository.IRepository)
   */
  public boolean configureRepository( final IRepository rep )
  {
    // no specific configuration to perform here
    
    return true;
  }

  /**
   * @throws RepositoryException
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository() throws RepositoryException
  {
    return PSICompactFactory.getRepository();
  }
}
