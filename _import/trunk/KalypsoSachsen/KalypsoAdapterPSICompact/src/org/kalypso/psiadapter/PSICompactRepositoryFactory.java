package org.kalypso.psiadapter;

import org.kalypso.util.repository.IRepository;
import org.kalypso.util.repository.IRepositoryFactory;
import org.kalypso.util.repository.RepositoryException;

/**
 * @author schlienger
 *
 */
public class PSICompactRepositoryFactory implements IRepositoryFactory
{
  /**
   * @see org.kalypso.util.repository.IRepositoryFactory#configureRepository(org.kalypso.util.repository.IRepository)
   */
  public boolean configureRepository( final IRepository rep )
  {
    // no specific configuration to perform here
    
    return true;
  }

  /**
   * @throws RepositoryException
   * @see org.kalypso.util.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository() throws RepositoryException
  {
    return PSICompactFactory.getRepository();
  }
}
