package org.kalypso.ui.repository.services;

import javax.xml.rpc.ServiceException;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.RepositoryException;

/**
 * @author schlienger
 */
public class ObservationServiceRepositoryFactory implements IRepositoryFactory
{
  /**
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository(org.kalypso.repository.IRepository)
   */
  public boolean configureRepository( final IRepository rep )
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository() throws RepositoryException
  {
    try
    {
      return new ObservationServiceRepository( );
    }
    catch( ServiceException e )
    {
      throw new RepositoryException( e );
    }
  }
}
