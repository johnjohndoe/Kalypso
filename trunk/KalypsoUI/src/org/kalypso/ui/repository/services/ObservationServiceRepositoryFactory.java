package org.kalypso.ui.repository.services;

import javax.xml.rpc.ServiceException;

import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;

/**
 * @author schlienger
 */
public class ObservationServiceRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * Does nothing
   * 
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository(  )
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
      return new ObservationServiceRepository( isReadOnly() );
    }
    catch( ServiceException e )
    {
      throw new RepositoryException( e );
    }
  }
}
