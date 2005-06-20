package org.kalypso.wiskiadapter;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.factory.AbstractRepositoryFactory;

/**
 * WiskiRepositoryFactory
 * 
 * @author schlienger
 */
public class WiskiRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository() throws RepositoryException
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository() throws RepositoryException
  {
    return new WiskiRepository( "Wiski", getClass().getName(), getConfiguration(), false );
  }
}
