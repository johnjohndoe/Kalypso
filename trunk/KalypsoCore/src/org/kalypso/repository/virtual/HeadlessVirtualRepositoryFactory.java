package org.kalypso.repository.virtual;

import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;

/**
 * VirtualRepositoryFactory
 * 
 * @author schlienger
 */
public class HeadlessVirtualRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository( ) throws RepositoryException
  {
    return true;
  }

  /**
   * Configuration string contains the location of the repository specification
   * file (xml)
   * 
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository( ) throws RepositoryException
  {
    return new VirtualRepository( this, getConfiguration(), isReadOnly() );
  }
}