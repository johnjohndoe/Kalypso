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
  private final static String SEPARATOR = "#";
  
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
    final String[] splits = getConfiguration().split( SEPARATOR );
    
    if( splits.length != 2 )
      throw new RepositoryException( "Configuration must contain location and identifier, separated by a " + SEPARATOR );
    
    return new VirtualRepository( this, splits[0], splits[1], isReadOnly() );
  }
}