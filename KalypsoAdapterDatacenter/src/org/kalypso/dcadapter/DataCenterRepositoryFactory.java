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
   * The configuration string should be build in the following way:
   * 
   * <pre>
   * 
   *  url#username#password
   *  
   * </pre>
   * 
   * <p>
   * url: the url for the connection Example:
   * jdbc:edbc://LOCALHOST:II7/vnode::kalypso/INGRES
   * <p>
   * username: the name of the user under which the connection will be
   * established
   * <p>
   * password: the password for that user
   * 
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository( ) throws RepositoryException
  {
    final String[] conf = getConfiguration().split( "#" );
    
    if( conf.length < 3 )
      throw new RepositoryException( "Invalid configuration in " + getClass().getName() + ": " + getConfiguration() );

    final String url = conf[0];
    final String userName = conf[1];
    final String password = conf[2];

    return new DataCenterRepository( this, url, userName, password );
  }
}