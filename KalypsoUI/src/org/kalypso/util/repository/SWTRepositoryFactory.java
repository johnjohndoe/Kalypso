package org.kalypso.util.repository;

import java.util.Properties;

import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.repository.file.view.SWTFileRepositoryConfig;

/**
 * Eine Factory für Repositories.
 * 
 * @author schlienger 
 */
public class SWTRepositoryFactory extends ConfigurableCachableObjectFactory implements IRepositoryFactory
{
  public SWTRepositoryFactory( Properties props )
  {
    super( props, true );
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryFactory#createRepository(java.lang.String)
   */
  public IRepository createRepository( String type ) throws FactoryException
  {
    return (IRepository)getObjectInstance( type, IRepository.class, getClass().getClassLoader() );
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryFactory#createConfigurator(java.lang.String)
   */
  public IRepositoryConfig createConfigurator( String type )
  {
    // TODO: z.Z. fest kodiert
    return new SWTFileRepositoryConfig(  );
  }
}
