package org.kalypso.util.repository;

import org.kalypso.util.factory.FactoryException;

/**
 * 
 * 
 * @author schlienger
 */
public interface IRepositoryFactory
{
  public IRepository createRepository( String type ) throws FactoryException;
  
  public IRepositoryConfig createConfigurator( String type );
}
