package org.kalypso.util.repository;

/**
 * A Configurator for a Repository.
 * 
 * @author schlienger
 *
 */
public interface IRepositoryConfig
{
  public void setIdentifier( String id );
  public String getIdentifier();
  
  public void setLocation( String loc );
  public String getLocation();
  
  public Object createControl( final Object argument );
}
