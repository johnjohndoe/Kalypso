package org.kalypso.util.loader;

import java.util.Properties;

/**
 * @author schlienger
 *
 */
public interface ILoader
{
  public Object load( final Properties source, final Object helper ) throws LoaderException;
  
  public void save( final Properties source, Object data ) throws LoaderException;

  public String getDescription();
  
  public void setSource( final Properties source ) throws LoaderException;

  public Properties getSource();
  
  public Object createControl( final Object argument ) throws LoaderException;
}
