package org.kalypso.util.configuration.impl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import org.kalypso.util.configuration.IConfiguration;

/**
 * Default Implementation of the <code>IConfiguration</code> interface. The configuration
 * elements are read from a properties file.
 * 
 * @author schlienger
 */
public class ConfigurationImpl implements IConfiguration
{
  final Properties m_props;

  /**
   * Constructs the configuration with the given properties file.
   * 
   * @param configurationLocation location of the properties file
   */
  public ConfigurationImpl( final URL configurationLocation ) throws IOException
  {
    m_props = new Properties();
    InputStream stream = null;
    
    try
    {
      stream = configurationLocation.openStream();
      m_props.load( stream );
    }
    finally
    {
      if( stream != null )
        stream.close();
    }
  }

  /**
   * @see org.kalypso.util.configuration.IConfiguration#getConfiguration(java.lang.String)
   */
  public String getConfiguration( final String confParameter )
  {
    return m_props.getProperty( confParameter );
  }
}