package org.kalypso.util.configuration;


/**
 * Allows to aks for configuration elements.
 * 
 * @author schlienger
 */
public interface IConfiguration
{
  /**
   * Returns the configuration for the given configuration parameter.
   */
  public String getConfiguration( String confParameter );
}