package org.kalypso.util.pool;

import java.net.URL;
import java.util.Properties;

/**
 * TODO: doc!
 * 
 * @author schlienger
 */
public interface IPoolableObjectType
{
  /**
   * Returns the type of the source
   */
  public String getType();
  
  /**
   * Returns the location information of the source
   */
  public Properties getSource();
  
  public String getSourceAsString();
  
  /**
   * Returns the context from which the source is accessed
   */
  public URL getContext();
}
