package org.kalypso.util.pool;

import java.net.URL;

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
  public String getLocation();
  
  /**
   * Returns the context from which the source is accessed
   */
  public URL getContext();
}
