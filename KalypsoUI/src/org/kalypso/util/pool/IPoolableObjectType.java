package org.kalypso.util.pool;

import java.net.URL;

/**
 * Identifies an object than is elligible to be pooled.
 * 
 * @author schlienger
 */
public interface IPoolableObjectType
{
  /**
   * @return the type of the source
   */
  public String getType( );

  /**
   * @return the location information of the source
   */
  public String getLocation( );

  /**
   * @return the context from which the source is accessed
   */
  public URL getContext( );

  /**
   * @return ignore exceptions flag. When true, exceptions are ignored during
   *         pooling process and object won't be pooled.
   */
  public boolean isIgnoreExceptions( );
}