package org.kalypso.util.pool;

import java.util.HashMap;

/**
 * @author schlienger
 *
 */
public interface IPoolableObjectType
{
  public String getType();
  
  public HashMap getSource();
  
  public Object getHelper();
}
