package org.kalypso.util.pool;

import java.util.Properties;

/**
 * @author schlienger
 *
 */
public interface IPoolableObjectType
{
  public String getType();
  
  public Properties getSource();
  
  public Object getHelper();
}
