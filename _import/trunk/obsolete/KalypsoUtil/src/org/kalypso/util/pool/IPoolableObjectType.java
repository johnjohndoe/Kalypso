package org.kalypso.util.pool;

/**
 * @author schlienger
 *
 */
public interface IPoolableObjectType
{
  public String getType();
  
  public String getSource();
  
  public Object getHelper();
}
