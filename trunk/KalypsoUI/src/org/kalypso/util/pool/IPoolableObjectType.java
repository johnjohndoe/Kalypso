package org.kalypso.util.pool;

import java.util.Properties;

import org.eclipse.core.resources.IProject;

/**
 * @author schlienger
 *
 */
public interface IPoolableObjectType
{
  public String getType();
  
  public Properties getSource();
  
  public String getSourceAsString();
  
  public IProject getProject();
}
