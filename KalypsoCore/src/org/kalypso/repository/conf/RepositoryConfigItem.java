package org.kalypso.repository.conf;

import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.repository.IRepositoryFactory;

/**
 * Using such an item you can create the <code>IRepositoryFactory</code> for
 * which it delivers the initial configuration.
 * 
 * @author schlienger
 */
public class RepositoryConfigItem
{
  private String m_className;

  private String m_conf;

  /**
   * Constructor with:
   * 
   * @param className
   *          name of the IRepositoryFactory class
   * @param conf
   *          configuration used when instanciating the factory class
   */
  public RepositoryConfigItem( final String className, final String conf )
  {
    m_className = className;
    m_conf = conf;
  }

  /**
   *  
   */
  public IRepositoryFactory createFactory() throws ClassUtilityException
  {
    return (IRepositoryFactory)ClassUtilities.newInstance( m_className, IRepositoryFactory.class,
        getClass().getClassLoader(), new Class[] { String.class }, new Object[] { m_conf } );
  }
}