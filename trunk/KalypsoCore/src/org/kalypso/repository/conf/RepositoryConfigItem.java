package org.kalypso.repository.conf;

import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepositoryFactory;

/**
 * Using such an item you can create the <code>IRepositoryFactory</code> for
 * which it delivers the initial configuration.
 * 
 * @author schlienger
 */
public class RepositoryConfigItem
{
  private final String m_className;

  private final String m_conf;

  private final boolean m_readOnly;

  /**
   * Constructor with:
   * 
   * @param className
   *          name of the IRepositoryFactory class
   * @param conf
   *          configuration used when instanciating the factory class
   * @param readOnly
   *          when true repository should be read only
   *  
   */
  public RepositoryConfigItem( final String className, final String conf, final boolean readOnly )
  {
    m_className = className;
    m_conf = conf;
    m_readOnly = readOnly;
  }

  /**
   *  
   */
  public IRepositoryFactory createFactory() throws ClassUtilityException
  {
    final IRepositoryFactory rf = (IRepositoryFactory)ClassUtilities.newInstance( m_className,
        IRepositoryFactory.class, getClass().getClassLoader() );

    rf.setReadOnly( m_readOnly );
    
    if( rf instanceof AbstractRepositoryFactory )
      ( (AbstractRepositoryFactory)rf ).setConfiguration( m_conf );
    
    return rf;
  }
}