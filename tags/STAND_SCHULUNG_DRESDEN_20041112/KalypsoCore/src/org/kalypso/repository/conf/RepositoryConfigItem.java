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
  private final String m_className;

  private final String m_conf;

  private final boolean m_readOnly;

  private final static String SEPARATOR = ";";
  
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
   * Constructor with factory.
   * @param factory
   */
  public RepositoryConfigItem( final IRepositoryFactory factory )
  {
    this( factory.getClass().getName(), factory.getConfiguration(), factory.isReadOnly() );
  }

  /**
   * Creates the underlying factory.
   * @param cl
   * @return factory
   * @throws ClassUtilityException
   */
  public IRepositoryFactory createFactory( final ClassLoader cl ) throws ClassUtilityException
  {
    final IRepositoryFactory rf = (IRepositoryFactory)ClassUtilities.newInstance( m_className,
        IRepositoryFactory.class, cl );

    rf.setReadOnly( m_readOnly );

    rf.setConfiguration( m_conf );

    return rf;
  }

  /**
   * Saves the state of this object in a simple string representation.
   */
  public String saveState()
  {
    final StringBuffer bf = new StringBuffer();

    bf.append( m_className ).append( SEPARATOR ).append( m_conf ).append( SEPARATOR ).append( String.valueOf( m_readOnly ) );

    return bf.toString();
  }

  /**
   * Restores a <code>RepositoryConfigItem</code> from the state provided as a
   * string. This is the pendant to the saveState() method.
   * @param state
   * @return a repository config item
   */
  public static RepositoryConfigItem restore( final String state )
  {
    final String[] splits = state.split( SEPARATOR );
    
    if( splits.length != 3 )
      return null;
    
    return new RepositoryConfigItem( splits[0], splits[1], Boolean.valueOf( splits[2] ).booleanValue() );
  }
}