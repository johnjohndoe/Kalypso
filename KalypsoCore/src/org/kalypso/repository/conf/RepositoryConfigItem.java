package org.kalypso.repository.conf;

import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.java.xml.DomHelper;
import org.kalypso.java.xml.DomHelper.DomLoadException;
import org.kalypso.repository.IRepositoryFactory;
import org.w3c.dom.Document;

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

  // tags used for state persistence
  private final static String TAG_ROOT = "RepositoryConfigItem";
  private final static String TAG_CLASSNAME = "classname";
  private final static String TAG_CONF = "conf";
  private final static String TAG_READONLY = "readonly";

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
   */
  public RepositoryConfigItem( final IRepositoryFactory factory )
  {
    this( factory.getClass().getName(), factory.getConfiguration(), factory.isReadOnly() );
  }

  /**
   * Creates the underlying factory.
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
   * Saves the state of this object in a XML-like string.
   */
  public String saveState()
  {
    final StringBuffer bf = new StringBuffer();

    bf.append( "<" ).append( TAG_ROOT ).append( ">" );
    
    bf.append( "<" ).append( TAG_CLASSNAME ).append( ">" );
    bf.append( m_className );
    bf.append( "</" ).append( TAG_CLASSNAME ).append( ">" );

    bf.append( "<" ).append( TAG_CONF ).append( ">" );
    bf.append( m_conf );
    bf.append( "</" ).append( TAG_CONF ).append( ">" );

    bf.append( "<" ).append( TAG_READONLY ).append( ">" );
    bf.append( String.valueOf( m_readOnly ) );
    bf.append( "</" ).append( TAG_READONLY ).append( ">" );

    bf.append( "</" ).append( TAG_ROOT ).append( ">\n" );
    
    return bf.toString();
  }

  /**
   * Restores a <code>RepositoryConfigItem</code> from the state provided as a
   * string. This is the pendant to the saveState() method.
   * 
   * @throws DomLoadException when error occured during parsing of state
   */
  public static RepositoryConfigItem restore( final String state ) throws DomLoadException
  {
    Document doc = DomHelper.loadDocument( state );
    
    final String className = doc.getElementsByTagName( TAG_CLASSNAME ).item(0).getFirstChild().getNodeValue();
    final String conf = doc.getElementsByTagName( TAG_CONF ).item(0).getFirstChild().getNodeValue();
    final boolean ro = Boolean.valueOf( doc.getElementsByTagName( TAG_READONLY ).item(0).getFirstChild().getNodeValue() ).booleanValue();
    
    return new RepositoryConfigItem( className, conf, ro );
  }
}