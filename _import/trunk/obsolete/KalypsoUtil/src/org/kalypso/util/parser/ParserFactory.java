package org.kalypso.util.parser;

import java.util.Properties;

import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;


/**
 * Eine Factory-Klasse was Instanzen von IParser anhand der
 * Parser-String-Spezifikation erzeugt.
 * 
 * @author schlienger
 */
public class ParserFactory
{
  private final ConfigurableCachableObjectFactory m_objFactory;
  
  /**
   * @param props beinhaltet der mapping type -- className für welche Objekte erzeugt werden
   * @param cl der ClassLoader der benutzt werden soll um die Klassen zu laden
   */
  public ParserFactory( final Properties props, final ClassLoader cl )
  {
    // cache not active because with use arguments when instanciating the parser
    m_objFactory = new ConfigurableCachableObjectFactory( props, false, cl );
  }

  /**
   * Erzeugt den gewünschten Parser anhand vom type und formatSpec
   * @throws FactoryException
   */
  public IParser createParser( final String type, final String formatSpec ) throws FactoryException
  {
    String[] args = null;
    
    if( formatSpec != null )
    {
      args = new String[1];
      args[0] = formatSpec;
    }
      
    return (IParser)m_objFactory.getObjectInstance( type, IParser.class, args );
  }
}
