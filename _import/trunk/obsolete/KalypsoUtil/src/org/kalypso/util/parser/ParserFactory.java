package org.kalypso.util.parser;

import java.util.Date;
import java.util.Properties;

import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.parser.impl.DateParser;
import org.kalypso.util.parser.impl.DoubleParser;
import org.kalypso.util.parser.impl.IntegerParser;
import org.kalypso.util.parser.impl.StringParser;


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
   * @param props beinhaltet der mapping type -- className f�r welche Objekte erzeugt werden
   * @param cl der ClassLoader der benutzt werden soll um die Klassen zu laden
   */
  public ParserFactory( final Properties props, final ClassLoader cl )
  {
    // cache not active because with use arguments when instanciating the parser
    m_objFactory = new ConfigurableCachableObjectFactory( props, false, cl );
  }

  /**
   * Erzeugt den gew�nschten Parser anhand vom type und formatSpec
   * 
   * @param type
   * @param formatSpec
   * @return parser
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
 
  /**
   * @param dataClass
   * @return adequate parser for the given dataclass
   */
  public static IParser createParser( final Class dataClass )
  {
    if( Date.class.isAssignableFrom( dataClass ) )
      return new DateParser();

    if( Integer.class.isAssignableFrom( dataClass ) )
      return new IntegerParser();
    
    if( Double.class.isAssignableFrom( dataClass ) )
      return new DoubleParser();
    
    return new StringParser();
  }
}
