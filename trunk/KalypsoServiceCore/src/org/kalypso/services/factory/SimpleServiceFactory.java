package org.kalypso.services.factory;

import java.io.IOException;
import java.util.Properties;

import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * <p>
 * Factory zum erzeugen von Services.
 * </p>
 * <p>
 * Erzeugt eigentlich nur Objekte via Standardkonstruktor über Reflektion
 * </p>
 * <p>
 * Das 'Name -> Service' Mapping kommt aus der Datei servicefactory.properties
 * </p>
 * 
 * @author belger, schlienger
 */
public class SimpleServiceFactory
{
  private static ConfigurableCachableObjectFactory m_factory = null;

  private static ConfigurableCachableObjectFactory getFactory() throws IOException
  {
    if( m_factory == null )
    {
      final Properties props = new Properties();
      props.load( SimpleServiceFactory.class.getResourceAsStream( "servicefactory.properties" ) );

      m_factory = new ConfigurableCachableObjectFactory( props, true, SimpleServiceFactory.class
          .getClassLoader() );
    }

    return m_factory;
  }

  /**
   * Returns the implementing class for the given service.
   * 
   * @throws FactoryException
   */
  public static Object createService( final String serviceName, final Class expected )
      throws FactoryException
  {
    try
    {
      return getFactory().getObjectInstance( serviceName, expected );
    }
    catch( IOException e )
    {
      throw new FactoryException( e );
    }
  }
}