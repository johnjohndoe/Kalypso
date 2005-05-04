package org.kalypsodeegree_impl.gml.schema;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

/**
 * <p>
 * Singleton über dem {@link org.kalypsodeegree_impl.gml.schema.GMLSchemaCache}
 * welches sich darüberhinaus noch um den XML-Katalog kümmert.
 * </p>
 * <p>
 * Muss vor der ersten Benutzung durch {@link #init(Properties)}initialisiert
 * werden.
 * </p>
 * 
 * @author schlienger
 */
public final class GMLSchemaCatalog
{
  private static GMLSchemaCache THE_CACHE;

  private static Properties THE_CATALOG;

  private GMLSchemaCatalog( )
  {
    // wird nicht instantiiert
  }

  public synchronized static void init( final Properties catalog )
  {
    if( THE_CACHE != null )
      throw new IllegalStateException( "Schema Cache bereits initialisiert." );

    THE_CATALOG = catalog;
    THE_CACHE = new GMLSchemaCache();
  }

  /**
   * @throws MalformedURLException
   */
  private synchronized static URL getURL( final String schemaName )
      throws MalformedURLException
  {
    final String property = THE_CATALOG.getProperty( schemaName );
    return new URL( property );
  }

  /**
   * Lädt ein (eventuell gecachetes Schema direkt aus einer URL. Als CacheId
   * wird die URL benutzt.
   * 
   * @deprecated Es sollte eigentlich immer die namspace version benutzt werden.
   */
  public synchronized static GMLSchema getSchema( final URL schemaURL )
  {
    return getSchema( schemaURL.toString(), schemaURL );
  }

  /**
   * Lädt ein (eventuell gecachetes Schema über den Katalog. Als CacheId wird
   * dieser Name benutzt.
   */
  public synchronized static GMLSchema getSchema( final String namespace )
  {
    try
    {
      final URL schemaURL = getURL( namespace );
      return getSchema( namespace, schemaURL );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  private synchronized static GMLSchema getSchema( final String keyID,
      final URL schemaUrl )
  {
    if( THE_CACHE == null )
      throw new IllegalStateException(
          "Schema Cache wurde noch nicht initialisiert. Bitte erst init aufrufen!" );

    return THE_CACHE.getSchema( keyID, schemaUrl );
  }
}
