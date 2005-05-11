package org.kalypsodeegree_impl.gml.schema;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;
import java.util.logging.Logger;

import org.kalypso.java.net.IUrlCatalog;

/**
 * <p>
 * Singleton über dem {@link org.kalypsodeegree_impl.gml.schema.GMLSchemaCache}
 * welches sich darüberhinaus noch um den XML-Katalog kümmert.
 * </p>
 * <p>
 * Muss vor der ersten Benutzung durch {@link #init(IUrlCatalog, File)}
 * initialisiert werden.
 * </p>
 * 
 * @author schlienger
 */
public final class GMLSchemaCatalog
{
  private final static Logger LOGGER = Logger.getLogger( GMLSchemaCache.class.getName() );

  private static GMLSchemaCache THE_CACHE;

  private static final IllegalStateException NOT_INITIALIZED = new IllegalStateException(
      "Schema-Katalog nicht initialisiert. Zuerst 'init' aufrufen!" );

  private static IUrlCatalog THE_CATALOG;

  private GMLSchemaCatalog()
  {
  // wird nicht instantiiert
  }

  /**
   * Initializes the schema-cache. Empties it, if it already exists.
   * 
   * @throws NullPointerException
   *           If catalog or cacheDirectory is null.
   */
  public synchronized static void init( final IUrlCatalog catalog, final File cacheDirectory )
  {
    GMLSchemaCatalog.THE_CATALOG = catalog;

    if( catalog == null )
      throw new NullPointerException();

    THE_CACHE = new GMLSchemaCache( cacheDirectory );

    LOGGER.info( "Schema-Katalog initialisiert mit DIR=" + cacheDirectory );
  }

  /**
   * Lädt ein (eventuell gecachetes Schema über den Katalog. Als CacheId wird
   * dieser Name benutzt.
   */
  public synchronized static GMLSchema getSchema( final String namespace )
  {
    try
    {
      final URL schemaURL = THE_CATALOG.getURL( namespace );
      if( schemaURL == null )
      {
        LOGGER.warning( "Kein Schema-Eintrag für: " + namespace );
        return null;
      }

      return getSchema( namespace, schemaURL );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      return null;
    }
  }
  
  /**
   * Lädt ein Schema aus dieser URL (nicht aus dem Cache!) und fügt es dann dem cache
   * hinzu (mit namespace als key).
   * 
   * @return null, wenn schema nicht geladen werden konnte
   */
  public synchronized static GMLSchema getSchema( final URL schemaLocation )
  {
    if( THE_CACHE == null )
      throw NOT_INITIALIZED;

    try
    {
      final GMLSchema schema = new GMLSchema( schemaLocation );
      final Date validity = new Date( schemaLocation.openConnection().getLastModified() );
      THE_CACHE.addSchema( schema.getTargetNS(), new GMLSchemaCache.GMLSchemaWrapper( schema, validity ) );
      
      return schema;
    }
    catch( final Exception e )
    {
      LOGGER.warning( "Konnte Schema nicht aus URL laden: " + schemaLocation );
      
      return null;
    }
  }

  private synchronized static GMLSchema getSchema( final String namespace, final URL schemaUrl )
  {
    if( THE_CACHE == null )
      throw NOT_INITIALIZED;

    return THE_CACHE.getSchema( namespace, schemaUrl );
  }
}
