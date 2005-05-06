package org.kalypsodeegree_impl.gml.schema;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Logger;

import org.kalypso.java.net.IUrlCatalog;

/**
 * <p>
 * Singleton �ber dem {@link org.kalypsodeegree_impl.gml.schema.GMLSchemaCache}
 * welches sich dar�berhinaus noch um den XML-Katalog k�mmert.
 * </p>
 * <p>
 * Muss vor der ersten Benutzung durch {@link #init(IUrlCatalog, File)}initialisiert
 * werden.
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
   * @throws NullPointerException If catalog or cacheDirectory is null.
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
   * L�dt ein (eventuell gecachetes Schema direkt aus einer URL. Als CacheId
   * wird die URL benutzt.
   * 
   * @deprecated Zur Zeit deprecated, damit man erkennt, wo sich etwas ge�ndert hat. Kann aber normal benutzt werden.
   */
  public synchronized static GMLSchema getSchema( final URL schemaURL )
  {
    return getSchema( schemaURL.toString(), schemaURL );
  }

  /**
   * L�dt ein (eventuell gecachetes Schema �ber den Katalog. Als CacheId wird
   * dieser Name benutzt.
   */
  public synchronized static GMLSchema getSchema( final String namespace )
  {
    try
    {
      final URL schemaURL = THE_CATALOG.getURL( namespace );
      if( schemaURL == null )
      {
        LOGGER.warning( "Kein Schema-Eintrag f�r: " + namespace );
        return null;
      }
      
      // immer gegen die URL cachen, nie den namespace als id nehmen,
      // da sont beim wechseln des catalog die alten
      // schemata geladen werden.
      return getSchema( schemaURL );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  private synchronized static GMLSchema getSchema( final String keyID, final URL schemaUrl )
  {
    if( THE_CACHE == null )
      throw NOT_INITIALIZED;

    return THE_CACHE.getSchema( keyID, schemaUrl );
  }
}
