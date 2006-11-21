/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.gml.schema;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.commons.cache.StringValidityFileCache;
import org.kalypso.commons.cache.StringValidityKey;
import org.kalypso.commons.serializer.ISerializer;
import org.shiftone.cache.Cache;
import org.shiftone.cache.policy.lfu.LfuCacheFactory;
import org.w3c.dom.Document;

/**
 * Cached GMLSchemata zweistufig. Zuerst wird das eigentliche Schema (aus einer URL) lokal in einem File-Cache
 * gespeichert und dann zusätzlich noch im Speicher gehalten.
 * 
 * @author schlienger
 */
public class GMLSchemaCache
{
  protected final static Logger LOGGER = Logger.getLogger( GMLSchemaCache.class.getName() );

  private final static int TIMEOUT = Integer.MAX_VALUE;

  private final static int SIZE = 30;

  private final Cache m_memCache;

  private final StringValidityFileCache m_fileCache;

  public GMLSchemaCache( final File cacheDirectory )
  {
    final LfuCacheFactory factory = new LfuCacheFactory();
    m_memCache = factory.newInstance( "gml.schemas", TIMEOUT, SIZE );

    m_fileCache = new StringValidityFileCache( new GMLSchemaSerializer(), cacheDirectory );
  }

  /**
   * Schreibt ein schema in diesen Cache.
   */
  public void addSchema( final String namespace, final GMLSchemaWrapper schemaWrapper )
  {
    m_memCache.addObject( namespace, schemaWrapper );
    m_fileCache.addObject( new StringValidityKey( namespace, schemaWrapper.getValidity() ), schemaWrapper.getSchema() );
  }

  /**
   * Lädt das Schmea aus dieser URL und nimmt diese id für den cache
   * 
   * @param namespace
   *          ID für den Cache, wenn null, wird die id anhand des geladenen schemas ermittelt
   */
  public GMLSchema getSchema( final String namespace, final URL schemaURL )
  {
    if( namespace == null )
      throw new NullPointerException( "Namespace darf nicht null sein" );

    Date validity = null;
    try
    {
      if( schemaURL != null )
      {
        final URLConnection connection = schemaURL.openConnection();
        connection.connect();
        validity = new Date( connection.getLastModified() );
      }
    }
    catch( final IOException e )
    {
      // ignorieren, dann immer die lokale Kopie nehmen
      // e.printStackTrace();
    }

    // if objekt already in memCache and is valid, just return it
    final GMLSchemaWrapper sw = (GMLSchemaWrapper)m_memCache.getObject( namespace );
    if( sw != null && ( validity == null || validity.compareTo( sw.getValidity() ) <= 0 ) )
      return sw.getSchema();

    // else, try to get it from file cache
    GMLSchema schema = null;

    final StringValidityKey key = new StringValidityKey( namespace, validity );
    final StringValidityKey realKey = m_fileCache.getRealKey( key );

    if( validity != null && ( realKey == null || realKey.getValidity().before( validity ) ) )
    {
      try
      {
        if( schemaURL != null )
          schema = new GMLSchema( schemaURL );
      }
      catch( final Exception e )
      {
        LOGGER.log( Level.WARNING, "Fehler beim Laden von Schema aus URL: " + schemaURL
            + "\nEs wird versucht die lokale Kopie zu laden.", e );
      }
      // als file speichern falls laden geklappt hat
      if( schema != null )
        m_fileCache.addObject( key, schema );
    }

    // falls noch valid oder laden hat nicht geklappt: aus dem File-Cache
    if( schema == null )
      schema = (GMLSchema)m_fileCache.get( key );

    if( schema != null )
      m_memCache.addObject( namespace, new GMLSchemaWrapper( schema, validity ) );

    return schema;
  }

  private static class GMLSchemaSerializer implements ISerializer
  {
    public Object read( final InputStream ins ) throws InvocationTargetException
    {
      try
      {
        return new GMLSchema( XMLHelper.getAsDOM( ins, true ), null );
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        throw new InvocationTargetException( e );
      }
    }

    public void write( final Object object, final OutputStream os ) throws InvocationTargetException
    {
      try
      {
        final Document document = ( (GMLSchema)object ).getXMLDocument();
        XMLHelper.writeDOM( document, null, os );
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        throw new InvocationTargetException( e );
      }
    }
  }

  public static class GMLSchemaWrapper
  {
    private final GMLSchema m_schema;

    private final Date m_validity;

    public GMLSchemaWrapper( final GMLSchema schema, final Date validity )
    {
      m_schema = schema;
      m_validity = validity;
    }

    public GMLSchema getSchema()
    {
      return m_schema;
    }

    public Date getValidity()
    {
      return m_validity;
    }
  }
}