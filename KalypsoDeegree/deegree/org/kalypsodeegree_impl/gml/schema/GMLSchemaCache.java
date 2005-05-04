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

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Date;

import org.apache.commons.io.IOUtils;
import org.kalypso.util.cache.StringValidityFileCache;
import org.kalypso.util.cache.StringValidityKey;
import org.kalypso.util.serializer.ISerializer;
import org.shiftone.cache.Cache;
import org.shiftone.cache.policy.lfu.LfuCacheFactory;
import org.w3c.dom.Document;

/**
 * Cached GMLSchemata zweistufig. Zuerst wird das eigentliche Schema (aus einer
 * URL) lokal in einem File-Cache gespeichert und dann zusätzlich noch im
 * Speicher gehalten.
 * 
 * @author schlienger
 */
public class GMLSchemaCache
{
  private final static int TIMEOUT = 0;

  private final static int SIZE = 30;

  private final Cache m_memCache;

  private final StringValidityFileCache m_fileCache;

  public GMLSchemaCache( )
  {
    final LfuCacheFactory factory = new LfuCacheFactory();
    m_memCache = factory.newInstance( "gml.schemas", TIMEOUT, SIZE );
    m_fileCache = new StringValidityFileCache( new GMLSchemaSerializer(), null );
  }

  /** Lädt das Schmea aus dieser URL und nimmt diese id für den cache */
  public GMLSchema getSchema( final String keyID, final URL schemaURL )
  {
    Date validity = null;
    try
    {
      final URLConnection connection = schemaURL.openConnection();
      validity = new Date( connection.getLastModified() );
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      // ignorieren, dann immer die lokalte Kopie nehmen
    }

    final GMLSchemaWrapper sw = (GMLSchemaWrapper) m_memCache.getObject( keyID );

    if( sw == null || sw.getValidity().before( validity ) )
    {
      GMLSchema schema = null;

      final StringValidityKey key = new StringValidityKey( keyID, validity );
      final StringValidityKey realKey = m_fileCache.getRealKey( key );

      if( validity != null
          && (realKey == null || realKey.getValidity().before( validity )) )
      {
        // vom server holen
        schema = new GMLSchema( schemaURL );

        // als file speichern
        m_fileCache.addObject( key, schema );
      }
      else
        schema = (GMLSchema) m_fileCache.get( key );

      if( schema != null )
        m_memCache.addObject( key, new GMLSchemaWrapper( schema, validity ) );

      return schema;
    }
    else
      return sw.getSchema();
  }

  private static class GMLSchemaSerializer implements ISerializer
  {
    public Object read( final InputStream ins )
        throws InvocationTargetException
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

    public void write( final Object object, final OutputStream os )
        throws InvocationTargetException
    {
      Writer writer = null;
      try
      {
        writer = new OutputStreamWriter( new BufferedOutputStream( os ) );

        final Document document = ((GMLSchema) object).getXMLDocument();
        XMLHelper.writeDOM( document, null, writer );
        writer.close();
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        throw new InvocationTargetException( e );
      }
      finally
      {
        IOUtils.closeQuietly( writer );
      }
    }
  }

  private static class GMLSchemaWrapper
  {
    private final GMLSchema m_schema;

    private final Date m_validity;

    public GMLSchemaWrapper( final GMLSchema schema, final Date validity )
    {
      m_schema = schema;
      m_validity = validity;
    }

    public GMLSchema getSchema( )
    {
      return m_schema;
    }

    public Date getValidity( )
    {
      return m_validity;
    }
  }
}