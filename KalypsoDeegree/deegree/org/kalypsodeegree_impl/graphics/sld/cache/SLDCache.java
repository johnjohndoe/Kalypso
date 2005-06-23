package org.kalypsodeegree_impl.graphics.sld.cache;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Date;

import org.kalypso.commons.cache.StringValidityFileCache;
import org.kalypso.commons.cache.StringValidityKey;
import org.kalypso.commons.serializer.ISerializer;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.shiftone.cache.Cache;
import org.shiftone.cache.policy.lfu.LfuCacheFactory;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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

/**
 * 
 * @author kuepferle
 *  
 */

public class SLDCache
{
  private final static int TIMEOUT = Integer.MAX_VALUE;

  private final static int SIZE = 30;

  private final Cache m_memCache;

  private final StringValidityFileCache m_fileCache;

  public SLDCache( final File cacheDirectory )
  {
    final LfuCacheFactory factory = new LfuCacheFactory();
    m_memCache = factory.newInstance( "sld.discriptor", TIMEOUT, SIZE );

    m_fileCache = new StringValidityFileCache( new SLDSerializer(), cacheDirectory );
  }

  public StyledLayerDescriptor getSLD( String keyID, URL sldURL )
  {
    Date validity = null;
    try
    {
      final URLConnection connection = sldURL.openConnection();
      connection.connect();
      validity = new Date( connection.getLastModified() );
    }
    catch( final IOException e )
    {
      // ignorieren, dann immer die lokale Kopie nehmen
      // e.printStackTrace();
    }
    final SLDWrapper sw = (SLDWrapper)m_memCache.getObject( keyID );
    if( sw != null && ( validity == null || validity.before( sw.getValidity() ) ) )
      return sw.getSLD();

    // else, try to get it from file cache
    StyledLayerDescriptor sld = null;

    final StringValidityKey key = new StringValidityKey( keyID, validity );
    final StringValidityKey realKey = m_fileCache.getRealKey( key );

    if( validity != null && ( realKey == null || realKey.getValidity().before( validity ) ) )
    {
      InputStream is;
      try
      {
        is = sldURL.openStream();
        sld = SLDFactory.createSLD( new InputStreamReader( is ) );
      }
      catch( IOException e1 )
      {
        e1.printStackTrace();
      }
      catch( XMLParsingException e )
      {
        e.printStackTrace();
      }

      // als file speichern
      m_fileCache.addObject( key, sld );
    }
    else
      sld = (StyledLayerDescriptor)m_fileCache.get( key );

    if( sld != null )
      m_memCache.addObject( keyID, new SLDWrapper( sld, validity ) );

    return sld;
  }

  private static class SLDSerializer implements ISerializer
  {

    /**
     * @see org.kalypso.commons.serializer.ISerializer#read(java.io.InputStream)
     */
    public Object read( InputStream ins ) throws InvocationTargetException
    {

      try
      {
        return SLDFactory.createSLD( new InputStreamReader( ins ) );
      }
      catch( XMLParsingException e )
      {
        e.printStackTrace();
        throw new InvocationTargetException( e );
      }
    }

    /**
     * @see org.kalypso.commons.serializer.ISerializer#write(java.lang.Object, java.io.OutputStream)
     */
    public void write( Object object, OutputStream os ) throws InvocationTargetException
    {
      String sld = ( (StyledLayerDescriptor_Impl)object ).exportAsXML();
      BufferedWriter bw = new BufferedWriter( new OutputStreamWriter( os ) );
      try
      {
        bw.write( sld.toCharArray() );
      }
      catch( IOException e )
      {
        e.printStackTrace();
        throw new InvocationTargetException( e );
      }
    }

  }

  private static class SLDWrapper
  {
    private final StyledLayerDescriptor m_sld;

    private final Date m_validity;

    public SLDWrapper( final StyledLayerDescriptor sld, final Date validity )
    {
      m_sld = sld;
      m_validity = validity;
    }

    public StyledLayerDescriptor getSLD()
    {
      return m_sld;
    }

    public Date getValidity()
    {
      return m_validity;
    }
  }
}