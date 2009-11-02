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
package org.kalypso.model.wspm.core.profil.util;

import java.net.URI;
import java.net.URL;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.catalog.ICatalog;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.GmlSerializerFeatureProviderFactory;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.shiftone.cache.Cache;
import org.shiftone.cache.adaptor.CacheMap;
import org.shiftone.cache.decorator.miss.MissHandler;
import org.shiftone.cache.decorator.miss.MissHandlingCache;
import org.shiftone.cache.policy.lru.LruCacheFactory;

/**
 * @author Gernot Belger
 */
public class DictionaryCache
{
  private final int cacheSize = 20;

  private final IFeatureProviderFactory m_factory = new GmlSerializerFeatureProviderFactory();

  private final CacheMap m_cacheMap;

  public DictionaryCache( )
  {
    // REMARK: we give a long period, as we cannod void the registration of the reaper-timer
    final Cache cache = new LruCacheFactory().newInstance( "DictionaryCache" + System.currentTimeMillis(), 1000 * 60 * 5, cacheSize );

    final MissHandler missHandler = new MissHandler()
    {
      @Override
      public Object fetchObject( final Object key ) throws Exception
      {
        final String urn = (String) key;
        return loadWorkspace( urn );
      }
    };

    final MissHandlingCache missHandlingCache = new MissHandlingCache( cache, missHandler );
    m_cacheMap = new CacheMap( missHandlingCache );
  }

  protected Object loadWorkspace( final String urn ) throws Exception
  {
    final ICatalog baseCatalog = KalypsoCorePlugin.getDefault().getCatalogManager().getBaseCatalog();
    final String uri = baseCatalog.resolve( urn, urn );

    if( uri.startsWith( "urn:" ) )
    {
      // id was not found in catalog, what to do?
      throw new IllegalArgumentException( "Unknown dictionary: " + urn );
    }

      final URL url = new URI( uri ).toURL();
      return GmlSerializer.createGMLWorkspace( url, m_factory );
  }

  public GMLWorkspace get( final String urn )
  {
    return (GMLWorkspace) m_cacheMap.get( urn );
  }

}
