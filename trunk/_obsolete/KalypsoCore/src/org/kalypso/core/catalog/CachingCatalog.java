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
package org.kalypso.core.catalog;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * This catalog implementation caches the results of its delegate.
 * 
 * @author Gernot Belger
 */
public class CachingCatalog implements ICatalog
{
  private final Map<CacheKey, String> m_cache = new HashMap<CacheKey, String>();

  private final ICatalog m_delegateCatalog;

  private long m_success = 0;

  private long m_failed = 0;

  private final static class CacheKey
  {
    private final String m_publicID;

    private final String m_systemID;

    private final boolean m_resolveContext;

    private final int m_hashCode;

    public CacheKey( final String publicID, final String systemID, final boolean resolveContext )
    {
      m_publicID = publicID;
      m_systemID = systemID;
      m_resolveContext = resolveContext;

      m_hashCode = new HashCodeBuilder().append( publicID ).append( systemID ).append( resolveContext ).toHashCode();
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode( )
    {
      return m_hashCode;
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object obj )
    {
      if( !(obj instanceof CacheKey) )
        return false;

      final CacheKey other = (CacheKey) obj;

      final EqualsBuilder builder = new EqualsBuilder().append( m_publicID, other.m_publicID ).append( m_systemID, other.m_systemID ).append( m_resolveContext, other.m_resolveContext );
      return builder.isEquals();
    }
  }

  public CachingCatalog( final ICatalog delegateCatalog )
  {
    m_delegateCatalog = delegateCatalog;
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#addEntry(java.lang.String, java.lang.String, java.lang.String)
   */
  public void addEntry( final String uri, final String systemID, final String publicID )
  {
    m_cache.clear();

    m_delegateCatalog.addEntry( uri, systemID, publicID );
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#addEntryRelative(java.lang.String, java.lang.String, java.lang.String)
   */
  public void addEntryRelative( final String uri, final String systemID, final String publicID )
  {
    m_cache.clear();

    m_delegateCatalog.addEntryRelative( uri, systemID, publicID );
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#addNextCatalog(java.net.URL)
   */
  public void addNextCatalog( final URL catalogURL )
  {
    m_cache.clear();

    m_delegateCatalog.addNextCatalog( catalogURL );
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#getEnryURNS(java.lang.String)
   */
  public List<String> getEntryURNS( final String urnPattern ) throws MalformedURLException, JAXBException
  {
    return m_delegateCatalog.getEntryURNS( urnPattern );
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#resolve(java.lang.String, java.lang.String, boolean)
   */
  public String resolve( final String systemID, final String publicID, final boolean resolveContext )
  {
    final CacheKey cacheKey = new CacheKey( publicID, systemID, resolveContext );
    final String result = resolveCached( cacheKey );
    if( result != null )
      return null;

    final String resolve = m_delegateCatalog.resolve( systemID, publicID, resolveContext );

    m_cache.put( cacheKey, resolve );

    return resolve;
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#resolve(java.lang.String, java.lang.String)
   */
  public String resolve( final String systemID, final String publicID )
  {
    final CacheKey cacheKey = new CacheKey( publicID, systemID, true );
    final String result = resolveCached( cacheKey );
    if( result != null )
      return null;

    final String resolve = m_delegateCatalog.resolve( systemID, publicID );

    m_cache.put( cacheKey, resolve );

    return resolve;
  }

  private String resolveCached( final CacheKey cacheKey )
  {
    if( m_cache.containsKey( cacheKey ) )
    {
      m_success++;
// if( m_success % 1000 == 0 )
// System.out.println( "Sucessfully cached another 1000 entries" );
      return m_cache.get( cacheKey );
    }

    m_failed++;
// if( m_failed % 1000 == 0 )
// System.out.println( "Failed for another 1000 entries" );
    return null;
  }
}
