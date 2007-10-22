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
package org.kalypso.grid;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * A cache for accessing the grid.
 * <p>
 * This cache is based on weak-references, and so quite unpredictible in its behaviour...
 * 
 * @author Gernot Belger
 */
public class GeoGridCache
{
  /**
   * Key class used for caching the grids.
   */
  private static class GridCacheKey
  {
    // REMARK: all fields appear to be unsused, but they are: via reflection in equals() and hashCode()

    @SuppressWarnings("unused")
    private final String m_mimeType;

    @SuppressWarnings("unused")
    private final URL m_url;

    @SuppressWarnings("unused")
    private final Coordinate m_origin;

    @SuppressWarnings("unused")
    private final Coordinate m_offsetX;

    @SuppressWarnings("unused")
    private final Coordinate m_offsetY;

    public GridCacheKey( final String mimeType, final URL url, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY )
    {
      m_mimeType = mimeType;
      m_url = url;
      m_origin = origin;
      m_offsetX = offsetX;
      m_offsetY = offsetY;
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object obj )
    {
      return EqualsBuilder.reflectionEquals( this, obj );
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode( )
    {
      return HashCodeBuilder.reflectionHashCode( this );
    }
  }

  private static final Map<GridCacheKey, WeakReference<IGeoGrid>> WEAK_CACHE = new HashMap<GridCacheKey, WeakReference<IGeoGrid>>();

  public static synchronized IGeoGrid getGrid( final String mimeType, final URL url, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY ) throws IOException
  {
    final GridCacheKey gridCacheKey = new GridCacheKey( mimeType, url, origin, offsetX, offsetY );

    final WeakReference<IGeoGrid> ref = WEAK_CACHE.get( gridCacheKey );
    if( ref != null )
    {
      final IGeoGrid cachedGrid = ref.get();
      if( cachedGrid != null )
        return cachedGrid;
    }

    // Grid not available, create it
    final IGeoGrid newGrid = GeoGridUtilities.createGrid( mimeType, url, origin, offsetX, offsetY );

    final WeakReference<IGeoGrid> newRef = new WeakReference<IGeoGrid>( newGrid );
    WEAK_CACHE.put( gridCacheKey, newRef );

    return newGrid;
  }

  public static void dump( )
  {
    for( final Map.Entry<GridCacheKey, WeakReference<IGeoGrid>> entry : WEAK_CACHE.entrySet() )
    {
      final IGeoGrid doubleGeoGrid = entry.getValue().get();
      System.out.print( doubleGeoGrid );
    }
  }
}
