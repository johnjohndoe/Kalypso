/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Visits all grid cells that lie inside the given geometry.<br>
 * TODO: this is not the fastest way... better would be to test containement with the PointInRing stuff from JTS.
 * Implement a new LinearRingGeoGridArea to do this.
 * 
 * @author Holger Albert
 * @author Gernot Belger
 */
public class PolygonGeoGridArea implements IGeoGridArea
{
  private static GeometryFactory GF = new GeometryFactory();

  private final IGeoGrid m_grid;

  private final Geometry m_geom;

  private int m_yStart;

  private int m_yEnd;

  private int m_xStart;

  private int m_xEnd;

  private boolean m_hasInit = false;

  public PolygonGeoGridArea( final IGeoGrid grid, final Geometry geom )
  {
    m_grid = grid;
    m_geom = geom;
  }

  private void init( ) throws GeoGridException
  {
    if( m_hasInit )
      return;

    final Envelope envelope = m_geom.getEnvelopeInternal();
    final GeoGridCell minMinCell = GeoGridUtilities.cellFromPosition( m_grid, new Coordinate( envelope.getMinX(), envelope.getMinY() ) );
    final GeoGridCell maxMaxCell = GeoGridUtilities.cellFromPosition( m_grid, new Coordinate( envelope.getMaxX(), envelope.getMaxY() ) );

    m_yStart = Math.max( 0, Math.min( minMinCell.y, maxMaxCell.y ) );
    m_yEnd = Math.min( m_grid.getSizeY(), Math.max( minMinCell.y, maxMaxCell.y ) + 1 );
    m_xStart = Math.max( 0, Math.min( minMinCell.x, maxMaxCell.x ) );
    m_xEnd = Math.min( m_grid.getSizeX(), Math.max( minMinCell.x, maxMaxCell.x ) + 1 );

    m_hasInit = true;
  }

  /**
   * @see org.kalypso.grid.IGeoGridArea#contains(int, int, com.vividsolutions.jts.geom.Coordinate)
   */
  @Override
  public boolean contains( int x, int y, Coordinate coordinate ) throws GeoGridException
  {
    init();

    if( x > m_xEnd || x < m_xStart )
      return false;

    if( y > m_yEnd || y < m_yStart )
      return false;

    return m_geom.contains( GF.createPoint( coordinate ) );
  }

}
