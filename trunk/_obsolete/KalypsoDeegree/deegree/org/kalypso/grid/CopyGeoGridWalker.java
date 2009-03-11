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

import java.math.BigDecimal;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This {@link IGeoGridWalker} writes every visited value to a given {@link IWriteableGeoGrid}.
 * 
 * @author Gernot Belger
 */
public class CopyGeoGridWalker implements IGeoGridWalker
{
  private final IWriteableGeoGrid m_outputGrid;

  private BigDecimal m_min = new BigDecimal( Double.MAX_VALUE );

  private BigDecimal m_max = new BigDecimal( -Double.MAX_VALUE );

  public CopyGeoGridWalker( final IWriteableGeoGrid outputGrid )
  {
    m_outputGrid = outputGrid;
  }

  /**
   * @see org.kalypso.grid.IGeoGridWalker#start(org.kalypso.grid.IGeoGrid)
   */
  public void start( final IGeoGrid r )
  {
    // nothing to do
  }

  /**
   * @see org.kalypso.grid.IGeoGridWalker#operate(int, int, com.vividsolutions.jts.geom.Coordinate)
   */
  public void operate( final int x, final int y, final Coordinate c ) throws GeoGridException
  {
    m_outputGrid.setValue( x, y, c.z );

    if( Double.isNaN( c.z ) )
      return;

    final BigDecimal value = new BigDecimal( c.z ).setScale( 4, BigDecimal.ROUND_HALF_UP );
    m_min = m_min.min( value );
    m_max = m_max.max( value );

  }

  /**
   * @see org.kalypso.grid.IGeoGridWalker#finish()
   */
  public Object finish( )
  {
    // nothing to do
    return null;
  }

  public BigDecimal getMin( )
  {
    return m_min;
  }

  public BigDecimal getMax( )
  {
    return m_max;
  }
}
