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

/**
 * A {@link IGeoGrid} implementation which is based on a delegated {@link GeoGridCache} and an arbitrary
 * {@link IGeoValueProvider}.
 * <p>
 * Each value of this grid is computed by subtracting the corresponding value of the {@link IGeoValueProvider} by the
 * value of the delegated grid.
 * 
 * @author Gernot Belger
 */
public class DoubleDiffDoubleRaster extends AbstractGeoGrid implements IGeoGrid
{
  private final IGeoGrid m_raster;

  private final IGeoValueProvider m_doubleProvider;

  public DoubleDiffDoubleRaster( final IGeoGrid raster, final IGeoValueProvider doubleProvider ) throws GeoGridException
  {
    super( raster.getOrigin(), raster.getOffsetX(), raster.getOffsetY() );

    m_raster = raster;
    m_doubleProvider = doubleProvider;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRaster#getValue(int, int)
   */
  public double getValue( final int x, final int y ) throws GeoGridException
  {
    final Coordinate c = GeoGridUtilities.toCoordinate( this, x, y, null );

    final double rasterValue = m_raster.getValue( x, y );

    final double providerValue = m_doubleProvider.getValue( c );

    return providerValue - rasterValue;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRaster#getValueChecked(int, int)
   */
  @Override
  public double getValueChecked( final int x, final int y ) throws GeoGridException
  {
    final Coordinate c = GeoGridUtilities.toCoordinate( this, x, y, null );

    final double rasterValue = m_raster.getValueChecked( x, y );

    final double providerValue = m_doubleProvider.getValue( c );

    return providerValue - rasterValue;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRaster#getSizeX()
   */
  public int getSizeX( ) throws GeoGridException
  {
    return m_raster.getSizeX();
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRaster#getSizeY()
   */
  public int getSizeY( ) throws GeoGridException
  {
    return m_raster.getSizeY();
  }
}
