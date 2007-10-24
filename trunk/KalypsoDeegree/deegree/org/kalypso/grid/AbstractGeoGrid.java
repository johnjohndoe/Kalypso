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
package org.kalypso.grid;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Abstract (default) implementation of a {@link IGeoGrid}.
 * <p>
 * Essesntially holds the geocoordinates (origin, offset, bbox) and provides a primititve (i.e. non-optimized)
 * implementation of the walk-method.
 * </p>
 * 
 * @author Gernot Belger
 */
public abstract class AbstractGeoGrid implements IGeoGrid
{
  private final Coordinate m_offsetY;

  private final Coordinate m_origin;

  private final Coordinate m_offsetX;

  public AbstractGeoGrid( final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY )
  {
    m_origin = origin;
    m_offsetX = offsetX;
    m_offsetY = offsetY;
  }

  /**
   * Calculates the envelope of the whole grid.
   */
  public Envelope getBoundingBox( ) throws GeoGridException
  {
    return GeoGridUtilities.toEnvelope( this );
  }

  /**
   * @see org.kalypso.gis.doubleraster.IDoubleGeoGrid#getOrigin()
   */
  public Coordinate getOrigin( )
  {
    return m_origin;
  }

  /**
   * @see org.kalypso.gis.doubleraster.IDoubleGeoGrid#getOffsetX()
   */
  public Coordinate getOffsetX( )
  {
    return m_offsetX;
  }

  /**
   * @see org.kalypso.gis.doubleraster.IDoubleGeoGrid#getOffsetY()
   */
  public Coordinate getOffsetY( )
  {
    return m_offsetY;
  }

  /**
   * Wie getValue, gibt nur Double.NaN zurück für Punkte ausserhalb des Raster
   * 
   * @see IGeoGrid#getValueChecked(int, int)
   */
  public double getValueChecked( final int x, final int y ) throws GeoGridException
  {
    if( (x < 0) || (x >= getSizeX()) || (y < 0) || (y >= getSizeX()) )
      return Double.NaN;

    return getValue( x, y );
  }

  /**
   * @see org.kalypso.gis.doubleraster.IDoubleProvider#getValue(Coordinate)
   */
  public double getValue( final Coordinate crd ) throws GeoGridException
  {
    final GeoGridCell cell = GeoGridUtilities.cellFromPosition( this, crd );
    return getValue( cell );
  }

  private double getValue( final GeoGridCell cell ) throws GeoGridException
  {
    return getValue( cell.x, cell.y );
  }

  /**
   * Does nothing on default.
   * 
   * @see org.kalypso.gis.doubleraster.IDoubleGrid#dispose()
   */
  public void dispose( )
  {
  }

  /**
   * Overridden in order to make sure that dispose is called as soon as object is recollected.
   * 
   * @see java.lang.Object#finalize()
   */
  @Override
  protected void finalize( ) throws Throwable
  {
    dispose();

    super.finalize();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getWalkingStrategy()
   */
  public IGeoWalkingStrategy getWalkingStrategy( )
  {
    return new DefaultWalkingStrategy();
  }

}
