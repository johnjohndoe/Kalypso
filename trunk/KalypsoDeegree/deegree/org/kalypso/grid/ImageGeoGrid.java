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

import java.awt.image.Raster;
import java.net.URL;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * A {@link IGeoGrid} implementations based on the JavaAdvancedImaging API (JAI).<br>
 * Provides a {@link IGeoGrid} for various image file formats (jpg, tiff, png, gif, ...).<br>
 * The underlying image must be a greyscale'd image whose greyvalues represent the grid values.
 * <p>
 * The values of this grid are accessed on demand, which results in the following behaviour: the first access is quite
 * slow (several seconds), but succeeding access is accpetable fast. However, no remarkable memory consumption is
 * noticed.
 * </p>
 * 
 * @author Dejan
 */
public class ImageGeoGrid extends AbstractGeoGrid implements IGeoGrid
{
  private final int m_sizeX;

  private final int m_sizeY;

  private RenderedOp m_image;

  public ImageGeoGrid( final URL imageURL, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY )
  {
    super( origin, offsetX, offsetY );

    m_image = JAI.create( "url", imageURL );

    m_sizeX = m_image.getWidth();
    m_sizeY = m_image.getHeight();
  }

  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#getSizeX()
   */
  public int getSizeX( )
  {
    return m_sizeX;
  }

  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#getSizeY()
   */
  public int getSizeY( )
  {
    return m_sizeY;
  }

  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#getValue(int, int)
   */
  public double getValue( final int x, final int y )
  {
    final int tileX = m_image.XToTileX( x );
    final int tileY = m_image.YToTileY( y );

    final Raster tile = m_image.getTile( tileX, tileY );

    return tile.getSampleDouble( x, y, 0 );
  }

  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_image != null )
      m_image.dispose();
    m_image = null;
  }

}
