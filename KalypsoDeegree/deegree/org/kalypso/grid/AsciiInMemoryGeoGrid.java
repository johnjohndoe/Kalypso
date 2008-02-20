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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.net.URL;

import org.apache.commons.io.IOUtils;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * A {@link IGeoGrid} implementation for the ESRI Ascii file format, which reads the whole file on instantiation and
 * holds all values in memory.
 * 
 * @author Dejan Antanaskovic
 */
public class AsciiInMemoryGeoGrid extends AbstractGeoGrid implements IGeoGrid
{
  private double[][] m_grid;

  private int m_sizeX;

  private int m_sizeY;

  private double m_min;

  private double m_max;

  public AsciiInMemoryGeoGrid( final URL asciiFileURL, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY ) throws IOException
  {
    super( origin, offsetX, offsetY );

    BufferedReader br = null;
    try
    {
      br = new BufferedReader( new InputStreamReader( asciiFileURL.openStream() ) );// new FileReader(
      // asciiFileURL.getFile() ) );
      final String[] data = new String[6];
      String line;
      // reading header data
      for( int i = 0; i < 6; i++ )
      {
        line = br.readLine();
        final int index = line.indexOf( " " ); //$NON-NLS-1$
        final String subString = line.substring( index );
        data[i] = subString.trim();
      }
      m_sizeX = Integer.parseInt( data[0] );
      m_sizeY = Integer.parseInt( data[1] );

      final double noDataValue = Double.parseDouble( data[5] );
      double currentValue;

      m_grid = new double[m_sizeY][m_sizeX];

      double min = Double.MAX_VALUE;
      double max = Double.MIN_VALUE;

      String[] strRow;
      for( int y = 0; y < m_sizeY; y++ )
      {
        strRow = br.readLine().trim().split( " " );
        for( int x = 0; x < m_sizeX; x++ )
        {
          currentValue = Double.parseDouble( strRow[x] );
          m_grid[y][x] = (currentValue != noDataValue) ? currentValue : Double.NaN;

          if( currentValue > max )
            max = currentValue;

          if( currentValue < min )
            min = currentValue;
        }
      }
      br.close();

      m_min = min;
      m_max = max;
    }
    finally
    {
      IOUtils.closeQuietly( br );
    }
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
    return m_grid[y][x];
  }

  /**
   * @see org.kalypso.gis.doubleraster.IDoubleGrid#dispose()
   */
  @Override
  public void dispose( )
  {
    // Get rid of grid value in order to free memory more quickly
    m_grid = null;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getMax()
   */
  public BigDecimal getMax( )
  {
    return new BigDecimal( m_max );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getMin()
   */
  public BigDecimal getMin( )
  {
    return new BigDecimal( m_min );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#setMax(java.math.BigDecimal)
   */
  public void setMax( BigDecimal maxValue ) throws GeoGridException
  {
    if( maxValue != null )
      m_max = maxValue.doubleValue();

  }

  /**
   * @see org.kalypso.grid.IGeoGrid#setMin(java.math.BigDecimal)
   */
  public void setMin( BigDecimal minValue ) throws GeoGridException
  {
    if( minValue != null )
      m_max = minValue.doubleValue();

  }
}
