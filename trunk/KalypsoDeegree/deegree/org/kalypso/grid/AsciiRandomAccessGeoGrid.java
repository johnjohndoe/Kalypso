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

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Scanner;

import org.apache.commons.io.FileUtils;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * A {@link IGeoGrid} implementation for the ESRI Ascii file format, which does not read the file into memory but tries
 * to access values on demand.
 * <p>
 * In order to achieve this, the line start positions are read on instantiation (slow!).
 * </p>
 * If any value is accessed, the corresponding line is parsed from the file and the value is returned.
 * </p>
 * <p>
 * If a value of the same line is accessed, this is quick, as the last lines is always hold im memory.
 * </p>
 * TODO: still slow, how to improve performance? <br>
 * TODO: overide the walk method in order to make sure, that we iterate thorugh lines, not columns
 * 
 * @author Dejan Antanaskovic
 * @author Thomas Jung
 * @author Gernot Belger
 */
public class AsciiRandomAccessGeoGrid extends AbstractGeoGrid implements IGeoGrid
{
  /* Index of current row which is in memory */
  private int m_currentRow;

  /* Current row-values in memory */
  private final double[] m_rowData;

  /* Indices of row position in the asc file */
  private final long[] m_rowPositions;

  private final int m_sizeX;

  private final int m_sizeY;

  private final File m_ascTmpFile;

  private final RandomAccessFile m_randomAccessFile;

  private final double m_noDataValue;

  public AsciiRandomAccessGeoGrid( final URL asciiFileURL, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY ) throws IOException
  {
    super( origin, offsetX, offsetY );

    /* Copy file to local folder, as we are going to open it as random access file */
    m_ascTmpFile = File.createTempFile( "ascTmp", ".asc" );
    m_ascTmpFile.deleteOnExit();

    // TODO: maybe don't do it, if it is already a file
    FileUtils.copyURLToFile( asciiFileURL, m_ascTmpFile );

    /* open tmp-file as random-access */
    m_randomAccessFile = new RandomAccessFile( m_ascTmpFile, "r" );

    /* reading header data */
    final String[] data = new String[6];
    for( int i = 0; i < 6; i++ )
    {
      final String line = m_randomAccessFile.readLine();

      final int index = line.indexOf( " " ); //$NON-NLS-1$
      final String subString = line.substring( index );
      data[i] = subString.trim();
    }
    m_sizeX = Integer.parseInt( data[0] );
    m_sizeY = Integer.parseInt( data[1] );

    m_noDataValue = Double.parseDouble( data[5] );

    m_rowData = new double[m_sizeX];
    m_rowPositions = new long[m_sizeY];

    // read row-positions
    for( int i = 0; i < m_sizeY; i++ )
    {
      m_rowPositions[i] = m_randomAccessFile.getFilePointer();

      if( !seekLineEnd() )
        break;
    }
  }

  private boolean seekLineEnd( ) throws IOException
  {
    int c = -1;
    boolean eol = false;
    int count = 0;
    while( !eol )
    {
      switch( c = m_randomAccessFile.read() )
      {
        case -1:
        case '\n':
          eol = true;
          break;
        case '\r':
          eol = true;
          final long cur = m_randomAccessFile.getFilePointer();
          if( (m_randomAccessFile.read()) != '\n' )
          {
            m_randomAccessFile.seek( cur );
          }
          break;
        default:
          count++;
          break;
      }
    }

    if( (c == -1) && (count == 0) )
    {
      return false;
    }

    return true;
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
  public double getValue( final int x, final int y ) throws GeoGridException
  {
    /* If row is not cached yet, read it */
    if( y != m_currentRow )
    {
      try
      {
        readRow( y );
      }
      catch( final IOException ioe )
      {
        final String msg = String.format( "Could not access grid-value %d/%d", x, y );
        throw new GeoGridException( msg, ioe );
      }
    }

    return m_rowData[x];
  }

  private void readRow( final int y ) throws IOException
  {
    m_randomAccessFile.seek( m_rowPositions[y] );
    final String line = m_randomAccessFile.readLine();

    final Scanner scanner = new Scanner( line );

    for( int x = 0; x < m_sizeX; x++ )
    {
      final String next = scanner.next();
      final BigDecimal value = new BigDecimal( next );
      m_rowData[x] = value.equals( m_noDataValue ) ? Double.NaN : value.doubleValue();
    }

    m_currentRow = y;
  }

  /**
   * @see java.lang.Object#finalize()
   */
  @Override
  protected void finalize( ) throws Throwable
  {
    dispose();

    super.finalize();
  }

  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#dispose()
   */
  @Override
  public void dispose( )
  {
    try
    {
      m_randomAccessFile.close();
    }
    catch( final IOException e )
    {
      // We it this exception, it should rarely occur and as this file is nor more used should be no error for the user
      e.printStackTrace();
    }
    finally
    {
      m_ascTmpFile.delete();
    }
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getMax()
   */
  public BigDecimal getMax( )
  {
    return null;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getMin()
   */
  public BigDecimal getMin( )
  {
    return null;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#setMax(java.math.BigDecimal)
   */
  public void setMax( BigDecimal maxValue ) throws GeoGridException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.grid.IGeoGrid#setMin(java.math.BigDecimal)
   */
  public void setMin( BigDecimal minValue ) throws GeoGridException
  {
    // TODO Auto-generated method stub

  }
}
