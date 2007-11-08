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
import java.math.BigInteger;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypsodeegree.model.geometry.ByteUtils;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.util.Assert;

/**
 * A {@link IGeoGrid} implementation based on the Kalypso Binary-Grid-Format.
 * <p>
 * The grid is accessed on demand, so the memory consumption of this implementation is very low.
 * </p>
 * <p>
 * Use one of the tow factory methods {@link #openGrid(URL, Coordinate, Coordinate, Coordinate)} ord
 * {@link #createGrid(File, int, int, int, Coordinate, Coordinate, Coordinate)} to instantiate this class.
 * </p>
 * <p>
 * Format description:
 * </p>
 * 
 * <pre>
 *  Version:        Version Number (Currently 0)
 *  SizeX:          Grid Size in horizontal direction
 *  SizeY:          Grid Size in vertical direction
 *  Scale:          Number of fraction digits of the cell values
 *  /Cell-Values/:  SizeX times SizeY cell values.
 *  Min:            Minimum value
 *  Max:            Maximum value
 * </pre>
 * 
 * <p>
 * All values are encoded as lower endian integers (4 bytes).
 * </p>
 * 
 * @author Dejan Antanaskovic
 * @author Thomas Jung
 * @author Gernot Belger
 */
public class BinaryGeoGrid extends AbstractGeoGrid implements IWriteableGeoGrid
{
  private final static int HEADER_SIZE = 4 * 4;

  private static final int NO_DATA = Integer.MIN_VALUE;

  /* Buffer for encoding read/written integers. */
  private final byte[] m_intBuffer = new byte[4];

  private RandomAccessFile m_randomAccessFile;

  /* If set, this file will be deleted on dispose. Used if grid is hold in temporary file. */
  private final File m_binFile;

  private final int m_sizeX;

  private final int m_sizeY;

  private final int m_scale;

  private BigDecimal m_min;

  private BigDecimal m_max;

  /**
   * Opens an exsiting grid for read-only access.<br>
   * Dispose the grid after it is no more needed in order to release the given resource.
   */
  public static BinaryGeoGrid openGrid( final URL url, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY ) throws IOException
  {
    /* Tries to find a file from the given url. */
    File fileFromUrl = ResourceUtilities.findJavaFileFromURL( url );
    File binFile = null;
    if( fileFromUrl == null )
      fileFromUrl = FileUtils.toFile( url );

    if( fileFromUrl == null )
    {
      /*
       * If url cannot be converted to a file, write its contents to a temporary file which will be deleted after the
       * grid gets disposed.
       */
      fileFromUrl = File.createTempFile( "local", ".bin" );
      fileFromUrl.deleteOnExit();
      FileUtils.copyURLToFile( url, fileFromUrl );
      binFile = fileFromUrl; // set in order ot delete on dispose
    }

    final RandomAccessFile randomAccessFile = new RandomAccessFile( fileFromUrl, "r" );
    return new BinaryGeoGrid( randomAccessFile, binFile, origin, offsetX, offsetY );
  }

  /**
   * Crates a new grid file with the given size and scale.<br>
   * The grid is then opened in write mode, so its values can then be set.<br>
   * The grid must be disposed afterwards in order to flush the written information.
   */
  public static BinaryGeoGrid createGrid( final File file, final int sizeX, final int sizeY, final int scale, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY ) throws IOException
  {
    final RandomAccessFile randomAccessFile = new RandomAccessFile( file, "rw" );
    return new BinaryGeoGrid( randomAccessFile, sizeX, sizeY, scale, origin, offsetX, offsetY );
  }

  /**
   * @param binFile
   *            If set, this file will be deleted on dispose
   */
  private BinaryGeoGrid( final RandomAccessFile randomAccessFile, final File binFile, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY ) throws IOException
  {
    super( origin, offsetX, offsetY );

    m_binFile = binFile;

    m_randomAccessFile = randomAccessFile;

    /* Read header */
    m_randomAccessFile.seek( 0 );
    final int version = readInt();
    Assert.isTrue( version == 0, "Unknwon binary file format version: " + version );

    m_sizeX = readInt();
    m_sizeY = readInt();
    m_scale = readInt();

    /* Read statistical data */
    getStatistically();

  }

  public BinaryGeoGrid( final RandomAccessFile randomAccessFile, final int sizeX, final int sizeY, final int scale, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY ) throws IOException
  {
    super( origin, offsetX, offsetY );

    m_randomAccessFile = randomAccessFile;
    m_binFile = null;

    m_sizeX = sizeX;
    m_sizeY = sizeY;
    m_scale = scale;
    m_min = BigDecimal.valueOf( Double.MAX_VALUE );
    m_max = BigDecimal.valueOf( Double.MIN_VALUE );

    /* Initialize grid */
    m_randomAccessFile.setLength( HEADER_SIZE + sizeX * sizeY * 4 + 2 * 4 );

    /* Read header */
    m_randomAccessFile.seek( 0 );

    writeInt( 0 ); // Version number
    writeInt( sizeX );
    writeInt( sizeY );
    writeInt( scale );

    /* Set everything to non-data */
    for( int y = 0; y < sizeY; y++ )
    {
      for( int x = 0; x < sizeX; x++ )
        writeInt( NO_DATA );
    }

    /* Read statistical data */

    // ? values are not set ?!
    writeBigDecimal( m_min );
    writeBigDecimal( m_max );
  }

  private BigDecimal readBigDecimal( ) throws IOException
  {
    final int intVal = readInt();
    return new BigDecimal( BigInteger.valueOf( intVal ), m_scale );
  }

  private final int readInt( ) throws IOException
  {
    m_randomAccessFile.read( m_intBuffer );
    return ByteUtils.readBEInt( m_intBuffer, 0 );
  }

  private void writeBigDecimal( final BigDecimal value ) throws IOException
  {
    if( value == null )
      writeInt( NO_DATA );
    else
    {
      final BigDecimal scaled = value.setScale( m_scale, BigDecimal.ROUND_HALF_UP );
      writeInt( scaled.unscaledValue().intValue() );
    }
  }

  private final void writeInt( final int value ) throws IOException
  {
    ByteUtils.writeBEInt( m_intBuffer, 0, value );
    m_randomAccessFile.write( m_intBuffer );
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
    if( m_randomAccessFile == null )
      return Double.NaN;

    try
    {
      seekValue( x, y );

      final int intVal = readInt();

      if( intVal == NO_DATA )
        return Double.NaN;

      final BigDecimal decimal = new BigDecimal( BigInteger.valueOf( intVal ), m_scale );
      return decimal.doubleValue();
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Could not access grid-value %d/%d", x, y );
      throw new GeoGridException( msg, e );
    }
  }

  /**
   * Sets the value of a grid cell. The given value is scaled to the scale of this grid.
   * 
   * @throws DoubleGridException
   *             If the grid is not opened for write access.
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#getValue(int, int)
   */
  public void setValue( final int x, final int y, final double value ) throws GeoGridException
  {
    try
    {
      seekValue( x, y );

      final BigDecimal decimal = Double.isNaN( value ) ? null : BigDecimal.valueOf( value );
      writeBigDecimal( decimal );
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Could not write grid-value %d/%d", x, y );
      throw new GeoGridException( msg, e );
    }
  }

  /**
   * Sets the value of a grid cell. The given value is rescaled to the scale of this grid.
   * 
   * @throws DoubleGridException
   *             If the grid is not opened for write access.
   */
  public void setValue( final int x, final int y, final BigDecimal value ) throws GeoGridException
  {
    try
    {
      seekValue( x, y );

      writeBigDecimal( value );
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Could not write grid-value %d/%d", x, y );
      throw new GeoGridException( msg, e );
    }
  }

  private void seekValue( final int x, final int y ) throws IOException
  {
    final long pos = y * m_sizeX * 4 + x * 4 + HEADER_SIZE;
    m_randomAccessFile.seek( pos );
  }

  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#dispose()
   */
  @Override
  public void dispose( )
  {
    try
    {
      if( m_randomAccessFile != null )
      {
        m_randomAccessFile.close();
        m_randomAccessFile = null;
      }
    }
    catch( final IOException e )
    {
      // We eat this exception, it should rarely occur and as this file is nor more used should be no error for the user
      e.printStackTrace();
    }

    if( m_binFile != null )
      m_binFile.delete();
  }

  /**
   * Returns the minimum of all values of this grid.
   */
  public BigDecimal getMin( )
  {
    return m_min;
  }

  /**
   * Returns the maximum of all values of this grid.
   */
  public BigDecimal getMax( )
  {
    return m_max;
  }

  /**
   * Gets the statistically values of this grid.
   * 
   * @throws IOException
   *             If the file position is not valid.
   */
  public void getStatistically( ) throws IOException
  {
    final long pos = HEADER_SIZE + m_sizeY * m_sizeX * 4;
    m_randomAccessFile.seek( pos );
    m_min = readBigDecimal();
    m_max = readBigDecimal();
  }

  /**
   * Sets the statistically values of this grid.
   * 
   * @throws IOException
   *             If the grid is not opened for write access.
   */
  public void setStatistically( final BigDecimal min, final BigDecimal max ) throws IOException
  {
    final long pos = HEADER_SIZE + m_sizeY * m_sizeX * 4;
    m_randomAccessFile.seek( pos );
    writeBigDecimal( min );
    writeBigDecimal( max );

    m_min = min;
    m_max = max;
  }
}
