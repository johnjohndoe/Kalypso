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
package org.kalypso.kalypsosimulationmodel.core.wind;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.math.BigDecimal;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.deegree.framework.util.Pair;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.grid.BinaryGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoWalkingStrategy;

import com.vividsolutions.jts.geom.Coordinate;

/**
 *  Wrapper class for saving pairs of values in to standard binary grid {@link BinaryGeoGrid}.
 * 
 * 
 * @author ig
 */
public class BinaryGeoGridWrapperForPairsModel extends BinaryGeoGrid
{
  
  public BinaryGeoGridWrapperForPairsModel( final RandomAccessFile randomAccessFile, final int sizeX, final int sizeY, final int scale, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY, final String sourceCRS, final boolean fillGrid ) throws GeoGridException
  {
    super( randomAccessFile, sizeX * 2, sizeY, scale, origin, offsetX, offsetY, sourceCRS, fillGrid );
  }

  protected BinaryGeoGridWrapperForPairsModel( final RandomAccessFile randomAccessFile, final File binFile, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY, final String sourceCRS ) throws IOException
  {
    super( randomAccessFile, binFile, origin, offsetX, offsetY, sourceCRS );
  }

  /**
   * Opens an existing grid.<br>
   * Dispose the grid after it is no more needed in order to release the given resource.
   *
   * @param writeable
   *          If <code>true</code>, the grid is opened for writing and a {@link IWriteableGeoGrid} is returned.
   */
  public static BinaryGeoGridWrapperForPairsModel openGrid( final URL url, final Coordinate origin, final Coordinate offsetX, final Coordinate offsetY, final String sourceCRS, final boolean writeable ) throws IOException
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
      binFile = fileFromUrl; // set in order to delete on dispose
    }

    final String flags = writeable ? "rw" : "r";
    
    final RandomAccessFile randomAccessFile = new RandomAccessFile( fileFromUrl, flags );
    return new BinaryGeoGridWrapperForPairsModel( randomAccessFile, binFile, origin, offsetX, offsetY, sourceCRS );
    
  }
  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#getSizeX()
   */
  @Override
  public int getSizeX( )
  {
    return super.getSizeX() / 2;
  }

  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#getValue(int, int)
   */
  @Override
  public double getValue( final int x, final int y )
  {
    throw new UnsupportedOperationException( "In this grid you can only get the pairs of values, use appropriate mathod" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#getValue(int, int)
   */
  public Pair< Double, Double > getPairValue( final int x, final int y ) throws GeoGridException
  {
    double lDoubleFirst = super.getValue( x * 2, y );
    double lDoubleSecond = super.getValue( x * 2 + 1, y );
    
    return new Pair< Double, Double >( lDoubleFirst, lDoubleSecond );
  }

  @Override
  public void setValue( final int x, final int y, final BigDecimal value )
  {
    throw new UnsupportedOperationException( "In this grid you can only put the pairs of values, use appropriate mathod" ); //$NON-NLS-1$
  }
 
  @Override
  public void setValue( final int x, final int y, final double value )
  {
      throw new UnsupportedOperationException( "In this grid you can only put the pairs of values, use appropriate mathod" ); //$NON-NLS-1$
  }
  
  /**
   * Sets the pair of values of a grid cell. The given values are scaled to the scale of this grid.
   *
   * @throws DoubleGridException
   *           If the grid is not opened for write access.
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#getValue(int, int)
   */
  public void setPairValue( final int x, final int y, final Pair< Double, Double > value ) throws GeoGridException
  {
    super.setValue( x * 2, y, value.first );
    super.setValue( x * 2 + 1, y, value.second );
  }

  /**
   * Returns a walking strategy suitable to iterate thorugh this grid.
   */
  @Override
  public IGeoWalkingStrategy getWalkingStrategy( ) 
  {
    return new SimpleWindDataGridWalkingStrategy();
  }
}
