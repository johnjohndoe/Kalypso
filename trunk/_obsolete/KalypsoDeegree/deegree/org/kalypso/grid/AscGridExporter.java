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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.Formatter;
import java.util.Locale;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;

import com.vividsolutions.jts.geom.Envelope;

/**
 * Helper class to export a {@link IGeoGrid} to the ESRI Ascii Grid file format.
 * <p>
 * REMARK: could not be implemented as a {@link IGeoGridWalker} as the walker does not guarantee the order of walked
 * cells.
 * </p>
 * 
 * @author Gernot Belger
 */
public class AscGridExporter
{
  private final double m_noDataValue;

  private final int m_scale;

  public AscGridExporter( final double noDataValue, final int scale )
  {
    m_scale = scale;
    Assert.isTrue( scale > 0 );

    m_noDataValue = noDataValue;
  }

  /**
   * Writes a grid into a file in the ESRI Ascii Grid format.
   * 
   * @throws IOException
   *             If write access to the output file fails.
   */
  public void export( final IGeoGrid inputGrid, final File outputFile, final IProgressMonitor monitor ) throws GeoGridException, CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Writing ascii file", 100 );

    Formatter formatter = null;
    try
    {
      final String encoding = Charset.defaultCharset().name();
      // Using default charset (no special is needed for asc and US-Locale for decimal '.')
      formatter = new Formatter( outputFile, encoding, Locale.US );

      export( inputGrid, formatter, progress.newChild( 100 ) );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    catch( final FileNotFoundException e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      formatter.close();

      final IOException ioException = formatter.ioException();
      if( ioException != null )
        throw new CoreException( StatusUtilities.statusFromThrowable( ioException, "Datei konnte nicht geschlossen werden" ) );

      progress.done();
    }
  }

  /**
   * Writes a grid into a formatter in the ESRI Ascii Grid format.
   */
  public void export( final IGeoGrid inputGrid, final Formatter destination, final IProgressMonitor monitor ) throws GeoGridException, CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Writing ascii file", 100 );

    final Envelope envelope = inputGrid.getEnvelope();
    final int sizeX = inputGrid.getSizeX();
    final int sizeY = inputGrid.getSizeY();
    final double cellsize = inputGrid.getOffsetX().x;

    final String m_valueFormat = "%." + m_scale + "f ";

    progress.setWorkRemaining( sizeY );

    // adjust written origin according to xllcorner
    final Double size = cellsize / 2.0;

    double minX = envelope.getMinX();
    double minY = envelope.getMinY();

    double xllcorner = minX - size;
    double yllcorner = minY - size;

    // TODO: add parameter to let user decide, if xllcorner or xllcenter

    /* Header */
    // CellSize: .asc only support quadratic, cartesian cells, so we just take xOffset.x
    destination.format( Locale.US, "ncols         %d%n", sizeX );
    destination.format( "nrows         %d%n", sizeY );
    destination.format( "xllcorner     %.3f%n", xllcorner ); // xllcorner
    destination.format( "yllcorner     %.3f%n", yllcorner ); // yllcorner
    destination.format( "cellsize      " + m_valueFormat + "%n", cellsize );
    destination.format( "NODATA_value  " + m_valueFormat + "%n", m_noDataValue );

    /* Values */
    for( int y = 0; y < sizeY; y++ )
    {
      for( int x = 0; x < sizeX; x++ )
      {
        final double value = inputGrid.getValue( x, y );
        if( Double.isNaN( value ) )
          destination.format( m_valueFormat, m_noDataValue );
        else
          destination.format( m_valueFormat, value );
      }

      destination.format( "%n" );

      ProgressUtilities.worked( progress, 1 );
    }
  }
}