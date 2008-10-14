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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Scanner;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypsodeegree.KalypsoDeegreeDebug;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.util.Assert;

/**
 * Converter which converts an ESRI Ascii Grid into a {@link BinaryGrid}.
 * 
 * @author Gernot Belger
 */
public class ConvertAscii2Binary
{
  private final URL m_asciiFileURL;

  private final File m_ascbinFile;

  private final int m_scale;

  private final String m_sourceCRS;

  private RectifiedGridDomain m_gridDomain;

  public ConvertAscii2Binary( final URL asciiFileURL, final File ascbinFile, final int scale, final String sourceCRS )
  {
    Assert.isTrue( scale >= 0, "Scale must not be negative" ); //$NON-NLS-1$

    m_asciiFileURL = asciiFileURL;
    m_ascbinFile = ascbinFile;
    m_scale = scale;
    m_sourceCRS = sourceCRS;
  }

  public void doConvert( final IProgressMonitor monitor )
  {
    KalypsoDeegreeDebug.GRID_OPS.printf( "%s", "converting ascii-grid to binary (" + m_ascbinFile.getName() + ")...\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("ConvertAscii2Binary.4"), 100 ); //$NON-NLS-1$

    /* Convert to binary file */
    InputStream bis = null;
    try
    {
      bis = new BufferedInputStream( m_asciiFileURL.openStream() );

      final Scanner scanner = new Scanner( bis );

      // reading header data
      final AsciiGridReader asciiGridReader = new AsciiGridReader( scanner );

      final String noData = asciiGridReader.getNoDataValue();
      final Double cellSize = asciiGridReader.getCellSize();
      m_gridDomain = asciiGridReader.getGridDomain( m_sourceCRS );

      final GM_Point origin = m_gridDomain.getOrigin( m_sourceCRS );
      final int sizeX = m_gridDomain.getNumColumns();
      final int sizeY = m_gridDomain.getNumRows();
      final Coordinate coordOrigin = JTSAdapter.export( origin.getPosition() );

      final Coordinate offsetX = new Coordinate( cellSize, 0 );
      final Coordinate offsetY = new Coordinate( 0, -cellSize );

      progress.setWorkRemaining( sizeY + 2 );

      /* Write header */
      final BinaryGeoGrid binaryGrid = BinaryGeoGrid.createGrid( m_ascbinFile, sizeX, sizeY, m_scale, coordOrigin, offsetX, offsetY, m_sourceCRS, false );
      ProgressUtilities.worked( progress, 1 );

      /* The current filename - */
      final String asciiFileURL = m_asciiFileURL.getPath();
      final String asciiFileName = FileUtilities.nameFromPath( asciiFileURL );

      final Double nan = Double.NaN;
      for( int y = 0; y < sizeY; y++ )
      {
        if( y % 10 == 0 )
          progress.subTask( String.format( "%s  %d / %d", asciiFileName, y, sizeY ) );

        for( int x = 0; x < sizeX; x++ )
        {
          final String next = scanner.next(); // do not use 'nextDouble' it is much too slow
          final BigDecimal currentValue = new BigDecimal( next );
          if( currentValue.toString().equals( noData ) )
            binaryGrid.setValue( x, y, nan );
          else
            binaryGrid.setValue( x, y, currentValue );
        }

        ProgressUtilities.worked( progress, 1 );
      }

      /* Write statistically data */
      binaryGrid.saveStatistically();

      bis.close();
      binaryGrid.dispose();

      ProgressUtilities.worked( monitor, 1 );

      KalypsoDeegreeDebug.GRID_OPS.printf( "%s", "converting ascii-grid to binary...   done.\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoDeegreeDebug.GRID_OPS.printf( "%s", "converting ascii-grid to binary...   failed.\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    finally
    {
      IOUtils.closeQuietly( bis );
    }
  }

  public RectifiedGridDomain getGridDomain( )
  {
    return m_gridDomain;
  }

}
