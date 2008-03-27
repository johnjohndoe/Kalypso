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
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Scanner;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;

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

  public ConvertAscii2Binary( final URL asciiFileURL, final File ascbinFile, final int scale, final String sourceCRS )
  {
    Assert.isTrue( scale >= 0, "Scale must not be negative" );

    m_asciiFileURL = asciiFileURL;
    m_ascbinFile = ascbinFile;
    m_scale = scale;
    m_sourceCRS = sourceCRS;
  }

  public void doConvert( final IProgressMonitor monitor ) throws IOException, CoreException, GeoGridException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Konvertiere Raster in binäres Format", 100 );

    /* Convert to binary file */
    InputStream bis = null;
    try
    {
      bis = new BufferedInputStream( m_asciiFileURL.openStream() );

      final String[] data = new String[6];

      final Scanner scanner = new Scanner( bis );

      // reading header data
      for( int i = 0; i < 6; i++ )
      {
        final String line = scanner.nextLine();
        final int index = line.indexOf( " " ); //$NON-NLS-1$
        final String subString = line.substring( index );
        data[i] = subString.trim();
      }
      final int sizeX = Integer.parseInt( data[0] );
      final int sizeY = Integer.parseInt( data[1] );

      progress.setWorkRemaining( sizeY + 2 );

      final BigDecimal noData = new BigDecimal( data[5] );

      /* Write header */
      final BinaryGeoGrid binaryGrid = BinaryGeoGrid.createGrid( m_ascbinFile, sizeX, sizeY, m_scale, null, null, null, m_sourceCRS );
      ProgressUtilities.worked( monitor, 1 );

      for( int y = 0; y < sizeY; y++ )
      {
        for( int x = 0; x < sizeX; x++ )
        {
          final String next = scanner.next(); // do not use 'nextDouble' it is much too slow
          final BigDecimal currentValue = new BigDecimal( next );
          if( !currentValue.equals( noData ) )
            binaryGrid.setValue( x, y, currentValue );
        }

        ProgressUtilities.worked( monitor, 1 );
      }

      /* Write statistically data */
      binaryGrid.saveStatistically();

      bis.close();
      binaryGrid.dispose();

      ProgressUtilities.worked( monitor, 1 );
    }
    finally
    {
      IOUtils.closeQuietly( bis );
    }
  }
}
