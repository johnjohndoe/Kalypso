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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Scanner;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.NotImplementedException;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Ascii-Grid Wrapper class which reads the header of an ESRI Ascii Grid<br>
 * <br>
 * Ascii-Grid Header:<br>
 * NCOLS xxx <br>
 * NROWS xxx <br>
 * XLLCENTER xxx | XLLCORNER xxx<br>
 * YLLCENTER xxx | YLLCORNER xxx <br>
 * CELLSIZE xxx<br>
 * NODATA_VALUE xxx<br>
 * 
 * @author kuch
 */
public class AsciiGridReader
{
  private String[] m_headerData;

  private String[] m_headerKeys;

  public AsciiGridReader( final File input ) throws IOException
  {
    parseHeader( input );
  }

  public AsciiGridReader( final Scanner scanner ) throws IOException
  {
    readAsciiGridHeader( scanner );
  }

  private void parseHeader( final File input ) throws IOException
  {
    InputStream is = null;

    try
    {
      is = new BufferedInputStream( new FileInputStream( input ) );

      readAsciiGridHeader( new Scanner( is ) );

      is.close();

    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }

  public void readAsciiGridHeader( Scanner scanner ) throws IOException
  {
    m_headerKeys = new String[6];
    m_headerData = new String[6];

    String line;

    // reading header data
    for( int i = 0; i < 6; i++ )
    {
      line = scanner.nextLine();

      if( line.startsWith( "/*" ) )
      {
        i--;
        continue;
      }

      final String[] values = line.split( "\\s+" );

      m_headerKeys[i] = values[0].trim();
      m_headerData[i] = values[1].trim();
    }

    if( scanner.ioException() != null )
      throw scanner.ioException();
  }

  public String getCols( )
  {
    return m_headerData[0];
  }

  public String getRows( )
  {
    return m_headerData[1];
  }

  public Double getOriginCornerX( )
  {
    if( "xllcorner".equals( m_headerKeys[2].toLowerCase() ) )
    {
      final Double size = getCellSize() / 2.0;

      return new Double( m_headerData[2] ) + size;
    }
    else if( "xllcenter".equals( m_headerKeys[2].toLowerCase() ) )
    {
      return new Double( m_headerData[2] );
    }

    throw new NotImplementedException();
  }

  public Double getOriginCornerY( )
  {
    if( "yllcorner".equals( m_headerKeys[3].toLowerCase() ) )
    {
      final Double size = getCellSize() / 2.0;
      return new Double( m_headerData[3] ) - size;
    }
    else if( "yllcenter".equals( m_headerKeys[3].toLowerCase() ) )
    {
      return new Double( m_headerData[3] );
    }

    throw new NotImplementedException();
  }

  public Double getCellSize( )
  {
    return new Double( m_headerData[4] );
  }

  public String getNoDataValue( )
  {
    return m_headerData[5];
  }

  public RectifiedGridDomain getGridDomain( final String cs )
  {
    try
    {
      final int nCols = new Integer( getCols() ).intValue();
      final int nRows = new Integer( getRows() ).intValue();

      final OffsetVector offsetX = new OffsetVector( getCellSize(), 0 );
      final OffsetVector offsetY = new OffsetVector( 0, -getCellSize() );

      final Double adjustedCornerY = getOriginCornerY() + nRows * getCellSize();

      final GM_Point origin = GeometryFactory.createGM_Point( getOriginCornerX(), adjustedCornerY, cs );

      final double[] low = { 0.0, 0.0 };
      final double[] high = { nCols, nRows };
      final GridRange gridRange = new GridRange_Impl( low, high );

      return new RectifiedGridDomain( origin, offsetX, offsetY, gridRange );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }
}
