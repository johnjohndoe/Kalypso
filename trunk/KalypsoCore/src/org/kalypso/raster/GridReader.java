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
package org.kalypso.raster;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.NotImplementedException;

/**
 * Ascii-Grid Wrapper class<br>
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
public class GridReader
{
  private String[] m_headerData;

  private String[] m_headerKeys;

  public GridReader( final File input ) throws IOException
  {
    parseHeader( input );
  }

  private void parseHeader( final File input ) throws IOException
  {
    BufferedReader reader = null;

    try
    {
      reader = new BufferedReader( new FileReader( input ) );
      m_headerKeys = new String[6];
      m_headerData = new String[6];

      String line;

      // reading header data
      for( int i = 0; i < 6; i++ )
      {
        line = reader.readLine();
        final String[] values = line.split( "\\s+ " );

        m_headerKeys[i] = values[0].trim();
        m_headerData[i] = values[1].trim();
      }
      reader.close();

    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
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
      return new Double( m_headerData[2] );
    else if( "xllcenter".equals( m_headerKeys[2].toLowerCase() ) )
    {
      final Double size = getCellSize() / 2.0;
      return new Double( m_headerData[2] ) - size;
    }

    throw (new NotImplementedException());
  }

  public Double getOriginCornerY( )
  {
    if( "yllcorner".equals( m_headerKeys[3].toLowerCase() ) )
      return new Double( m_headerData[3] );
    else if( "yllcenter".equals( m_headerKeys[3].toLowerCase() ) )
    {
      final Double size = getCellSize() / 2.0;
      return new Double( m_headerData[3] ) - size;
    }

    throw (new NotImplementedException());
  }

  public Double getCellSize( )
  {
    return new Double( m_headerData[4] );
  }

  public String getNoDataValue( )
  {
    return m_headerData[5];
  }
}
