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
package org.kalypso.model.wspm.tuhh.core.profile.importer.hw;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Formatter;
import java.util.LinkedHashMap;
import java.util.Map;

import org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentHandler;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public class HeightWidthCreator implements IWProfContentHandler
{
  private final Map<String, HeightWidthData> m_data = new LinkedHashMap<String, HeightWidthData>();

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentHandler#finished()
   */
  @Override
  public void finished( )
  {
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentHandler#newPoint(java.lang.String,
   *      java.math.BigDecimal, java.lang.String, java.math.BigDecimal, org.kalypsodeegree.model.geometry.GM_Point,
   *      double, java.lang.String, java.lang.String, java.net.URL, java.lang.String, int, int, int)
   */
  @Override
  public void newPoint( final String riverId, final BigDecimal station, final String profileName, final BigDecimal distance, final GM_Point location, final double value, final String comment, final String profileComment, final URL photoURL, final String objectType, final int attributeType, final int ord, final int partOrd )
  {
    final HeightWidthData data = getData( riverId, station, profileName );
    data.addPoint( distance, value, objectType, attributeType, ord, partOrd );
  }

  private HeightWidthData getData( final String riverId, final BigDecimal station, final String profileName )
  {
    final String key = String.format( "%s - %s - %s", riverId, profileName, station );

    final HeightWidthData data = m_data.get( key );
    if( data != null )
      return data;

    System.out.println( key );

    final HeightWidthData newData = new HeightWidthData( key );
    m_data.put( key, newData );
    return newData;
  }

  public void writeToFile( final File outputFile, final File errFile ) throws IOException
  {
    Formatter formatterOut = null;
    Formatter formatterErr = null;
    try
    {
      formatterOut = new Formatter( outputFile, Charset.defaultCharset().name() );
      formatterErr = new Formatter( errFile, Charset.defaultCharset().name() );

      for( final HeightWidthData data : m_data.values() )
      {
        data.formatOut( formatterOut );
        if( formatterOut.ioException() != null )
          throw formatterOut.ioException();
        data.formatErr( formatterErr );
        if( formatterErr.ioException() != null )
          throw formatterErr.ioException();
      }
    }
    finally
    {
      if( formatterOut != null )
        formatterOut.close();
      if( formatterErr != null )
        formatterErr.close();
    }
  }

}
