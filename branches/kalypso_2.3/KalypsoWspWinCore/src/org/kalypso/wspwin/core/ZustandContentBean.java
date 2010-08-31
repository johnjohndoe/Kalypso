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
package org.kalypso.wspwin.core;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * Represents the contents of a .str file
 * 
 * @author Belger
 */
public class ZustandContentBean
{
  private final ProfileBean[] m_profileBeans;

  private final ZustandSegmentBean[] m_segmentBeans;

  public ZustandContentBean( final ProfileBean[] profileBeans, final ZustandSegmentBean[] segmentBeans )
  {
    m_profileBeans = profileBeans;
    m_segmentBeans = segmentBeans;
  }

  public ProfileBean[] getProfileBeans( )
  {
    return m_profileBeans;
  }

  public ZustandSegmentBean[] getSegmentBeans( )
  {
    return m_segmentBeans;
  }

  /**
   * Reads a .str file
   */
  public static ZustandContentBean read( final File strFile ) throws IOException, ParseException
  {
    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( strFile ) );

      final int[] counts = WspCfgBean.readStrHeader( reader );
      final int profilCount = counts[0];
      final int segmentCount = counts[1];

      final ProfileBean[] profileBeans = ProfileBean.readProfiles( reader, profilCount );
      reader.readLine(); // read empty line
      final ZustandSegmentBean[] segmentBeans = readZustandSegments( reader, segmentCount );

      return new ZustandContentBean( profileBeans, segmentBeans );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private static ZustandSegmentBean[] readZustandSegments( final LineNumberReader reader, final int segmentCount ) throws ParseException, IOException
  {
    final List<ZustandSegmentBean> beans = new ArrayList<ZustandSegmentBean>( 20 );
    for( int i = 0; i < segmentCount; i++ )
    {
      if( !reader.ready() )
        throw new ParseException( Messages.getString("org.kalypso.wspwin.core.ZustandContentBean.0") + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$

      final String line = reader.readLine();
      if( line == null || line.trim().length() == 0 )
        throw new ParseException( Messages.getString("org.kalypso.wspwin.core.ZustandContentBean.1") + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$

      final StringTokenizer tokenizer = new StringTokenizer( line );
      if( tokenizer.countTokens() != 7 )
        throw new ParseException( Messages.getString("org.kalypso.wspwin.core.ZustandContentBean.2") + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$

      try
      {
        final double stationFrom = Double.parseDouble( tokenizer.nextToken() );
        final double stationTo = Double.parseDouble( tokenizer.nextToken() );
        final double distanceVL = Double.parseDouble( tokenizer.nextToken() );
        final double distanceHF = Double.parseDouble( tokenizer.nextToken() );
        final double distanceVR = Double.parseDouble( tokenizer.nextToken() );

        final String fileNameFrom = tokenizer.nextToken();
        final String fileNameTo = tokenizer.nextToken();

        final ZustandSegmentBean bean = new ZustandSegmentBean( stationFrom, stationTo, fileNameFrom, fileNameTo, distanceVL, distanceHF, distanceVR );
        beans.add( bean );
      }
      catch( final NumberFormatException e )
      {
        e.printStackTrace();
        throw new ParseException( Messages.getString("org.kalypso.wspwin.core.ZustandContentBean.3") + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$
      }

    }

    return beans.toArray( new ZustandSegmentBean[beans.size()] );
  }

}
