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
package org.kalypso.ui.model.wspm.core.wspwin;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;

/**
 * @author Gernot
 */
public class RunOffEventBean
{
  private final String m_name;

  private final Map<Double, Double> m_entries;

  public RunOffEventBean( final String name, final Map<Double, Double> entries )
  {
    m_name = name;
    m_entries = new TreeMap<Double, Double>( entries );
  }

  public String getName( )
  {
    return m_name;
  }

  public Map<Double, Double> getEntries( )
  {
    return Collections.unmodifiableMap( m_entries );
  }

  /** Reads a qwt or wsf file */
  public static RunOffEventBean[] read( final File qwtFile ) throws ParseException, IOException
  {
    final List<RunOffEventBean> beans = new ArrayList<RunOffEventBean>( 10 );

    LineIterator lineIt = null;
    try
    {
      int count = 0;
      for( lineIt = FileUtils.lineIterator( qwtFile, null ); lineIt.hasNext(); )
      {
        final String nextLine = lineIt.nextLine();
        count++;
        final StringTokenizer tokenizer = new StringTokenizer( nextLine );
        if( tokenizer.countTokens() != 2 )
          throw new ParseException( "Syntax error in block start: " + nextLine, count );

        final String eventName = tokenizer.nextToken();
        final int eventLength = Integer.parseInt( tokenizer.nextToken() );

        // read block: station -> value
        final Map<Double, Double> entries = new HashMap<Double, Double>( eventLength );
        for( int i = 0; i < eventLength; i++ )
        {
          if( !lineIt.hasNext() )
            throw new ParseException( "End of file reached for event: " + eventName, count );

          final String line = lineIt.nextLine();
          count++;
          final StringTokenizer tz = new StringTokenizer( line );
          if( tz.countTokens() != 2 )
            throw new ParseException( "Syntax error in line: " + nextLine, count );

          final double station = Double.parseDouble( tz.nextToken() );
          final double value = Double.parseDouble( tz.nextToken() );
          entries.put( station, value );
        }

        beans.add( new RunOffEventBean( eventName, entries ) );
      }

      return beans.toArray( new RunOffEventBean[beans.size()] );
    }
    finally
    {
      LineIterator.closeQuietly( lineIt );
    }
  }
}
