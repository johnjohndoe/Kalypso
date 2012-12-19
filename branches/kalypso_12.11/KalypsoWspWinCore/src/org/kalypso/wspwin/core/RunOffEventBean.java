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
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;
import java.util.TreeMap;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class RunOffEventBean
{
  private final String m_name;

  private final Map<BigDecimal, BigDecimal> m_entries = new TreeMap<>();

  public RunOffEventBean( final String name )
  {
    m_name = name;
  }

  public String getName( )
  {
    return m_name;
  }

  public Map<BigDecimal, BigDecimal> getEntries( )
  {
    return Collections.unmodifiableMap( m_entries );
  }

  public void addEntry( final BigDecimal station, final BigDecimal value )
  {
    m_entries.put( station, value );
  }

  /** Reads a qwt or wsf file */
  public static RunOffEventBean[] read( final File qwtFile ) throws ParseException, IOException
  {
    // the qwt and wsf files may not exist; return empty list of beans
    if( !qwtFile.exists() )
      return new RunOffEventBean[] {};

    final List<RunOffEventBean> beans = new ArrayList<>( 10 );

    LineIterator lineIt = null;
    try
    {
      int count = 0;
      for( lineIt = FileUtils.lineIterator( qwtFile, null ); lineIt.hasNext(); )
      {
        final String nextLine = lineIt.nextLine().trim();
        count++;

        if( nextLine.isEmpty() )
          continue;

        final StringTokenizer tokenizer = new StringTokenizer( nextLine );
        if( tokenizer.countTokens() != 2 )
          throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.RunOffEventBean.0" ) + nextLine, count ); //$NON-NLS-1$

        final String eventName = tokenizer.nextToken();

        final RunOffEventBean bean = new RunOffEventBean( eventName );

        final int eventLength = Integer.parseInt( tokenizer.nextToken() );

        // read block: station -> value
        for( int i = 0; i < eventLength; i++ )
        {
          if( !lineIt.hasNext() )
            throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.RunOffEventBean.1" ) + eventName, count ); //$NON-NLS-1$

          final String line = lineIt.nextLine();
          count++;
          final StringTokenizer tz = new StringTokenizer( line );
          if( tz.countTokens() != 2 )
            throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.RunOffEventBean.2" ) + nextLine, count ); //$NON-NLS-1$

          final double station = Double.parseDouble( tz.nextToken() );
          final double value = Double.parseDouble( tz.nextToken() );
          bean.addEntry( BigDecimal.valueOf( station ), BigDecimal.valueOf( value ) );
        }

        beans.add( bean );
      }

      return beans.toArray( new RunOffEventBean[beans.size()] );
    }
    finally
    {
      LineIterator.closeQuietly( lineIt );
    }
  }

  public static void write( final File outputFile, final RunOffEventBean[] fixation ) throws IOException
  {
    try( final PrintWriter pw = new PrintWriter( outputFile) )
    {
      for( final RunOffEventBean runOff : fixation )
      {
        final String name = runOff.getName();
        final String cleanName = StringUtils.remove( name, ' ' );
        final String shortName = StringUtils.abbreviateMiddle( cleanName, ".", 20 ); //$NON-NLS-1$

        final Map<BigDecimal, BigDecimal> entries = runOff.getEntries();

        pw.format( "%s %d%n", shortName, entries.size() ); //$NON-NLS-1$
        for( final Entry<BigDecimal, BigDecimal> entry : entries.entrySet() )
        {
          final BigDecimal station = entry.getKey();
          final BigDecimal value = entry.getValue();
          pw.format( Locale.US, "%.4f %.4f%n", station, value ); //$NON-NLS-1$
        }
      }

      if( pw.checkError() )
        throw new IOException();

      pw.close();
    }
  }
}