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
import java.util.Collection;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.tuple.Pair;
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * @author thuel2
 */
public class LocalEnergyLossBean
{
  /**
   * Reads a psi file (Energieverluste/Verlustbeiwerte/local energy losses)
   */
  public static LocalEnergyLossBean[] read( final File lelFile ) throws ParseException, IOException
  {
    final List<LocalEnergyLossBean> beans = new ArrayList<>( 0 );

    LineIterator lineIt = null;
    try
    {
      if( lelFile.exists() )
      {
        int count = 0;
        for( lineIt = FileUtils.lineIterator( lelFile, null ); lineIt.hasNext(); )
        {
          final String nextLine = lineIt.nextLine();
          count++;

          final StringTokenizer tokenizer = new StringTokenizer( nextLine );
          if( tokenizer.countTokens() % 2 != 0 )
            throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.LocalEnergyLossBean.1" ) + nextLine, count ); //$NON-NLS-1$

          final int countKinds = tokenizer.countTokens() / 2 - 1;

          final String key = tokenizer.nextToken();

          if( !STATION.equalsIgnoreCase( key ) )
            throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.LocalEnergyLossBean.2" ) + STATION + "': " + nextLine, count ); //$NON-NLS-1$ //$NON-NLS-2$

          final BigDecimal station = new BigDecimal( tokenizer.nextToken() );

          // read pairs: kind -> value
          final Collection<Pair<String, BigDecimal>> entries = new ArrayList<>();
          for( int i = 0; i < countKinds; i++ )
          {
            final String kind = tokenizer.nextToken();
            final BigDecimal value = new BigDecimal( tokenizer.nextToken() );
            entries.add( Pair.of( kind, value ) );
          }

          final LocalEnergyLossBean lossBean = new LocalEnergyLossBean( station, entries.toArray( new Pair[entries.size()] ) );
          beans.add( lossBean );
        }
      }
      return beans.toArray( new LocalEnergyLossBean[beans.size()] );
    }
    finally
    {
      LineIterator.closeQuietly( lineIt );
    }
  }

  private final BigDecimal m_station;

  private final Pair<String, BigDecimal>[] m_entries;

  private static String STATION = "STATION"; //$NON-NLS-1$

  public LocalEnergyLossBean( final BigDecimal station, final Pair<String, BigDecimal>[] entries )
  {
    m_station = station;
    m_entries = entries;
  }

  public static void write( final File lelFile, final LocalEnergyLossBean[] beans ) throws IOException
  {
    try( final PrintWriter psiWriter = new PrintWriter( lelFile ) )
    {
      for( final LocalEnergyLossBean localEnergyLossBean : beans )
      {
        psiWriter.format( "STATION %s", localEnergyLossBean.getStation() );//$NON-NLS-1$

        final Pair<String, BigDecimal>[] entries = localEnergyLossBean.getEntries();
        for( final Pair<String, BigDecimal> entry : entries )
        {
          final String type = entry.getLeft();
          final BigDecimal value = entry.getRight();

          psiWriter.format( " %s %s", type, value );//$NON-NLS-1$
        }

        psiWriter.println();
      }
    }
  }

  public Pair<String, BigDecimal>[] getEntries( )
  {
    return m_entries;
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }
}