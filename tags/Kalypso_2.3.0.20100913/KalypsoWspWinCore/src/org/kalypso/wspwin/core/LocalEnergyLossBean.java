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
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * @author thuel2
 */
public class LocalEnergyLossBean
{
  private final double m_station;

  private final Map<LOSSKIND, Double> m_entries;

  private static String STATION = "STATION"; //$NON-NLS-1$

  public static enum LOSSKIND
  {
    EINLAUF,
    AUSLAUF,
    KRUEMMER,
    RECHEN,
    ZUSATZVERLUST;
  }

  public LocalEnergyLossBean( final Double station, final Map<LOSSKIND, Double> entries )
  {
    m_station = station;
    m_entries = new TreeMap<LOSSKIND, Double>( entries );
  }

  public Double getStation( )
  {
    return m_station;
  }

  public Map<LOSSKIND, Double> getEntries( )
  {
    return Collections.unmodifiableMap( m_entries );
  }

  /**
   * Reads a psi file (Energieverluste/Verlustbeiwerte/local energy losses)
   */
  public static LocalEnergyLossBean[] read( File lelFile ) throws ParseException, IOException
  {
    final List<LocalEnergyLossBean> beans = new ArrayList<LocalEnergyLossBean>( 0 );

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
            throw new ParseException( Messages.getString("org.kalypso.wspwin.core.LocalEnergyLossBean.1") + nextLine, count ); //$NON-NLS-1$
          final int countKinds = tokenizer.countTokens() / 2 - 1;
          final String key = tokenizer.nextToken();
          if( !STATION.equalsIgnoreCase( key ) )
            throw new ParseException( Messages.getString("org.kalypso.wspwin.core.LocalEnergyLossBean.2") + STATION + "': " + nextLine, count ); //$NON-NLS-1$ //$NON-NLS-2$
          final Double station = Double.parseDouble( tokenizer.nextToken() );

          // read pairs: kind -> value
          final Map<LOSSKIND, Double> entries = new HashMap<LOSSKIND, Double>( countKinds );
          for( int i = 0; i < countKinds; i++ )
          {
            // TODO spelling of kind is changed / normalized. Is it better to wait until conversion to feature?
            final LOSSKIND kind = LOSSKIND.valueOf( tokenizer.nextToken().toUpperCase().replaceAll( "‹", "UE" ) ); //$NON-NLS-1$ //$NON-NLS-2$
            double value = Double.parseDouble( tokenizer.nextToken() );
            // energy losses of same kind are summed up
            if( entries.containsKey( kind ) )
              value = value + entries.get( kind );
            entries.put( kind, value );
          }
          beans.add( new LocalEnergyLossBean( station, entries ) );
        }
      }
      return beans.toArray( new LocalEnergyLossBean[beans.size()] );
    }
    finally
    {
      LineIterator.closeQuietly( lineIt );
    }
  }
}
