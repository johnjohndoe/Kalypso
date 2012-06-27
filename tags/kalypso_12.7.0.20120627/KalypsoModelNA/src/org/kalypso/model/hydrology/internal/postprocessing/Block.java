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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.contribs.java.lang.NumberUtils;

/**
 * @author Gernot Belger
 */
public class Block
{
  private final String m_key;

  private final BlockTimeStep m_timeStep;

  private final Calendar m_currentStep;

  private final SortedMap<Date, Double> m_data = new TreeMap<Date, Double>();

  public Block( final String key, final BlockTimeStep timeStep )
  {
    m_key = key;
    m_timeStep = timeStep;
    m_currentStep = m_timeStep.getStart();
  }

  public String getKey( )
  {
    return m_key;
  }

  public void readValues( final LineNumberReader reader, final int numValues ) throws IOException
  {
    int valueCount = 0;

    while( reader.ready() )
    {
      final String line = reader.readLine();
      if( line == null )
        break;

      if( line.startsWith( "#" ) ) //$NON-NLS-1$
        continue;

      if( StringUtils.isBlank( line ) )
        continue;

      final String[] values = StringUtils.split( line, null );
      for( final String item : values )
      {
        m_timeStep.step( m_currentStep );

        final Date valueDate = m_currentStep.getTime();

        final double value = NumberUtils.parseQuietDouble( item );

        m_data.put( valueDate, value );

        valueCount++;

        if( valueCount >= numValues )
          return;
      }
    }
  }

  public void exportToFile( final File exportFile, final DateFormat dateFormat ) throws IOException
  {
    final PrintWriter writer = new PrintWriter( exportFile );

    final Set<Entry<Date, Double>> entrySet = m_data.entrySet();
    for( final Entry<Date, Double> entry : entrySet )
    {
      final Date dateKey = entry.getKey();
      final Double value = entry.getValue();
      final String dateString = dateFormat.format( dateKey );
      writer.print( dateString );
      writer.print( ' ' );
      writer.format( "%.3f", value ); //$NON-NLS-1$
    }
    writer.close();
  }

  public Date[] getDates( )
  {
    return m_data.keySet().toArray( new Date[m_data.keySet().size()] );
  }

  public Double getValue( final Date date )
  {
    return m_data.get( date );
  }
}
