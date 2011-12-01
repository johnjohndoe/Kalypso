/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;

import org.apache.commons.io.IOUtils;
import org.kalypso.model.hydrology.internal.NATimeSettings;

public class BlockTimeSeries
{
  private static DatatypeFactory DATATYPE_FACTORY;
  static
  {
    try
    {
      DATATYPE_FACTORY = DatatypeFactory.newInstance();
    }
    catch( final DatatypeConfigurationException e )
    {
      e.printStackTrace();
    }
  }

  // simulationszeitraum: von 970101 24 uhr bis 980102 24 uhr 24.000
  // simulationszeitraum: von 970101 24 uhr bis 102 24 uhr 24.000
  private final static Pattern pTime = Pattern.compile( ".+simulationszeitraum.+([0-9]{6}).+?([0-9]{1,2}).+[0-9]{3,6}.+[0-9]{1,2}.+?(\\d+\\.\\d+)\\D*" ); //$NON-NLS-1$

  // synth. n.: haufigkeit: 0.100 jahre,dauer: 13.00 h , zeitschr.: 0.083 h , verteilung: 2
  private final static Pattern pSynthTime = Pattern.compile( ".+synth. n..+haufigkeit:.+(\\d+\\.\\d+).+jahre,dauer.+(\\d+\\.\\d+).+h , zeitschr.:.+(\\d+\\.\\d+).+" ); //$NON-NLS-1$

  private final static Pattern PATTERN_BLOCK_HEADER = Pattern.compile( "\\D*(\\d+)\\D+(\\d+)\\D+(\\d+)\\D*" ); //$NON-NLS-1$

  private final Map<String, Block> m_blocks = new HashMap<String, Block>();

  private final DateFormat m_dateFormat;

  public BlockTimeSeries( final TimeZone timeZone )
  {
    m_dateFormat = new SimpleDateFormat( "yyMMdd" ); //$NON-NLS-1$
    m_dateFormat.setTimeZone( timeZone );
  }

  public BlockTimeSeries( )
  {
    this( NATimeSettings.getInstance().getTimeZone() );
  }

  /**
   * imports all ts from given blockfile
   */
  public void importBlockFile( final File blockFile )
  {
    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( blockFile ) );

      BlockTimeStep timeStep = searchTimeoffset( reader );

      while( reader.ready() )
      {
        final Entry<String, Integer> blockInfo = searchBlockHeader( reader );
        if( blockInfo == null )
          break;

        final String key = blockInfo.getKey();
        final int valuesCount = blockInfo.getValue();

        // Add values to existing block
        final Block block = getOrCreateBlock( key, timeStep );
        block.readValues( reader, valuesCount );
      }
      reader.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      System.out.println( "could not read blockfile " ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private Block getOrCreateBlock( String key, BlockTimeStep timeStep )
  {
    if( m_blocks.containsKey( key ) )
      return m_blocks.get( key );

    final Block block = new Block( key, timeStep );
    m_blocks.put( key, block );
    return block;
  }

  private BlockTimeStep searchTimeoffset( LineNumberReader reader ) throws ParseException, IOException
  {
    while( reader.ready() )
    {
      String line = reader.readLine();
      if( line == null )
        break;

      if( line.startsWith( "#" ) ) //$NON-NLS-1$
        continue;

      Matcher m = pTime.matcher( line );
      Matcher synthM = pSynthTime.matcher( line );
      if( m.matches() )
      {
        final String sDate = m.group( 1 );
        final String sTime = m.group( 2 );
        final String sStep = m.group( 3 );

        final Calendar startCal = parseDate24( sDate, sTime );
        final Duration timestep = parseDuration( sStep );
        return new BlockTimeStep( startCal, timestep );
      }
      else if( synthM.matches() )
      {
        // synthetisches Ereignis hat kein Anfangsdatum, daher wird 01.01.2000 angenommen!
        final String sDate = "000101"; //$NON-NLS-1$
        String sTime = "0"; //$NON-NLS-1$
        final String sStep = synthM.group( 3 );

        final Calendar startCal = parseDate24( sDate, sTime );
        final Duration timestep = parseDuration( sStep );
        return new BlockTimeStep( startCal, timestep );
      }
    }

    return null;
  }

  private Entry<String, Integer> searchBlockHeader( LineNumberReader reader ) throws NumberFormatException, IOException
  {
    while( reader.ready() )
    {
      final String line = reader.readLine();
      if( line == null )
        break;

      if( line.startsWith( "#" ) ) //$NON-NLS-1$
        continue;

      Matcher m = PATTERN_BLOCK_HEADER.matcher( line );
      if( m.matches() )
      {
        final String key = m.group( 1 );
        int valuesCount = Integer.parseInt( m.group( 3 ) );

        // HACK: create singleton map in order to create entry
        return Collections.singletonMap( key, valuesCount ).entrySet().iterator().next();
      }
    }

    return null;
  }

  private Duration parseDuration( String sStep )
  {
    // TRICKY: the read value is in hours.
    // If we just calculate the millies from that, we cannot really compute with that duration and get strange effects
    // due to skip-seconds etc.

    final float hours = Float.parseFloat( sStep );
    
    // Try to find the best match
    
    // month and year do not have a fixed number of hours, so we cannot match
    
    final Duration dayMatch = getDurationAsDays( hours );
    if( dayMatch != null )
      return dayMatch;

    final Duration hourMatch = getDurationAsHours( hours );
    if( hourMatch != null )
      return hourMatch;

    final Duration minuteMatch = getDurationAsMinutes( hours );
    return minuteMatch;
    
    // TODO: implement SECONDS?
    
//
//    if( "0.083".equals( sStep ) ) //$NON-NLS-1$
//      return DATATYPE_FACTORY.newDuration( 300000l );
//    else
//    {
//      float step = Float.parseFloat( sStep );
//      return DATATYPE_FACTORY.newDuration( ((long) (step * 1000f)) * 3600l );
//    }
  }

  private Duration getDurationAsDays( float hours )
  {
    final float days = hours / 24.0f;
    final int daysRounded = Math.round( days );

    if( days == daysRounded )
      return DATATYPE_FACTORY.newDuration( true, 0, 0, daysRounded, 0, 0, 0 );

    return null;
  }

  private Duration getDurationAsHours( float hours )
  {
    final int hoursRounded = Math.round( hours );
    if( hours == hoursRounded )
      return DATATYPE_FACTORY.newDuration( true, 0, 0, 0, hoursRounded, 0, 0 );

    return null;
  }

  // TODO: check
  // we assume, that the smallest amount here is minutes; so we just round the hours.
  // Actually, the hours of the calculation core have too less digits e.g. 0.083 is 4.98 minutes, but the real timestep
  // is 5.0 minutes. We need to change the calc core in order to return more digits!
  private Duration getDurationAsMinutes( float hours )
  {
    final float minutes = hours * 60.0f;
    final int minutesRounded = Math.round( minutes );

// if( minutes == daysRounded )
    return DATATYPE_FACTORY.newDuration( true, 0, 0, 0, 0, minutesRounded, 0 );

// return null;
  }

  private Calendar parseDate24( String sDate, String sTime ) throws ParseException
  {
    final Date parseDate = m_dateFormat.parse( sDate );
//    final Calendar startCal = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
    final Calendar startCal = Calendar.getInstance( NATimeSettings.getInstance().getTimeZone() );
    startCal.setTime( parseDate );

    int hours = Integer.parseInt( sTime );
    // 24 means 0 same day ! (RRM/fortran-logic)
    if( hours == 24 )
      hours = 0; //$NON-NLS-1$

    startCal.add( Calendar.HOUR_OF_DAY, hours );

    return startCal;
  }

  public String[] getKeys( )
  {
    return m_blocks.keySet().toArray( new String[m_blocks.keySet().size()] );
  }

  public void exportToFile( final String key, final File exportFile, final DateFormat dateFormat ) throws IOException
  {
    if( m_blocks.containsKey( key ) )
    {
      final Block block = m_blocks.get( key );
      block.exportToFile( exportFile, dateFormat );
    }
  }

  public Block getTimeSerie( final String key )
  {
    return m_blocks.get( key );
  }

  public boolean dataExistsForKey( final String key )
  {
    return m_blocks.containsKey( key );
  }
}