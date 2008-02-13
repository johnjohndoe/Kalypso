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
package org.kalypso.ogc.sensor.adapter;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class NativeObservationDVWKAdapter implements INativeObservationAdapter
{
  public static Pattern m_patternLine = Pattern.compile( "[A-Za-z0-9]{4}\\s([0-9\\s]{10})\\s*([0-9]{1,2})\\s*([0-9]{1,2})([A-Za-z\\s]{1})(.*)" );

  public static Pattern m_subPatternData = Pattern.compile( "\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})\\s*([0-9]{1,5})" );

  private String m_title;

  private String m_axisTypeValue;

  private String m_SNAME = "titel";

  private TimeZone m_timeZone;

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    m_title = config.getAttribute( "label" );
    m_axisTypeValue = config.getAttribute( "axisType" );
  }

  public IObservation createObservationFromSource( final File source ) throws Exception
  {
    return createObservationFromSource( source, null, true );
  }

  public IObservation createObservationFromSource( final File source, final TimeZone timeZone, final boolean continueWithErrors ) throws Exception
  {
    final MetadataList metaDataList = new MetadataList();
    metaDataList.put( ObservationConstants.MD_ORIGIN, source.getAbsolutePath() );

    /* this is due to backwards compatibility */
    if( timeZone == null )
      m_timeZone = TimeZone.getTimeZone( "GMT+1" );
    else
      m_timeZone = timeZone;

    // create axis
    final IAxis[] axis = createAxis();
    final ITuppleModel tuppelModel = createTuppelModel( source, axis, continueWithErrors );
    if( tuppelModel == null )
      return null;
    final SimpleObservation observation = new SimpleObservation( "href", "ID", m_SNAME, false, null, metaDataList, axis, tuppelModel );
    return observation;
  }

  private ITuppleModel createTuppelModel( final File source, IAxis[] axis, boolean continueWithErrors ) throws IOException
  {
    final int MAX_NO_OF_ERRORS = 30;
    int numberOfErrors = 0;

    StringBuffer errorBuffer = new StringBuffer();
    FileReader fileReader = new FileReader( source );
    LineNumberReader reader = new LineNumberReader( fileReader );
    final List<Date> dateCollector = new ArrayList<Date>();
    final List<Double> valueCollector = new ArrayList<Double>();
    String lineIn = null;
    GregorianCalendar previousNlineCalendar = null;
    while( (lineIn = reader.readLine()) != null )
    {
      if( !continueWithErrors && (numberOfErrors > MAX_NO_OF_ERRORS) )
        return null;
      try
      {

        final Matcher matcher = m_patternLine.matcher( lineIn );
        if( matcher.matches() )
        {
          // end of file?
          if( matcher.group( 4 ).equals( "E" ) )
            break;

          final String dateTime = matcher.group( 1 );
          final int day = Integer.parseInt( dateTime.substring( 0, 2 ).trim() );
          final int month = Integer.parseInt( dateTime.substring( 2, 4 ).trim() );
          final int year = Integer.parseInt( dateTime.substring( 4, 8 ).trim() );
          final int hour = Integer.parseInt( dateTime.substring( 8, 10 ).trim() );

          // comment line
          if( day == 0 && month == 0 && year == 0 )
            continue;

          final GregorianCalendar calendar = new GregorianCalendar( m_timeZone );
// final GregorianCalendar calendar = new GregorianCalendar( TimeZone.getTimeZone( "UTC" ) );
          calendar.set( year, month - 1, day, hour, 0, 0 );

          if( previousNlineCalendar != null )
          {
            while( previousNlineCalendar.compareTo( calendar ) < 0 )
            {
              dateCollector.add( previousNlineCalendar.getTime() );
              valueCollector.add( 0.0 );
              previousNlineCalendar.add( GregorianCalendar.MINUTE, 5 );
            }
          }

          // data line
          if( matcher.group( 4 ).equals( "N" ) )
          {
            // TODO check if this is means all zeros for the whole day
            // or just for this hour,
            // or all zeros until the next entry?

            // all zeros until the next entry!
            previousNlineCalendar = calendar;
            continue;

// for( int i = 0; i < 12; i++ )
// {
// dateCollector.add( calendar.getTime() );
// valueCollector.add( 0.0 );
// calendar.add( GregorianCalendar.MINUTE, 5 );
// }
          }
          else
          {
            previousNlineCalendar = null;
            final Matcher dataMatcher = m_subPatternData.matcher( matcher.group( 5 ) );
            if( dataMatcher.matches() )
            {
              for( int i = 1; i < 13; i++ )
              {
                try
                {
                  final double value = new Double( dataMatcher.group( i ) );
                  dateCollector.add( calendar.getTime() );
                  valueCollector.add( value );
                }
                catch( Exception e )
                {
                  errorBuffer.append( "line " + reader.getLineNumber() + " value not parseable: \"" + lineIn + "\"\n" );
                  numberOfErrors++;
                }
                calendar.add( GregorianCalendar.MINUTE, 5 );
              }
            }
          }
        }
        else
          numberOfErrors++;
      }
      catch( Exception e )
      {
        errorBuffer.append( "line " + reader.getLineNumber() + " throws exception \"" + e.getLocalizedMessage() + "\"\n" );
        numberOfErrors++;
      }
    }
    if( !continueWithErrors && numberOfErrors > MAX_NO_OF_ERRORS )
    {

      MessageBox messageBox = new MessageBox( null, SWT.ICON_QUESTION | SWT.YES | SWT.NO );
      messageBox.setMessage( "Too many errors, probably wrong format selected. Continue (slow operation)?" );
      messageBox.setText( "Import errors" );
      if( messageBox.open() == SWT.NO )
        return null;
      else
        continueWithErrors = true;
    }
    // TODO handle error
    System.out.println( errorBuffer.toString() );

    Object[][] tuppleData = new Object[dateCollector.size()][2];
    for( int i = 0; i < dateCollector.size(); i++ )
    {
      tuppleData[i][0] = dateCollector.get( i );
      tuppleData[i][1] = valueCollector.get( i );
    }
    return new SimpleTuppleModel( axis, tuppleData );
  }

  @Override
  public String toString( )
  {
    return m_title;
  }

  /**
   * @see org.kalypso.ogc.sensor.adapter.INativeObservationAdapter#createAxis()
   */
  public IAxis[] createAxis( )
  {
    final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "", Date.class, true );
    final IAxis valueAxis = new DefaultAxis( TimeserieUtils.getName( m_axisTypeValue ), m_axisTypeValue, TimeserieUtils.getUnit( m_axisTypeValue ), Double.class, false );
    final IAxis[] axis = new IAxis[] { dateAxis, valueAxis };
    return axis;
  }
}