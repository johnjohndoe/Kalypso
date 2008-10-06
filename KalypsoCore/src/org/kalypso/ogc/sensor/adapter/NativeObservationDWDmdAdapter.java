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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
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
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * @author Jessica Huebsch, <a href="mailto:j.huebsch@tuhh.de">j.huebsch@tuhh.de</a>
 */
public class NativeObservationDWDmdAdapter implements INativeObservationAdapter
{
  private final DateFormat m_dwdMDDateFormat = new SimpleDateFormat( "ddMMyyyyHHmmss" );

  public static Pattern m_dwdMDfirstHeaderPattern = Pattern.compile( "[\\d]{5}[\\d\\w\\s]{15}(.{30}).+?" );

  public static Pattern m_dwdMDsecondHeaderPattern = Pattern.compile( ".{20}(.{5}).{4}([0-9]{1}).{28}(.{5}).+?" );

  public static Pattern m_dwdMDDataPattern = Pattern.compile( "([0-9]{5})([\\s\\d]{2}[\\s\\d]{2}[0-9]{4}[\\d\\s]{2}[\\d\\s]{2}[\\s\\d]{2})(.{1})(.+?)" );

  private String m_title;

  private String m_axisTypeValue;

  private String m_name;

  private int m_intervall;

  private Integer m_dimension;

  private int m_div;

  private final static int SEARCH_BLOCK_HEADER = 0;

  private final static int SEARCH_VALUES = 1;

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    m_title = config.getAttribute( "label" );
    m_axisTypeValue = config.getAttribute( "axisType" );
  }

  public IObservation createObservationFromSource( File source ) throws Exception
  {
    return createObservationFromSource( source, null, true );
  }

  public IObservation createObservationFromSource( File source, TimeZone timeZone, boolean continueWithErrors ) throws Exception
  {
    final MetadataList metaDataList = new MetadataList();

    /* this is due to backwards compatibility */
    if( timeZone == null )
      timeZone = TimeZone.getTimeZone( "GMT+1" );

    m_dwdMDDateFormat.setTimeZone( timeZone );
    // create axis
    IAxis[] axis = createAxis();
    ITuppleModel tuppelModel = createTuppelModel( source, axis, continueWithErrors );
    if( tuppelModel == null )
      return null;
    final SimpleObservation observation = new SimpleObservation( "href", "ID", m_name, false, null, metaDataList, axis, tuppelModel );
    return observation;
  }

  private ITuppleModel createTuppelModel( File source, IAxis[] axis, boolean continueWithErrors ) throws IOException
  {
    final int MAX_NO_OF_ERRORS = 30;
    int numberOfErrors = 0;

    StringBuffer errorBuffer = new StringBuffer();
    FileReader fileReader = new FileReader( source );
    LineNumberReader reader = new LineNumberReader( fileReader );
    final List<Date> dateCollector = new ArrayList<Date>();
    final List<Double> valueCollector = new ArrayList<Double>();
    String lineIn = null;
    int step = SEARCH_BLOCK_HEADER;
    while( (lineIn = reader.readLine()) != null )
    {
      if( !continueWithErrors && (numberOfErrors > MAX_NO_OF_ERRORS) )
        return null;
      switch( step )
      {
        case SEARCH_BLOCK_HEADER:
          Matcher matcher = m_dwdMDfirstHeaderPattern.matcher( lineIn );
          if( matcher.matches() )
          {
            m_name = matcher.group( 1 ).trim();
            lineIn = reader.readLine();
            matcher = m_dwdMDsecondHeaderPattern.matcher( lineIn );
            if( matcher.matches() )
            {
              m_intervall = Integer.parseInt( matcher.group( 1 ).trim() ) * 60 * 1000;
              m_dimension = Integer.parseInt( matcher.group( 2 ) );
              if( m_dimension == 2 )
                m_div = 100;
              else if( m_dimension == 3 )
                m_div = 1000;
              // read not needed comment lines
              for( int i = 0; i < Integer.parseInt( matcher.group( 3 ).trim() ); i++ )
              {
                lineIn = reader.readLine();
              }
            }
          }
          else
          {
            errorBuffer.append( "line " + reader.getLineNumber() + " is not parseable: \"" + lineIn + "\"\n" );
            numberOfErrors++;
          }
          step++;
          break;
        case SEARCH_VALUES:
          matcher = m_dwdMDDataPattern.matcher( lineIn );
          if( matcher.matches() )
          {
            Date date = null;
            String valueLine = null;
            try
            {
              date = m_dwdMDDateFormat.parse( matcher.group( 2 ) );
            }
            catch( Exception e )
            {
              errorBuffer.append( "line " + reader.getLineNumber() + " date not parseable: \"" + lineIn + "\"\n" );
              numberOfErrors++;
            }
            try
            {
              String label = matcher.group( 3 ).trim();
              if( label.equals( "" ) )
              {
                valueLine = matcher.group( 4 );
                long startDate = date.getTime();
                for( int i = 0; i < 12; i++ )
                {
                  String valueString = valueLine.substring( i * 5, 5 * (i + 1) );
                  Double value = (new Double( Double.parseDouble( valueString ) )) / m_div;
                  valueCollector.add( value );
                  Date valueDate = new Date( startDate + (i) * m_intervall );
                  dateCollector.add( valueDate );
                }
              }
              // No precipitation the whole day (24 hours * 12 values = 288 values)
              else if( label.equals( "N" ) )
              {
                Double value = 0.0;
                long startDate = date.getTime();
                for( int i = 0; i < 288; i++ )
                {
                  valueCollector.add( value );
                  Date valueDate = new Date( startDate + (i) * m_intervall );
                  dateCollector.add( valueDate );
                }
              }
              else if( label.equals( "A" ) )
              {
                Double value = 9999.0;
                long startDate = date.getTime();
                for( int i = 0; i < 12; i++ )
                {
                  valueCollector.add( value );
                  Date valueDate = new Date( startDate + (i) * m_intervall );
                  dateCollector.add( valueDate );
                }
              }
              else if( label.equals( "E" ) )
              {
                // do nothing
              }
            }
            catch( Exception e )
            {
              errorBuffer.append( "line " + reader.getLineNumber() + " value not parseable: \"" + lineIn + "\"\n" );
              numberOfErrors++;
            }
          }
          else
          {
            errorBuffer.append( "line " + reader.getLineNumber() + " is not parseable: \"" + lineIn + "\"\n" );
            numberOfErrors++;
          }
          break;
        default:
          break;
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
    TimeserieUtils.getUnit( m_axisTypeValue );
    final IAxis valueAxis = new DefaultAxis( TimeserieUtils.getName( m_axisTypeValue ), m_axisTypeValue, TimeserieUtils.getUnit( m_axisTypeValue ), Double.class, false );
    final IAxis[] axis = new IAxis[] { dateAxis, valueAxis };
    return axis;
  }
}