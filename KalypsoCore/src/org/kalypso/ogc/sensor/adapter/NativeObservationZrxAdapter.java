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
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class NativeObservationZrxAdapter implements INativeObservationAdapter
{
  private DateFormat m_zrxDateFormat = new SimpleDateFormat( "yyyyMMddHHmmss" );

  public static Pattern m_zrxHeaderPattern = Pattern.compile( "#.*" );

  public static Pattern m_zrxDataPattern = Pattern.compile( "([0-9]{14})\\s+([0-9]+(.[0-9]*))\\s*" );

  public static Pattern m_zrxSNAMEPattern = Pattern.compile( "(#\\S*SNAME)(\\w+)(\\|\\*\\|\\S*)" );

  private String m_title;

  private String m_axisTypeValue;
  
  private String m_SNAME = "titel";

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
    final MetadataList metaDataList = new MetadataList();
    // TODO: allgemein setzten im Import dialog!
    TimeZone timeZone = TimeZone.getTimeZone( "GMT+1" );
    m_zrxDateFormat.setTimeZone( timeZone );
    // create axis
    IAxis[] axis = createAxis();
    ITuppleModel tuppelModel = createTuppelModel( source, axis );
    final SimpleObservation observation = new SimpleObservation( "href", "ID", m_SNAME, false, null, metaDataList, axis, tuppelModel );
    return observation;
  }

  private ITuppleModel createTuppelModel( File source, IAxis[] axis ) throws IOException
  {
    StringBuffer errorBuffer = new StringBuffer();
    FileReader fileReader = new FileReader( source );
    LineNumberReader reader = new LineNumberReader( fileReader );
    final List<Date> dateCollector = new ArrayList<Date>();
    final List<Double> valueCollector = new ArrayList<Double>();
    String lineIn = null;
    while( (lineIn = reader.readLine()) != null )
    {
      try
      {
        Matcher matcher = m_zrxDataPattern.matcher( lineIn );
        if( matcher.matches() )
        {
          Date date = null;
          Double value = null;
          try
          {
            date = m_zrxDateFormat.parse( matcher.group( 1 ) );
          }
          catch( Exception e )
          {
            errorBuffer.append( "line " + reader.getLineNumber() + " date not parseable: \"" + lineIn + "\"\n" );
          }
          try
          {
            value = new Double( matcher.group( 2 ) );
          }
          catch( Exception e )
          {
            errorBuffer.append( "line " + reader.getLineNumber() + " value not parseable: \"" + lineIn + "\"\n" );
          }
          dateCollector.add( date );
          valueCollector.add( value );
        }
        else
        {
          matcher = m_zrxHeaderPattern.matcher( lineIn );
          if( matcher.matches() )
          {
            matcher = m_zrxSNAMEPattern.matcher( lineIn );
            if( matcher.matches() )
              m_SNAME = matcher.group( 2 );
          }
          else
            errorBuffer.append( "line " + reader.getLineNumber() + " is not parseable: \"" + lineIn + "\"\n" );
        }
      }
      catch( Exception e )
      {
        errorBuffer.append( "line " + reader.getLineNumber() + " throws exception \"" + e.getLocalizedMessage() + "\"\n" );
      }
    }
    Object[][] tuppleData = new Object[dateCollector.size()][2];
    for( int i = 0; i < dateCollector.size(); i++ )
    {
      tuppleData[i][0] = dateCollector.get( i );
      tuppleData[i][1] = valueCollector.get( i );
    }
    // TODO handle error
    System.out.println( errorBuffer.toString() );
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