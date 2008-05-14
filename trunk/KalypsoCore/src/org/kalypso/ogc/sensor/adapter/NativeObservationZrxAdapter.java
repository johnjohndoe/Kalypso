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
import org.kalypso.core.i18n.Messages;
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
public class NativeObservationZrxAdapter implements INativeObservationAdapter
{
  private final DateFormat m_zrxDateFormat = new SimpleDateFormat( "yyyyMMddHHmm" ); //$NON-NLS-1$

  private final DateFormat m_zrxDateFormatSec = new SimpleDateFormat( "yyyyMMddHHmmss" ); //$NON-NLS-1$

  public static Pattern m_zrxHeaderPattern = Pattern.compile( "#.*" ); //$NON-NLS-1$

  public static Pattern m_zrxDataPattern = Pattern.compile( "([0-9]{12,14})\\s+(-??[0-9]+(.[0-9]*))\\s*" ); //$NON-NLS-1$

  public static Pattern m_zrxSNAMEPattern = Pattern.compile( "(#\\S*SNAME)(\\w+)(\\|\\*\\|\\S*)" ); //$NON-NLS-1$

  private String m_title;

  private String m_axisTypeValue;

  private String m_SNAME = "titel"; //$NON-NLS-1$

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    m_title = config.getAttribute( "label" ); //$NON-NLS-1$
    m_axisTypeValue = config.getAttribute( "axisType" ); //$NON-NLS-1$
  }

  public IObservation createObservationFromSource( final File source ) throws Exception
  {
    return createObservationFromSource( source, null, true );
  }

  public IObservation createObservationFromSource( final File source, TimeZone timeZone, final boolean continueWithErrors ) throws Exception
  {
    final MetadataList metaDataList = new MetadataList();
    metaDataList.put( ObservationConstants.MD_ORIGIN, source.getAbsolutePath() );

    /* this is due to backwards compatibility */
    if( timeZone == null )
      timeZone = TimeZone.getTimeZone( "GMT+1" ); //$NON-NLS-1$

    m_zrxDateFormat.setTimeZone( timeZone );
    m_zrxDateFormatSec.setTimeZone( timeZone );
    // create axis
    IAxis[] axis = createAxis();
    ITuppleModel tuppelModel = createTuppelModel( source, axis, continueWithErrors );
    if( tuppelModel == null )
      return null;
    final SimpleObservation observation = new SimpleObservation( "href", "ID", m_SNAME, false, null, metaDataList, axis, tuppelModel ); //$NON-NLS-1$ //$NON-NLS-2$
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
    while( (lineIn = reader.readLine()) != null )
    {
      if( !continueWithErrors && (numberOfErrors > MAX_NO_OF_ERRORS) )
        return null;
      try
      {
        Matcher matcher = m_zrxDataPattern.matcher( lineIn );
        if( matcher.matches() )
        {
          Date date = null;
          Double value = null;
          if( matcher.group( 1 ).length() == 14 ) // date with seconds
          {
            try
            {
              date = m_zrxDateFormatSec.parse( matcher.group( 1 ) );
            }
            catch( Exception e )
            {
              errorBuffer.append( Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.11") + reader.getLineNumber() + Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.12") + lineIn + "\"\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              numberOfErrors++;
            }
          }
          else
          {
            try
            {
              date = m_zrxDateFormat.parse( matcher.group( 1 ) );
            }
            catch( Exception e )
            {
              errorBuffer.append( Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.14") + reader.getLineNumber() + Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.15") + lineIn + "\"\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              numberOfErrors++;
            }
          }
          try
          {
            value = new Double( matcher.group( 2 ) );
          }
          catch( Exception e )
          {
            errorBuffer.append( Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.17") + reader.getLineNumber() + Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.18") + lineIn + "\"\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            numberOfErrors++;
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
          {
            errorBuffer.append( Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.20") + reader.getLineNumber() + Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.21") + lineIn + "\"\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            numberOfErrors++;
          }
        }
      }
      catch( Exception e )
      {
        errorBuffer.append( Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.23") + reader.getLineNumber() + Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.24") + e.getLocalizedMessage() + "\"\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        numberOfErrors++;
      }
    }
    if( !continueWithErrors && numberOfErrors > MAX_NO_OF_ERRORS )
    {

      MessageBox messageBox = new MessageBox( null, SWT.ICON_QUESTION | SWT.YES | SWT.NO );
      messageBox.setMessage( Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.26") ); //$NON-NLS-1$
      messageBox.setText( Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.27") ); //$NON-NLS-1$
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
    final IAxis dateAxis = new DefaultAxis( Messages.getString("org.kalypso.ogc.sensor.adapter.NativeObservationZrxAdapter.28"), TimeserieConstants.TYPE_DATE, "", Date.class, true ); //$NON-NLS-1$ //$NON-NLS-2$
    final IAxis valueAxis = new DefaultAxis( TimeserieUtils.getName( m_axisTypeValue ), m_axisTypeValue, TimeserieUtils.getUnit( m_axisTypeValue ), Double.class, false );
    final IAxis[] axis = new IAxis[] { dateAxis, valueAxis };
    return axis;
  }
}