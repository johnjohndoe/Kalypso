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
package org.kalypso.ogc.sensor.timeseries.interpolation;

import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;

/**
 * InterpolationFilterTest
 * 
 * @author schlienger
 */
public class InterpolationFilterTest extends TestCase
{
  private final static SimpleDateFormat sdf = new SimpleDateFormat( "yyyy-MM-dd HH:mm:ss" ); //$NON-NLS-1$

  private IObservation m_obs;

  private IAxis m_dateAxis;

  private IAxis m_valueAxis;

  @Override
  protected void setUp( ) throws Exception
  {
    super.setUp();

    final URL url = InterpolationFilterTest.class.getResource( "InterpolationFilterTest.zml" ); //$NON-NLS-1$
    m_obs = ZmlFactory.parseXML( url, "" ); //$NON-NLS-1$
    assertNotNull( m_obs );

    m_dateAxis = ObservationUtilities.findAxisByClass( m_obs.getAxisList(), Date.class );
    assertNotNull( m_dateAxis );
    m_valueAxis = KalypsoStatusUtils.findAxisByClass( m_obs.getAxisList(), Double.class, true );
    assertNotNull( m_valueAxis );
  }

  public void testGetValues( ) throws SensorException, ParseException
  {
    final InterpolationFilter filter = new InterpolationFilter( Calendar.HOUR_OF_DAY, 1, true, "0", 0, true );
    filter.initFilter( null, m_obs, null );

    sdf.setTimeZone( TimeZone.getTimeZone( "UTC" ) );

    // test with same date-range
    final ITuppleModel m1 = filter.getValues( null );
    verifyTuppleModel( m1, sdf.parse( "2004-11-23 13:00:00" ), sdf.parse( "2004-11-25 13:00:00" ), new Double( 60.0 ), new Double( 37.0 ) ); //$NON-NLS-1$ //$NON-NLS-2$

    // test with bigger date-range
    final Date from2 = sdf.parse( "2004-11-23 10:00:00" ); //$NON-NLS-1$
    final Date to2 = sdf.parse( "2004-11-25 17:00:00" ); //$NON-NLS-1$
    final ITuppleModel m2 = filter.getValues( new ObservationRequest( new DateRange( from2, to2 ) ) );
    verifyTuppleModel( m2, from2, to2, new Double( 0 ), new Double( 37.0 ) );

    // test with smaller date-range
    final Date from3 = sdf.parse( "2004-11-23 19:00:00" ); //$NON-NLS-1$
    final Date to3 = sdf.parse( "2004-11-25 11:00:00" ); //$NON-NLS-1$
    final ITuppleModel m3 = filter.getValues( new ObservationRequest( new DateRange( from3, to3 ) ) );
    verifyTuppleModel( m3, from3, to3, new Double( 55 ), new Double( 37.0 ) );
  }

  private void verifyTuppleModel( final ITuppleModel m, final Date from, final Date to, final Double firstValue, final Double lastValue ) throws SensorException
  {
    final Calendar cal = Calendar.getInstance();
    cal.setTime( from );
    int i = 0;
    while( cal.getTime().before( to ) )
    {
      i++;
      cal.add( Calendar.HOUR_OF_DAY, 1 );
    }
    i++;

    assertEquals( i, m.getCount() );

    assertEquals( from, m.getElement( 0, m_dateAxis ) );
    assertEquals( firstValue.doubleValue(), ((Number) m.getElement( 0, m_valueAxis )).doubleValue(), 0.001 );
    assertEquals( to, m.getElement( m.getCount() - 1, m_dateAxis ) );
    assertEquals( lastValue.doubleValue(), ((Number) m.getElement( m.getCount() - 1, m_valueAxis )).doubleValue(), 0.001 );
  }
  
    /** Bugfix test: the second value was never interpolatied but taken from the first value. */
  public void testSecondValueProblem() throws SensorException
  {
    final URL url = InterpolationFilterTest.class.getResource( "resources/Nienhagen.zml" );
    final IObservation obs = ZmlFactory.parseXML( url, "" );
    final ITuppleModel orgValues = obs.getValues( null );

    //    System.out.println( "Original: " );
    //    System.out.println( ObservationUtilities.dump( orgValues, "\t" ) );
    //    System.out.println();
    //    System.out.println();

    final int timeUnit = Calendar.HOUR_OF_DAY; //WiskiUtils.getDistUnitCalendarField( m_tsinfo.getWiskiDistUnit() );
    final int timeStep = 1;//m_tsinfo.getWiskiDistValue();

    final InterpolationFilter intfil = new InterpolationFilter( timeUnit, timeStep, false, "0",
        KalypsoStati.STATUS_USERMOD.intValue() );
    intfil.initFilter( null, obs, null );
    final ITuppleModel intValues = intfil.getValues( null );

    //    System.out.println( "Interpolated: " );
    //    System.out.println( ObservationUtilities.dump( intValues, "\t" ) );

    final int orgCount = orgValues.getCount();
    final int intCount = intValues.getCount();

    assertEquals( "Length of timersieries must not change", orgCount, intCount );

    final IAxis orgValueAxis = ObservationUtilities.findAxisByClass( obs.getAxisList(), Double.class );
    final IAxis intValueAxis = ObservationUtilities.findAxisByClass( intfil.getAxisList(), Double.class );

    final IAxis orgDateAxis = ObservationUtilities.findAxisByClass( obs.getAxisList(), Date.class );
    final IAxis intDateAxis = ObservationUtilities.findAxisByClass( intfil.getAxisList(), Date.class );

    for( int i = 0; i < orgCount; i++ )
    {
      final Date orgDate = (Date)orgValues.getElement( i, orgDateAxis );
      final Date intDate = (Date)intValues.getElement( i, intDateAxis );

      assertEquals( "Dates are not equal at index " + i, orgDate, intDate );

      final Double orgValue = (Double)orgValues.getElement( i, orgValueAxis );
      final Double intValue = (Double)intValues.getElement( i, intValueAxis );

      // test with equals, because we know that we dont have really iterpolated
      assertEquals( "Values are not equal at index " + i, orgValue, intValue );
    }
  }

  public void testStatusAfterBadStatusProblem() throws SensorException
  {
    final URL url = InterpolationFilterTest.class.getResource( "resources/KalteBode.zml" );
    final IObservation obs = ZmlFactory.parseXML( url, "" );
    //    final ITuppleModel orgValues = obs.getValues( null );

    //    System.out.println( "Original: " );
    //    System.out.println( ObservationUtilities.dump( orgValues, "\t" ) );
    //    System.out.println();
    //    System.out.println();

    final int timeUnit = Calendar.HOUR_OF_DAY; //WiskiUtils.getDistUnitCalendarField( m_tsinfo.getWiskiDistUnit() );
    final int timeStep = 1;//m_tsinfo.getWiskiDistValue();

    final InterpolationFilter intfil = new InterpolationFilter( timeUnit, timeStep, false, "0",
        KalypsoStati.STATUS_USERMOD.intValue() );
    intfil.initFilter( null, obs, null );
    final ITuppleModel intValues = intfil.getValues( null );

    //    System.out.println( "Interpolated: " );
    //    System.out.println( ObservationUtilities.dump( intValues, "\t" ) );

    final InterpolationFilter int2fil = new InterpolationFilter( timeUnit, timeStep, false, "0",
        KalypsoStati.STATUS_USERMOD.intValue() );
    int2fil.initFilter( null, intfil, null );
    final ITuppleModel int2Values = int2fil.getValues( null );

    //    System.out.println( "Interpolated (2. time): " );
    //    System.out.println( ObservationUtilities.dump( int2Values, "\t" ) );

    final int intCount = intValues.getCount();
    final int int2Count = int2Values.getCount();

    assertEquals( "Length of timeseries must not change", intCount, int2Count );

    final IAxis intValueAxis = ObservationUtilities.findAxisByClass( intfil.getAxisList(), Double.class );
    final IAxis int2ValueAxis = ObservationUtilities.findAxisByClass( int2fil.getAxisList(), Double.class );

    final IAxis intDateAxis = ObservationUtilities.findAxisByClass( intfil.getAxisList(), Date.class );
    final IAxis int2DateAxis = ObservationUtilities.findAxisByClass( int2fil.getAxisList(), Date.class );

    final IAxis intStatusAxis = ObservationUtilities.findAxisByName( intfil.getAxisList(), "_kalypso_status_Volumen" );
    final IAxis int2StatusAxis = ObservationUtilities.findAxisByName( int2fil.getAxisList(), "_kalypso_status_Volumen" );

    for( int i = 0; i < intCount; i++ )
    {
      final Date intDate = (Date)intValues.getElement( i, intDateAxis );
      final Date int2Date = (Date)int2Values.getElement( i, int2DateAxis );

      assertEquals( "Dates are not equal at index " + i, intDate, int2Date );

      final Double intValue = (Double)intValues.getElement( i, intValueAxis );
      final Double int2Value = (Double)int2Values.getElement( i, int2ValueAxis );

      assertEquals( "Values are not equal at index " + i, intValue, int2Value );

      final Number intStatus = (Number)intValues.getElement( i, intStatusAxis );
      final Number int2Status = (Number)int2Values.getElement( i, int2StatusAxis );

      assertEquals( "Stati are not equal at index " + i, intStatus, int2Status );
    }
  }
}
