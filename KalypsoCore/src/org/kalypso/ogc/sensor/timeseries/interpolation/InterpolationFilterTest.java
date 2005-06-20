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

import java.util.Calendar;
import java.util.Date;
import java.util.Vector;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * InterpolationFilterTest
 * 
 * @author schlienger
 */
public class InterpolationFilterTest extends TestCase
{
  private final static Calendar CAL = Calendar.getInstance();

  private SimpleObservation m_obs;

  private Date m_from;

  private Date m_to;

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    super.setUp();

    IAxis[] axes = new IAxis[2];
    axes[0] = new DefaultAxis( "date", "date", "", Date.class, true );
    axes[1] = new DefaultAxis( "value", "W", "cm", Double.class, false );

    m_from = CAL.getTime();

    SimpleTuppleModel model = new SimpleTuppleModel( axes );
    for( int i = 0; i < 10 * 24 * 4; i++ )
    {
      Vector tupple = new Vector();
      tupple.add( CAL.getTime() );
      tupple.add( new Double( Math.random() * 10 ) );

      model.addTupple( tupple );

      CAL.add( Calendar.MINUTE, 15 );
    }

    m_to = CAL.getTime();

    m_obs = new SimpleObservation( "", "", "", true, null, null, axes );
    m_obs.setValues( model );
  }

  public void testGetValues() throws SensorException
  {
    InterpolationFilter filter = new InterpolationFilter( Calendar.HOUR_OF_DAY, 1, true, -1, 0 );

    filter.initFilter( null, m_obs, null );

    CAL.setTime( m_to );
    CAL.add( Calendar.DAY_OF_MONTH, 5 );

    final ITuppleModel values = filter.getValues( new DateRangeArgument( m_from, CAL.getTime() ) );

    System.out.println( ObservationUtilities.dump( values, "\t" ) );
  }
}
