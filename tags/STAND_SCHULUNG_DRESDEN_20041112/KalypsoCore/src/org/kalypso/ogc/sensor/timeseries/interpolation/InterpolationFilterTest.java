package org.kalypso.ogc.sensor.timeseries.interpolation;

import java.util.Calendar;
import java.util.Date;
import java.util.Vector;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.util.runtime.args.DateRangeArgument;

import junit.framework.TestCase;

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
  protected void setUp( ) throws Exception
  {
    super.setUp();
    
    IAxis[] axes = new IAxis[2];
    axes[0] = new DefaultAxis("date", "date", "", Date.class, 0, true );
    axes[1] = new DefaultAxis("value", "W", "cm", Double.class, 1, false );
    
    m_from = CAL.getTime();
    
    SimpleTuppleModel model = new SimpleTuppleModel(axes);
    for( int i = 0; i < 10*24*4; i++ )
    {
      Vector tupple = new Vector();
      tupple.add( CAL.getTime() );
      tupple.add( new Double( Math.random() * 10 ) );
      
      model.addTupple( tupple );
      
      CAL.add( Calendar.MINUTE, 15 );
    }
    
    m_to = CAL.getTime();
    
    m_obs = new SimpleObservation("", "", true, null, null, axes );
    m_obs.setValues( model );
  }
  
  public void testGetValues() throws SensorException
  {
    InterpolationFilter filter = new InterpolationFilter( Calendar.HOUR_OF_DAY, 1, true, -1 );
    
    filter.initFilter( null, m_obs );
    
    CAL.setTime( m_to );
    CAL.add( Calendar.DAY_OF_MONTH, 5 );
    
    final ITuppleModel values = filter.getValues( new DateRangeArgument( m_from, CAL.getTime() ) );
    
    System.out.println( ObservationUtilities.dump( values, "\t" ) );
  }
}
