package org.kalypso.ogc.sensor.jfreechart;

import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.jfree.data.time.FixedMillisecond;
import org.jfree.data.time.TimeSeries;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;

/**
 * A JFreeChart DataSet that can be created with an IObservation.
 * 
 * @author schlienger
 */
public class ObservationTimeSeries extends TimeSeries
{
  public ObservationTimeSeries( IObservation obs, Date from, Date to ) throws SensorException
  {
    super( obs.getName(), FixedMillisecond.class );

    try
    {
      IAxis dateAxis = findAxis( obs, Date.class);
      IAxis valueAxis = findAxis( obs, Number.class );
      
      ITuppleModel model = obs.getValues( from, to );
      
      for( int i = 0; i < model.getCount(); i++ )
      {
        add( new FixedMillisecond( (Date) model.getElement(i, dateAxis.getPosition())),
             (Number)model.getElement( i, valueAxis.getPosition()) );
      }
    }
    catch( NoSuchElementException e )
    {
      throw new SensorException( e );
    }
  }
  
  /**
   * Helper that returns an axis which is compatible with specified Class of data
   */
  private IAxis findAxis( IObservation obs, Class desired ) throws NoSuchElementException
  {
    List axisList = obs.getAxisList();
    
    for( Iterator iter = axisList.iterator(); iter.hasNext(); )
    {
      IAxis axis = (IAxis)iter.next();
      
      if( desired.isAssignableFrom( axis.getDataClass() ) )
        return axis;
    }
    
    throw new NoSuchElementException( "No Date-compatible axis for observation " + obs.getName() );
  }
}
