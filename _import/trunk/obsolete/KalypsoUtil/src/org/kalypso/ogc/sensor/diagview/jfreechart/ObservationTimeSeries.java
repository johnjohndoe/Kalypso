package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.Date;
import java.util.NoSuchElementException;

import org.jfree.data.time.FixedMillisecond;
import org.jfree.data.time.TimeSeries;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * A JFreeChart DataSet that can be created with an IObservation.
 * 
 * @author schlienger
 */
public class ObservationTimeSeries extends TimeSeries
{
  public ObservationTimeSeries( IObservation obs, IVariableArguments args ) throws SensorException
  {
    super( obs.getName(), FixedMillisecond.class );

    try
    {
      IAxis dateAxis = ObservationUtilities.findAxis( obs, Date.class);
      IAxis valueAxis = ObservationUtilities.findAxis( obs, Number.class );
      
      ITuppleModel model = obs.getValues( args );
      
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
}
