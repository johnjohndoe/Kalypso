package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.Date;
import java.util.NoSuchElementException;

import org.jfree.data.time.FixedMillisecond;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * A JFreeChart DataSet that can be created with an IObservation.
 * 
 * @author schlienger
 */
public class ObservationTimeSeries extends TimeSeriesCollection
{
  public ObservationTimeSeries( final IObservation obs, final IVariableArguments args ) throws SensorException
  {
    this( obs, ObservationUtilities.findAxis( obs, Date.class )[0], ObservationUtilities.findAxis( obs, Number.class ), args );
  }
  
  public ObservationTimeSeries( final IObservation obs,  final IAxis dateAxis, final IAxis[] valueAxis, final IVariableArguments args ) throws SensorException
  {
    try
    {
      for( int i = 0; i < valueAxis.length; i++ )
      {
        if( !KalypsoStatusUtils.isStatusAxis( valueAxis[i] ) )
        {
          TimeSeries s = new TimeSeries( valueAxis[i].getLabel(), FixedMillisecond.class );
  
          ITuppleModel model = obs.getValues( args );
  
          for( int j = 0; j < model.getCount(); j++ )
          {
            s.addOrUpdate( new FixedMillisecond( (Date)model.getElement( j, dateAxis.getPosition() ) ),
                ((Number)model.getElement( j, valueAxis[i].getPosition() ) ).doubleValue() );
          }
  
          addSeries( s );
        }
      }
    }
    catch( NoSuchElementException e )
    {
      throw new SensorException( e );
    }
  }
  
}