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
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.status.MaskedNumber;

/**
 * A JFreeChart DataSet that can be created with an IObservation.
 * 
 * @author schlienger
 */
public class ObservationTimeSeries extends TimeSeriesCollection
{
  public ObservationTimeSeries( IObservation obs, IVariableArguments args ) throws SensorException
  {
    super();

    IAxis dateAxis = ObservationUtilities.findAxis( obs, Date.class )[0];
    IAxis[] valueAxis = ObservationUtilities.findAxis( obs, MaskedNumber.class );

    try
    {
      for( int i = 0; i < valueAxis.length; i++ )
      {
        TimeSeries s = new TimeSeries( valueAxis[i].getLabel(), FixedMillisecond.class );

        ITuppleModel model = obs.getValues( args );

        for( int j = 0; j < model.getCount(); j++ )
        {
          s.addOrUpdate( new FixedMillisecond( (Date)model.getElement( j, dateAxis.getPosition() ) ),
              ((MaskedNumber)model.getElement( j, valueAxis[i].getPosition() ) ).doubleValue() );
        }

        addSeries( s );
      }
    }
    catch( NoSuchElementException e )
    {
      throw new SensorException( e );
    }
  }
}