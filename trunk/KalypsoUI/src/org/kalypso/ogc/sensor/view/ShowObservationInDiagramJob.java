package org.kalypso.ogc.sensor.view;

import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.kalypso.ogc.sensor.DateRangeArgument;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationTimeSeries;

/**
 * Job that takes care of fetching data and updating the dataset for the jfreechart.
 * 
 * @author schlienger
 */
public class ShowObservationInDiagramJob implements Runnable
{
  private final IObservation m_obs;
  private final TimeSeriesCollection m_tsCol;
  
  public ShowObservationInDiagramJob( final TimeSeriesCollection tsCol, final IObservation obs )
  {
    m_tsCol = tsCol;
    m_obs = obs;
  }
  
  /**
   * @see java.lang.Runnable#run()
   */
  public void run()
  {
    Calendar c = Calendar.getInstance();

    Date to = c.getTime();
    c.add( Calendar.DAY_OF_YEAR, -31 );
    Date from = c.getTime();

    try
    {
      // TODO: ok so mit der Collection von Series?
      List series = new ObservationTimeSeries(m_obs, new DateRangeArgument( from, to ) ).getSeries();
      
      for( Iterator it = series.iterator(); it.hasNext(); )
      {
        TimeSeries s = (TimeSeries)it.next();
      
        m_tsCol.addSeries( s );
      }
    }
    catch( SensorException e )
    {
      throw new RuntimeException( "Fehler während die Aktualisierung des Diagramms", e );
    }
  }
}
