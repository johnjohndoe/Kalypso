package org.kalypso.ogc.sensor.view;

import java.util.Calendar;
import java.util.Date;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.jfree.data.time.TimeSeriesCollection;
import org.kalypso.ogc.sensor.DateRangeArgument;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationTimeSeries;
import org.kalypso.plugin.KalypsoGisPlugin;

/**
 * Job that takes care of fetching data and updating the dataset for the jfreechart.
 * 
 * @author schlienger
 */
public class ShowObservationInDiagramJob extends Job
{
  private final IObservation m_obs;
  private final TimeSeriesCollection m_tsCol;
  
  public ShowObservationInDiagramJob( final TimeSeriesCollection tsCol, final IObservation obs )
  {
    super( "Diagram QuickView Update" );
  
    m_tsCol = tsCol;
    m_obs = obs;
    
    setPriority( Job.SHORT );
  }
  
  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  protected IStatus run( IProgressMonitor monitor )
  {
    Calendar c = Calendar.getInstance();

    Date to = c.getTime();
    c.add( Calendar.DAY_OF_YEAR, -31 );
    Date from = c.getTime();

    try
    {
      m_tsCol.addSeries( new ObservationTimeSeries(m_obs, new DateRangeArgument( from, to ) ) );
    }
    catch( SensorException e )
    {
      return new Status( IStatus.ERROR,
          KalypsoGisPlugin.getDefault().getBundle().getSymbolicName(), 0,
          "Fehler während die Aktualisierung des Diagramms", e );
    }
    
    return Status.OK_STATUS;
  }
}
