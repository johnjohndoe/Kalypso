package org.kalypso.ogc.sensor.diagview.jfreechart;

import org.jfree.chart.plot.Plot;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;

/**
 * @author schlienger
 */
public final class ChartFactory
{
  /**
   * Creates and returns an observation chart.
   * 
   * @param template
   * @return plot
   * @throws SensorException
   */
  static Plot createObservationPlot( final IDiagramTemplate template ) throws SensorException
  {
    synchronized( template )
    {
      final ObservationPlot plot = new ObservationPlot( template );

      return plot;
    }
  }
}