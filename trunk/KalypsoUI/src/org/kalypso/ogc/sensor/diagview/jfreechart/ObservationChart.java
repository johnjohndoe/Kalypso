package org.kalypso.ogc.sensor.diagview.jfreechart;

import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardLegend;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.util.factory.FactoryException;

/**
 * @author schlienger
 */
public class ObservationChart extends JFreeChart implements ITemplateEventListener
{
  /**
   * Creates an ObservationChart
   * @param template
   * 
   * @throws FactoryException
   */
  public ObservationChart( final IDiagramTemplate template ) throws FactoryException
  {
    super( template.getTitle(), JFreeChart.DEFAULT_TITLE_FONT, ChartFactory
        .createObservationPlot( template ), template.isShowLegend() );
    
    if( template.isShowLegend() )
    {
      final StandardLegend leg = new StandardLegend();
      leg.setTitle( template.getLegendName() );
      
      setLegend( leg );
    }
  }

  /**
   *  Clears the curves in the chart
   */
  public void clearChart()
  {
    ( (ObservationPlot)getPlot() ).clearCurves();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( final TemplateEvent evt )
  {
    try
    {
      if( evt.getType() == TemplateEvent.TYPE_ADD && evt.getObject() instanceof IDiagramCurve )
        ( (ObservationPlot)getPlot() ).addCurve( (IDiagramCurve)evt.getObject() );
      
      if( evt.getType() == TemplateEvent.TYPE_REMOVE_ALL )
        clearChart();
    }
    catch( SensorException e )
    {
      throw new RuntimeException( e );
    }
  }
}