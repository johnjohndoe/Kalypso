package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.lang.reflect.InvocationTargetException;

import javax.swing.SwingUtilities;

import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardLegend;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * @author schlienger
 */
public class ObservationChart extends JFreeChart implements
    ITemplateEventListener
{
  /**
   * Creates an ObservationChart
   * 
   * @param template
   * @throws SensorException
   */
  public ObservationChart( final IDiagramTemplate template )
      throws SensorException
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
   * Clears the curves in the chart
   */
  protected void clearChart( )
  {
    ((ObservationPlot) getPlot()).clearCurves();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( final TemplateEvent evt )
  {
    final Runnable runnable = new Runnable()
    {
      public void run( )
      {
        try
        {
          if( evt.getType() == TemplateEvent.TYPE_ADD
              && evt.getObject() instanceof IDiagramCurve )
            ((ObservationPlot) getPlot()).addCurve( (IDiagramCurve) evt
                .getObject() );

//          if( evt.getType() == TemplateEvent.TYPE_REFRESH && 
//              evt.getObject() instanceof Collection )
//          {
//            clearChart();
//
//            final Iterator itThemes = ((Collection) evt.getObject()).iterator();
//            while( itThemes.hasNext() )
//            {
//              final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) itThemes
//                  .next();
//              final Iterator it = theme.getCurves().iterator();
//              while( it.hasNext() )
//                ((ObservationPlot) getPlot()).addCurve( (IDiagramCurve) it
//                    .next() );
//            }
//          }

          if( evt.getType() == TemplateEvent.TYPE_REMOVE_ALL )
            clearChart();
        }
        catch( SensorException e )
        {
          throw new RuntimeException( e );
        }
      }
    };
    
    try
    {
      SwingUtilities.invokeAndWait( runnable );
    }
    catch( InterruptedException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( InvocationTargetException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }
}