package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.Collection;
import java.util.Iterator;

import javax.swing.SwingUtilities;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.internal.Workbench;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardLegend;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
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
   * Disposes the chart.
   */
  public void dispose( )
  {
    clearChart();
  }

  /**
   * Clears the curves in the chart
   */
  protected void clearChart( )
  {
    getObservationPlot().clearCurves();
  }
  
  /**
   * @return plot casted as obs plot
   */
  public ObservationPlot getObservationPlot()
  {
    return (ObservationPlot) getPlot();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( final TemplateEvent evt )
  {   
    final CatchRunnable runnable = new CatchRunnable()
    {
      protected void runIntern( ) throws Throwable
      {
        final ObservationPlot obsPlot = getObservationPlot();
        
        // ADD A CURVE
        if( evt.isType( TemplateEvent.TYPE_ADD )
            && evt.getObject() instanceof IDiagramCurve )
        {
          obsPlot.addCurve( (IDiagramCurve) evt
              .getObject() );
        }

        // REMOVE A CURVE
        if( evt.isType( TemplateEvent.TYPE_REMOVE )
            && evt.getObject() instanceof IDiagramCurve )
        {
          obsPlot.removeCurve( (IDiagramCurve) evt
              .getObject() );
        }

        // REFRESH LIST OF THEMES
        if( evt.getType() == TemplateEvent.TYPE_REFRESH
            && evt.getObject() instanceof Collection )
        {
          clearChart();

          final Iterator itThemes = ((Collection) evt.getObject()).iterator();
          while( itThemes.hasNext() )
          {
            final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) itThemes
                .next();
            final Iterator it = theme.getCurves().iterator();
            while( it.hasNext() )
              obsPlot
                  .addCurve( (IDiagramCurve) it.next() );
          }
          
          fireChartChanged();
        }
        
        // REFRESH ONE THEME
        if( evt.getType() == TemplateEvent.TYPE_REFRESH
            && evt.getObject() instanceof IDiagramTemplateTheme )
        {
          final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) evt.getObject();
          final Iterator it = theme.getCurves().iterator();
          while( it.hasNext() )
          {
            final IDiagramCurve crv = (IDiagramCurve) it.next();
            obsPlot.removeCurve( crv );
            obsPlot.addCurve( crv );
          }
          
          fireChartChanged();
        }

        // REMOVE ALL THEMES
        if( evt.getType() == TemplateEvent.TYPE_REMOVE_ALL )
          clearChart();
      }
    };

    try
    {
      if( !SwingUtilities.isEventDispatchThread() )
        SwingUtilities.invokeAndWait( runnable );
      else
        runnable.run();
      
      if( runnable.getThrown() != null )
        throw runnable.getThrown();
    }
    catch( Throwable e )
    {
      // TODO: hier kann ne Nullpointer exception geben (es gibt nicht immer ein activeWokbenchWindow)
      MessageDialog.openError( Workbench.getInstance()
          .getActiveWorkbenchWindow().getShell(), "Aktualisierungsfehler", e
          .toString() );
    }
  }
}