package org.kalypso.model.wspm.ui.view.chart.handler;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Point;

import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ITooltipChartLayer;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * handler to manage tooltips and clickinfo; registers on creation, deregisters on disposation
 *
 * @author kimwerner
 */
public class ProfilClickHandler implements MouseMoveListener
{
  private final ChartComposite m_chart;

  public ProfilClickHandler( final ChartComposite chart )
  {
    m_chart = chart;

    chart.getPlot().addMouseMoveListener( this );
  }

  public void dispose( )
  {
    if( !m_chart.isDisposed() )
      m_chart.removeMouseMoveListener( this );
  }

  /**
   * @see org.eclipse.swt.events.MouseMoveListener#mouseMove(org.eclipse.swt.events.MouseEvent)
   */
  public void mouseMove( final MouseEvent e )
  {
    if( m_chart.getPlot().isEditing() )
    {
      m_chart.getPlot().redraw();
    }
    else if( !m_chart.getPlot().isDragging() )
    {
      final Point point = new Point( e.x, e.y );

      final ITooltipChartLayer[] tooltipLayers = m_chart.getChartModel().getLayerManager().getTooltipLayers();
      // Array umdrehen, damit die oberen Layer zuerst befragt werden
      ArrayUtils.reverse( tooltipLayers );

      for( final ITooltipChartLayer layer : tooltipLayers )
        if( layer.isVisible() )
        {
          final EditInfo info = layer.getHover( point );
          if( info != null )
          {
            //info.m_pos = point;
            m_chart.getPlot().setTooltipInfo( info );
            m_chart.getPlot().redraw();
            return;
          }
        }

      m_chart.getPlot().setTooltipInfo( null );
      m_chart.getPlot().redraw();
    }
  }
}
