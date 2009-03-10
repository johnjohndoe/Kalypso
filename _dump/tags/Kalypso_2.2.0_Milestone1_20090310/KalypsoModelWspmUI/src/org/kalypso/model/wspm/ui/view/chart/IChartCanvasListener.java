package org.kalypso.model.wspm.ui.view.chart;

/**
 * Changes in {@link de.belger.swtchart.ChartCanvas} are propagated to this listeners.
 * 
 * @author gernot
 * @see de.belger.swtchart.ChartCanvas
 */
public interface IChartCanvasListener
{
  /** A layer was added/removed */
  void onLayersChanged( );
}
