package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.StandardXYItemRenderer;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;

/**
 * @author schlienger
 */
public class ObservationPlot extends XYPlot
{
  private final Map m_diag2chartAxis;

  private final Map m_chartAxes2Pos;

  private final List m_curves = new ArrayList();

  public ObservationPlot( final Map diag2chartAxis, final Map chartAxes2Pos )
  {
    super();

    m_diag2chartAxis = diag2chartAxis;
    m_chartAxes2Pos = chartAxes2Pos;

    setRenderer( new StandardXYItemRenderer( StandardXYItemRenderer.LINES ) );
  }

  /**
   * Removes all curves from plot.
   */
  public void clearCurves()
  {
    m_curves.clear();

    for( int i = 0; i < getDatasetCount(); i++ )
      setDataset( i, null );
  }

  /**
   * Adds a curve to the plot
   */
  public void addCurve( final IDiagramCurve curve ) throws SensorException
  {
    System.out.println( "Add curve: " + curve );
    
    m_curves.add( curve );

    int pos = m_curves.size();

    CurveDataset cds = new CurveDataset( curve );

    setDataset( pos, cds );

    mapDatasetToDomainAxis( pos, ( (Integer)m_chartAxes2Pos.get( m_diag2chartAxis.get( cds
        .getXDiagAxis() ) ) ).intValue() );
    mapDatasetToRangeAxis( pos, ( (Integer)m_chartAxes2Pos.get( m_diag2chartAxis.get( cds
        .getYDiagAxis() ) ) ).intValue() );
  }
}