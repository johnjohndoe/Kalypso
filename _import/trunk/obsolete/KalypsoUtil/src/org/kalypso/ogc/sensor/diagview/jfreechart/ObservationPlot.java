package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.HashMap;
import java.util.Map;

import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.StandardXYItemRenderer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;

/**
 * @author schlienger
 */
public class ObservationPlot extends XYPlot
{
  /** maps the diagram axis (from the template) to the chart axis */
  private final Map m_diag2chartAxis;

  /** maps the chart axis to its position in the plot */
  private final Map m_chartAxes2Pos;

  /** maps the diagram axes (from the template) to a dataset */
  private final Map m_axes2ds = new HashMap();

  /**
   * Constructor.
   * 
   * @param diag2chartAxis
   *          [required]
   * @param chartAxes2Pos
   *          [required]
   */
  public ObservationPlot( final Map diag2chartAxis, final Map chartAxes2Pos )
  {
    super();

    if( diag2chartAxis == null || chartAxes2Pos == null )
      throw new IllegalArgumentException( "null not allowed" );

    m_diag2chartAxis = diag2chartAxis;
    m_chartAxes2Pos = chartAxes2Pos;

    setRenderer( new StandardXYItemRenderer( StandardXYItemRenderer.LINES ) );
  }

  /**
   * Removes all curves from plot.
   */
  public synchronized void clearCurves()
  {
    for( int i = 0; i < getDatasetCount(); i++ )
      setDataset( i, null );

    m_axes2ds.clear();
  }

  /**
   * Adds a curve to the plot
   */
  public synchronized void addCurve( final IDiagramCurve curve ) throws SensorException
  {
    IAxisMapping[] mings = curve.getMappings();
    IAxis xAxis = null;
    IDiagramAxis xDiagAxis = null;
    IAxis yAxis = null;
    IDiagramAxis yDiagAxis = null;

    for( int i = 0; i < mings.length; i++ )
    {
      if( mings[i].getDiagramAxis().getDirection().equals( IDiagramAxis.DIRECTION_HORIZONTAL ) )
      {
        xAxis = mings[i].getObservationAxis();
        xDiagAxis = mings[i].getDiagramAxis();
      }
      else
      {
        yAxis = mings[i].getObservationAxis();
        yDiagAxis = mings[i].getDiagramAxis();
      }
    }

    if( xAxis == null || yAxis == null || xDiagAxis == null || yDiagAxis == null )
      throw new IllegalArgumentException( "Kann Kurve " + curve
          + " im Diagramm nicht hinizufügen. Die Achsen sind nicht gültig." );

    final XYCurveSerie xyc = new XYCurveSerie( curve, xAxis, yAxis, xDiagAxis, yDiagAxis );

    final String key = xDiagAxis.getIdentifier() + "#-#" + yDiagAxis.getIdentifier();

    CurveDataset cds = (CurveDataset)m_axes2ds.get( key );
    
    if( cds == null )
    {
      cds = new CurveDataset();

      m_axes2ds.put( key, cds );

      int pos = m_axes2ds.values().size();

      setDataset( pos, cds );
      setRenderer( pos, getRenderer() );

      mapDatasetToDomainAxis( pos, ( (Integer)m_chartAxes2Pos
          .get( m_diag2chartAxis.get( xDiagAxis ) ) ).intValue() );
      mapDatasetToRangeAxis( pos, ( (Integer)m_chartAxes2Pos
          .get( m_diag2chartAxis.get( yDiagAxis ) ) ).intValue() );
    }
    
    cds.addCurveSerie( xyc );
  }
}