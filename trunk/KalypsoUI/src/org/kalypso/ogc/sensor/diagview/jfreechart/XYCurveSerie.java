package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.Date;

import org.jfree.data.Series;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;

/**
 * A CurveSerie.
 * 
 * @author schlienger
 */
class XYCurveSerie extends Series
{
  private transient final IAxis m_xAxis;

  private transient final IDiagramAxis m_xDiagAxis;

  private transient final IAxis m_yAxis;

  private transient final IDiagramAxis m_yDiagAxis;

  private transient final IDiagramCurve m_curve;

  private transient ITuppleModel m_values = null;

  /**
   * Constructor. Fetches the values (ITuppleModel).
   * 
   * @param curve
   * @param xAxis
   *          the IAxis from the IObservation to be used as X-Axis.
   * @param yAxis
   *          the IAxis from the IObservation to be used as Y-Axis.
   * @param xDiagAxis
   *          the IDiagramAxis mapped to xAxis
   * @param yDiagAxis
   *          the IDiagramAxis mapped to yAxis
   */
  public XYCurveSerie( final IDiagramCurve curve, final IAxis xAxis, final IAxis yAxis,
      final IDiagramAxis xDiagAxis, final IDiagramAxis yDiagAxis ) throws SensorException
  {
    super( curve.getName() );

    m_curve = curve;
    m_xAxis = xAxis;
    m_yAxis = yAxis;
    m_xDiagAxis = xDiagAxis;
    m_yDiagAxis = yDiagAxis;

    m_values = m_curve.getObservation().getValues( curve.getArguments() );
  }

  public IDiagramAxis getXDiagAxis()
  {
    return m_xDiagAxis;
  }

  public IDiagramAxis getYDiagAxis()
  {
    return m_yDiagAxis;
  }

  public int getItemCount() throws SensorException
  {
    return m_values.getCount();
  }

  public Number getXValue( int item ) throws SensorException
  {
    final Object obj = m_values.getElement( item, m_xAxis );

    if( obj instanceof Number )
      return (Number)obj;
    else if( obj instanceof Date )
      return new Double( ( (Date)obj ).getTime() );

    return null;
  }

  public Number getYValue( int item ) throws SensorException
  {
    final Object obj = m_values.getElement( item, m_yAxis );

    if( obj instanceof Number )
      return (Number)obj;

    return null;
  }
}