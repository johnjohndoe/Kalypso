package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.NoSuchElementException;

import org.jfree.data.AbstractSeriesDataset;
import org.jfree.data.XYDataset;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.ICurve;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;

/**
 * 
 * @author schlienger
 */
class CurveDataset extends AbstractSeriesDataset implements XYDataset
{
  private IAxis m_xAxis = null;
  private IAxis m_yAxis = null;
  private final ICurve m_curve;
  private final ITuppleModel m_values;
  
  public CurveDataset( final ICurve curve ) throws SensorException
  {
    m_curve = curve;
    m_values = m_curve.getObservation().getValues(null);

    IAxisMapping[] mings = curve.getMappings();

    for( int i = 0; i < mings.length; i++ )
    {
      if( mings[i].getDiagramAxis().getDirection().equals( IDiagramAxis.DIRECTION_HORIZONTAL ) )
        m_xAxis = mings[i].getObservationAxis();
      else
        m_yAxis = mings[i].getObservationAxis();
    }
  }

  /**
   * @see org.jfree.data.AbstractSeriesDataset#getSeriesCount()
   */
  public int getSeriesCount()
  {
    return 1;
  }

  /**
   * @see org.jfree.data.AbstractSeriesDataset#getSeriesName(int)
   */
  public String getSeriesName( int series )
  {
    return m_curve.getName();
  }

  /**
   * @see org.jfree.data.XYDataset#getItemCount(int)
   */
  public int getItemCount( int series )
  {
    return m_values.getCount();
  }

  /**
   * @see org.jfree.data.XYDataset#getXValue(int, int)
   */
  public Number getXValue( int series, int item )
  {
    final Object obj = m_values.getElement( item, m_xAxis.getPosition() );
    
    if( obj instanceof Number )
      return (Number)obj;
    
    throw new NoSuchElementException();
  }

  /**
   * @see org.jfree.data.XYDataset#getX(int, int)
   */
  public double getX( int series, int item )
  {
    return getXValue( series, item ).doubleValue();
  }

  /**
   * @see org.jfree.data.XYDataset#getYValue(int, int)
   */
  public Number getYValue( int series, int item )
  {
    final Object obj = m_values.getElement( item, m_yAxis.getPosition() );
    
    if( obj instanceof Number )
      return (Number)obj;
    
    throw new NoSuchElementException();
  }

  /**
   * @see org.jfree.data.XYDataset#getY(int, int)
   */
  public double getY( int series, int item )
  {
    return getYValue( series, item ).doubleValue();
  }
}