package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.ArrayList;
import java.util.List;

import org.jfree.data.AbstractIntervalXYDataset;
import org.kalypso.ogc.sensor.SensorException;

/**
 * TODO: inserted synchronized at some places because if pages are switched too fast in kalypso wizzard
 * then I presume that many swing ui threads are trying to update the chart, thus leading to
 * possible array out of bound exceptions because of concurrent accesses.
 * 
 * @author schlienger
 */
class CurveDataset extends AbstractIntervalXYDataset
{
  private final List m_curves = new ArrayList();

  public CurveDataset( )
  {
    // empty
  }

  public void addCurveSerie( final XYCurveSerie xyc )
  {
    synchronized( m_curves )
    {
      m_curves.add( xyc );

      fireDatasetChanged();
    }
  }

  /**
   * @see org.jfree.data.AbstractSeriesDataset#getSeriesCount()
   */
  public int getSeriesCount( )
  {
    synchronized( m_curves )
    {
      return m_curves.size();
    }
  }

  /**
   * @see org.jfree.data.AbstractSeriesDataset#getSeriesName(int)
   */
  public String getSeriesName( int series )
  {
    synchronized( m_curves )
    {
      return ((XYCurveSerie) m_curves.get( series )).getName();
    }
  }

  /**
   * @see org.jfree.data.XYDataset#getItemCount(int)
   */
  public int getItemCount( int series )
  {
    synchronized( m_curves )
    {
      try
      {
        return ((XYCurveSerie) m_curves.get( series )).getItemCount();
      }
      catch( SensorException e )
      {
        e.printStackTrace();
        return 0;
      }
    }
  }

  /**
   * @see org.jfree.data.XYDataset#getXValue(int, int)
   */
  public Number getXValue( int series, int item )
  {
    synchronized( m_curves )
    {
      try
      {
        return ((XYCurveSerie) m_curves.get( series )).getXValue( item );
      }
      catch( SensorException e )
      {
        e.printStackTrace();
        return new Integer( 0 );
      }
    }
  }

  /**
   * @see org.jfree.data.XYDataset#getX(int, int)
   */
  public double getX( int series, int item )
  {
    Number value = getXValue( series, item );
    return value == null ? Double.NaN : value.doubleValue();
  }

  /**
   * @see org.jfree.data.XYDataset#getYValue(int, int)
   */
  public Number getYValue( int series, int item )
  {
    try
    {
      return ((XYCurveSerie) m_curves.get( series )).getYValue( item );
    }
    catch( SensorException e )
    {
      e.printStackTrace();
      return new Integer( 0 );
    }
  }

  /**
   * @see org.jfree.data.XYDataset#getY(int, int)
   */
  public double getY( int series, int item )
  {
    Number value = getYValue( series, item );
    return value == null ? Double.NaN : value.doubleValue();
  }

  /**
   * @see org.jfree.data.IntervalXYDataset#getStartXValue(int, int)
   */
  public Number getStartXValue( int series, int item )
  {
    if( item > 0 )
      return getXValue( series, item - 1 );

    return getXValue( series, item );
  }

  /**
   * @see org.jfree.data.IntervalXYDataset#getEndXValue(int, int)
   */
  public Number getEndXValue( int series, int item )
  {
    return getXValue( series, item );
  }

  /**
   * @see org.jfree.data.IntervalXYDataset#getStartYValue(int, int)
   */
  public Number getStartYValue( int series, int item )
  {
    if( item > 0 )
      return getYValue( series, item - 1 );

    return getYValue( series, item );
  }

  /**
   * @see org.jfree.data.IntervalXYDataset#getEndYValue(int, int)
   */
  public Number getEndYValue( int series, int item )
  {
    return getYValue( series, item );
  }
}