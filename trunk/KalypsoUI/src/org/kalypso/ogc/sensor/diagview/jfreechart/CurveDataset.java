/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.ArrayList;
import java.util.List;

import org.jfree.data.xy.AbstractIntervalXYDataset;
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
  
  public void removeCurveSerie( final XYCurveSerie xyc )
  {
    synchronized( m_curves )
    {
      if( m_curves.contains( xyc ) )
      {
        m_curves.remove( xyc );
        
        fireDatasetChanged();
      }
    }
  }

  /**
   * @see org.jfree.data.general.SeriesDataset#getSeriesCount()
   */
  public int getSeriesCount( )
  {
    synchronized( m_curves )
    {
      return m_curves.size();
    }
  }

  /**
   * @see org.jfree.data.general.SeriesDataset#getSeriesName(int)
   */
  public String getSeriesName( int series )
  {
    synchronized( m_curves )
    {
      final String name = ((XYCurveSerie) m_curves.get( series )).getName();
      return name;
    }
  }

  /**
   * @see org.jfree.data.xy.XYDataset#getItemCount(int)
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
   * @see org.jfree.data.xy.XYDataset#getXValue(int, int)
   */
  public double getXValue( int series, int item )
  {
    final Number x = getX( series, item );
    
    return x == null ? Double.NaN : x.doubleValue();
  }

  /**
   * @see org.jfree.data.xy.XYDataset#getX(int, int)
   */
  public Number getX( int series, int item )
  {
    synchronized( m_curves )
    {
      try
      {
        final Number value = ((XYCurveSerie) m_curves.get( series )).getXValue( item );
        return value;
      }
      catch( SensorException e )
      {
        e.printStackTrace();
        return null;
      }
    }
  }

  /**
   * @see org.jfree.data.xy.XYDataset#getYValue(int, int)
   */
  public double getYValue( int series, int item )
  {
    final Number y = getY( series, item );
    
    return y == null ? Double.NaN : y.doubleValue();
  }

  /**
   * @see org.jfree.data.xy.XYDataset#getY(int, int)
   */
  public Number getY( int series, int item )
  {
    synchronized( m_curves )
    {
      try
      {
        final Number value = ((XYCurveSerie) m_curves.get( series )).getYValue( item );
        return value;
      }
      catch( SensorException e )
      {
        e.printStackTrace();
        return null;
      }
    }
  }

  /**
   * @see org.jfree.data.xy.IntervalXYDataset#getStartXValue(int, int)
   */
  public double getStartXValue( int series, int item )
  {
    if( item > 0 )
      return getXValue( series, item - 1 );

    return getXValue( series, item );
  }

  /**
   * @see org.jfree.data.xy.IntervalXYDataset#getEndXValue(int, int)
   */
  public double getEndXValue( int series, int item )
  {
    return getXValue( series, item );
  }

  /**
   * @see org.jfree.data.xy.IntervalXYDataset#getStartYValue(int, int)
   */
  public double getStartYValue( int series, int item )
  {
    if( item > 0 )
      return getYValue( series, item - 1 );

    return getYValue( series, item );
  }

  /**
   * @see org.jfree.data.xy.IntervalXYDataset#getEndYValue(int, int)
   */
  public double getEndYValue( int series, int item )
  {
    return getYValue( series, item );
  }

  /**
   * @see org.jfree.data.xy.IntervalXYDataset#getStartX(int, int)
   */
  public Number getStartX( int series, int item )
  {
    if( item > 0 )
      return getX( series, item - 1 );

    return getX( series, item );
  }

  /**
   * @see org.jfree.data.xy.IntervalXYDataset#getEndX(int, int)
   */
  public Number getEndX( int series, int item )
  {
    return getX( series, item );
  }

  /**
   * @see org.jfree.data.xy.IntervalXYDataset#getStartY(int, int)
   */
  public Number getStartY( int series, int item )
  {
    if( item > 0 )
      return getY( series, item - 1 );

    return getY( series, item );
  }

  /**
   * @see org.jfree.data.xy.IntervalXYDataset#getEndY(int, int)
   */
  public Number getEndY( int series, int item )
  {
    return getY( series, item );
  }
}