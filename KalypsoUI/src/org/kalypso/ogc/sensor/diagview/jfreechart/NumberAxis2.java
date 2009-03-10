/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.diagview.jfreechart;

import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.ValueAxisPlot;
import org.jfree.data.Range;

/**
 * @author schlienger
 */
public class NumberAxis2 extends NumberAxis
{
  private Double m_max = null;
  private Double m_min = null;

  public NumberAxis2()
  {
    super();
  }

  public NumberAxis2( String label )
  {
    super( label );
  }

  void setMin( final Double min )
  {
    m_min = min;
  }
  
  void setMax( final Double max )
  {
    m_max = max;
  }
  
  /**
   * Rescales the axis to ensure that all data is visible.
   */
  protected void autoAdjustRange()
  {
    Plot plot = getPlot();
    if( plot == null )
    {
      return; // no plot, no data
    }

    if( plot instanceof ValueAxisPlot )
    {
      ValueAxisPlot vap = (ValueAxisPlot)plot;

      Range r = vap.getDataRange( this );
      if( r == null )
      {
        r = new Range( DEFAULT_LOWER_BOUND, DEFAULT_UPPER_BOUND );
      }

      double upper = r.getUpperBound();
      double lower = r.getLowerBound();
      if( autoRangeIncludesZero() )
      {
        lower = Math.min( lower, 0.0 );
        upper = Math.max( upper, 0.0 );
      }
      double range = upper - lower;

      // if fixed auto range, then derive lower bound...
      double fixedAutoRange = getFixedAutoRange();
      if( fixedAutoRange > 0.0 )
      {
        lower = upper - fixedAutoRange;
      }
      else
      {
        // ensure the autorange is at least <minRange> in size...
        double minRange = getAutoRangeMinimumSize();
        if( range < minRange )
        {
          //double expand = ( minRange - range ) / 2;
          upper = minRange; //upper + expand;
          lower = 0; //getlower - expand;
        }

        if( autoRangeStickyZero() )
        {
          if( upper <= 0.0 )
          {
            upper = Math.min( 0.0, upper + getUpperMargin() * range );
          }
          else
          {
            upper = upper + getUpperMargin() * range;
          }
          if( lower >= 0.0 )
          {
            lower = Math.max( 0.0, lower - getLowerMargin() * range );
          }
          else
          {
            lower = lower - getLowerMargin() * range;
          }
        }
        else
        {
          upper = upper + getUpperMargin() * range;
          lower = lower - getLowerMargin() * range;
        }
      }

      if( m_min!= null && lower > m_min.doubleValue() )
        lower = m_min.doubleValue();
      
      if( m_max != null && upper < m_max.doubleValue() )
        upper = m_max.doubleValue();
      
      setRange( new Range( lower, upper ), false, false );
    }
  }
}
