/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.graphics.sld.Interval;

/**
 * 
 * @author N. Peiler
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Interval_Impl implements Interval, Comparable
{

  private double m_lowerLimit;

  private double m_upperLimit;

  /**
   * constructs an Inteval with the given lower and upper limit
   * 
   * @param lowerLimit
   * @param upperLimit
   */
  public Interval_Impl( double lowerLimit, double upperLimit )
  {
    setLowerLimit( lowerLimit );
    setUpperLimit( upperLimit );
  }

  /**
   * checks if the Interval contains the value x
   * 
   * @param x
   * @return true, if Interval contains the value; otherwise false
   */
  public boolean contains( double x )
  {
    return ( ( m_lowerLimit <= x ) && ( x <= m_upperLimit ) );
  }

  /**
   * @return Returns the lowerLimit.
   */
  public double getLowerLimit()
  {
    return m_lowerLimit;
  }

  /**
   * @return Returns the upperLimit.
   */
  public double getUpperLimit()
  {
    return m_upperLimit;
  }

  /**
   * @param lowerLimit
   *          The lowerLimit to set.
   */
  public void setLowerLimit( double lowerLimit )
  {
    m_lowerLimit = lowerLimit;
  }

  /**
   * @param upperLimit
   *          The upperLimit to set.
   */
  public void setUpperLimit( double upperLimit )
  {
    m_upperLimit = upperLimit;
  }

  public int compareTo( Object o )
  {
    int result = 0;
    double diffLowerLimit = getLowerLimit() - ( (Interval)o ).getLowerLimit();
    if( diffLowerLimit > 0 )
    {
      result = 1;
    }
    if( diffLowerLimit < 0 )
    {
      result = -1;
    }
    return result;
  }
}