/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.rcm.internal.binding;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * A inverse distance element.
 * 
 * @author Holger Albert
 */
public class InverseDistanceElement implements Comparable<InverseDistanceElement>
{
  /**
   * The area geometry.
   */
  private final Geometry m_areaGeometry;

  /**
   * The point geometry of the station.
   */
  private final Point m_point;

  /**
   * The index of the original order of the point list.
   */
  private final int m_index;

  /**
   * The distance between the centroid of the catchment and the point.
   */
  private double m_distance;

  /**
   * The calculated factor for this ombrometer point. If it was not used, it will be 0.0.
   */
  private double m_factor;

  /**
   * The constructor.
   * 
   * @param areaGeometry
   *          The area geometry.
   * @param point
   *          The point geometry of the station.
   * @param index
   *          The index of the original order of the point list.
   */
  public InverseDistanceElement( final Geometry areaGeometry, final Point point, final int index )
  {
    m_areaGeometry = areaGeometry;
    m_point = point;
    m_index = index;

    m_distance = Double.NaN;
    if( areaGeometry != null && point != null )
      m_distance = areaGeometry.getCentroid().distance( point );

    m_factor = 0.0;
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final InverseDistanceElement o )
  {
    return Double.compare( getDistance(), o.getDistance() );
  }

  /**
   * This function returns the area geometry.
   * 
   * @return The area geometry.
   */
  public Geometry getAreaGeometry( )
  {
    return m_areaGeometry;
  }

  /**
   * This function returns the point geometry of the station.
   * 
   * @return The point geometry of the station.
   */
  public Point getPoint( )
  {
    return m_point;
  }

  /**
   * This function returns the index of the original order of the point list.
   * 
   * @return The index of the original order of the point list.
   */
  public int getIndex( )
  {
    return m_index;
  }

  /**
   * This function returns the distance between the centroid of the catchment and the point.
   * 
   * @return The distance between the centroid of the catchment and the point.
   */
  public double getDistance( )
  {
    return m_distance;
  }

  /**
   * This function returns the calculated factor for this point. If it was not used, it will be 0.0.
   * 
   * @return The calculated factor for this point. If it was not used, it will be 0.0.
   */
  public double getFactor( )
  {
    return m_factor;
  }

  /**
   * This function sets the calculated factor for this point.
   * 
   * @param factor
   *          The calculated factor for this point.
   */
  public void setFactor( final double factor )
  {
    m_factor = factor;
  }
}