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
   * The area geometry of the catchment.
   */
  private Geometry m_areaGeometry;

  /**
   * The point geometry of the ombrometer station.
   */
  private Point m_ombrometerPoint;

  /**
   * The index of the original order of the ombrometer point list.
   */
  private int m_index;

  /**
   * The distance between the centroid of the catchment and the ombrometer point.
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
   *          The area geometry of the catchment.
   * @param ombrometerPoint
   *          The point geometry of the ombrometer station.
   *@param index
   *          The index of the original order of the ombrometer point list.
   */
  public InverseDistanceElement( Geometry areaGeometry, Point ombrometerPoint, int index )
  {
    m_areaGeometry = areaGeometry;
    m_ombrometerPoint = ombrometerPoint;
    m_index = index;

    m_distance = Double.NaN;
    if( areaGeometry != null && ombrometerPoint != null )
      m_distance = areaGeometry.getCentroid().distance( ombrometerPoint );

    m_factor = 0.0;
  }

  /**
   * This function returns the area geometry of the catchment.
   * 
   * @return The area geometry of the catchment.
   */
  public Geometry getAreaGeometry( )
  {
    return m_areaGeometry;
  }

  /**
   * This function returns the point geometry of the ombrometer station.
   * 
   * @return The point geometry of the ombrometer station.
   */
  public Point getOmbrometerPoint( )
  {
    return m_ombrometerPoint;
  }

  /**
   * This function returns the index of the original order of the ombrometer point list.
   * 
   * @return The index of the original order of the ombrometer point list.
   */
  public int getIndex( )
  {
    return m_index;
  }

  /**
   * This function returns the distance between the centroid of the catchment and the ombrometer point.
   * 
   * @return The distance between the centroid of the catchment and the ombrometer point.
   */
  public double getDistance( )
  {
    return m_distance;
  }

  /**
   * This function returns the calculated factor for this ombrometer point. If it was not used, it will be 0.0.
   * 
   * @return The calculated factor for this ombrometer point. If it was not used, it will be 0.0.
   */
  public double getFactor( )
  {
    return m_factor;
  }

  /**
   * This function sets the calculated factor for this ombrometer point.
   * 
   * @param factor
   *          The calculated factor for this ombrometer point.
   */
  public void setFactor( double factor )
  {
    m_factor = factor;
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( InverseDistanceElement o )
  {
    return Double.compare( getDistance(), o.getDistance() );
  }
}