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
package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * <p>
 * Sucht von allen Features jenes, welches am nächsten zu einem Punkt liegt.
 * </p>
 * <p>
 * Falls viele Objekte durchsucht werden, sollte die Suche zuerst durch ein .query auf der FeatureList bzw. dem
 * Workspace eingeschränkt werden.
 * </p>
 * 
 * @author belger
 */
public class FindNearestVisitor implements FeatureVisitor
{
  /**
   * Hilfspunkt für die Suche, wird lazy instatiiert, da wir nicht wissen, ob überhaupt eine Geoemetry gefunden wird
   */
  private GM_Point point = null;

  /** das bislang näheste feature */
  private Feature m_result = null;

  /** bisher minimale entfernung zum Punkt */
  private double m_minDist = Double.MAX_VALUE;

  private final double m_radius;

  private final GM_Position m_pos;

  public FindNearestVisitor( final GM_Position pos, final double radius )
  {
    m_pos = pos;
    m_radius = radius;
  }

  public Feature getResult()
  {
    return m_result;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    GM_Object fGeo = null;
    String type = f.getFeatureType().getName();
    // TODO handle this better
    if( type.equals( RectifiedGridCoverage.getNameStatic() ) )
    {
      GM_Object[] geoProps = f.getGeometryProperties();
      fGeo = geoProps[0];
    }
    else
    {
      fGeo = f.getDefaultGeometryProperty();
    }

    if( fGeo != null )
    {
      if( point == null )
        point = GeometryFactory.createGM_Point( m_pos, fGeo.getCoordinateSystem() );

      final double dist = fGeo.distance( point );
      if( dist < m_radius && dist < m_minDist )
      {
        m_result = f;
        m_minDist = dist;
      }
    }

    return true;
  }

}