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
package org.kalypso.ogc.gml.command;

import java.awt.Graphics;

import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author doemming
 */
public class Handle
{

  public static final int MASK_BOX = 1;

  public static final int MASK_TOPOLOGY = 2;

  private final Feature m_feature;

  private final GM_Position m_position;

  private final IValuePropertyType m_propType;

  public Handle( final Feature feature, IValuePropertyType propType, GM_Position position )
  {
    m_feature = feature;
    m_propType = propType;
    m_position = position;
  }

  public GM_Position getPosition( )
  {
    return m_position;
  }

  public void paint( final Graphics g, final GeoTransform projection, final int boxRadiusDrawnHandle, final int gisRadiusTopology, int mask )
  {
    paint( g, projection, boxRadiusDrawnHandle, gisRadiusTopology, 0, 0, mask );
  }

  public void paint( final Graphics g, final GeoTransform projection, final int boxRadiusDrawnHandle, final int gisRadiusTopology, final int dx, final int dy, int mask )
  {
    final int x = (int) projection.getDestX( m_position.getX() );
    final int y = (int) projection.getDestY( m_position.getY() );

    // visible box
    if( (mask & MASK_BOX) == MASK_BOX )
      g.drawRect( x - boxRadiusDrawnHandle + dx, y - boxRadiusDrawnHandle + dy, 2 * boxRadiusDrawnHandle, 2 * boxRadiusDrawnHandle );

    // visible circle of tologyradius (usual just a point, if not in extrem zoom state)
    if( (mask & MASK_TOPOLOGY) == MASK_TOPOLOGY )
    {
      final int radiusTopology = (int) Math.abs( x - projection.getDestX( m_position.getX() + gisRadiusTopology ) );
      g.drawOval( x - radiusTopology + dx, y - radiusTopology + dy, 2 * radiusTopology, 2 * radiusTopology );
    }

    g.drawLine( x, y, x + dx, y + dy );

  }

  public Feature getFeature( )
  {
    return m_feature;
  }

  public IValuePropertyType getPropertyType( )
  {
    return m_propType;
  }
}