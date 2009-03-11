/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.map.widgets.providers.handles;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;

import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * This is a handle, representing a point of a geometry.
 * 
 * @author Holger Albert
 */
public class Handle implements IHandle
{
  /**
   * The point being represented as position.
   */
  private final GM_Position m_position;

  /**
   * The complete feature, which containts the geometry, the point is in.
   */
  private final Feature m_feature;

  /**
   * The radius, in which this handle can be selected.
   */
  private final int m_radius;

  /**
   * Is this handle marked as active? If true, it is painted in a different color. And users of this handle could
   * determine, what to do with active handles.
   */
  private boolean m_active;

  /**
   * The geometry.
   */
  private final IValuePropertyType m_vpt;

  /**
   * The constructor.
   * 
   * @param position
   *          The position represented by this handle.
   * @param feature
   *          The feature, which contains the geometry, the position is in.
   * @param vpt
   *          The IValuePropertyType of the geometry.
   * @param radius
   *          The radius, in which this handle should be selectable.
   */
  public Handle( GM_Position position, Feature feature, final IValuePropertyType vpt, int radius )
  {
    m_position = position;
    m_feature = feature;
    m_vpt = vpt;
    m_radius = radius;
    m_active = false;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.handles.IHandle#isSelectable()
   */
  public boolean isSelectable( Point p, GeoTransform projection )
  {
    final int x = (int) projection.getDestX( m_position.getX() );
    final int y = (int) projection.getDestY( m_position.getY() );

    /* Does the AWT-Point lies inside the rectangle? */
    if( (p.getX() > (x - m_radius)) && (p.getY() > (y - m_radius)) && (p.getX() < (x + m_radius)) && (p.getY() < (y + m_radius)) )
    {
      return true;
    }

    return false;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.handles.IHandle#paint(java.awt.Graphics)
   */
  public void paint( Graphics g, GeoTransform projection, Point startPoint, Point currentPoint )
  {
    /* Calculate the coordinates for the map. */
    final int x = (int) projection.getDestX( m_position.getX() );
    final int y = (int) projection.getDestY( m_position.getY() );

    int dx = 0;
    int dy = 0;

    if( (startPoint != null) && (currentPoint != null) )
    {
      /* Calculate the difference between the two points. */
      dx = (int) (currentPoint.getX() - startPoint.getX());
      dy = (int) (currentPoint.getY() - startPoint.getY());
    }

    g.setColor( Color.GRAY );

    /* If this handle is a active one, take another color to paint it. */
    if( m_active == true )
      g.setColor( Color.BLUE );

    g.drawRect( x - m_radius + dx, y - m_radius + dy, 2 * m_radius, 2 * m_radius );

    g.drawLine( x, y, x + dx, y + dy );
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.handles.IHandle#setActive(boolean)
   */
  public void setActive( boolean active )
  {
    m_active = active;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.handles.IHandle#isActive()
   */
  public boolean isActive( )
  {
    return m_active;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.handles.IHandle#getFeature()
   */
  public Feature getFeature( )
  {
    return m_feature;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.handles.IHandle#getValuePropertyType()
   */
  public IValuePropertyType getValuePropertyType( )
  {
    return m_vpt;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.handles.IHandle#getPosition()
   */
  public GM_Position getPosition( )
  {
    return m_position;
  }
}