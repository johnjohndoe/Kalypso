/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.util;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;

import org.eclipse.core.runtime.Assert;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * TODO: move this class to GM_Tools<br>
 * Helper class that provides the snapping mechanism for {@link GM_Point}s
 * <ul>
 * <li>Snapping to the nearest point in the snapping radius is the default behavior.
 * <li>Snapping will not occur if SHIFT button is pressed during the operation.
 * </ul>
 *
 * @author Thomas Jung
 */
public class GM_PointSnapper
{
  /** Snapping radius in screen-pixels. */
  public static final int SNAPPING_RADIUS = 14;

  private boolean m_snappingActive = true;

  private GM_Point m_snapPoint = null;

  private final IMapPanel m_mapPanel;

  private final GM_Position[] m_positions;

  public GM_PointSnapper( final GM_Position[] positions, final IMapPanel panel )
  {
    Assert.isNotNull( panel );
    Assert.isNotNull( positions );

    m_positions = positions;
    m_mapPanel = panel;
  }

  /**
   * Move the mouse to the given position and try to snap the point.
   *
   * @return The snapped point, or <code>null</code> if none was found.
   */
  public GM_Point moved( final GM_Point p )
  {
    m_snapPoint = null;

    if( m_snappingActive )
    {
      final double snapRadius = MapUtilities.calculateWorldDistance( m_mapPanel, p, SNAPPING_RADIUS );
      m_snapPoint = findNode( p, snapRadius );

      return m_snapPoint;
    }

    return null;
  }

  // TODO: implement better search! This is just for testing!
  private GM_Point findNode( final GM_Point p, final double snapRadius )
  {
    for( final GM_Position m_position : m_positions )
    {
      if( p.getPosition().getDistance( m_position ) < snapRadius )
        return GeometryFactory.createGM_Point( m_position, p.getCoordinateSystem() );
    }
    return null;
  }

  /**
   * Paints a snap rectangle at the given position.<br>
   * Always use this paint method to draw snapped points, in order to have identical visualization everywhere.
   */
  public void paint( final Graphics g )
  {
    if( m_snapPoint != null )
    {
      final Point point = MapUtilities.retransform( m_mapPanel, m_snapPoint );

      final int snapRadius = SNAPPING_RADIUS / 2;

      final int lowX = (int) point.getX() - snapRadius;
      final int lowY = (int) point.getY() - snapRadius;

      final Color color = g.getColor();

      final Color preViewColor = new Color( 50, 50, 255 );
      g.setColor( preViewColor );

      g.drawRect( lowX, lowY, SNAPPING_RADIUS, SNAPPING_RADIUS );
      g.setColor( color );
    }
  }

  public GM_Point getSnapPoint( )
  {
    return m_snapPoint;
  }

  public void activate( final boolean isActive )
  {
    m_snappingActive = isActive;
  }
}