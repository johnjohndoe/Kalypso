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
package org.kalypso.ogc.gml.map.widgets.mapfunctions;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;

/**
 * Helper class to drag a rectangle on the map. Used by many widgets.
 * 
 * @author Holger Albert
 */
public class RectangleSelector
{
  /**
   * pixel coordinates
   */
  private Point m_startPoint = null;

  /**
   * pixel coordinates
   */
  private Point m_endPoint = null;

  /**
   * Constructs the RectangleSelector and sets the start point.
   * 
   * @param startPoint
   *            The start point of the rectangle.
   */
  public RectangleSelector( final Point startPoint )
  {
    m_startPoint = startPoint;
  }

  /**
   * Set the end point.
   * 
   * @param endPoint
   *            sets the end point of the rectangle.
   */
  public void setEndPoint( final Point endPoint )
  {
    m_endPoint = endPoint;
  }

  /**
   * Paints the rectangle.
   */
  public void paint( final Graphics g )
  {
    final Rectangle rectangle = getRectangle();
    if( rectangle != null )
    {
      if( rectangle.width != 0 && rectangle.height != 0 )
      {
        // TODO: set color/line width and so on
        // TODO 2: configure g at a central point
        final Graphics2D g2 = (Graphics2D) g;
        g2.setStroke( new BasicStroke() );
        g2.setColor( new Color( 255, 0, 0 ) );
        g.drawRect( rectangle.x, rectangle.y, rectangle.width, rectangle.height );
      }
    }
  }

  /**
   * This function returns the complete rectangle, if available, otherwise null.
   * 
   * @return The rectangle.
   */
  public Rectangle getRectangle( )
  {
    if( m_startPoint == null || m_endPoint == null )
      return null;

    return RectangleUtils.createNormalizedRectangle( m_startPoint, m_endPoint );
  }

  /**
   * This function returns the end point of the rectangle.
   * 
   * @return The end point of the rectangle.
   */
  public Point getEndPoint( )
  {
    return m_endPoint;
  }

  /**
   * This function returns the start point of the rectangle.
   * 
   * @return The start point of the rectangle.
   */
  public Point getStartPoint( )
  {
    return m_startPoint;
  }

}
