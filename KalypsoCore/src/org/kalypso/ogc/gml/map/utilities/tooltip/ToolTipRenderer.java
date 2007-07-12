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
package org.kalypso.ogc.gml.map.utilities.tooltip;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.geom.Rectangle2D;

/**
 * @author kuch
 */
public class ToolTipRenderer
{
  private static final int ROW_HEIGHT = 14;

  private static final int BORDER = 5;

  private String[] m_rows; // content of tooltip (rows which will be rendered)

  private Color m_backgroundColor = new Color( 1f, 1f, 0.6f, 0.65f );

  public ToolTipRenderer( )
  {
  }

  private double calculateBoxWidth( final Graphics g )
  {
    final Rectangle2D[] rectangles = new Rectangle2D[m_rows.length];
    for( int i = 0; i < m_rows.length; i++ )
      rectangles[i] = g.getFontMetrics().getStringBounds( m_rows[i], g );

    double width = 0;
    for( final Rectangle2D rectangle : rectangles )
      if( rectangle.getWidth() > width )
        width = rectangle.getWidth();

    return width;
  }

  public void paintToolTip( final Point point, final Graphics g )
  {
    if( m_rows == null )
      throw new IllegalStateException();

    final double boxWidth = calculateBoxWidth( g );

    /* draw outer rectangle */
    final Point myPoint = new Point( new Double( point.getX() ).intValue() + 20, new Double( point.getY() ).intValue() );

    g.setColor( m_backgroundColor );
    g.fillRect( new Double( myPoint.getX() ).intValue() - ToolTipRenderer.BORDER, new Double( myPoint.getY() ).intValue() - ToolTipRenderer.ROW_HEIGHT, new Double( boxWidth ).intValue() + 2
        * ToolTipRenderer.BORDER, m_rows.length * ToolTipRenderer.ROW_HEIGHT );

    g.setColor( Color.BLACK );
    g.drawRect( new Double( myPoint.getX() ).intValue() - ToolTipRenderer.BORDER, new Double( myPoint.getY() ).intValue() - ToolTipRenderer.ROW_HEIGHT, new Double( boxWidth ).intValue() + 2
        * ToolTipRenderer.BORDER, m_rows.length * ToolTipRenderer.ROW_HEIGHT );

    /* draw tooltip labels */
    for( final String element : m_rows )
    {
      g.drawString( element, new Double( myPoint.getX() ).intValue(), new Double( myPoint.getY() ).intValue() );
      myPoint.setLocation( myPoint.getX(), myPoint.getY() + 13 );
    }
  }

  public void setBackgroundColor( final Color c )
  {
    m_backgroundColor = c;
  }

  public void setInputData( final String[] rows )
  {
    m_rows = rows;
  }
}
