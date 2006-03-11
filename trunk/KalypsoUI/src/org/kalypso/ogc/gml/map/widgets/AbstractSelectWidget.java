/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Graphics;
import java.awt.Point;

/**
 * 
 * @author doemming
 */
public abstract class AbstractSelectWidget extends AbstractWidget
{
  public AbstractSelectWidget( String name, String toolTip )
  {
    super( name, toolTip );
  }

  /**
   * pixel coordinates
   */
  private Point m_endPoint = null;

  /**
   * pixel coordinates
   */
  private Point m_startPoint = null;

  /**
   * radius in pixel
   */
  private final int m_radius = 20;

  abstract int getSelectionMode();

  abstract boolean allowOnlyOneSelectedFeature();

  @Override
  public void dragged( Point p )
  {
    if( m_startPoint == null )
    {
      m_startPoint = p;
      m_endPoint = null;
    }
    else
      m_endPoint = p;
  }

  @Override
  public void leftPressed( Point p )
  {
    m_startPoint = p;
    m_endPoint = null;
  }

  @Override
  public void leftReleased( Point p )
  {
    if( m_endPoint != null ) // last update of endPoint
      m_endPoint = p;
    else
      m_startPoint = p;

    try
    {
      getMapPanel().select( m_startPoint, m_endPoint, m_radius, getSelectionMode(), allowOnlyOneSelectedFeature() );
    }
    finally
    {
      m_startPoint = null;
      m_endPoint = null;
    }
  }

  @Override
  public void paint( Graphics g )
  {
    if( m_startPoint != null && m_endPoint != null )
    {
      int px = (int)( m_startPoint.getX() < m_endPoint.getX() ? m_startPoint.getX() : m_endPoint.getX() );
      int py = (int)( m_startPoint.getY() < m_endPoint.getY() ? m_startPoint.getY() : m_endPoint.getY() );
      int dx = (int)Math.abs( m_endPoint.getX() - m_startPoint.getX() );
      int dy = (int)Math.abs( m_endPoint.getY() - m_startPoint.getY() );

      if( dx != 0 && dy != 0 )
        g.drawRect( px, py, dx, dy );
    }
  }

  public void perform()
  {
  // nothing
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  @Override
  public void finish( )
  {
    m_startPoint = null;
    m_endPoint = null;
    super.finish();
  }
}