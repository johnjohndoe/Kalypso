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
package org.kalypso.ogc.gml.widgets;

import java.awt.Graphics;
import java.awt.Point;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.util.command.ICommand;

/**
 * 
 * @author von Dömming
 */
public class ZoomInWidget extends AbstractWidget
{
  private Point endPoint = null;

  private Point startPoint = null;

  public void dragged( Point p )
  {
    if( startPoint == null )
      startPoint = p;
    {
      endPoint = p;
    }
  }

  public void leftPressed( Point p )
  {
    startPoint = p;
    endPoint = null;
  }

  public void leftReleased( Point p )
  {
    endPoint = p;
    perform();
  }

  public void paint( Graphics g )
  {
    if( startPoint != null && endPoint != null )
    {
      final double ratio = getRatio();

      double dx = Math.abs( endPoint.getX() - startPoint.getX() );
      double dy = Math.abs( endPoint.getY() - startPoint.getY() );

      if( dx < 5 )
        dx = 5;

      if( dx * ratio > dy )
        dy = dx * ratio;
      else
        dx = dy / ratio;

      final int x1 = (int)( startPoint.getX() - dx );
      final int y1 = (int)( startPoint.getY() - dy );

      g.drawRect( x1, y1, (int)dx * 2, (int)dy * 2 );
    }
  }

  protected final ICommand performIntern()
  {
    if( startPoint != null && endPoint != null )
    {
      final double ratio = getRatio();

      double dx = Math.abs( endPoint.getX() - startPoint.getX() );
      double dy = Math.abs( endPoint.getY() - startPoint.getY() );

      if( dx < 5 )
        dx = 5;

      if( dx * ratio > dy )
        dy = dx * ratio;
      else
        dx = dy / ratio;

      GM_Envelope zoomBox = getDragbox( (int)startPoint.getX(), (int)startPoint.getY(), (int)dx );

      startPoint = null;
      endPoint = null;

      return new ChangeExtentCommand( getMapPanel(), zoomBox );
    }

    return null;
  }

}