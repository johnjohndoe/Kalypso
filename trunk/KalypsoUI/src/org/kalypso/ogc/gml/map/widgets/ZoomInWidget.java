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

import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author von Dömming
 */
public class ZoomInWidget extends AbstractWidget
{
  public ZoomInWidget( String name, String toolTip )
  {
    super( name, toolTip );

  }

  private Point endPoint = null;

  private Point startPoint = null;

  @Override
  public void dragged( final Point p )
  {
    if( startPoint == null )
      startPoint = p;
    {
      endPoint = p;
    }
    //TODO: check if this repaint is really necessary
    IMapPanel panel = getMapPanel();
    if (panel != null)
      panel.repaintMap();

  }

  @Override
  public void leftPressed( final Point p )
  {
    startPoint = p;
    endPoint = null;
  }

  @Override
  public void leftReleased( final Point p )
  {
    endPoint = p;
    perform();
  }

  @Override
  public void paint( final Graphics g )
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

      final int x1 = (int) (startPoint.getX() - dx);
      final int y1 = (int) (startPoint.getY() - dy);

      g.drawRect( x1, y1, (int) dx * 2, (int) dy * 2 );
    }
  }

  public void perform( )
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

      GM_Envelope zoomBox = getDragbox( (int) startPoint.getX(), (int) startPoint.getY(), (int) dx );

      startPoint = null;
      endPoint = null;
      ChangeExtentCommand command = new ChangeExtentCommand( getMapPanel(), zoomBox );
      postViewCommand( command, null );
    }
  }

}