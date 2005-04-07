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

import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author bce
 */
public interface IWidget 
{
  public void rightReleased( final Point p );

  public void clickPopup( final Point p );

  public void dragged( Point p );

  public void finish();

  // MouseClicks
  public void leftClicked( Point p );

  public void leftPressed( Point p );

  public void leftReleased( Point p );

  public void middleClicked( Point p );

  public void middlePressed( Point p );

  public void middleReleased( Point p );

  // MouseMotions
  public void moved( Point p );

  // Graphics
  public void paint( Graphics g );

  /**
   * fuehrt die aktion aus
   */
  public void perform();

  public void rightClicked( Point p );

  public void rightPressed( Point p );

  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel );

  public String getName();

  public String getToolTip();
}