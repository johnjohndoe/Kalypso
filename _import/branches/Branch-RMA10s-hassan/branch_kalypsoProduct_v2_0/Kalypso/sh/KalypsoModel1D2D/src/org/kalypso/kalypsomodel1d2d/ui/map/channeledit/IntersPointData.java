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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.PROF;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.SIDE;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.WIDTHORDER;

import com.vividsolutions.jts.geom.Point;

/**
 * Stores the intersection points for a single channel segment created by intersecting the banklines with the profiles.<br>
 * The intersection points have information such as:<br>
 * -geographical information (x,y,z) (Point)<br>
 * -corresponding profile of the segment (upstream/downstream) (PROF)<br>
 * -corresponding bankline of the segment (left/right) (SIDE)<br>
 * -the rank of the intersection point relative to the width coordinates of the profiles (WIDTHORDER)<br>
 * -the width coordinate of the intersection point on the profile<br>
 * 
 * @author Thomas Jung
 */
public class IntersPointData
{
  private Point m_point;

  private final PROF m_prof;

  private final SIDE m_side;

  private WIDTHORDER m_widthOrder;
  
  private double m_width;

  public IntersPointData( Point point, CreateChannelData.PROF prof, CreateChannelData.SIDE side, double width )
  {
    m_point = point;
    m_prof = prof;
    m_side = side;
    m_widthOrder = null;
    m_width = width;
  }

  public Point getPoint( )
  {
    return m_point;
  }

  public void setPoint( Point point)
  {
    m_point = point;
  }

  public double getWidth( )
  {
    return m_width;
  }

  public void setWidth( double width)
  {
    m_width = width;
  }
  
  public CreateChannelData.PROF getProf( )
  {
    return m_prof;
  }

  public CreateChannelData.SIDE getSide( )
  {
    return m_side;
  }

  public boolean isLeft( )
  {
    if( m_side == SIDE.LEFT )
      return true;

    return false;
  }

  public boolean isRight( )
  {
    if( m_side == SIDE.RIGHT )
      return true;

    return false;
  }

  public boolean isUp( )
  {
    if( m_prof == PROF.UP ) // next
      return true;

    return false;
  }

  public boolean isDown( )
  {
    if( m_prof == PROF.DOWN ) // previous
      return true;

    return false;
  }

  public void setWidthOrder( CreateChannelData.WIDTHORDER widthOrder )
  {
    m_widthOrder = widthOrder;
  }

  public CreateChannelData.WIDTHORDER getWidthOrder( )
  {
    return m_widthOrder;
  }
}