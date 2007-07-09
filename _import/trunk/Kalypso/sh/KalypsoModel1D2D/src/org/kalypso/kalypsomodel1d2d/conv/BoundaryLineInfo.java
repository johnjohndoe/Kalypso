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
package org.kalypso.kalypsomodel1d2d.conv;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

/**
 * @author antanas
 */
public class BoundaryLineInfo extends BoundaryConditionInfo
{
  private final IFE1D2DNode[] m_nodeArray;

  public BoundaryLineInfo( final int ID, final IFE1D2DNode[] nodeArray )
  {
    super( ID, TYPE.CONTI );
    m_nodeArray = nodeArray;
  }

  public IFE1D2DNode[] getNodes( )
  {
    return m_nodeArray;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.BoundaryConditionInfo#setObservation(org.kalypso.observation.IObservation,
   *      org.kalypso.observation.result.IComponent, org.kalypso.observation.result.IComponent)
   */
  public void setObservation( final IObservation<TupleResult> obs, final IComponent timeComponent, final IComponent valueComponent, final TYPE type )
  {
    super.setObservation( obs, timeComponent, valueComponent );
    setType( type );
  }

  public void setTheta( final double direction )
  {
    double deltaX = 0;
    double deltaY = 0;
    // direction perpendicular to boundary line
    double theta = 0;
    // For 2D there is a boundary line
    if( m_nodeArray.length > 1 )
    {
      // second point should be the one, which is more west!
      if( m_nodeArray[0].getPoint().getX() < m_nodeArray[m_nodeArray.length-1].getPoint().getX() )
      {
        deltaX = m_nodeArray[0].getPoint().getX() - m_nodeArray[m_nodeArray.length-1].getPoint().getX();
        deltaY = m_nodeArray[0].getPoint().getY() - m_nodeArray[m_nodeArray.length-1].getPoint().getY();
      }
      else
      {
        deltaX = m_nodeArray[m_nodeArray.length-1].getPoint().getX() - m_nodeArray[0].getPoint().getX();
        deltaY = m_nodeArray[m_nodeArray.length-1].getPoint().getY() - m_nodeArray[0].getPoint().getY();
      }
      double l = Math.sqrt( Math.pow( deltaX, 2 ) + Math.pow( deltaY, 2 ) );

      if( deltaY > 0 )
        theta = direction - 180d + Math.acos( deltaX / l );
      else if( deltaY < 0 )
        theta = direction + 180d - (Math.acos( deltaX / l )/ Math.PI * 180d);
      else if( deltaY == 0 && deltaX > 0 )
        theta = direction - 180d;
      else
        theta = direction;
    }
    else
    {
      theta = -1 * (270d - direction);
    }
    super.setTheta( theta );
  }
}
