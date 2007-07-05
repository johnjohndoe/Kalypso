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
package org.kalypso.kalypsomodel1d2d.sim;

import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;

/**
 * @author Thomas Jung
 * 
 */
public class NodeResultMinMaxCatcher
{
  double m_minDepth;

  double m_maxDepth;

  double m_minVelocityAbs;

  double m_maxVelocityAbs;

  double m_minWaterlevel;

  double m_maxWaterlevel;

  public void addNodeResult( final INodeResult nodeResult )
  {
    final double velocity = nodeResult.getAbsoluteVelocity();
    final double depth = nodeResult.getDepth();
    final double waterlevel = nodeResult.getWaterlevel();

    if( velocity < m_minVelocityAbs )
      m_minVelocityAbs = velocity;
    if( velocity > m_maxVelocityAbs )
      m_maxVelocityAbs = velocity;

    if( depth < m_minDepth )
      m_minDepth = depth;
    if( depth > m_maxDepth )
      m_maxDepth = depth;

    if( waterlevel < m_minWaterlevel )
      m_minWaterlevel = waterlevel;
    if( waterlevel > m_maxWaterlevel )
      m_maxWaterlevel = waterlevel;

  }

  public void addNodeResultMinMaxCatcher( NodeResultMinMaxCatcher minMaxCatcher )
  {
    if( minMaxCatcher.getMaxDepth() > m_maxDepth )
      m_maxDepth = minMaxCatcher.getMaxDepth();
    if( minMaxCatcher.getMinDepth() < m_minDepth )
      m_minDepth = minMaxCatcher.getMinDepth();

    if( minMaxCatcher.getMaxWaterlevel() > m_maxWaterlevel )
      m_maxWaterlevel = minMaxCatcher.getMaxWaterlevel();
    if( minMaxCatcher.getMinWaterlevel() < m_minWaterlevel )
      m_minWaterlevel = minMaxCatcher.getMinWaterlevel();

    if( minMaxCatcher.getMaxVelocityAbs() > m_maxVelocityAbs )
      m_maxVelocityAbs = minMaxCatcher.getMaxVelocityAbs();
    if( minMaxCatcher.getMinVelocityAbs() < m_minVelocityAbs )
      m_minVelocityAbs = minMaxCatcher.getMinVelocityAbs();

  }

  public double getMinDepth( )
  {
    return m_minDepth;
  }

  public double getMaxDepth( )
  {
    return m_maxDepth;
  }

  public double getMinVelocityAbs( )
  {
    return m_minVelocityAbs;
  }

  public double getMaxVelocityAbs( )
  {
    return m_maxVelocityAbs;
  }

  public double getMinWaterlevel( )
  {
    return m_minWaterlevel;
  }

  public double getMaxWaterlevel( )
  {
    return m_maxWaterlevel;
  }

}
