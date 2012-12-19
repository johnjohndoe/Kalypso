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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;

/**
 * @author Thomas Jung
 */
public class MultiTriangleEater implements ITriangleEater
{
  private final List<ITriangleEater> m_eaters = new LinkedList<>();

  @Override
  public void add( final INodeResult... nodes )
  {
    for( final ITriangleEater eater : m_eaters )
      eater.add( nodes );
  }

  @Override
  public void finished( ) throws CoreException
  {
    for( final ITriangleEater eater : m_eaters )
      eater.finished();
  }

  public void addEater( final ITriangleEater triangleEater )
  {
    m_eaters.add( triangleEater );
  }

  public void addEater( final TriangulatedSurfaceTriangleEater triangleEater )
  {
    m_eaters.add( triangleEater );
  }

  public void addEater( final HMOTriangleEater triangleEater )
  {
    m_eaters.add( triangleEater );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#setTime(java.util.Date)
   */
  @Override
  public void setTime( final Date time )
  {
    for( final ITriangleEater eater : m_eaters )
      eater.setTime( time );
  }
}
