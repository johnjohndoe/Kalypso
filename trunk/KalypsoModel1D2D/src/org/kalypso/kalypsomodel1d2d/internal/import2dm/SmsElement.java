/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.internal.import2dm;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.refinement.RefinementUtils;

class SmsElement
{
  private final Integer[] m_nodeIds;

  private final ISMSModel m_model;

  public SmsElement( final ISMSModel model, final Integer[] nodeIds )
  {
    m_model = model;
    m_nodeIds = nodeIds;
  }

  public GM_Surface<GM_SurfacePatch> toSurface( ) throws GM_Exception
  {
    final Collection<GM_Position> posList = new ArrayList<GM_Position>();

    for( final Integer nodeId : m_nodeIds )
    {
      final GM_Position nodePos = m_model.getNode( nodeId );
      posList.add( nodePos );
    }

    final GM_Position firstNode = m_model.getNode( m_nodeIds[0] );
    posList.add( firstNode );

    final String srs = m_model.getSrs();

    final GM_Position[] poses = posList.toArray( new GM_Position[posList.size()] );
    return RefinementUtils.getSurface( poses, srs );
  }
}