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
package org.kalypso.model.wspm.core.profil.impl.marker;

import java.util.Comparator;

import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.observation.result.IComponent;

public class PointMarkerComparator implements Comparator<IProfilPointMarker>
{
  private final IComponent m_property;

  public PointMarkerComparator( final IComponent propertyToCompare )
  {
    m_property = propertyToCompare;
  }

  public int compare( final IProfilPointMarker o1, final IProfilPointMarker o2 )
  {
    if( (Double) o1.getPoint().getValue( m_property ) > (Double) o2.getPoint().getValue( m_property ) )
      return 1;
    if( (Double) o1.getPoint().getValue( m_property ) < (Double) o2.getPoint().getValue( m_property ) )
      return -1;
    return 0;
  }
}
