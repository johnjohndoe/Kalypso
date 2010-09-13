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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;

/**
 * @author Gernot Belger
 */
class PunktattributMapping
{
  private final Map<Integer, String[]> m_pointAttributeMapping = new HashMap<Integer, String[]>();

  public void addAttributeMapping( final String markerID, final int pointAttribute )
  {
    final String[] markerIDs = getMarkerIDs( pointAttribute );
    final String[] newMarkerIDs = (String[]) ArrayUtils.add( markerIDs, markerID );
    m_pointAttributeMapping.put( pointAttribute, newMarkerIDs );
  }

  public String[] getMarkerIDs( final int pointAttribute )
  {
    if( m_pointAttributeMapping.containsKey( pointAttribute ) )
      return m_pointAttributeMapping.get( pointAttribute );

    return new String[] {};
  }
}
