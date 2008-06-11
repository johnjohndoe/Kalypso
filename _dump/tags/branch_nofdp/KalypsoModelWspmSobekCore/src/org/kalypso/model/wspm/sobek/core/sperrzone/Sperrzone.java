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
package org.kalypso.model.wspm.sobek.core.sperrzone;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypsodeegree.model.feature.Feature;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author kuch
 */
public class Sperrzone implements ISperrzone
{
  private final Feature m_owner;

  private final Map<IBranch, Set<Geometry>> m_map = new HashMap<IBranch, Set<Geometry>>();

  /**
   * @param owner
   *            feature sperrzone comes from
   * @param branch
   *            sperrzone lays on branch(es) x
   * @param geoms
   *            geometries of the sperrzone
   */
  public Sperrzone( final Feature owner )
  {
    m_owner = owner;
  }

  public Feature getOwner( )
  {
    return m_owner;
  }

  public void addSperrzone( final IBranch branch, final Geometry gmo )
  {
    addSperrzone( branch, new Geometry[] { gmo } );
  }

  public void addSperrzone( final IBranch branch, final Geometry[] gmos )
  {
    Set<Geometry> list = m_map.get( branch );
    if( list == null )
    {
      list = new HashSet<Geometry>();
      m_map.put( branch, list );
    }

    for( final Geometry gmo : gmos )
    {
      list.add( gmo );
    }
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.sperrzone.ISperrzone#getBranch()
   */
  public IBranch[] getBranch( )
  {
    return m_map.keySet().toArray( new IBranch[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.sperrzone.ISperrzone#getGeometries(org.kalypso.model.wspm.sobek.core.interfaces.IBranch)
   */
  public Geometry[] getGeometries( final IBranch branch )
  {
    final Set<Geometry> list = m_map.get( branch );
    if( list == null )
      return new Geometry[] {};

    return list.toArray( new Geometry[] {} );
  }
}
