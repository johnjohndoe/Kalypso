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
package org.kalypso.model.hydrology.internal.preprocessing.hydrotope;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * Infos about all hydrotops belonging to one catchment.
 * 
 * @author Gernot Belger
 */
public class CatchmentInfo
{
  private final List<HydrotopeInfo> m_hydrotopes = new ArrayList<>();

  private final Map<String, HydrotopeInfo> m_hydrotopeHash = new LinkedHashMap<>();

  private final Catchment m_catchment;

  private Sealing m_totalSealing = null;

  private final boolean m_doAttributeDissolve;

  /**
   * @param doAttributeDissolve
   *          If hydrotopes with same attributes should be combined into a single one. Set to <code>false</code> for
   *          debug purpose only.
   */
  public CatchmentInfo( final Catchment catchment, final boolean doAttributeDissolve )
  {
    m_catchment = catchment;
    m_doAttributeDissolve = doAttributeDissolve;
  }

  public void add( final HydrotopeInfo hydrotopeInfo )
  {
    final String attributeHashKey = hydrotopeInfo.getAttributeHash();

    if( m_doAttributeDissolve && m_hydrotopeHash.containsKey( attributeHashKey ) )
    {
      final HydrotopeInfo existingInfo = m_hydrotopeHash.get( attributeHashKey );
      existingInfo.addArea( hydrotopeInfo );
    }
    else
    {
      m_hydrotopeHash.put( attributeHashKey, hydrotopeInfo );
      m_hydrotopes.add( hydrotopeInfo );
    }
  }

  public String checkArea( )
  {
    final GM_Polygon geometry = m_catchment.getGeometry();
    final double catchmentAre = geometry.getArea();

    final double hydrotopArea = getTotalSealing().getArea();

    final double fehler = Math.abs( catchmentAre - hydrotopArea );
    final double fehlerinProzent = 100.0 * fehler / hydrotopArea;
    if( fehlerinProzent > 1.0 )
      return Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.3", hydrotopArea, m_catchment.getId(), catchmentAre, fehler, fehlerinProzent ); //$NON-NLS-1$

    return null;
  }

  public String getHydroFeatureId( final int pos )
  {
    return m_hydrotopes.get( pos ).getFeatureId();
  }

  public Sealing getTotalSealing( )
  {
    if( m_totalSealing == null )
    {
      m_totalSealing = new Sealing();

      for( final HydrotopeInfo hydrotopeInfo : m_hydrotopes )
      {
        final Sealing hydrotopeSealing = hydrotopeInfo.createSealing();

        m_totalSealing = m_totalSealing.add( hydrotopeSealing );
      }
    }

    return m_totalSealing;
  }

  public List<HydrotopeInfo> getHydrotops( )
  {
    return Collections.unmodifiableList( m_hydrotopes );
  }

  public Catchment getCatchment( )
  {
    return m_catchment;
  }
}