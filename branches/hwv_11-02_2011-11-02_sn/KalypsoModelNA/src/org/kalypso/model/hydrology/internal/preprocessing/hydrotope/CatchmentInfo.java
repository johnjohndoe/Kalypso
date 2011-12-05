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
import java.util.List;

import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * Infos about all hydrotops belonging to one catchment.
 * 
 * @author Gernot Belger
 */
public class CatchmentInfo
{
  private final List<HydrotopeInfo> m_hydrotops = new ArrayList<HydrotopeInfo>();

  private final Catchment m_catchment;

  private final LanduseHash m_landuseHash;

  private Sealing m_totalSealing;

  private Sealing m_totalSudsSealing;

  public CatchmentInfo( final Catchment catchment, final LanduseHash landuseHash )
  {
    m_catchment = catchment;
    m_landuseHash = landuseHash;
    m_totalSealing = new Sealing();
    m_totalSudsSealing = new Sealing();
  }

  public void add( final IHydrotope hydrotop ) throws SimulationException
  {
    final HydrotopeInfo hydrotopInfo = new HydrotopeInfo( hydrotop, m_landuseHash, m_hydrotops.size() + 1 );
    hydrotopInfo.calculateSealing();
    m_hydrotops.add( hydrotopInfo );

    final Sealing hydrotopeSealing = hydrotopInfo.getHydrotopeSealingMinusSuds();
    final Sealing sudsSealing = hydrotopInfo.getSudsSealing();

    m_totalSealing = m_totalSealing.add( hydrotopeSealing );
    m_totalSudsSealing = m_totalSudsSealing.add( sudsSealing );
  }

  public String checkArea( )
  {
    final GM_Surface< ? > geometry = m_catchment.getGeometry();
    final double catchmentAre = geometry.getArea();

    final double hydrotopArea = m_totalSealing.getArea();

    final double fehler = Math.abs( catchmentAre - hydrotopArea );
    final double fehlerinProzent = 100.0 * fehler / hydrotopArea;
    if( fehlerinProzent > 1.0 )
      return Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.3", hydrotopArea, m_catchment.getId(), catchmentAre, fehler, fehlerinProzent ); //$NON-NLS-1$

    return null;
  }

  public String getHydroFeatureId( final int pos )
  {
    return m_hydrotops.get( pos ).getHydrotop().getId();
  }

  public Sealing getTotalSealing( )
  {
    return m_totalSealing;
  }

  public List<HydrotopeInfo> getHydrotops( )
  {
    return Collections.unmodifiableList( m_hydrotops );
  }

  public Catchment getCatchment( )
  {
    return m_catchment;
  }

  public Sealing getTotalSudsSealing( )
  {
    return m_totalSudsSealing;
  }

}
