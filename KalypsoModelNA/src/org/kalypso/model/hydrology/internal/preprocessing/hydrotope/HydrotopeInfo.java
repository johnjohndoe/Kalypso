/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

/**
 * Wrapper that holds {@link IHydrotope} with additional info about it's suds.
 * 
 * @author Gernot Belger
 */
public class HydrotopeInfo
{
  private final IHydrotope m_hydrotop;

  private final LanduseHash m_landuseHash;

  private final int m_localID;

  private Sealing m_sealing;

  public HydrotopeInfo( final IHydrotope hydrotop, final LanduseHash landuseHash, final int localID )
  {
    m_hydrotop = hydrotop;
    m_landuseHash = landuseHash;
    m_localID = localID;
  }

  public int getLocalID( )
  {
    return m_localID;
  }

  public IHydrotope getHydrotop( )
  {
    return m_hydrotop;
  }

  private double getSealingRate( ) throws SimulationException
  {
    final String landuseName = m_hydrotop.getLanduse();
    final Double landuseSealing = m_landuseHash.getSealingRate( landuseName );
    if( landuseSealing == null )
    {
      final String msg = String.format( "Unknown landuse: '%s'. Please recreate hydrotopes!", landuseName ); //$NON-NLS-1$
      throw new SimulationException( msg );
    }

    return landuseSealing.doubleValue();
  }

  public void calculateSealing( ) throws SimulationException
  {
    final GM_MultiSurface geometry = m_hydrotop.getGeometry();
    final double hydrotopArea = geometry.getArea();
    final double landuseSealing = getSealingRate();

    final double corrSealing = m_hydrotop.getCorrSealing();
    // FIXME:
    // HOTFIX: should actually lead to an exception instead: planer client produces corrSeling > 1.0
    // final double corrSealingFixed = Math.abs( Math.min( corrSealing, 1.0 ) );
    final double totalSealingFaktor = landuseSealing * corrSealing;
    m_sealing = Sealing.createFromSealing( hydrotopArea, totalSealingFaktor );

    final double naturalArea = m_sealing.getNaturalArea();
    if( naturalArea < 0.0 )
      throw new SimulationException( String.format( "Hydrotop natural area is less than 0.0 m2: %.4f", naturalArea ) ); //$NON-NLS-1$
  }

  public Sealing getSealing( )
  {
    return m_sealing;
  }
}
