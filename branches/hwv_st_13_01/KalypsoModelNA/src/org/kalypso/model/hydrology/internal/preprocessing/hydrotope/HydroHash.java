/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.model.hydrology.internal.preprocessing.hydrotope;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Dejan Antanascovic
 */
public class HydroHash
{
  private final Map<Catchment, CatchmentInfo> m_hydroInfos = new LinkedHashMap<Catchment, CatchmentInfo>();

  private final LanduseHash m_landuseHash;

  public HydroHash( final LanduseHash landuseHash )
  {
    m_landuseHash = landuseHash;
  }

  public void initHydrotopes( final NAHydrotop naHydrotop, final Catchment[] catchments ) throws GM_Exception, SimulationException
  {
    final IFeatureBindingCollection<IHydrotope> hydrotopes = naHydrotop.getHydrotopes();

    for( final Catchment catchment : catchments )
    {
      final Geometry catchmentGeometry = JTSAdapter.export( catchment.getGeometry() );

      final List<IHydrotope> hydInEnvList = hydrotopes.query( catchment.getBoundedBy() );
      for( final IHydrotope hydrotope : hydInEnvList )
      {
        final Geometry hydrotopGeometry = JTSAdapter.export( hydrotope.getGeometry() );
        if( catchmentGeometry.contains( hydrotopGeometry.getInteriorPoint() ) )
          addHydrotope( catchment, hydrotope );
      }
    }
  }

  private void addHydrotope( final Catchment catchment, final IHydrotope hydrotop ) throws SimulationException
  {
    final CatchmentInfo info = getHydrotopInfo( catchment );
    info.add( hydrotop );
  }

  public CatchmentInfo getHydrotopInfo( final Catchment catchment )
  {
    final CatchmentInfo info = m_hydroInfos.get( catchment );
    if( info != null )
      return info;

    final CatchmentInfo newInfo = new CatchmentInfo( catchment, m_landuseHash );
    m_hydroInfos.put( catchment, newInfo );
    return newInfo;
  }

  public String getHydroFeatureId( final Catchment catchment, final int pos )
  {
    final CatchmentInfo info = getHydrotopInfo( catchment );

    return info.getHydroFeatureId( pos );
  }

  public Collection<Catchment> getCatchments( )
  {
    return Collections.unmodifiableCollection( m_hydroInfos.keySet() );
  }

  public List<HydrotopeInfo> getHydrotops( final Catchment catchment )
  {
    final CatchmentInfo hydrotopInfo = getHydrotopInfo( catchment );
    return hydrotopInfo.getHydrotops();
  }

  public LanduseHash getLanduseHash( )
  {
    return m_landuseHash;
  }
}
