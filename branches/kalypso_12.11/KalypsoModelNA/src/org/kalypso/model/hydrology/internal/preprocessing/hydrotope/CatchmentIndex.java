/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.hydrology.internal.preprocessing.hydrotope;

import java.util.List;

import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.JSISpatialIndex;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Used to find the right catchment for a hydrotope.
 * 
 * @author Gernot Belger
 */
class CatchmentIndex
{
  private final Catchment[] m_catchments;

  private SpatialIndexExt m_catchmentIndex;

  public CatchmentIndex( final Catchment[] catchments )
  {
    m_catchments = catchments;
  }

  public Catchment findCatchment( final IHydrotope hydrotope )
  {
    final IXLinkedFeature catchmentLink = hydrotope.getCatchmentLink();
    if( catchmentLink != null )
      return (Catchment)catchmentLink.getFeature();

    final SpatialIndexExt catchmentIndex = getCatchmentIndex();

    try
    {
      final Geometry hydrotopGeometry = JTSAdapter.export( hydrotope.getGeometry() );
      if( hydrotopGeometry == null )
        return null;

      final Envelope boundingBox = hydrotopGeometry.getEnvelopeInternal();

      final List<Catchment> query = catchmentIndex.query( boundingBox );
      for( final Catchment catchment : query )
      {
        final Geometry catchmentGeometry = JTSAdapter.export( catchment.getGeometry() );
        if( catchmentGeometry.contains( hydrotopGeometry.getInteriorPoint() ) )
          return catchment;
      }

      return null;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /* Lazy, because often this is never needed */
  private SpatialIndexExt getCatchmentIndex( )
  {
    if( m_catchmentIndex == null )
      m_catchmentIndex = indexCatchments( m_catchments );

    return m_catchmentIndex;
  }

  private SpatialIndexExt indexCatchments( final Catchment[] catchments )
  {
    final SpatialIndexExt index = new JSISpatialIndex();
    for( final Catchment catchment : catchments )
    {
      final GM_Envelope envelope = catchment.getEnvelope();
      final Envelope boundingBox = JTSAdapter.export( envelope );

      index.insert( boundingBox, catchment );
    }

    return index;
  }
}