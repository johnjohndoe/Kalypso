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
package org.kalypso.model.flood.binding;

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Thomas Jung
 */
public class FloodModel extends UnversionedModel implements IFloodModel
{
  private final FeatureWrapperCollection<IFloodPolygon> m_polygones;

  private final FeatureWrapperCollection<IRunoffEvent> m_events;

  public FloodModel( final Feature featureToBind )
  {
    super( featureToBind, QNAME );

    m_polygones = new FeatureWrapperCollection<IFloodPolygon>( featureToBind, IFloodPolygon.class, QNAME_PROP_POLYGONE_MEMBER );
    m_events = new FeatureWrapperCollection<IRunoffEvent>( featureToBind, IRunoffEvent.class, QNAME_PROP_EVENT_MEMBER );
  }

  /**
   * @see org.kalypso.model.flood.binding.IFloodModel#getPolygons()
   */
  @Override
  public IFeatureWrapperCollection<IFloodPolygon> getPolygons( )
  {
    return m_polygones;
  }

  /**
   * @see org.kalypso.model.flood.binding.IFloodModel#getTerrainModel()
   */
  @Override
  public ICoverageCollection getTerrainModel( )
  {
    final Feature coveragesFeature = (Feature) getFeature().getProperty( QNAME_PROP_COVERAGES_MEMBER );
    if( coveragesFeature != null )
    {
      return (ICoverageCollection) coveragesFeature.getAdapter( ICoverageCollection.class );
    }
    return null;
  }

  /**
   * @see org.kalypso.model.flood.binding.IFloodModel#getEvents()
   */
  @Override
  public IFeatureWrapperCollection<IRunoffEvent> getEvents( )
  {
    return m_events;
  }
}
