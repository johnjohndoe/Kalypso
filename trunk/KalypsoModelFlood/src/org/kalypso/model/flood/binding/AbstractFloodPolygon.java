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
package org.kalypso.model.flood.binding;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Thomas Jung
 * @author Gernot Belger
 */
public abstract class AbstractFloodPolygon extends Feature_Impl implements IFloodPolygon
{
  private final FeatureBindingCollection<IRunoffEvent> m_runoffEvents;

  public AbstractFloodPolygon( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
    m_runoffEvents = new FeatureBindingCollection<>( this, IRunoffEvent.class, QNAME_PROP_EVENT );
  }

  @Override
  public boolean appliesToEvent( final String eventId )
  {
    for( final IRunoffEvent event : m_runoffEvents )
    {
      if( eventId.equals( event.getId() ) )
        return true;
    }

    return false;
  }

  @Override
  public boolean contains( final GM_Position crd )
  {
    return getArea().contains( crd );
  }

  @Override
  public GM_Polygon getArea( )
  {
    return getProperty( QNAME_PROP_AREA, GM_Polygon.class );
  }

  @Override
  public IFeatureBindingCollection<IRunoffEvent> getEvents( )
  {
    return m_runoffEvents;
  }
}