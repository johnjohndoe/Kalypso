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
package org.kalypso.risk.model.simulation.statistics;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.ILandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.shape.ShapeFile;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Gernot Belger
 */
public class StatisticElementBuilder
{
  private final Collection<RiskStatisticItem> m_items = new ArrayList<>();

  public StatisticElementBuilder( final IRasterizationControlModel controlModel )
  {
  }

  public void addElements( final ILandusePolygonCollection landusePolygons, final ShapeFile shape ) throws GM_Exception
  {
    // FIXME intersect shape with landuse

    final IFeatureBindingCollection<ILandusePolygon> landusePolygonCollection = landusePolygons.getLandusePolygonCollection();
    for( final ILandusePolygon landusePolygon : landusePolygonCollection )
    {
      final GM_Surface< ? > geometry = landusePolygon.getGeometry();
      final Polygon area = (Polygon) JTSAdapter.export( geometry );

      final String name = landusePolygon.getName();
      m_items.add( new RiskStatisticItem( name, area ) );
    }

    // init landuse and statistic collector
    // TODO: add elements for all landuse classes/groups

    // TODO Auto-generated method stub

  }

  public RiskStatisticItem[] getItems( )
  {
    return m_items.toArray( new RiskStatisticItem[m_items.size()] );
  }
}