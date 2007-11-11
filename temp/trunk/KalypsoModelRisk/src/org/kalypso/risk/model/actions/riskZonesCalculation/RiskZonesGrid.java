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
package org.kalypso.risk.model.actions.riskZonesCalculation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

public class RiskZonesGrid extends AbstractDelegatingGeoGrid implements IGeoGrid
{
  private final Map<String, List<IGeoGrid>> m_gridMap;

  private final IFeatureWrapperCollection<IAnnualCoverageCollection> m_annualCoverageCollection;

  private final IFeatureWrapperCollection<ILandusePolygon> m_landusePolygonCollection;

  public RiskZonesGrid( final IGeoGrid resultGrid, final IFeatureWrapperCollection<IAnnualCoverageCollection> annualCoverageCollection, final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection ) throws Exception
  {
    super( resultGrid );
    m_annualCoverageCollection = annualCoverageCollection;
    m_landusePolygonCollection = landusePolygonCollection;
    m_gridMap = new HashMap<String, List<IGeoGrid>>();
    for( final IAnnualCoverageCollection collection : m_annualCoverageCollection )
    {
      final List<IGeoGrid> gridList = new ArrayList<IGeoGrid>();
      for( final ICoverage coverage : collection )
        gridList.add( GeoGridUtilities.toGrid( coverage ) );
      m_gridMap.put( collection.getGmlID(), gridList );
    }
  }

  public final double getValue( final int x, final int y ) throws GeoGridException
  {
    final double[] damage = new double[m_annualCoverageCollection.size()];
    final double[] probability = new double[m_annualCoverageCollection.size()];
    int i = 0;
    for( final IAnnualCoverageCollection collection : m_annualCoverageCollection )
    {
      final double value = getValue( collection, x, y );
      damage[i] = Double.isNaN( value ) ? 0.0 : value;
      probability[i++] = 1.0 / collection.getReturnPeriod();
    }
    double result = 0.0;
    for( i = 1; i < probability.length; i++ )
    {
      final double p = Math.abs( probability[i - 1] - probability[i] );
      result += (damage[i - 1] + damage[i]) * p / 2;
    }
    if( result == 0 || Double.isNaN( result ) )
      return Double.NaN;
    final Coordinate coordinate = GeoGridUtilities.toCoordinate( this, x, y, null );
    final GM_Position positionAt = JTSAdapter.wrap( coordinate );
    final List<ILandusePolygon> list = m_landusePolygonCollection.query( positionAt );
    if( list == null || list.size() == 0 )
      return Double.NaN;
    else
      for( final ILandusePolygon polygon : list )
      {
        if( polygon.contains( positionAt ) )
          return polygon.getRiskZone( result );
      }
    return Double.NaN;
  }

  private double getValue( final IAnnualCoverageCollection collection, final int x, final int y ) throws GeoGridException
  {
    final List<IGeoGrid> list = m_gridMap.get( collection.getGmlID() );
    for( final IGeoGrid geoGrid : list )
    {
      final double value = geoGrid.getValueChecked( x, y );
      if( !Double.isNaN( value ) )
        return value;
    }
    return Double.NaN;
  }

}
