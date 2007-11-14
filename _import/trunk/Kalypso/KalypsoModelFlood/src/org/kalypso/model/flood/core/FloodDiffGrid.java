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
package org.kalypso.model.flood.core;

import java.math.BigDecimal;
import java.util.List;

import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.model.flood.binding.IFloodClipPolygon;
import org.kalypso.model.flood.binding.IFloodExtrapolationPolygon;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class FloodDiffGrid extends AbstractDelegatingGeoGrid implements IGeoGrid
{

  private final IFeatureWrapperCollection<ITinReference> m_tins;

  private BigDecimal m_min;

  private BigDecimal m_max;

  private final IFeatureWrapperCollection<IFloodPolygon> m_polygons;

  public FloodDiffGrid( final IGeoGrid terrainGrid, final IFeatureWrapperCollection<ITinReference> tins, final IFeatureWrapperCollection<IFloodPolygon> polygons )
  {
    super( terrainGrid );

    m_tins = tins;
    m_polygons = polygons;

    m_min = new BigDecimal( Double.MAX_VALUE ).setScale( 2, BigDecimal.ROUND_HALF_UP );
    m_max = new BigDecimal( Double.MIN_VALUE ).setScale( 2, BigDecimal.ROUND_HALF_UP );
  }

  /**
   * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int)
   */
  @Override
  public double getValue( final int x, final int y ) throws GeoGridException
  {
    final double terrainValue = super.getValue( x, y );

    if( Double.isNaN( terrainValue ) )
      return Double.NaN;

    // get coordinate for raster cell x/y
    final Coordinate crd = GeoGridUtilities.calcCoordinateWithoutZ( this, x, y, terrainValue, null );

    double depthValue = Double.NaN;

    /* get the wsp value */
    final double wspValue = getWspValue( crd );
    if( Double.isNaN( wspValue ) )
      return Double.NaN;

    /* check polygon stuff */
    // get the polygons
    final List<IFloodPolygon> polygons = getPolygons( crd );
    if( polygons.size() > 0 )
    {
      // - if not clip (+): Double.NaN

      /* - if clip (-): Double.NaN */
      if( containsClipPolygons( polygons ) == true )
        return Double.NaN;

      /* - if extrapolation: getExtrapolationsvalue */
      final IFloodExtrapolationPolygon extrapolPolygon = getExtrapolPolygons( polygons );
      if( extrapolPolygon != null )
      {
        double extrapolValue = getExtrpolValue( extrapolPolygon );
        depthValue = extrapolValue - terrainValue;
      }
    }
    else
      depthValue = wspValue - terrainValue;

    /* check min/max */
    m_min = m_min.min( new BigDecimal( depthValue ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
    m_max = m_max.max( new BigDecimal( depthValue ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );

    return depthValue;
  }

  private double getWspValue( final Coordinate crd )
  {
    final GM_Position pos = JTSAdapter.wrap( crd );

    final List<ITinReference> tinsList = m_tins.query( pos );
    for( final ITinReference tinReference : tinsList )
    {
      final double wspValue = tinReference.getValue( pos );
      if( !Double.isNaN( wspValue ) )
        return wspValue;
    }

    return Double.NaN;
  }

  private double getExtrpolValue( IFloodExtrapolationPolygon polygon )
  {
    // TODO: get reference point
    final GM_Object[] geometryProperties = polygon.getWrappedFeature().getGeometryProperties();
    for( GM_Object object : geometryProperties )
    {
      if( object instanceof GM_Point )
      {
        final GM_Point point = (GM_Point) object;
        final GM_Position position = point.getPosition();
        final Coordinate crd = JTSAdapter.export( position );

        // get wsp value
        return getWspValue( crd );
      }
    }
    return Double.NaN;
  }

  private IFloodExtrapolationPolygon getExtrapolPolygons( List<IFloodPolygon> polygons )
  {
    for( IFloodPolygon floodPolygon : polygons )
    {
      if( floodPolygon instanceof IFloodExtrapolationPolygon )
      {
        IFloodExtrapolationPolygon polygon = (IFloodExtrapolationPolygon) floodPolygon;

        return polygon;
      }
    }
    return null;
  }

  private boolean containsClipPolygons( List<IFloodPolygon> polygons )
  {
    for( IFloodPolygon floodPolygon : polygons )
    {
      if( floodPolygon instanceof IFloodClipPolygon )
        return true;
    }
    return false;
  }

  private List<IFloodPolygon> getPolygons( final Coordinate crd )
  {
    final GM_Position pos = JTSAdapter.wrap( crd );

    final List<IFloodPolygon> polygonList = m_polygons.query( pos );

    return polygonList;
  }

  @Override
  public BigDecimal getMin( )
  {
    return m_min;
  }

  @Override
  public BigDecimal getMax( )
  {
    return m_max;
  }

}
