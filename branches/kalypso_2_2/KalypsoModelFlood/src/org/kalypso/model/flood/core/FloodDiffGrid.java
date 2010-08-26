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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.model.flood.binding.IFloodClipPolygon;
import org.kalypso.model.flood.binding.IFloodExtrapolationPolygon;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.IFloodVolumePolygon;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class FloodDiffGrid extends AbstractDelegatingGeoGrid implements IGeoGrid
{
  private final IFeatureWrapperCollection<ITinReference> m_tins;

  private final IFeatureWrapperCollection<IFloodPolygon> m_polygons;

  private final Map<IFloodExtrapolationPolygon, Double> m_polygonWsps = new HashMap<IFloodExtrapolationPolygon, Double>();

  private BigDecimal m_min;

  private BigDecimal m_max;

  private GM_Triangle m_triangle;

  private final IRunoffEvent m_event;

  public FloodDiffGrid( final IGeoGrid terrainGrid, final IFeatureWrapperCollection<ITinReference> tins, final IFeatureWrapperCollection<IFloodPolygon> polygons, final IRunoffEvent event )
  {
    super( terrainGrid );

    m_tins = tins;
    m_polygons = polygons;
    m_event = event;

    m_min = new BigDecimal( Double.MAX_VALUE ).setScale( 2, BigDecimal.ROUND_HALF_UP );
    m_max = new BigDecimal( -Double.MAX_VALUE ).setScale( 2, BigDecimal.ROUND_HALF_UP );
  }

  /**
   * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int)
   */
  @Override
  public double getValue( final int x, final int y ) throws GeoGridException
  {
    final double value = getValueInternal( x, y );
    if( Double.isNaN( value ) )
      return Double.NaN;

    /* check min/max */
    m_min = m_min.min( new BigDecimal( value ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
    m_max = m_max.max( new BigDecimal( value ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );

    return value;
  }

  private double getValueInternal( final int x, final int y ) throws GeoGridException
  {
    final double terrainValue = super.getValue( x, y );

    if( Double.isNaN( terrainValue ) )
      return Double.NaN;

    // get coordinate for raster cell x/y
    final Coordinate crd = GeoGridUtilities.calcCoordinateWithoutZ( this, x, y, terrainValue, null );

    /* get the wsp value */
    final double wspValue = getWspValue( crd );

    /* check polygon stuff */
    // get the polygons
    final List<IFloodPolygon> polygons = getPolygons( crd );

    // - if not clip (+): Double.NaN

    /* - if clip (-): Double.NaN */
    if( containsClipPolygons( polygons ) == true )
      return Double.NaN;

    /* - if extrapolation: getExtrapolationsvalue */
    final IFloodExtrapolationPolygon extrapolPolygon = getExtrapolPolygons( polygons );
    if( extrapolPolygon != null )
    {
      final double extrapolValue = getExtrapolValue( extrapolPolygon );
      return extrapolValue - terrainValue;
    }

    /* - if volume: get Volume-Waterlevel */
    final IFloodVolumePolygon volumePolyon = getVolumePolygons( polygons );
    if( volumePolyon != null )
    {
      final BigDecimal volumeWspValue = volumePolyon.getWaterlevel();
      final double volumeWsp = volumeWspValue == null ? Double.NaN : volumeWspValue.doubleValue();
      if( !Double.isNaN( volumeWsp ) )
        return volumeWsp - terrainValue;
    }

    if( Double.isNaN( wspValue ) )
      return Double.NaN;

    return wspValue - terrainValue;
  }

  private double getWspValue( final Coordinate crd ) throws GeoGridException
  {
    final GM_Position pos = JTSAdapter.wrap( crd );

    final GM_Point point = GeometryFactory.createGM_Point( pos, getSourceCRS() );

    try
    {
      for( final ITinReference tinReference : m_tins )
      {
        final String tinCRS = tinReference.getTin().getCoordinateSystem();
        final GeoTransformer transformer = new GeoTransformer( tinCRS );
        final GM_Point transformedPoint = (GM_Point) transformer.transform( point );
        final GM_Position transPos = transformedPoint.getPosition();

        // we remember the last found triangle...
        if( m_triangle != null && m_triangle.contains( transPos ) == true )
        {
          final double wspValue = m_triangle.getValue( transPos );
          if( !Double.isNaN( wspValue ) )
            return wspValue;
        }
        else
        {
          m_triangle = tinReference.getTraingle( transPos );
          if( m_triangle != null )
          {
            final double wspValue = m_triangle.getValue( transPos );
            if( !Double.isNaN( wspValue ) )
              return wspValue;
          }
        }
      }
      return Double.NaN;
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return Double.NaN;
    }
  }

  private double getExtrapolValue( final IFloodExtrapolationPolygon polygon ) throws GeoGridException
  {
    // REMARK: hash for each polygon its wsp, to we do not need to recalculate it each time
    if( m_polygonWsps.containsKey( polygon ) )
      return m_polygonWsps.get( polygon );

    final GM_Point refPoint = polygon.getRefPoint();
    if( refPoint == null )
      return Double.NaN;

    final GM_Position position = refPoint.getPosition();
    final Coordinate crd = JTSAdapter.export( position );

    // get wsp value
    final double wspValue = getWspValue( crd );

    m_polygonWsps.put( polygon, wspValue );

    return wspValue;
  }

  private IFloodExtrapolationPolygon getExtrapolPolygons( final List<IFloodPolygon> polygons )
  {
    for( final IFloodPolygon floodPolygon : polygons )
    {
      if( floodPolygon instanceof IFloodExtrapolationPolygon )
        return (IFloodExtrapolationPolygon) floodPolygon;
    }
    return null;
  }

  private IFloodVolumePolygon getVolumePolygons( final List<IFloodPolygon> polygons )
  {
    for( final IFloodPolygon floodPolygon : polygons )
    {
      if( floodPolygon instanceof IFloodVolumePolygon )
        return (IFloodVolumePolygon) floodPolygon;
    }
    return null;
  }

  private boolean containsClipPolygons( final List<IFloodPolygon> polygons )
  {
    for( final IFloodPolygon floodPolygon : polygons )
    {
      if( floodPolygon instanceof IFloodClipPolygon )
        return true;
    }
    return false;
  }

  private List<IFloodPolygon> getPolygons( final Coordinate crd )
  {
    final GM_Position pos = JTSAdapter.wrap( crd );

    final List<IFloodPolygon> list = m_polygons.query( pos );
    final List<IFloodPolygon> polygonList = new LinkedList<IFloodPolygon>();

    if( list == null || list.size() == 0 )
      return polygonList;
    else
    {
      for( final IFloodPolygon polygon : list )
      {
        if( polygon.contains( pos )) 
            if(polygon.getEvents().contains( m_event ) )
                polygonList.add( polygon );
      }
    }

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

  /**
   * @see org.kalypso.grid.IGeoGrid#setMax(java.math.BigDecimal)
   */
  @Override
  public void setMax( final BigDecimal maxValue )
  {
    if( maxValue != null )
      m_max = maxValue;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#setMin(java.math.BigDecimal)
   */
  @Override
  public void setMin( final BigDecimal minValue )
  {
    if( minValue != null )
      m_min = minValue;
  }
  
}
