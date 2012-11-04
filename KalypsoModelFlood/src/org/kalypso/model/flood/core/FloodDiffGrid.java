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

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.parallel.SequentialBinaryGeoGridReader;
import org.kalypso.model.flood.binding.IFloodClipPolygon;
import org.kalypso.model.flood.binding.IFloodExtrapolationPolygon;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.IFloodVolumePolygon;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class FloodDiffGrid extends SequentialBinaryGeoGridReader
{
  private final IFeatureBindingCollection<ITinReference> m_tins;

  private final IFeatureBindingCollection<IFloodPolygon> m_polygons;

  private final Map<IFloodExtrapolationPolygon, Double> m_polygonWsps = new HashMap<>();

  private double m_min;

  private double m_max;

  private final IRunoffEvent m_event;

  public FloodDiffGrid( final IGeoGrid inputGrid, final URL pUrl, final IFeatureBindingCollection<ITinReference> tins, final IFeatureBindingCollection<IFloodPolygon> polygons, final IRunoffEvent event ) throws IOException, GeoGridException
  {
    super( inputGrid, pUrl );

    m_tins = tins;
    m_polygons = polygons;
    m_event = event;

    m_min = Double.MAX_VALUE;
    m_max = -Double.MAX_VALUE;
  }

  @Override
  protected double getValue( final int x, final int y, final Coordinate crd ) throws GeoGridException
  {
    final double z = crd.z;
    if( Double.isNaN( z ) )
      return z;

    final double value = getValueInternal( x, y, z );

    /* update min/max */
    if( !Double.isNaN( value ) )
    {
      m_min = Math.min( m_min, value );
      m_max = Math.max( m_max, value );
    }

    return value;
  }

  private double getValueInternal( final int x, final int y, final double terrainValue ) throws GeoGridException
  {
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

  private double getWspValue( final Coordinate crd )
  {
    final GM_Position pos = JTSAdapter.wrap( crd );

    final GM_Point point = GeometryFactory.createGM_Point( pos, getSourceCRS() );

    try
    {
      for( final ITinReference tinReference : m_tins )
      {
        final String tinCRS = tinReference.getTin().getCoordinateSystem();
        final IGeoTransformer transformer = GeoTransformerFactory.getGeoTransformer( tinCRS );
        final GM_Point transformedPoint = (GM_Point)transformer.transform( point );
        final GM_Position transPos = transformedPoint.getPosition();

        final double wspValue = tinReference.getValue( transPos );
        if( !Double.isNaN( wspValue ) )
          return wspValue;
      }

      return Double.NaN;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return Double.NaN;
    }
  }

  private double getExtrapolValue( final IFloodExtrapolationPolygon polygon )
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
        return (IFloodExtrapolationPolygon)floodPolygon;
    }
    return null;
  }

  private IFloodVolumePolygon getVolumePolygons( final List<IFloodPolygon> polygons )
  {
    for( final IFloodPolygon floodPolygon : polygons )
    {
      if( floodPolygon instanceof IFloodVolumePolygon )
        return (IFloodVolumePolygon)floodPolygon;
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

    // FIXME: check coordinate system!

    final List<IFloodPolygon> list = m_polygons.query( pos );

    if( list == null || list.size() == 0 )
      return Collections.emptyList();

    final List<IFloodPolygon> polygonList = new LinkedList<>();
    for( final IFloodPolygon polygon : list )
    {
      if( polygon.contains( pos ) && polygon.getEvents().contains( m_event ) )
        polygonList.add( polygon );
    }

    return polygonList;
  }

  @Override
  public BigDecimal getMin( )
  {
    return new BigDecimal( m_min ).setScale( 2, BigDecimal.ROUND_HALF_UP );
  }

  @Override
  public BigDecimal getMax( )
  {
    return new BigDecimal( m_max ).setScale( 2, BigDecimal.ROUND_HALF_UP );
  }
}