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
package org.kalypso.risk.model.simulation;

import java.io.IOException;
import java.net.URL;
import java.util.List;

import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.parallel.SequentialBinaryGeoGridReader;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.simulation.statistics.StatisticCollector;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.google.common.primitives.Doubles;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author barbarins
 */
public class RiskSpecificDamageGrid extends SequentialBinaryGeoGridReader
{
  private final IFeatureBindingCollection<ILandusePolygon> m_polygonCollection;

  private final List<ILanduseClass> m_landuseClasses;

  private final double m_cellSize;

  private final int m_returnPeriod;

  private final IGeoTransformer m_geoTransformer;

  private final StatisticCollector m_statistics;

  public RiskSpecificDamageGrid( final IGeoGrid inputGrid, final URL pUrl, final IFeatureBindingCollection<ILandusePolygon> polygonCollection, final List<ILanduseClass> landuseClasses, final double cellSize, final int returnPeriod, final StatisticCollector statistics ) throws IOException, GeoGridException
  {
    super( inputGrid, pUrl );

    m_polygonCollection = polygonCollection;
    m_landuseClasses = landuseClasses;
    m_cellSize = cellSize;
    m_returnPeriod = returnPeriod;
    m_statistics = statistics;

    final ILandusePolygon landusePolygon = m_polygonCollection.get( 0 );
    final String coordinateSystem = landusePolygon.getGeometry().getCoordinateSystem();

    m_geoTransformer = GeoTransformerFactory.getGeoTransformer( coordinateSystem );
  }

  @Override
  protected double getValue( final int x, final int y, final Coordinate crd ) throws GeoGridException
  {
    final double wspHeight = crd.z;
    if( Double.isNaN( wspHeight ) )
      return Double.NaN;

    // possible that waterdepth input grid contains water depth less than zero!
    if( wspHeight <= 0.0 )
      return Double.NaN;

    if( m_polygonCollection.size() == 0 )
      return Double.NaN;

    final double cx = crd.x;
    final double cy = crd.y;

    try
    {
      final GM_Position position = m_geoTransformer.transform( GeometryFactory.createGM_Position( cx, cy ), getSourceCRS() );

      final double damageValue = calculateDamageValue( position, wspHeight );

      final Coordinate resultCrd = new Coordinate( position.getX(), position.getY(), damageValue );

      if( Doubles.isFinite( damageValue ) )
        m_statistics.addSpecificDamage( m_returnPeriod, resultCrd, m_cellSize );

      return damageValue;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new GeoGridException( e.getLocalizedMessage(), e );
    }
  }

  private double calculateDamageValue( final GM_Position position, final double wspHeight )
  {
    /* This list has some unknown cs. */
    final List<ILandusePolygon> list = m_polygonCollection.query( position );
    if( list == null || list.size() == 0 )
      return Double.NaN;

    for( final ILandusePolygon polygon : list )
    {
      // TODO: hotspot, slow. Consider using the statistics object that already has a specialized geo index for all areas (associate area with its class,
      // so we do not have to look up the class as well)
      if( polygon.contains( position ) )
      {
        final double damageValue = polygon.getDamageValue( wspHeight );

        if( Double.isNaN( damageValue ) )
          return Double.NaN;

        if( damageValue <= 0.0 )
          return Double.NaN;

      // /* set statistic for landuse class */
      // final Integer landuseClassOrdinalNumber = polygon.getLanduseClassOrdinalNumber();
      // final ILanduseClass landuseClass = m_landuseClasses.get( landuseClassOrdinalNumber );
      // if( landuseClass == null )
      //  System.out.println( String.format( "Unknown landuse class: %s", landuseClassOrdinalNumber ) ); //$NON-NLS-1$

        return damageValue;
      }
    }

    return Double.NaN;
  }
}