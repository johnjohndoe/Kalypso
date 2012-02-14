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
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URL;
import java.util.List;

import org.deegree.model.spatialschema.ByteUtils;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.ParallelBinaryGridProcessorBean;
import org.kalypso.grid.SequentialBinaryGeoGridReader;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author barbarins
 *
 */
public class RiskSpecificDamageGrid extends SequentialBinaryGeoGridReader
{
  private final Coordinate m_origin;

  private final Coordinate m_offsetX;

  private final Coordinate m_offsetY;

  private final IFeatureBindingCollection<ILandusePolygon> m_polygonCollection;

  private final List<ILanduseClass> m_landuseClasses;

  private final double m_cellSize;

  private final int m_returnPeriod;

  private final int m_sizeX;

  private final IGeoTransformer m_geoTransformer;

  public RiskSpecificDamageGrid( final IGeoGrid inputGrid, final URL pUrl, final IFeatureBindingCollection<ILandusePolygon> polygonCollection, final List<ILanduseClass> landuseClasses, final double cellSize, final int returnPeriod ) throws IOException, GeoGridException
  {
    super( inputGrid, pUrl );

    m_origin = getDelegate().getOrigin();
    m_offsetX = getDelegate().getOffsetX();
    m_offsetY = getDelegate().getOffsetY();
    m_sizeX = getDelegate().getSizeX();
    // m_sizeY = getDelegate().getSizeY();

    m_polygonCollection = polygonCollection;
    m_landuseClasses = landuseClasses;
    m_cellSize = cellSize;
    m_returnPeriod = returnPeriod;

    final ILandusePolygon landusePolygon = m_polygonCollection.get( 0 );
    final String coordinateSystem = landusePolygon.getGeometry().getCoordinateSystem();

    m_geoTransformer = GeoTransformerFactory.getGeoTransformer( coordinateSystem );
  }

  @Override
  public final double getValue( final int k, final ParallelBinaryGridProcessorBean bean ) throws GeoGridException, Exception
  {
    // FIXME: really UGLY! At least, the handling of the underlying stream and the calculation of the position should be
    // made by the general framework.
    // It is really bad design that every implementation needs to do this again!

    // convert 4 bytes to integer
    final int z = ByteUtils.readBEInt( bean.m_blockData, k * 4 );

    if( z == Integer.MIN_VALUE /* NO_DATA */)
      return Double.NaN;

    final BigDecimal decimal = new BigDecimal( BigInteger.valueOf( z ), m_scale );
    final double value = decimal.doubleValue();

    // possible that waterdepth input grid contains water depth less than zero!
    if( value <= 0.0 )
      return Double.NaN;

    if( m_polygonCollection.size() == 0 )
      return Double.NaN;

    final int x = k % m_sizeX;
    final int y = k / m_sizeX + bean.m_startPosY;
    final double cx = m_origin.x + x * m_offsetX.x + y * m_offsetY.x;
    final double cy = m_origin.y + x * m_offsetX.y + y * m_offsetY.y;

    final GM_Position position = m_geoTransformer.transform( GeometryFactory.createGM_Position( cx, cy ), getSourceCRS() );

    /* This list has some unknown cs. */
    final List<ILandusePolygon> list = m_polygonCollection.query( position );
    if( list == null || list.size() == 0 )
      return Double.NaN;
    else
    {
      for( final ILandusePolygon polygon : list )
      {
        if( polygon.contains( position ) )
        {
          final Integer landuseClassOrdinalNumber = polygon.getLanduseClassOrdinalNumber();
          final double damageValue = polygon.getDamageValue( value );

          if( Double.isNaN( damageValue ) )
            return Double.NaN;

          if( damageValue <= 0.0 )
            return Double.NaN;

          /* set statistic for landuse class */
          final ILanduseClass landuseClass = m_landuseClasses.get( landuseClassOrdinalNumber );
          if( landuseClass == null )
            System.out.println( String.format( "Unknown landuse class: %s", landuseClassOrdinalNumber ) ); //$NON-NLS-1$
          else
            RiskModelHelper.fillStatistics( m_returnPeriod, landuseClass, damageValue, m_cellSize );
          return damageValue;
        }
      }
    }

    return Double.NaN;
  }
}