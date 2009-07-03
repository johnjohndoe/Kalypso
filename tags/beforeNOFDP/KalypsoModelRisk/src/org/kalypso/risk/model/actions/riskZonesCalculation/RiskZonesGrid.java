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
package org.kalypso.risk.model.actions.riskZonesCalculation;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRiskZoneDefinition;
import org.kalypso.risk.model.utils.RiskModelHelper;
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

  private final List<ILanduseClass> m_landuseClassesList;

  private final double m_cellSize;

  private BigDecimal m_min;

  private BigDecimal m_max;

  private final List<IRiskZoneDefinition> m_riskZoneDefinitionsList;

  public RiskZonesGrid( final IGeoGrid resultGrid, final IFeatureWrapperCollection<IAnnualCoverageCollection> annualCoverageCollection, final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection, final List<ILanduseClass> landuseClassesList, final List<IRiskZoneDefinition> riskZoneDefinitionsList ) throws Exception
  {
    super( resultGrid );

    m_cellSize = Math.abs( resultGrid.getOffsetX().x - resultGrid.getOffsetY().x ) * Math.abs( resultGrid.getOffsetX().y - resultGrid.getOffsetY().y );
    m_annualCoverageCollection = annualCoverageCollection;
    m_landusePolygonCollection = landusePolygonCollection;
    m_landuseClassesList = landuseClassesList;
    m_riskZoneDefinitionsList = riskZoneDefinitionsList;
    m_gridMap = new HashMap<String, List<IGeoGrid>>();

    for( final IAnnualCoverageCollection collection : m_annualCoverageCollection )
    {
      final List<IGeoGrid> gridList = new ArrayList<IGeoGrid>();

      for( final ICoverage coverage : collection )
        gridList.add( GeoGridUtilities.toGrid( coverage ) );

      m_gridMap.put( collection.getGmlID(), gridList );
    }

    m_min = new BigDecimal( Double.MAX_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );
    m_max = new BigDecimal( -Double.MAX_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );
  }

  @Override
  public final double getValue( final int x, final int y ) throws GeoGridException
  {
    final double[] damage = new double[m_annualCoverageCollection.size()];
    final double[] probability = new double[m_annualCoverageCollection.size()];

    /* fill the probabilies and damages */
    for( int i = 0; i < probability.length; i++ )
    {
      final IAnnualCoverageCollection collection = m_annualCoverageCollection.get( i );

      final double value = getValue( collection, x, y );
      damage[i] = Double.isNaN( value ) ? 0.0 : value;
      probability[i] = 1.0 / collection.getReturnPeriod();
    }

    /* calculate average annual damage */
    double averageAnnualDamageValue = RiskModelHelper.calcAverageAnnualDamageValue( damage, probability );

    if( averageAnnualDamageValue == 0 || Double.isNaN( averageAnnualDamageValue ) )
      return Double.NaN;

    final Coordinate coordinate = GeoGridUtilities.toCoordinate( this, x, y, null );
    final GM_Position position = JTSAdapter.wrap( coordinate );
    final List<ILandusePolygon> list = m_landusePolygonCollection.query( position );

    if( list == null || list.size() == 0 )
      return Double.NaN;

    for( final ILandusePolygon polygon : list )
    {
      if( polygon.contains( position ) )
      {
        final int landuseClassOrdinalNumber = polygon.getLanduseClassOrdinalNumber();

        /* set statistic for landuse class */
        fillStatistics( polygon, averageAnnualDamageValue, landuseClassOrdinalNumber );

        final double riskZoneValue = getRiskZone( averageAnnualDamageValue, polygon.isUrbanLanduseType() );

        /* check min/max */
        m_min = m_min.min( new BigDecimal( riskZoneValue ).setScale( 4, BigDecimal.ROUND_HALF_UP ) );
        m_max = m_max.max( new BigDecimal( riskZoneValue ).setScale( 4, BigDecimal.ROUND_HALF_UP ) );

        // TODO: maybe in addition we should provide the real grid values (not the ordinal numbers), too

        return riskZoneValue;
      }
    }

    return Double.NaN;
  }

  private void fillStatistics( final ILandusePolygon polygon, final double averageAnnualDamageValue, final int landuseClassOrdinalNumber )
  {
    /* add the current average annual damage value to all landuse polygons that covers the current raster cell */
    polygon.updateStatisticsAverageAnnualDamage( averageAnnualDamageValue );

    /* find the right landuse class that holds the polygon */
    for( final ILanduseClass landuseClass : m_landuseClassesList )
    {
      if( landuseClass.getOrdinalNumber() == landuseClassOrdinalNumber )
      {
        /* check for min / max */
        if( averageAnnualDamageValue < landuseClass.getMinDamage() )
          landuseClass.setMinDamage( averageAnnualDamageValue );
        if( averageAnnualDamageValue > landuseClass.getMaxDamage() )
          landuseClass.setMaxDamage( averageAnnualDamageValue );

        /* update total damage value of the current landuse class */
        final double totalDamage = landuseClass.getTotalDamage() + averageAnnualDamageValue * m_cellSize;
        landuseClass.setTotalDamage( totalDamage );

        /* update the average annual damage value for the landuse class */
        landuseClass.setAverageAnnualDamage( polygon.getStatisticsAverageAnnualDamage() );
      }
    }
  }

  /**
   * returns the flow depth value for a given position.
   * 
   * @param collection
   *            grid collection of water depth grids
   * @param x
   *            column number of the grid
   * @param y
   *            row number of the grid
   * @return the first found valid grid value within the grid collection
   * @throws GeoGridException
   */
  private double getValue( final IAnnualCoverageCollection collection, final int x, final int y ) throws GeoGridException
  {
    final List<IGeoGrid> list = m_gridMap.get( collection.getGmlID() );
    for( final IGeoGrid geoGrid : list )
    {
      final double value = geoGrid.getValueChecked( x, y );

      if( Double.isNaN( value ) )
        return Double.NaN;

      // we allow no negative flow depths!
      if( value < 0.0 )
        return 0.0;

      return value;
    }
    return Double.NaN;
  }

  private double getRiskZone( final double damageValue, final Boolean isUrbanLanduseType )
  {
    if( isUrbanLanduseType == null )
      return Double.NaN;

    IRiskZoneDefinition selectedRiskZoneDefinition = null;

    double minDifference = Double.MAX_VALUE;

    for( final IRiskZoneDefinition riskZoneDefinition : m_riskZoneDefinitionsList )
      if( isUrbanLanduseType.equals( riskZoneDefinition.isUrbanLanduseType() ) && damageValue >= riskZoneDefinition.getLowerBoundary() )
      {
        final double difference = damageValue - riskZoneDefinition.getLowerBoundary();
        if( difference < minDifference )
        {
          minDifference = difference;
          selectedRiskZoneDefinition = riskZoneDefinition;
        }
      }
    return selectedRiskZoneDefinition != null ? selectedRiskZoneDefinition.getOrdinalNumber() : Double.NaN;
  }

}
