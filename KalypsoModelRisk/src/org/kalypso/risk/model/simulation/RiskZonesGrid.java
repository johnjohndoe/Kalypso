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
package org.kalypso.risk.model.simulation;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.BinaryGeoGridReader;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRiskZoneDefinition;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.util.Assert;

public class RiskZonesGrid extends AbstractDelegatingGeoGrid implements IGeoGrid
{
  private final Map<String, List<BinaryGeoGridReader>> m_gridMap;

  private final IFeatureBindingCollection<IAnnualCoverageCollection> m_annualCoverageCollection;

  private final IFeatureWrapperCollection<ILandusePolygon> m_landusePolygonCollection;

  private final List<ILanduseClass> m_landuseClassesList;

  private final double m_cellSize;

  private BigDecimal m_min;

  private BigDecimal m_max;

  private final List<IRiskZoneDefinition> m_riskZoneDefinitionsList;

  private final IGeoGrid m_resultGrid;

  private boolean m_produceZoneIdentifiers = false;

  private final SortedMap<Double, IRiskZoneDefinition> m_urbanZonesDefinitions = new TreeMap<Double, IRiskZoneDefinition>();

  private final SortedMap<Double, IRiskZoneDefinition> m_nonUrbanZonesDefinitions = new TreeMap<Double, IRiskZoneDefinition>();

  private Coordinate m_origin;

  private Coordinate m_offsetX;

  private Coordinate m_offsetY;

  private IGeoTransformer m_geoTransformer;

  public RiskZonesGrid( final IGeoGrid resultGrid, final IFeatureBindingCollection<IAnnualCoverageCollection> annualCoverageCollection, final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection, final List<ILanduseClass> landuseClassesList, final List<IRiskZoneDefinition> riskZoneDefinitionsList ) throws Exception
  {
    super( resultGrid );
    m_riskZoneDefinitionsList = riskZoneDefinitionsList;
    m_produceZoneIdentifiers = false;
    m_resultGrid = resultGrid;

    m_cellSize = Math.abs( resultGrid.getOffsetX().x - resultGrid.getOffsetY().x ) * Math.abs( resultGrid.getOffsetX().y - resultGrid.getOffsetY().y );
    m_annualCoverageCollection = annualCoverageCollection;
    m_landusePolygonCollection = landusePolygonCollection;
    m_landuseClassesList = landuseClassesList;
    m_gridMap = new HashMap<String, List<BinaryGeoGridReader>>();

    final ILandusePolygon landusePolygon = m_landusePolygonCollection.get( 0 );
    String coordinateSystem = landusePolygon.getGeometry().getCoordinateSystem();
    if( coordinateSystem == null )
    {
      coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    }

    m_geoTransformer = GeoTransformerFactory.getGeoTransformer( coordinateSystem );

    m_origin = m_resultGrid.getOrigin();
    m_offsetX = m_resultGrid.getOffsetX();
    m_offsetY = m_resultGrid.getOffsetY();

    for( final IAnnualCoverageCollection collection : m_annualCoverageCollection )
    {
      final List<BinaryGeoGridReader> gridList = new ArrayList<BinaryGeoGridReader>();
      IFeatureBindingCollection<ICoverage> coverages = collection.getCoverages();
      for( final ICoverage coverage : coverages )
      {
        RectifiedGridCoverageGeoGrid grid = (RectifiedGridCoverageGeoGrid) GeoGridUtilities.toGrid( coverage );
        BinaryGeoGridReader lReader = new BinaryGeoGridReader( grid, grid.getGridURL() );
        gridList.add( lReader );
      }

      m_gridMap.put( collection.getId(), gridList );
    }

    m_min = new BigDecimal( Double.MAX_VALUE ).setScale( 1, BigDecimal.ROUND_HALF_UP );
    m_max = new BigDecimal( -Double.MAX_VALUE ).setScale( 1, BigDecimal.ROUND_HALF_UP );

    for( final IRiskZoneDefinition riskZoneDefinition : m_riskZoneDefinitionsList )
      if( riskZoneDefinition.isUrbanLanduseType() )
        m_urbanZonesDefinitions.put( riskZoneDefinition.getLowerBoundary(), riskZoneDefinition );
      else
        m_nonUrbanZonesDefinitions.put( riskZoneDefinition.getLowerBoundary(), riskZoneDefinition );

  }

  /**
   * @see org.kalypso.grid.AbstractDelegatingGeoGrid#dispose()
   */
  @Override
  public void dispose( )
  {
    final Set<Entry<String, List<BinaryGeoGridReader>>> entrySet = m_gridMap.entrySet();
    for( final Entry<String, List<BinaryGeoGridReader>> entry : entrySet )
    {
      final List<BinaryGeoGridReader> myList = entry.getValue();
      for( final BinaryGeoGridReader geoGrid : myList )
      {
        geoGrid.dispose();
      }
    }

    super.dispose();
  }

  @Override
  public final double getValue( final int x, final int y ) throws GeoGridException
  {
    if( m_landusePolygonCollection.size() == 0 )
      return Double.NaN;

    try
    {
      /* we need a sorted list of the annual coverage collections */
      final SortedMap<Double, IAnnualCoverageCollection> covMap = new TreeMap<Double, IAnnualCoverageCollection>();
      for( final IAnnualCoverageCollection cov : m_annualCoverageCollection )
      {
        final IAnnualCoverageCollection previousValue = covMap.put( cov.getReturnPeriod().doubleValue(), cov );
        if( previousValue != null )
          throw new IllegalArgumentException( org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.simulation.RiskZonesGrid.1" ) + cov.getReturnPeriod().doubleValue() ); //$NON-NLS-1$
      }

      final Collection<IAnnualCoverageCollection> collections = covMap.values();
      final IAnnualCoverageCollection[] covArray = collections.toArray( new IAnnualCoverageCollection[collections.size()] );

      final double cx = m_origin.x + x * m_offsetX.x + y * m_offsetY.x;
      final double cy = m_origin.y + x * m_offsetX.y + y * m_offsetY.y;
      final Coordinate coordinate = new Coordinate( cx, cy );

      Assert.isTrue( m_annualCoverageCollection.size() == covArray.length );

      /* fill the probabilities and damages */
      final double[] damage = new double[covArray.length];
      final double[] probability = new double[covArray.length];

      for( int i = covArray.length - 1; i >= 0; i-- )
      {
        final IAnnualCoverageCollection collection = covArray[i];
        final double value = getValue( collection, coordinate );

        damage[i] = Double.isNaN( value ) ? 0.0 : value;
        probability[i] = 1.0 / collection.getReturnPeriod();
      }

      /* calculate average annual damage */
      final double averageAnnualDamageValue = RiskModelHelper.calcAverageAnnualDamageValue( damage, probability );

      if( averageAnnualDamageValue <= 0.0 || Double.isNaN( averageAnnualDamageValue ) )
        return Double.NaN;

      final GM_Position positionAt = JTSAdapter.wrap( coordinate );
      final GM_Position position = m_geoTransformer.transform( positionAt, m_resultGrid.getSourceCRS() );

      /* This list has some unknown cs. */
      final List<ILandusePolygon> list = m_landusePolygonCollection.query( position );

      if( list == null || list.size() == 0 )
        return Double.NaN;

      for( final ILandusePolygon polygon : list )
      {
        if( polygon.contains( position ) )
        {
          final Integer landuseClassOrdinalNumber = polygon.getLanduseClassOrdinalNumber();

          /* set statistic for landuse class */
          fillStatistics( averageAnnualDamageValue, landuseClassOrdinalNumber );

          final double riskZone = getRiskZone( averageAnnualDamageValue, polygon.isUrbanLanduseType() );
          if( Double.isNaN( riskZone ) )
            return Double.NaN;
          final double returnValue;
          if( m_produceZoneIdentifiers )
            returnValue = riskZone;
          else
            returnValue = polygon.isUrbanLanduseType() ? averageAnnualDamageValue : -averageAnnualDamageValue;

          if( Double.isInfinite( returnValue ) || Double.isNaN( returnValue ) )
            return Double.NaN;

          /* check min/max */
          m_min = m_min.min( new BigDecimal( returnValue ).setScale( 4, BigDecimal.ROUND_HALF_UP ) );
          m_max = m_max.max( new BigDecimal( returnValue ).setScale( 4, BigDecimal.ROUND_HALF_UP ) );

          // if(!Double.isNaN( returnValue )&& returnValue!=0.0)
          // System.out.println(returnValue);

          return returnValue;
        }
      }
      return Double.NaN;
    }
    catch( final Exception ex )
    {
      throw new GeoGridException( Messages.getString( "org.kalypso.risk.model.simulation.RiskZonesGrid.0" ), ex ); //$NON-NLS-1$
    }
  }

  private void fillStatistics( final double averageAnnualDamageValue, final Integer landuseClassOrdinalNumber )
  {
    /* add the current average annual damage value to all landuse polygons that covers the current raster cell */
    // polygon.updateStatisticsAverageAnnualDamage( averageAnnualDamageValue );
    /* find the right landuse class that holds the polygon */
    for( final ILanduseClass landuseClass : m_landuseClassesList )
    {
      final double cellSize = landuseClass.getCellSize();
      if( Double.isNaN( cellSize ) )
      {
        landuseClass.setCellSize( m_cellSize );
      }

      if( landuseClass.getOrdinalNumber() == landuseClassOrdinalNumber )
      {
        landuseClass.updateStatisticsAverageAnnualDamage( averageAnnualDamageValue );
      }
    }
  }

  /**
   * returns the flow depth value for a given position.
   * 
   * @param collection
   *          grid collection of water depth grids
   * @param x
   *          column number of the grid
   * @param y
   *          row number of the grid
   * @return the first found valid grid value within the grid collection
   * @throws GeoGridException
   */
  private double getValue( final IAnnualCoverageCollection collection, final Coordinate coordinate ) throws GeoGridException
  {
    final List<BinaryGeoGridReader> list = m_gridMap.get( collection.getId() );
    for( final BinaryGeoGridReader geoGrid : list )
    {
      if( geoGrid.getEnvelope().contains( coordinate ) )
      {
        final double value = geoGrid.getValue( coordinate );

        if( Double.isNaN( value ) )
          return Double.NaN;

        // we allow no negative flow depths!
        // if( value < 0.0 )
        // return 0.0;

        return value;
      }
    }
    return Double.NaN;
  }

  private double getRiskZone( final double damageValue, final Boolean isUrbanLanduseType )
  {
    if( isUrbanLanduseType == null )
      return Double.NaN;

    final SortedMap<Double, IRiskZoneDefinition> defs = getRiskZoneDefinition( isUrbanLanduseType );
    final SortedMap<Double, IRiskZoneDefinition> headMap = defs.headMap( damageValue );
    if( headMap.isEmpty() )
      return Double.NaN;

    final Double lastKey = headMap.lastKey();
    return headMap.get( lastKey ).getOrdinalNumber();
  }

  private SortedMap<Double, IRiskZoneDefinition> getRiskZoneDefinition( final boolean isUrbanLanduse )
  {
    return isUrbanLanduse ? m_urbanZonesDefinitions : m_nonUrbanZonesDefinitions;
  }

}
