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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.IGeoGridWalker;
import org.kalypso.grid.IGeoWalkingStrategy;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.shape.ShapeFile;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Gernot Belger
 */
public class StatisticCalculationOperation implements ICoreRunnableWithProgress
{
  private final StatisticCalculationData m_data;

  private final StatisticCollector m_statistics = new StatisticCollector();

  public StatisticCalculationOperation( final StatisticCalculationData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor );
    progress.beginTask( "Statistic Calculation", 100 );

    buildStatisticElements( progress.newChild( 5, SubMonitor.SUPPRESS_NONE ) );

    collectSpecificDamages( progress.newChild( 70, SubMonitor.SUPPRESS_NONE ) );
    collectAverageDamage( progress.newChild( 20, SubMonitor.SUPPRESS_NONE ) );

    writeResultObservation( progress.newChild( 5, SubMonitor.SUPPRESS_NONE ) );

    monitor.done();

    return new Status( IStatus.OK, KalypsoRiskPlugin.PLUGIN_ID, "Operation successfully terminated" );
  }

  private void buildStatisticElements( final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor );
    progress.beginTask( "Building groups", 100 );

    try
    {
      final IRasterizationControlModel controlModel = m_data.getControlModel();
      final ILandusePolygonCollection landusePolygons = m_data.getLandusePolygons();

      final ShapeFile shape = m_data.loadSelectedShape();

      final StatisticElementBuilder builder = new StatisticElementBuilder( controlModel );
      builder.addElements( landusePolygons, shape );
      m_statistics.setItems( builder.getItems() );
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoRiskPlugin.PLUGIN_ID, "Failed to load shape file", e );
      throw new CoreException( status );
    }
  }

  private void collectSpecificDamages( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor );

    final IRasterDataModel rasterModel = m_data.getRasterModel();

    final IFeatureBindingCollection<IAnnualCoverageCollection> specificDamages = rasterModel.getSpecificDamageCoverageCollection();

    int coverageCount = 0;
    for( final IAnnualCoverageCollection specificDamageEvent : specificDamages )
    {
      final IFeatureBindingCollection<ICoverage> coverages = specificDamageEvent.getCoverages();
      coverageCount += coverages.size();
    }

    progress.beginTask( "Collecting specific damage", coverageCount );

    for( final IAnnualCoverageCollection specificDamageEvent : specificDamages )
    {
      final Integer returnPeriod = specificDamageEvent.getReturnPeriod();
      /* Ignore invalid specific damages */
      if( returnPeriod == null || returnPeriod <= 0 )
        continue;

      final IFeatureBindingCollection<ICoverage> coverages = specificDamageEvent.getCoverages();
      for( int i = 0; i < coverages.size(); i++ )
      {
        final ICoverage coverage = coverages.get( i );
        progress.subTask( String.format( "%s - Grid %d", specificDamageEvent.getName(), i ) );

        final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

        try
        {
          final IGeoWalkingStrategy walkingStrategy = grid.getWalkingStrategy();
          final IGeoGridWalker walker = new SpecificDamageWalker( m_statistics, returnPeriod );
          // FIXME: monitor
          walkingStrategy.walk( grid, walker, null, progress.newChild( 1, SubMonitor.SUPPRESS_BEGINTASK | SubMonitor.SUPPRESS_SUBTASK ) );
        }
        catch( final GeoGridException e )
        {
          e.printStackTrace();
          // TODO: error handling?
        }

        grid.dispose();
      }
    }
  }

  private void collectAverageDamage( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor );

    final IRasterDataModel rasterModel = m_data.getRasterModel();

    final ICoverageCollection riskZonesCoverage = rasterModel.getRiskZonesCoverage();

    final IFeatureBindingCollection<ICoverage> coverages = riskZonesCoverage.getCoverages();
    progress.beginTask( "Collecting average damage", coverages.size() );

    for( int i = 0; i < coverages.size(); i++ )
    {
      progress.subTask( String.format( "Grid %d", i ) );

      final ICoverage coverage = coverages.get( i );

      final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

      try
      {
        final IGeoWalkingStrategy walkingStrategy = grid.getWalkingStrategy();
        final IGeoGridWalker walker = new AverageDamageWalker( m_statistics );
        walkingStrategy.walk( grid, walker, null, progress.newChild( 1 ) );
      }
      catch( final GeoGridException e )
      {
        e.printStackTrace();
        // TODO: error handling?
      }

      grid.dispose();
    }
  }

  private void writeResultObservation( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor );
    progress.beginTask( "Building result table", 100 );

    final IRasterizationControlModel controlModel = m_data.getControlModel();
    m_statistics.createResultObservation( controlModel );
  }
}
