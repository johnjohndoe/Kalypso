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

import java.io.IOException;

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
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypso.grid.SequentialBinaryGeoGrid;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

/**
 * @author Gernot Belger
 */
public class StatisticCalculationOperation implements ICoreRunnableWithProgress
{
  private final StatisticCalculationData m_data;

  // REMARK: we know that the landuse is transformed into the Kalypso srs after loading.
  private final StatisticCollector m_statistics = new StatisticCollector( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

  public StatisticCalculationOperation( final StatisticCalculationData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor );
    progress.beginTask( Messages.getString( "StatisticCalculationOperation_0" ), 100 ); //$NON-NLS-1$

    buildStatisticElements( progress.newChild( 10, SubMonitor.SUPPRESS_NONE ) );

    collectSpecificDamages( progress.newChild( 80, SubMonitor.SUPPRESS_NONE ) );

    writeResultObservation( progress.newChild( 10, SubMonitor.SUPPRESS_NONE ) );

    monitor.done();

    return new Status( IStatus.OK, KalypsoRiskPlugin.PLUGIN_ID, Messages.getString( "StatisticCalculationOperation_1" ) ); //$NON-NLS-1$
  }

  private void buildStatisticElements( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final IRasterizationControlModel controlModel = m_data.getControlModel();
      final ILandusePolygonCollection landusePolygons = m_data.getLandusePolygons();

      final ShapeFile shape = m_data.loadSelectedShape();
      final String shapeNameAttribute = m_data.getSelectedAttribute();
      final String shapeSRS = m_data.getShapeSRS();

      final StatisticElementBuilder builder = new StatisticElementBuilder( controlModel, m_statistics.getSRSName() );
      builder.createElements( landusePolygons, shape, shapeNameAttribute, shapeSRS, monitor );
      m_statistics.setItems( builder.getItems() );
    }
    catch( final IOException | GM_Exception | DBaseException | FactoryException | TransformException e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoRiskPlugin.PLUGIN_ID, Messages.getString( "StatisticCalculationOperation_2" ), e ); //$NON-NLS-1$
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

    progress.beginTask( Messages.getString( "StatisticCalculationOperation_3" ), coverageCount ); //$NON-NLS-1$

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
        progress.subTask( String.format( Messages.getString( "StatisticCalculationOperation_4" ), specificDamageEvent.getName(), i ) ); //$NON-NLS-1$

        final RectifiedGridCoverageGeoGrid templateGrid = (RectifiedGridCoverageGeoGrid)GeoGridUtilities.toGrid( coverage );

        try
        {
          final IGeoGrid grid = new SequentialBinaryGeoGrid( templateGrid, templateGrid.getGridURL() );

          final IGeoWalkingStrategy walkingStrategy = grid.getWalkingStrategy();
          // TODO: would be nice to use the sequential way of accessing the grid here; but that api is not designed
          // to be useed like that
          final IGeoGridWalker walker = new SpecificDamageWalker( m_statistics, returnPeriod );
          // FIXME: monitor
          walkingStrategy.walk( grid, walker, null, progress.newChild( 1, SubMonitor.SUPPRESS_BEGINTASK | SubMonitor.SUPPRESS_SUBTASK ) );

          grid.dispose();
        }
        catch( final GeoGridException e )
        {
          e.printStackTrace();
          // TODO: error handling?
        }
        catch( final IOException e )
        {
          e.printStackTrace();
          // TODO: error handling?
        }

        templateGrid.dispose();
      }
    }
  }

  private void writeResultObservation( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor );
    progress.beginTask( Messages.getString( "StatisticCalculationOperation_5" ), 100 ); //$NON-NLS-1$

    final IRasterizationControlModel controlModel = m_data.getControlModel();
    m_statistics.createResultObservation( controlModel );
  }
}
