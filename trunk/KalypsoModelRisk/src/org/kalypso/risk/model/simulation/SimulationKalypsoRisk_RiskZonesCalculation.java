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

import java.io.File;
import java.net.URL;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gml.ui.coverage.CoverageManagementHelper;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Dejan Antanaskovic
 */
public class SimulationKalypsoRisk_RiskZonesCalculation implements ISimulationSpecKalypsoRisk, ISimulation
{
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "Specification_RiskZonesCalculation.xml" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final int importantDigits = KalypsoRiskPreferencePage.MAX_RISKTHEMEINFO_PRECISION;

    try
    {
      final IProgressMonitor simulationMonitorAdaptor = new SimulationMonitorAdaptor( monitor );
      final GMLWorkspace controlModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.CONTROL_MODEL.name() ), null );
      final IRasterizationControlModel controlModel = (IRasterizationControlModel) controlModelWorkspace.getRootFeature().getAdapter( IRasterizationControlModel.class );

      final GMLWorkspace rasterModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.RASTER_MODEL.name() ), null );
      final IRasterDataModel rasterModel = (IRasterDataModel) rasterModelWorkspace.getRootFeature().getAdapter( IRasterDataModel.class );

      final GMLWorkspace vectorModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.VECTOR_MODEL.name() ), null );
      final IVectorDataModel vectorModel = (IVectorDataModel) vectorModelWorkspace.getRootFeature().getAdapter( IVectorDataModel.class );

      final File outputRasterTmpDir = new File( tmpdir, "outputRaster" ); //$NON-NLS-1$
      outputRasterTmpDir.mkdir();

      // FIXME: move this into a separate operation
      doRiskZonesCalculation( outputRasterTmpDir, controlModel, rasterModel, vectorModel, importantDigits, simulationMonitorAdaptor );

      final File tmpControlModel = File.createTempFile( IRasterizationControlModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      final File tmpRasterModel = File.createTempFile( IRasterDataModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      final File tmpVectorModel = File.createTempFile( IVectorDataModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpControlModel, controlModelWorkspace, "UTF-8" ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpRasterModel, rasterModelWorkspace, "UTF-8" ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpVectorModel, vectorModelWorkspace, "UTF-8" ); //$NON-NLS-1$
      resultEater.addResult( MODELSPEC_KALYPSORISK.CONTROL_MODEL.name(), tmpControlModel );
      resultEater.addResult( MODELSPEC_KALYPSORISK.RASTER_MODEL.name(), tmpRasterModel );
      resultEater.addResult( MODELSPEC_KALYPSORISK.VECTOR_MODEL.name(), tmpVectorModel );
      resultEater.addResult( MODELSPEC_KALYPSORISK.OUTPUT_RASTER_FOLDER.name(), outputRasterTmpDir );
    }
    catch( final Exception ex )
    {
      if( ex instanceof SimulationException )
      {
        final String message = ex.getLocalizedMessage();
        if( message == null || message.trim().length() == 0 )
          throw new SimulationException( Messages.getString( "SimulationKalypsoRisk_RiskZonesCalculation.0" ), ex ); //$NON-NLS-1$
        else
          throw new SimulationException( message, ex ); //$NON-NLS-1$
      }
      else
        throw new SimulationException( Messages.getString( "SimulationKalypsoRisk_RiskZonesCalculation.0" ), ex ); //$NON-NLS-1$
    }
  }

  private void doRiskZonesCalculation( final File tmpdir, final IRasterizationControlModel controlModel, final IRasterDataModel rasterModel, final IVectorDataModel vectorModel, final int importantDigits, final IProgressMonitor monitor ) throws SimulationException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.risk.model.simulation.RiskZonesCalculationHandler.7" ), 100 ); //$NON-NLS-1$

    if( rasterModel.getSpecificDamageCoverageCollection().isEmpty() )
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.RiskZonesCalculationHandler.6" ) ); //$NON-NLS-1$

    try
    {
      // progress.subTask( "deleting old grids" );

      /* remove existing (invalid) coverages from the model and clean statistic */
      /* Rememebr location of old results; we delete later, because the user may still cancel this operation */
      final ICoverageCollection outputCoverages = rasterModel.getRiskZonesCoverage();
      final IFeatureBindingCollection<ICoverage> outputCoveragesList = outputCoverages.getCoverages();
      final Set<URL> oldGridLocations = new HashSet<>( outputCoveragesList.size() );
      for( final ICoverage coverage : outputCoveragesList )
        oldGridLocations.add( CoverageManagementHelper.getFileLocation( coverage ) );
      oldGridLocations.remove( null );

      outputCoveragesList.clear();
      ProgressUtilities.worked( progress, 10 );

      final ICoverageCollection baseCoverages = RiskModelHelper.getMaxReturnPeriodCollection( rasterModel.getSpecificDamageCoverageCollection() );
      final IFeatureBindingCollection<ICoverage> baseCoveragesList = baseCoverages.getCoverages();

      progress.setWorkRemaining( baseCoveragesList.size() );

      for( int i = 0; i < baseCoveragesList.size(); i++ )
      {
        final ICoverage srcSpecificDamageCoverage = baseCoveragesList.get( i );

        final IGeoGrid inputGrid = GeoGridUtilities.toGrid( srcSpecificDamageCoverage );
        final IGeoGrid outputGrid = new RiskZonesGrid( inputGrid, rasterModel.getSpecificDamageCoverageCollection(), vectorModel.getLandusePolygonCollection(), controlModel.getRiskZoneDefinitionsList() );

        // TODO: change name: better: use input name
        final String outputCoverageFileName = String.format( "%s_%02d.bin", outputCoverages.getId(), i ); //$NON-NLS-1$
        final String outputCoverageFileRelativePath = CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + outputCoverageFileName;
        final File outputCoverageFile = new File( tmpdir.getAbsolutePath(), outputCoverageFileName );
        final ICoverage newCoverage = GeoGridUtilities.addCoverage( outputCoverages, outputGrid, importantDigits, outputCoverageFile, outputCoverageFileRelativePath, "image/bin", progress.newChild( 1, SubMonitor.SUPPRESS_ALL_LABELS ) ); //$NON-NLS-1$

        outputGrid.dispose();
        inputGrid.dispose();

        newCoverage.setName( Messages.getString( "org.kalypso.risk.model.simulation.RiskCalcRiskZonesRunnable.4" ) + i + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
        newCoverage.setDescription( Messages.getString( "org.kalypso.risk.model.simulation.RiskZonesCalculationHandler.9" ) + new Date().toString() ); //$NON-NLS-1$

        /* fireModellEvent to redraw a map */
        final GMLWorkspace workspace = rasterModel.getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, rasterModel, new Feature[] { outputCoverages }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

      /* Delete old result files */
      for( final URL oldGridLocation : oldGridLocations )
        CoverageManagementHelper.deleteFile( oldGridLocation );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.RiskCalcRiskZonesRunnable.6" ) + org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_RiskZonesCalculation.4" ) + e.getLocalizedMessage() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }
}
