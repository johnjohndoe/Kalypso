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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.gml.ui.map.CoverageManagementHelper;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.SubstractionGrid;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class SimulationKalypsoRisk_InnundationDifferenceCalculation implements ISimulationSpecKalypsoRisk, ISimulation
{
  private final static String RASTER_MODEL_1 = "RASTER_MODEL_1"; //$NON-NLS-1$

  private final static String RASTER_MODEL_2 = "RASTER_MODEL_2"; //$NON-NLS-1$

  private final static String OUTPUT_RASTER_MODEL = "OUTPUT_RASTER_MODEL"; //$NON-NLS-1$

  private final static String OUTPUT_RASTER_FOLDER = "OUTPUT_RASTER_FOLDER"; //$NON-NLS-1$

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "Specification_InnundationDifferenceCalculation.xml" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      final IProgressMonitor simulationMonitorAdaptor = new SimulationMonitorAdaptor( monitor );

      final GMLWorkspace ws_model_1 = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RASTER_MODEL_1 ), null );
      final IRasterDataModel model_1 = (IRasterDataModel) ws_model_1.getRootFeature().getAdapter( IRasterDataModel.class );

      final GMLWorkspace ws_model_2 = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RASTER_MODEL_2 ), null );
      final IRasterDataModel model_2 = (IRasterDataModel) ws_model_2.getRootFeature().getAdapter( IRasterDataModel.class );

      final GMLWorkspace ws_model_output = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( OUTPUT_RASTER_MODEL ), null );
      final IRasterDataModel model_output = (IRasterDataModel) ws_model_output.getRootFeature().getAdapter( IRasterDataModel.class );

      final File outputRasterTmpDir = new File( tmpdir, "rasters" ); //$NON-NLS-1$
      outputRasterTmpDir.mkdirs();

      doInnundationDifferenceCalculation( outputRasterTmpDir, model_output, model_1, model_2, simulationMonitorAdaptor );
      final File tmpRasterModel = File.createTempFile( IRasterDataModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpRasterModel, ws_model_output, "UTF-8" ); //$NON-NLS-1$

      resultEater.addResult( OUTPUT_RASTER_MODEL, tmpRasterModel );
      resultEater.addResult( OUTPUT_RASTER_FOLDER, outputRasterTmpDir );
    }
    catch( final Exception e )
    {
      throw new SimulationException( e.getLocalizedMessage() );
    }
  }

  private void doInnundationDifferenceCalculation( final File tmpdir, final IRasterDataModel rasterModelOutput, final IRasterDataModel rasterModelInput1, final IRasterDataModel rasterModelInput2, final IProgressMonitor monitor ) throws SimulationException
  {
    final int importantDigits = KalypsoRiskPreferencePage.MAX_RISKTHEMEINFO_PRECISION;

    final SubMonitor subMonitor = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.0" ), 100 ); //$NON-NLS-1$

    try
    {
      /* remove existing (invalid) coverages from the model */
      final IFeatureBindingCollection<IAnnualCoverageCollection> resultCollection = rasterModelOutput.getWaterlevelCoverageCollection();
      for( final IAnnualCoverageCollection collection : resultCollection )
      {
        final IFeatureBindingCollection<ICoverage> coverages = collection.getCoverages();
        for( final ICoverage coverage : coverages )
          CoverageManagementHelper.deleteGridFile( coverage );
      }
      resultCollection.clear();

      final IFeatureBindingCollection<IAnnualCoverageCollection> inputCoverageCollection1 = rasterModelInput1.getWaterlevelCoverageCollection();
      final IFeatureBindingCollection<IAnnualCoverageCollection> inputCoverageCollection2 = rasterModelInput2.getWaterlevelCoverageCollection();

      if( inputCoverageCollection1.size() != inputCoverageCollection2.size() )
        return;

      // select representative HQ collection (HQ100)
      IAnnualCoverageCollection collection1_HQ100 = null;
      IAnnualCoverageCollection collection2_HQ100 = null;
      for( int i = 0; i < inputCoverageCollection1.size(); i++ )
      {
        final IAnnualCoverageCollection collection1 = inputCoverageCollection1.get( i );
        final IAnnualCoverageCollection collection2 = inputCoverageCollection2.get( i );
        if( collection1.getReturnPeriod() == 100 )
          collection1_HQ100 = collection1;
        if( collection2.getReturnPeriod() == 100 )
          collection2_HQ100 = collection2;
      }

      if( collection1_HQ100 == null || collection2_HQ100 == null )
        return;

      final IAnnualCoverageCollection resultCoverageCollection = resultCollection.addNew( IAnnualCoverageCollection.QNAME );
      resultCoverageCollection.setReturnPeriod( 100 );
      resultCoverageCollection.setName( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.1" ) ); //$NON-NLS-1$
      resultCoverageCollection.setDescription( String.format( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.2" ), 100 ) ); //$NON-NLS-1$

      // calculate actual difference
      final IFeatureBindingCollection<ICoverage> coverages1_HG100 = collection1_HQ100.getCoverages();
      for( int i = 0; i < coverages1_HG100.size(); i++ )
      {
        final ICoverage inputCoverage1 = coverages1_HG100.get( i );
        ICoverage inputCoverage2 = null;

        // find the appropriate coverage from another collection
        // we assumed that both collections contains coverage sets which member coverage always covers the same area in
        // both sets, which is the case for Planer-Client calculation
        final IFeatureBindingCollection<ICoverage> coverages2_HQ100 = collection2_HQ100.getCoverages();
        for( final ICoverage coverage : coverages2_HQ100 )
        {
          if( coverage.getEnvelope().equals( inputCoverage1.getEnvelope() ) )
          {
            inputCoverage2 = coverage;
            break;
          }
        }

        if( inputCoverage2 != null )
        {
          final IGeoGrid inputGrid1 = GeoGridUtilities.toGrid( inputCoverage1 );
          final IGeoGrid inputGrid2 = GeoGridUtilities.toGrid( inputCoverage2 );
          final SubstractionGrid outputGrid = new SubstractionGrid( inputGrid1, inputGrid2 );
          outputGrid.usePositiveValuesOnly( true );

          final String outputCoverageFileName = String.format( "%s_%02d.bin", resultCoverageCollection.getId(), i ); //$NON-NLS-1$
          final String outputCoverageFileRelativePath = CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + outputCoverageFileName;
          final File outputCoverageFile = new File( tmpdir.getAbsolutePath(), outputCoverageFileName );
          final ICoverage newCoverage = GeoGridUtilities.addCoverage( resultCoverageCollection, outputGrid, importantDigits, outputCoverageFile, outputCoverageFileRelativePath, "image/bin", subMonitor.newChild( 100, SubMonitor.SUPPRESS_ALL_LABELS ) ); //$NON-NLS-1$

          outputGrid.dispose();
          inputGrid1.dispose();
          inputGrid2.dispose();

          newCoverage.setName( String.format( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.3" ), i ) ); //$NON-NLS-1$ //$NON-NLS-2$
          newCoverage.setDescription( String.format( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.4" ), new Date().toString() ) ); //$NON-NLS-1$
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.5" ) + org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_RiskZonesCalculation.4" ) + e.getLocalizedMessage() ); //$NON-NLS-1$ //$NON-NLS-2$
    }

  }

}
