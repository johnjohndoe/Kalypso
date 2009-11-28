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
import java.io.FileOutputStream;
import java.net.URL;
import java.util.Date;
import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.kalypso.gml.ui.map.CoverageManagementHelper;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class SimulationKalypsoRisk_RiskZonesDifferenceCalculation implements ISimulationSpecKalypsoRisk, ISimulation
{
  private final static String RASTER_MODEL_1 = "RASTER_MODEL_1"; //$NON-NLS-1$

  private final static String RASTER_MODEL_2 = "RASTER_MODEL_2"; //$NON-NLS-1$

  private final static String OUTPUT_RASTER_MODEL = "OUTPUT_RASTER_MODEL"; //$NON-NLS-1$

  private final static String OUTPUT_RASTER_FOLDER = "OUTPUT_RASTER_FOLDER"; //$NON-NLS-1$

  private final static String OUTPUT_PROPERTIES_FILE = "OUTPUT_PROPERTIES_FILE";

  private double m_totalDifference = 0.0;

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "Specification_RiskZonesDifferenceCalculation.xml" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
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

      doRiskZonesCalculation( outputRasterTmpDir, model_output, model_1, model_2, simulationMonitorAdaptor );
      final File tmpRasterModel = File.createTempFile( IRasterDataModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpRasterModel, ws_model_output, "UTF-8" ); //$NON-NLS-1$

      final File propertiesFile = new File( tmpdir, "stats.txt" );
      final Properties properties = new Properties();
//      properties.put( "TOTAL_DIFFERENCE", NumberFormat.getCurrencyInstance().format( m_totalDifference ) );
      properties.put( "TOTAL_DIFFERENCE_UNFORMAT", String.format("%.8f", m_totalDifference ) );
      properties.put( "TOTAL_DIFFERENCE", String.format("%.2f Eur", m_totalDifference ) );
      properties.put( "YEARLY_COSTS", "n/a" );
      properties.put( "VALUE_BENEFIT", "n/a" );
      final FileOutputStream propertiesStream = new FileOutputStream( propertiesFile );
      properties.store( propertiesStream, "Scenario statistics" );
      propertiesStream.flush();
      propertiesStream.close();

      resultEater.addResult( OUTPUT_RASTER_MODEL, tmpRasterModel );
      resultEater.addResult( OUTPUT_RASTER_FOLDER, outputRasterTmpDir );
      resultEater.addResult( OUTPUT_PROPERTIES_FILE, propertiesFile );
    }
    catch( final Exception e )
    {
      throw new SimulationException( e.getLocalizedMessage() );
    }
  }

  private void doRiskZonesCalculation( final File tmpdir, final IRasterDataModel rasterModelOutput, final IRasterDataModel rasterModelInput1, final IRasterDataModel rasterModelInput2, final IProgressMonitor monitor ) throws SimulationException
  {
    final IPreferenceStore preferences = KalypsoRiskPreferencePage.getPreferences();
    final int importantDigits = preferences.getInt( KalypsoRiskPreferencePage.KEY_RISKTHEMEINFO_IMPORTANTDIGITS );
    
    final SubMonitor subMonitor = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.risk.model.simulation.RiskZonesCalculationHandler.7" ), 100 ); //$NON-NLS-1$

    try
    {
      /* remove existing (invalid) coverages from the model */
      final ICoverageCollection outputCoverages = rasterModelOutput.getRiskZonesCoverage();
      for( final ICoverage coverage : outputCoverages )
        CoverageManagementHelper.deleteGridFile( coverage );

      outputCoverages.clear();

      final ICoverageCollection inputCoverages1 = rasterModelInput1.getRiskZonesCoverage();
      final ICoverageCollection inputCoverages2 = rasterModelInput2.getRiskZonesCoverage();

      if( inputCoverages1.size() != inputCoverages2.size() )
        return;

      for( int i = 0; i < inputCoverages1.size(); i++ )
      {
        final ICoverage inputCoverage1 = inputCoverages1.get( i );
        ICoverage inputCoverage2 = null;
        for( final ICoverage iCoverage : inputCoverages2 )
        {
          if( iCoverage.getEnvelope().equals( inputCoverage1.getEnvelope() ) )
          {
            inputCoverage2 = iCoverage;
            break;
          }
        }

        if( inputCoverage2 != null )
        {
          final IGeoGrid inputGrid1 = GeoGridUtilities.toGrid( inputCoverage1 );
          final IGeoGrid inputGrid2 = GeoGridUtilities.toGrid( inputCoverage2 );
          final IGeoGrid outputGrid = new RiskZonesDifferenceGrid( inputGrid1, inputGrid2 );

          final String outputCoverageFileName = String.format( "%s_%02d.bin", outputCoverages.getGmlID(), i ); //$NON-NLS-1$
          final String outputCoverageFileRelativePath = CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + outputCoverageFileName;
          final File outputCoverageFile = new File( tmpdir.getAbsolutePath(), outputCoverageFileName );
          final ICoverage newCoverage = GeoGridUtilities.addCoverage( outputCoverages, outputGrid, importantDigits, outputCoverageFile, outputCoverageFileRelativePath, "image/bin", subMonitor.newChild( 100, SubMonitor.SUPPRESS_ALL_LABELS ) ); //$NON-NLS-1$

          m_totalDifference += ((RiskZonesDifferenceGrid) outputGrid).getDifference();

          outputGrid.dispose();
          inputGrid1.dispose();
          inputGrid2.dispose();

          newCoverage.setName( Messages.getString( "org.kalypso.risk.model.simulation.RiskCalcRiskZonesRunnable.4" ) + i + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
          newCoverage.setDescription( Messages.getString( "org.kalypso.risk.model.simulation.RiskZonesCalculationHandler.9" ) + new Date().toString() ); //$NON-NLS-1$
        }

        /* fireModellEvent to redraw a map */
        final GMLWorkspace workspace = rasterModelOutput.getFeature().getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, rasterModelOutput.getFeature(), new Feature[] { outputCoverages.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.RiskCalcRiskZonesRunnable.6" ) + org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_RiskZonesCalculation.4" ) + e.getLocalizedMessage() ); //$NON-NLS-1$ //$NON-NLS-2$
    }

  }

}
