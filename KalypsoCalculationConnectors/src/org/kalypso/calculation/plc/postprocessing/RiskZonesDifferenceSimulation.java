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
package org.kalypso.calculation.plc.postprocessing;

import java.io.File;
import java.io.FileOutputStream;
import java.net.URL;
import java.util.Date;
import java.util.Locale;
import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.gml.ui.coverage.CoverageManagementHelper;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.simulation.ISimulationSpecKalypsoRisk;
import org.kalypso.risk.model.simulation.RiskZonesDifferenceGrid;
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
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Dejan Antanaskovic
 */
public class RiskZonesDifferenceSimulation implements ISimulationSpecKalypsoRisk, ISimulation
{
  public static final String ID = "KalypsoRisk_RiskZonesDifferenceCalculation"; //$NON-NLS-1$

  public final static String RASTER_MODEL_1 = "RASTER_MODEL_1"; //$NON-NLS-1$

  public final static String RASTER_MODEL_2 = "RASTER_MODEL_2"; //$NON-NLS-1$

  public final static String OUTPUT_RASTER_MODEL = "OUTPUT_RASTER_MODEL"; //$NON-NLS-1$

  public final static String OUTPUT_RASTER_FOLDER = "OUTPUT_RASTER_FOLDER"; //$NON-NLS-1$

  public final static String OUTPUT_PROPERTIES_FILE = "OUTPUT_PROPERTIES_FILE"; //$NON-NLS-1$

  private double m_totalDifference = 0.0;

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/Specification_RiskZonesDifferenceCalculation.xml" ); //$NON-NLS-1$
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

      final File outputRasterTmpDir = new File( tmpdir, "rasters" ); //$NON-NLS-1$
      outputRasterTmpDir.mkdirs();

      final File tmpRasterModel = File.createTempFile( IRasterDataModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      final GMLWorkspace ws_model_output = FeatureFactory.createGMLWorkspace( IRasterDataModel.QNAME, tmpRasterModel.toURI().toURL(), GmlSerializer.DEFAULT_FACTORY, -1 );
      final IRasterDataModel model_output = (IRasterDataModel) ws_model_output.getRootFeature().getAdapter( IRasterDataModel.class );

      doRiskZonesCalculation( outputRasterTmpDir, model_output, model_1, model_2, simulationMonitorAdaptor );

      GmlSerializer.serializeWorkspace( tmpRasterModel, ws_model_output, "UTF-8" ); //$NON-NLS-1$

      final File propertiesFile = new File( tmpdir, "stats.txt" ); //$NON-NLS-1$
      final Properties properties = new Properties();
      properties.put( IRiskZones.PROPERTY_TOTAL_DIFFERENCE, String.format( Locale.ENGLISH, IRiskZones.NUMBER_FORMAT, m_totalDifference ) ); //$NON-NLS-1$ //$NON-NLS-2$
      properties.put( IRiskZones.PROPERTY_ANNUAL_COSTS, IRiskZones.VALUE_NOT_AVAILABLE );
      properties.put( IRiskZones.PROPERTY_VALUE_BENEFIT, IRiskZones.VALUE_NOT_AVAILABLE );
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
      throw new SimulationException( "Fehler bei der Berechnung der Risikodifferenzen.", e );
    }
  }

  private void doRiskZonesCalculation( final File tmpdir, final IRasterDataModel rasterModelOutput, final IRasterDataModel rasterModelInput1, final IRasterDataModel rasterModelInput2, final IProgressMonitor monitor ) throws SimulationException
  {
    final int importantDigits = KalypsoRiskPreferencePage.MAX_RISKTHEMEINFO_PRECISION;
    final SubMonitor subMonitor = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.risk.model.simulation.RiskZonesCalculationHandler.7" ), 100 ); //$NON-NLS-1$

    try
    {
      /* remove existing (invalid) coverages from the model */
      final ICoverageCollection outputCoverages = rasterModelOutput.getRiskZonesCoverage();
      final IFeatureBindingCollection<ICoverage> outputCoveragesList = outputCoverages.getCoverages();
      for( final ICoverage coverage : outputCoveragesList )
        CoverageManagementHelper.deleteRangeSetFile( coverage );

      outputCoveragesList.clear();

      final ICoverageCollection inputCoverages1 = rasterModelInput1.getRiskZonesCoverage();
      final ICoverageCollection inputCoverages2 = rasterModelInput2.getRiskZonesCoverage();

      final IFeatureBindingCollection<ICoverage> inputCoverages1List = inputCoverages1.getCoverages();
      final IFeatureBindingCollection<ICoverage> inputCoverages2List = inputCoverages2.getCoverages();
      if( inputCoverages1List.size() != inputCoverages2List.size() )
        return;

      for( int i = 0; i < inputCoverages1List.size(); i++ )
      {
        final ICoverage inputCoverage1 = inputCoverages1List.get( i );
        ICoverage inputCoverage2 = null;
        for( final ICoverage coverage : inputCoverages2List )
        {
          if( coverage.getBoundedBy().equals( inputCoverage1.getBoundedBy() ) )
          {
            inputCoverage2 = coverage;
            break;
          }
        }

        if( inputCoverage2 != null )
        {
          final IGeoGrid inputGrid1 = GeoGridUtilities.toGrid( inputCoverage1 );
          final IGeoGrid inputGrid2 = GeoGridUtilities.toGrid( inputCoverage2 );
          final IGeoGrid outputGrid = new RiskZonesDifferenceGrid( inputGrid1, inputGrid2 );

          final String outputCoverageFileName = String.format( "%s_%02d.bin", outputCoverages.getId(), i ); //$NON-NLS-1$
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
        final GMLWorkspace workspace = rasterModelOutput.getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, rasterModelOutput, new Feature[] { outputCoverages }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

    }
    catch( final Exception e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.RiskCalcRiskZonesRunnable.6" ) + org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_RiskZonesCalculation.4" ), e ); //$NON-NLS-1$ //$NON-NLS-2$
    }

  }

}
