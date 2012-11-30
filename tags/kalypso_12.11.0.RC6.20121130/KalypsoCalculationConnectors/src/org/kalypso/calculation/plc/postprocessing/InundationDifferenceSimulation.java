/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra√üe 22
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
import java.net.URL;
import java.util.Date;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.SubstractionGrid;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.i18n.Messages;
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
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Dejan Antanaskovic, Stefan Kurzbach
 */
public class InundationDifferenceSimulation implements ISimulation
{
  public static final String ID = "KalypsoRisk_InnundationDifferenceCalculation"; //$NON-NLS-1$

  public static final String RASTER_MODEL_1 = "RASTER_MODEL_1"; //$NON-NLS-1$

  public static final String RASTER_MODEL_2 = "RASTER_MODEL_2"; //$NON-NLS-1$

  public static final String OUTPUT_RASTER_MODEL = "OUTPUT_RASTER_MODEL"; //$NON-NLS-1$

  public static final String OUTPUT_RASTER_FOLDER = "OUTPUT_RASTER_FOLDER"; //$NON-NLS-1$

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/Specification_InnundationDifferenceCalculation.xml" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      final IProgressMonitor simulationMonitorAdaptor = new SimulationMonitorAdaptor( monitor );

      final GMLWorkspace ws_model_1 = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RASTER_MODEL_1 ), null );
      final IFloodModel model_1 = (IFloodModel) ws_model_1.getRootFeature().getAdapter( IFloodModel.class );

      final GMLWorkspace ws_model_2 = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RASTER_MODEL_2 ), null );
      final IFloodModel model_2 = (IFloodModel) ws_model_2.getRootFeature().getAdapter( IFloodModel.class );

      final File rasterFolder = new File( tmpdir, "events" ); //$NON-NLS-1$
      rasterFolder.mkdirs();

      final File modelsFolder = new File( tmpdir, "models" ); //$NON-NLS-1$
      modelsFolder.mkdirs();
      final File floodModelFile = new File( modelsFolder, "flood.gml" ); //$NON-NLS-1$

      // differences
      doInnundationDifferenceCalculation( floodModelFile, rasterFolder, model_1, model_2, simulationMonitorAdaptor );

      resultEater.addResult( OUTPUT_RASTER_MODEL, floodModelFile );
      resultEater.addResult( OUTPUT_RASTER_FOLDER, rasterFolder );
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Problem bei der Berechnung der ‹berflutungsdifferenzen", e );
    }
  }

  private File doInnundationDifferenceCalculation( final File floodModelFile, final File rasterFolder, final IFloodModel rasterModelInput1, final IFloodModel rasterModelInput2, final IProgressMonitor monitor ) throws SimulationException
  {
    final int importantDigits = KalypsoRiskPreferencePage.MAX_RISKTHEMEINFO_PRECISION;

    final SubMonitor subMonitor = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.0" ), 100 ); //$NON-NLS-1$

    try
    {
      final GMLWorkspace tmpWorkspace = FeatureFactory.createGMLWorkspace( IFloodModel.QNAME, floodModelFile.toURI().toURL(), null );
      final IFloodModel floodModelOutput = (IFloodModel) tmpWorkspace.getRootFeature().getAdapter( IFloodModel.class );
      final IFeatureBindingCollection<IRunoffEvent> resultCollection = floodModelOutput.getEvents();

      final IFeatureBindingCollection<IRunoffEvent> inputCoverageCollection1 = rasterModelInput1.getEvents();
      final IFeatureBindingCollection<IRunoffEvent> inputCoverageCollection2 = rasterModelInput2.getEvents();

      int highestReturnPeriod = 1;
      for( int i = 0; i < inputCoverageCollection1.size(); i++ )
      {
        final IRunoffEvent collection = inputCoverageCollection1.get( i );
        final int returnPeriod = collection.getReturnPeriod();
        if( highestReturnPeriod < returnPeriod )
          highestReturnPeriod = returnPeriod;
      }

      // select representative HQ collection (highest HQ)
      IRunoffEvent collection1_HQ100 = null;
      IRunoffEvent collection2_HQ100 = null;
      for( int i = 0; i < inputCoverageCollection1.size(); i++ )
      {
        final IRunoffEvent collection1 = inputCoverageCollection1.get( i );
        final IRunoffEvent collection2 = inputCoverageCollection2.get( i );
        if( collection1.getReturnPeriod() == highestReturnPeriod )
          collection1_HQ100 = collection1;
        if( collection2.getReturnPeriod() == highestReturnPeriod )
          collection2_HQ100 = collection2;
      }

      final IRunoffEvent diffRunoffEvent = resultCollection.addNew( IRunoffEvent.QNAME );
      diffRunoffEvent.setReturnPeriod( highestReturnPeriod );
      diffRunoffEvent.setName( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.1" ) ); //$NON-NLS-1$
      diffRunoffEvent.setDescription( String.format( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.2" ), highestReturnPeriod ) ); //$NON-NLS-1$

      // calculate actual difference
      final IFeatureBindingCollection<ICoverage> coverages1_HG100 = collection1_HQ100.getResultCoverages().getCoverages();
      for( int i = 0; i < coverages1_HG100.size(); i++ )
      {
        final ICoverage inputCoverage1 = coverages1_HG100.get( i );
        ICoverage inputCoverage2 = null;

        // find the appropriate coverage from another collection
        // we assumed that both collections contains coverage sets which member coverage always covers the same area in
        // both sets, which is the case for Planer-Client calculation
        final IFeatureBindingCollection<ICoverage> coverages2_HQ100 = collection2_HQ100.getResultCoverages().getCoverages();
        for( final ICoverage coverage : coverages2_HQ100 )
        {
          if( coverage.getBoundedBy().equals( inputCoverage1.getBoundedBy() ) )
          {
            inputCoverage2 = coverage;
            break;
          }
        }

        final IGeoGrid inputGrid1 = GeoGridUtilities.toGrid( inputCoverage1 );
        final IGeoGrid inputGrid2 = GeoGridUtilities.toGrid( inputCoverage2 );
        final SubstractionGrid outputGrid = new SubstractionGrid( inputGrid2, inputGrid1 );
        outputGrid.usePositiveValuesOnly( true );

        final String outputCoverageFileName = String.format( "%s_%02d.bin", diffRunoffEvent.getId(), i ); //$NON-NLS-1$
        final String outputCoverageFileRelativePath = "../" + rasterFolder.getName() + "/" + outputCoverageFileName; //$NON-NLS-1$ //$NON-NLS-2$
        final File outputCoverageFile = new File( rasterFolder.getAbsolutePath(), outputCoverageFileName );
        final ICoverage newCoverage = GeoGridUtilities.addCoverage( diffRunoffEvent.getResultCoverages(), outputGrid, importantDigits, outputCoverageFile, outputCoverageFileRelativePath, "image/bin", subMonitor.newChild( 100, SubMonitor.SUPPRESS_ALL_LABELS ) ); //$NON-NLS-1$

        outputGrid.dispose();
        inputGrid1.dispose();
        inputGrid2.dispose();

        newCoverage.setName( String.format( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.3" ), i ) ); //$NON-NLS-1$ //$NON-NLS-2$
        newCoverage.setDescription( String.format( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.4" ), new Date().toString() ) ); //$NON-NLS-1$
      }
      GmlSerializer.serializeWorkspace( floodModelFile, tmpWorkspace, "UTF-8" ); //$NON-NLS-1$
      return floodModelFile;
    }
    catch( final Exception e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.InnundationDifferenceCalculation.5" ) + org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_RiskZonesCalculation.4" ), e ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

}
