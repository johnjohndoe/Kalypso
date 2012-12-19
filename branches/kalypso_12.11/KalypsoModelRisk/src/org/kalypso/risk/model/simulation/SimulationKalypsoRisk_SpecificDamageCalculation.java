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
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBElement;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.gml.ui.coverage.CoverageManagementHelper;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypso.grid.parallel.SequentialBinaryGeoGridReader;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.ILandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.simulation.statistics.StatisticCollector;
import org.kalypso.risk.model.simulation.statistics.StatisticElementBuilder;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.model.utils.RiskModelHelper.LAYER_TYPE;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

/**
 * @author Dejan Antanaskovic
 */
public class SimulationKalypsoRisk_SpecificDamageCalculation implements ISimulationSpecKalypsoRisk, ISimulation
{
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "Specification_SpecificDamageCalculation.xml" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final int importantDigits = KalypsoRiskPreferencePage.MAX_RISKTHEMEINFO_PRECISION;
    try
    {
      final IProgressMonitor simulationMonitorAdaptor = new SimulationMonitorAdaptor( monitor );
      simulationMonitorAdaptor.beginTask( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.1" ), 100 ); //$NON-NLS-1$
      final GMLWorkspace controlModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.CONTROL_MODEL.toString() ), null );
      final IRasterizationControlModel controlModel = (IRasterizationControlModel) controlModelWorkspace.getRootFeature().getAdapter( IRasterizationControlModel.class );

      final GMLWorkspace rasterModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.RASTER_MODEL.toString() ), null );
      final IRasterDataModel rasterModel = (IRasterDataModel) rasterModelWorkspace.getRootFeature().getAdapter( IRasterDataModel.class );

      final GMLWorkspace vectorModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.VECTOR_MODEL.toString() ), null );
      final IVectorDataModel vectorModel = (IVectorDataModel) vectorModelWorkspace.getRootFeature().getAdapter( IVectorDataModel.class );

      final File outputRasterTmpDir = new File( tmpdir, "outputRaster" ); //$NON-NLS-1$
      outputRasterTmpDir.mkdir();
      doDamagePotentialCalculation( outputRasterTmpDir, controlModel, rasterModel, vectorModel, importantDigits, simulationMonitorAdaptor );
      final File tmpControlModel = File.createTempFile( IRasterizationControlModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      final File tmpRasterModel = File.createTempFile( IRasterDataModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      final File tmpVectorModel = File.createTempFile( IVectorDataModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpControlModel, controlModelWorkspace, "UTF-8" ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpRasterModel, rasterModelWorkspace, "UTF-8" ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpVectorModel, vectorModelWorkspace, "UTF-8" ); //$NON-NLS-1$
      resultEater.addResult( MODELSPEC_KALYPSORISK.CONTROL_MODEL.toString(), tmpControlModel );
      resultEater.addResult( MODELSPEC_KALYPSORISK.RASTER_MODEL.toString(), tmpRasterModel );
      resultEater.addResult( MODELSPEC_KALYPSORISK.VECTOR_MODEL.toString(), tmpVectorModel );
      resultEater.addResult( MODELSPEC_KALYPSORISK.OUTPUT_RASTER_FOLDER.toString(), outputRasterTmpDir );

      final boolean updateMap = inputProvider.hasID( MODELSPEC_KALYPSORISK.MAP_SPECIFIC_DAMAGE_POTENTIAL.toString() );
      if( updateMap )
      {
        final URL mapURL = (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.MAP_SPECIFIC_DAMAGE_POTENTIAL.toString() );

        /* Load the map template. */
        final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapURL );
        final Layers layers = gisview.getLayers();
        final List<JAXBElement< ? extends StyledLayerType>> layersList = layers.getLayer();
        layersList.clear();

        final ObjectFactory layerObjectFactory = new ObjectFactory();
        for( final IAnnualCoverageCollection annualCoverageCollection : rasterModel.getSpecificDamageCoverageCollection() )
        {
          final StyledLayerType layer = RiskModelHelper.createMapLayer( LAYER_TYPE.SPECIFIC_DAMAGE_POTENTIAL, annualCoverageCollection );
          final JAXBElement<StyledLayerType> jaxbLayer = layerObjectFactory.createLayer( layer );
          layersList.add( jaxbLayer );
        }
        final File outMap = File.createTempFile( "tempMap", ".gml", tmpdir ); //$NON-NLS-1$ //$NON-NLS-2$
        GisTemplateHelper.saveGisMapView( gisview, outMap, "UTF-8" ); //$NON-NLS-1$
        resultEater.addResult( MODELSPEC_KALYPSORISK.MAP_SPECIFIC_DAMAGE_POTENTIAL.toString(), outMap );
      }
    }
    catch( final Exception e )
    {
      throw new SimulationException( Messages.getString("SimulationKalypsoRisk_SpecificDamageCalculation.0"), e ); //$NON-NLS-1$
    }
  }

  /**
   * Creates the specific damage coverage collection. <br>
   * The damage value for each grid cell is taken from the underlying polygon.
   */
  private void doDamagePotentialCalculation( final File tmpdir, final IRasterizationControlModel controlModel, final IRasterDataModel rasterModel, final IVectorDataModel vectorModel, final int importantDigits, final IProgressMonitor monitor ) throws SimulationException
  {
    final IFeatureBindingCollection<IAnnualCoverageCollection> specificDamageCoverageCollection = rasterModel.getSpecificDamageCoverageCollection();
    final ILandusePolygonCollection landusePolygons = vectorModel.getLandusePolygonCollection();
    final IFeatureBindingCollection<ILandusePolygon> polygonCollection = landusePolygons.getLandusePolygonCollection();
    final List<ILanduseClass> landuseClassesList = controlModel.getLanduseClassesList();

    final IFeatureBindingCollection<IAnnualCoverageCollection> waterlevelCoverageCollection = rasterModel.getWaterlevelCoverageCollection();
    if( waterlevelCoverageCollection.size() == 0 )
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.0" ) ); //$NON-NLS-1$

    final SubMonitor subMonitor = SubMonitor.convert( monitor, Messages.getString("SimulationKalypsoRisk_SpecificDamageCalculation.1"), 100 ); //$NON-NLS-1$

    final StatisticCollector statistics = createStatistics( controlModel, landusePolygons, subMonitor.newChild( 10 ) );

    try
    {
      /* clear existing data */
      final Collection<URL> oldGridFiles = new ArrayList<>();
      for( int i = 0; i < specificDamageCoverageCollection.size(); i++ )
      {
        final IAnnualCoverageCollection annualCoverageCollection = specificDamageCoverageCollection.get( i );
        final IFeatureBindingCollection<ICoverage> coverages = annualCoverageCollection.getCoverages();
        for( int k = 0; k < coverages.size(); k++ )
          oldGridFiles.add( CoverageManagementHelper.getFileLocation( coverages.get( k ) ) );
      }
      specificDamageCoverageCollection.clear();

      // Put classes into list by, with index ordinal-number for faster access later
      final List<ILanduseClass> landuseClasses = new ArrayList<>();
      for( final ILanduseClass landuseClass : landuseClassesList )
      {
        final int ordinalNumber = landuseClass.getOrdinalNumber();
        if( landuseClasses.size() > ordinalNumber && landuseClasses.get( ordinalNumber ) != null )
          Logger.getAnonymousLogger().log( Level.WARNING, String.format( "WARNING: two landuse classes with same ordinal number: %s", ordinalNumber ) ); //$NON-NLS-1$

        while( landuseClasses.size() < ordinalNumber + 1 )
          landuseClasses.add( null );

        landuseClasses.set( ordinalNumber, landuseClass );
      }

      subMonitor.setWorkRemaining( waterlevelCoverageCollection.size() * 100 );

      /* loop over all waterdepths */
      for( final IAnnualCoverageCollection srcAnnualCoverages : waterlevelCoverageCollection )
      {
        final IFeatureBindingCollection<ICoverage> srcAnnualCoveragesList = srcAnnualCoverages.getCoverages();
        final int srcAnnualCoverageSize = srcAnnualCoveragesList.size();
        final int perCoverageTicks = 100 / srcAnnualCoverageSize;

        final String taskName = Messages.getString( "org.kalypso.risk.model.simulation.DamagePotentialCalculationHandler.10", srcAnnualCoverages.getReturnPeriod() ); //$NON-NLS-1$
        subMonitor.subTask( taskName ); //$NON-NLS-1$

        /* create annual damage coverage collection */
        final IAnnualCoverageCollection destCoverageCollection = specificDamageCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
        destCoverageCollection.setName( srcAnnualCoverages.getName() );

        final int returnPeriod = srcAnnualCoverages.getReturnPeriod();

        /* For eeach grid */
        for( int i = 0; i < srcAnnualCoverageSize; i++ )
        {
          final SubMonitor subSubMonitor = subMonitor.newChild( perCoverageTicks, SubMonitor.SUPPRESS_ALL_LABELS );

          final ICoverage inputCoverage = srcAnnualCoveragesList.get( i );
          if( srcAnnualCoverageSize > 1 )
            subMonitor.subTask( String.format( "%s (%s)", taskName, inputCoverage.getName() ) ); //$NON-NLS-1$

          // FIXEME: cast should not be necessary
          final RectifiedGridCoverageGeoGrid inputGrid = (RectifiedGridCoverageGeoGrid) GeoGridUtilities.toGrid( inputCoverage );
          // FIXME: calculate cell size inside of reader
          final double cellSize = Math.abs( inputGrid.getOffsetX().x - inputGrid.getOffsetY().x ) * Math.abs( inputGrid.getOffsetX().y - inputGrid.getOffsetY().y );

          // create sequential grid reader
          // TODO: make factory method for that!
          final SequentialBinaryGeoGridReader inputGridReader = new RiskSpecificDamageGrid( inputGrid, inputGrid.getGridURL(), polygonCollection, landuseClasses, cellSize, returnPeriod, statistics );

          // prepare the name, path and file for the output grid
          final String outputCoverageFileName = String.format( "specificDamage_HQ%d_%02d.bin", srcAnnualCoverages.getReturnPeriod(), i ); //$NON-NLS-1$
          final String outputCoverageFileRelativePath = CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + outputCoverageFileName;
          final File outputCoverageFile = new File( tmpdir.getAbsolutePath(), outputCoverageFileName );

          final ICoverage newCoverage = GeoGridUtilities.addCoverage( destCoverageCollection, inputGridReader, importantDigits, outputCoverageFile, outputCoverageFileRelativePath, "image/bin", subSubMonitor ); //$NON-NLS-1$

          inputGridReader.close();

          newCoverage.setName( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.2", srcAnnualCoverages.getReturnPeriod(), i ) ); //$NON-NLS-1$
          newCoverage.setDescription( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.3", new Date().toString() ) ); //$NON-NLS-1$
          inputGrid.dispose();

          subSubMonitor.done();
        }

        /* set the return period of the specific damage grid */
        destCoverageCollection.setReturnPeriod( srcAnnualCoverages.getReturnPeriod() );
      }

      /* save statistics */
      statistics.createResultObservation( controlModel );

      /* really clear existing data; until now the user might have cancelled the operation */
      for( final URL url : oldGridFiles )
        CoverageManagementHelper.deleteFile( url );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.RiskCalcSpecificDamageRunnable.1" ) + ": " + e.getLocalizedMessage(), e ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  private StatisticCollector createStatistics( final IRasterizationControlModel controlModel, final ILandusePolygonCollection landusePolygons, final IProgressMonitor monitor ) throws SimulationException
  {
    try
    {
      // REMARK: we know that the landuses have been transformed to the kalypso srs
      final StatisticCollector statistics = new StatisticCollector( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

      final ShapeFile shape = null;
      final String shapeNameAttribute = null;
      final String shapeSRS = null;

      final StatisticElementBuilder builder = new StatisticElementBuilder( controlModel, statistics.getSRSName() );
      builder.createElements( landusePolygons, shape, shapeNameAttribute, shapeSRS, monitor );
      statistics.setItems( builder.getItems() );

      return statistics;
    }
    catch( final IOException | GM_Exception | DBaseException | FactoryException | TransformException e )
    {
      throw new SimulationException( Messages.getString( "StatisticCalculationOperation_2" ), e ); //$NON-NLS-1$
    }
  }
}
