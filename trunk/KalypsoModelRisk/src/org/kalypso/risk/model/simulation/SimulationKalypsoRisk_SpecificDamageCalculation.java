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
import java.math.BigDecimal;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBElement;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.gml.ui.map.CoverageManagementHelper;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypso.grid.parallel.SequentialBinaryGeoGridReader;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.om.FeatureComponent;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskLanduseHelper;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.model.utils.RiskModelHelper.LAYER_TYPE;
import org.kalypso.risk.model.utils.RiskStatisticTableValues;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;

/**
 * @author Dejan Antanaskovic
 */
public class SimulationKalypsoRisk_SpecificDamageCalculation implements ISimulationSpecKalypsoRisk, ISimulation
{
  private static final String DICT_URN = "urn:ogc:gml:dict:kalypso:risk:model:riskresultstat"; //$NON-NLS-1$

  private static final String DICT_ANNUAL = DICT_URN + "#ANNUAL"; //$NON-NLS-1$

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
    final IFeatureBindingCollection<ILandusePolygon> polygonCollection = vectorModel.getLandusePolygonCollection().getLandusePolygonCollection();
    final List<ILanduseClass> landuseClassesList = controlModel.getLanduseClassesList();

    if( rasterModel.getWaterlevelCoverageCollection().size() == 0 )
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.0" ) ); //$NON-NLS-1$

    try
    {
      /* clear existing data */
      final Collection<URL> oldGridFiles = new ArrayList<>();
      for( int i = 0; i < specificDamageCoverageCollection.size(); i++ )
      {
        final IAnnualCoverageCollection annualCoverageCollection = specificDamageCoverageCollection.get( i );
        final IFeatureBindingCollection<ICoverage> coverages = annualCoverageCollection.getCoverages();
        for( int k = 0; k < coverages.size(); k++ )
          oldGridFiles.add( CoverageManagementHelper.getGridLocation( coverages.get( k ) ) );
      }
      specificDamageCoverageCollection.clear();

      // Put classes into list by, with index ordinal-number for faster access later
      final List<ILanduseClass> landuseClasses = new ArrayList<ILanduseClass>();
      for( final ILanduseClass landuseClass : landuseClassesList )
      {
        landuseClass.clearStatisticEntries();

        final int ordinalNumber = landuseClass.getOrdinalNumber();
        if( landuseClasses.size() > ordinalNumber && landuseClasses.get( ordinalNumber ) != null )
          Logger.getAnonymousLogger().log( Level.WARNING, String.format( "WARNING: two landuse classes with same ordinal number: %s", ordinalNumber ) ); //$NON-NLS-1$

        while( landuseClasses.size() < ordinalNumber + 1 )
          landuseClasses.add( null );

        landuseClasses.set( ordinalNumber, landuseClass );
      }

      /* loop over all waterdepths */
      for( final IAnnualCoverageCollection srcAnnualCoverages : rasterModel.getWaterlevelCoverageCollection() )
      {
        final IFeatureBindingCollection<ICoverage> srcAnnualCoveragesList = srcAnnualCoverages.getCoverages();
        final int srcAnnualCoverageSize = srcAnnualCoveragesList.size();
        final int perCoverageTicks = 100 / srcAnnualCoverageSize;
        final String taskName = Messages.getString( "org.kalypso.risk.model.simulation.DamagePotentialCalculationHandler.10", srcAnnualCoverages.getReturnPeriod() ); //$NON-NLS-1$
        final SubMonitor subMonitor = SubMonitor.convert( monitor, taskName, 100 );

        /* create annual damage coverage collection */
        final IAnnualCoverageCollection destCoverageCollection = specificDamageCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
        destCoverageCollection.setName( srcAnnualCoverages.getName() );

        final int returnPeriod = srcAnnualCoverages.getReturnPeriod();

        /* For eeach grid */
        for( int i = 0; i < srcAnnualCoverageSize; i++ )
        {
          final ICoverage inputCoverage = srcAnnualCoveragesList.get( i );
          if( srcAnnualCoverageSize > 1 )
            subMonitor.subTask( String.format( "%s (%s)", taskName, inputCoverage.getName() ) ); //$NON-NLS-1$

          // FIXEME: cast should not be necessary
          final RectifiedGridCoverageGeoGrid inputGrid = (RectifiedGridCoverageGeoGrid) GeoGridUtilities.toGrid( inputCoverage );
          // FIXME: calculate cell size inside of reader
          final double cellSize = Math.abs( inputGrid.getOffsetX().x - inputGrid.getOffsetY().x ) * Math.abs( inputGrid.getOffsetX().y - inputGrid.getOffsetY().y );

          // create sequential grid reader
          // TODO: make factory method for that!
          final SequentialBinaryGeoGridReader inputGridReader = new RiskSpecificDamageGrid( inputGrid, inputGrid.getGridURL(), polygonCollection, landuseClasses, cellSize, returnPeriod );

          // prepare the name, path and file for the output grid
          final String outputCoverageFileName = String.format( "specificDamage_HQ%d_%02d.bin", srcAnnualCoverages.getReturnPeriod(), i ); //$NON-NLS-1$
          final String outputCoverageFileRelativePath = CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + outputCoverageFileName;
          final File outputCoverageFile = new File( tmpdir.getAbsolutePath(), outputCoverageFileName );

          final ICoverage newCoverage = GeoGridUtilities.addCoverage( destCoverageCollection, inputGridReader, importantDigits, outputCoverageFile, outputCoverageFileRelativePath, "image/bin", subMonitor.newChild( perCoverageTicks, SubMonitor.SUPPRESS_ALL_LABELS ) ); //$NON-NLS-1$

          inputGridReader.close();

          for( final ILanduseClass landuseClass : landuseClassesList )
          {
            landuseClass.updateStatistic( returnPeriod );
          }
          newCoverage.setName( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.2", srcAnnualCoverages.getReturnPeriod(), i ) ); //$NON-NLS-1$
          newCoverage.setDescription( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.3", new Date().toString() ) ); //$NON-NLS-1$
          inputGrid.dispose();
        }
        /* set the return period of the specific damage grid */
        destCoverageCollection.setReturnPeriod( srcAnnualCoverages.getReturnPeriod() );
      }

      /* statistics: calculate average annual damage value for each landuse class */
      RiskModelHelper.calcLanduseAnnualAverageDamage( controlModel );

      createResultObsTable( controlModel, vectorModel );

      /* really clear existing data; until now the user might have cancelled the operation */
      for( final URL url : oldGridFiles )
        CoverageManagementHelper.deleteGridFile( url );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.RiskCalcSpecificDamageRunnable.1" ) + ": " + e.getLocalizedMessage() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  private static void createResultObsTable( final IRasterizationControlModel controlModel, final IVectorDataModel vectorModel ) throws Exception
  {
    Assert.isNotNull( controlModel );

    final List<ILanduseClass> landuseClassesList = controlModel.getLanduseClassesList();

    /* task: create an observation */
    final Feature controlModelFeature = controlModel;

    final Feature fObs = controlModelFeature.createSubFeature( IRasterizationControlModel.PROPERTY_STATISTIC_OBS );

    // new observation
    final TupleResult result = new TupleResult();

    // iterate over landuses and get set of events
    final IRiskLanduseStatistic[] events = getEvents( landuseClassesList.toArray( new ILanduseClass[] {} ) );

    addComponentsToObs( fObs, result, events );

    /* fill TupleResult with data */
    fillResultWithData( result, controlModel, vectorModel );

    /* add observation to workspace */
    final IObservation<TupleResult> obs = new Observation<TupleResult>( "name", "description", result ); //$NON-NLS-1$ //$NON-NLS-2$
    // maybe set phenomenon?
    ObservationFeatureFactory.toFeature( obs, fObs );
  }

  private static IRiskLanduseStatistic[] getEvents( final ILanduseClass[] classes )
  {
    final Set<IRiskLanduseStatistic> mySet = new TreeSet<IRiskLanduseStatistic>();

    for( final ILanduseClass member : classes )
    {
      final List<IRiskLanduseStatistic> landuseStatisticList = member.getLanduseStatisticList();
      mySet.addAll( landuseStatisticList );
    }

    return mySet.toArray( new IRiskLanduseStatistic[] {} );
  }

  private static void addComponentsToObs( final Feature fObs, final TupleResult result, final IRiskLanduseStatistic[] riskLanduseStatistics )
  {
    /* add the landuse class name component */
    final String landuseHeader = "Landuse"; //$NON-NLS-1$

    final Component componentLanduse = new Component( "", landuseHeader, landuseHeader, "", "", XmlTypes.XS_STRING, "null", new Phenomenon( "", landuseHeader, landuseHeader ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    result.addComponent( componentLanduse );

    final int numOfDataColumns = riskLanduseStatistics.length;

    /* for each dataColumn add an attr_member */
    for( int i = 0; i < numOfDataColumns; i++ )
    {
      final String eventName = riskLanduseStatistics[i].getName();

      final String headerNameTotalDamage = "TotalDamage_" + eventName; //$NON-NLS-1$
      final String headerNameFloodedArea = "FloodedArea_" + eventName;//$NON-NLS-1$
      final String headerNameAveragedDamage = "AverageDamage_" + eventName;//$NON-NLS-1$

      final IComponent valueComponentTotalDamage = new Component( "", headerNameTotalDamage, headerNameTotalDamage, "none", "", XmlTypes.XS_DECIMAL, BigDecimal.valueOf( 0.0 ), new Phenomenon( "", "TotalDamage", headerNameTotalDamage ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      result.addComponent( valueComponentTotalDamage );

      final IComponent valueComponentFloodedArea = new Component( "", headerNameFloodedArea, headerNameFloodedArea, "none", "", XmlTypes.XS_DECIMAL, BigDecimal.valueOf( 0.0 ), new Phenomenon( "", "FloodedArea", headerNameFloodedArea ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      result.addComponent( valueComponentFloodedArea );

      final IComponent valueComponentAveragedDamage = new Component( "", headerNameAveragedDamage, headerNameAveragedDamage, "none", "", XmlTypes.XS_DECIMAL, BigDecimal.valueOf( 0.0 ), new Phenomenon( "", "AveragedDamage", headerNameAveragedDamage ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      result.addComponent( valueComponentAveragedDamage );
    }

    /* add the average annual damage component */
    final FeatureComponent createDictionaryComponent = ObservationFeatureFactory.createDictionaryComponent( fObs, DICT_ANNUAL );

    result.addComponent( createDictionaryComponent );

  }

  private static void fillResultWithData( final TupleResult result, final IRasterizationControlModel controlModel, final IVectorDataModel vectorModel )
  {
    /* get the size of the table */
    // number of columns is the one for the landuse names + number of flood events + last column
    /* fill for every landuse class */
    final ILanduseClass[] classes = RiskLanduseHelper.getLanduseTypeSet( controlModel, vectorModel );

    for( final ILanduseClass landuseClass : classes )
    {
      /* add the data to the observation */
      final IRecord newRecord = result.createRecord();

      // landuse class name
      final String landuseName = landuseClass.getName();
      newRecord.setValue( 0, landuseName );

      // specific damage values for each event
      final List<IRiskLanduseStatistic> landuseStatisticList = landuseClass.getLanduseStatisticList();

      for( final IRiskLanduseStatistic statistic : landuseStatisticList )
      {
        final IComponent[] components = result.getComponents();
        for( int i = 0; i < components.length; i++ )
        {
          final IComponent component = components[i];
          final String compName = component.getName();

          final String[] split = compName.split( "_" ); //$NON-NLS-1$

          final String eventType = split[0];
          if( split.length > 1 )
          {
            final String eventName = split[1];

            if( eventName.equals( statistic.getName() ) )
            {
              if( eventType.equals( "TotalDamage" ) ) //$NON-NLS-1$
                newRecord.setValue( i, statistic.getTotalDamage() );

              if( eventType.equals( "FloodedArea" ) ) //$NON-NLS-1$
                newRecord.setValue( i, statistic.getFloodedArea() );

              if( eventType.equals( "AverageDamage" ) ) //$NON-NLS-1$
                newRecord.setValue( i, statistic.getAverageDamage() );
            }
          }
        }
      }

      result.add( newRecord );

      final int recordSize = newRecord.getOwner().getComponents().length;
      for( int i = 1; i < recordSize - 1; i++ )
      {
        final Object value = newRecord.getValue( i );
        if( value == null )
          newRecord.setValue( i, new BigDecimal( 0.0 ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
      }

      // average annual damage value for the whole landuse class
      newRecord.setValue( recordSize - 1, landuseClass.getAverageAnnualDamage() );
    }

    calculateLastRow( result );
  }

  /**
   * the last row of the obs table represents the following data:<br>
   * <ul>
   * <li>for each event:
   * <ul>
   * <li>the summation of all flooded areas over all landuse classes
   * <li>the summation of all damages of over landuse classes
   * <li>the average damage value derived by the summation values
   * </ul>
   * <li>the average annual damage value derived by the average damage values for each event as computed before.
   * </ul>
   */
  private static void calculateLastRow( final TupleResult result )
  {
    /* specific damage value for each event */
    final IRecord lastRecord = result.createRecord();
    final int columnSize = result.getComponents().length;

    /* name */
    lastRecord.setValue( 0, "Total" ); //$NON-NLS-1$

    final Map<String, RiskStatisticTableValues> eventMap = new HashMap<String, RiskStatisticTableValues>();

    /* specific damage values per event */
    // At first we fill in the summation of total damage and flooded area to be sure that these values are present if we
    // want to calculate the averaged values
    for( int index = 1; index < columnSize; index++ )
    {
      final IComponent rowComp = result.getComponent( index );
      final String compName = rowComp.getName();
      final String[] split = compName.split( "_" );//$NON-NLS-1$
      final String eventType = split[0];
      if( split.length > 1 )
      {
        final String eventName = split[1];

        RiskStatisticTableValues statisticTableValues = eventMap.get( eventName );
        if( statisticTableValues == null )
        {
          statisticTableValues = new RiskStatisticTableValues( eventName );
          eventMap.put( eventName, statisticTableValues );
        }

        if( eventType.equals( "TotalDamage" ) )//$NON-NLS-1$
        {
          // calculate the sum for total damage and the flooded area
          final BigDecimal sum = calculateColumnSum( result, index );

          // set the value
          lastRecord.setValue( index, sum );

          // remember the value
          statisticTableValues.setTotalDamageValue( sum );
        }
        else if( eventType.equals( "FloodedArea" ) )//$NON-NLS-1$
        {
          // calculate the sum for total damage and the flooded area
          final BigDecimal sum = calculateColumnSum( result, index );

          // set the value
          lastRecord.setValue( index, sum );

          // remember the value
          statisticTableValues.setFloodedAreaValue( sum );
        }
      }
    }

    // next we fill in the average damage
    for( int index = 1; index < columnSize; index++ )
    {
      final IComponent rowComp = result.getComponent( index );
      final String compName = rowComp.getName();
      final String[] split = compName.split( "_" );//$NON-NLS-1$
      final String eventType = split[0];
      if( split.length > 1 )
      {
        final String eventName = split[1];

        final RiskStatisticTableValues statisticTableValues = eventMap.get( eventName );
        if( statisticTableValues == null )
          System.out.println( org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_RiskZonesCalculation.18" ) ); //$NON-NLS-1$

        if( eventType.equals( "AverageDamage" ) )//$NON-NLS-1$
        {
          // get the values of the summation and calculate the new average value with it
          final BigDecimal averageDamageValue = statisticTableValues.getAverageDamageValue();
          lastRecord.setValue( index, averageDamageValue );
        }
      }
    }

    // at last we calculate the average annual damage value
    final int index = columnSize - 1;

    final IComponent rowComp = result.getComponent( index );
    final String phenName = rowComp.getPhenomenon().getName();

    if( phenName.equals( "AnnualValue" ) )//$NON-NLS-1$
    {
      // get the calculated total values for all events with corresponding periods
      final Map<Double, RiskStatisticTableValues> periodSortedMap = getPeriods( columnSize, result, eventMap );

      /* calculate the average annual damage by integrating the specific average damage values */
      final double averageSum = calculateAverageDamageValue( periodSortedMap );

      // calculate the sum for average annual damage
      lastRecord.setValue( index, averageSum );
    }
    // and add the record
    result.add( lastRecord );
  }

  private static final Map<Double, RiskStatisticTableValues> getPeriods( final int columnSize, final TupleResult result, final Map<String, RiskStatisticTableValues> eventMap )
  {
    final Map<Double, RiskStatisticTableValues> periodSortedMap = new TreeMap<Double, RiskStatisticTableValues>();

    // collect all Averaged Values for all events
    for( int index = 1; index < columnSize; index++ )
    {
      final IComponent rowComp = result.getComponent( index );
      final String compName = rowComp.getName();
      final String[] split = compName.split( "_" );//$NON-NLS-1$
      final String eventType = split[0];
      if( eventType.equals( "AverageDamage" ) && split.length > 1 ) //$NON-NLS-1$
      {
        final String eventName = split[1];

        final String anu = eventName.substring( 2 );

        final Scanner scanner = new Scanner( anu );
        Double annuality = 0.0;
        if( scanner.hasNextDouble() )
          annuality = scanner.nextDouble();

        if( annuality == null )
          continue;

        periodSortedMap.put( annuality, eventMap.get( eventName ) );
      }
    }
    return periodSortedMap;

  }

  private static BigDecimal calculateColumnSum( final TupleResult result, final int index )
  {
    BigDecimal sum = new BigDecimal( 0.0 );
    for( int rowIndex = 0; rowIndex < result.size(); rowIndex++ )
    {
      final IRecord record = result.get( rowIndex );
      final Object object = record.getValue( index );
      if( object instanceof BigDecimal )
      {
        final BigDecimal value = (BigDecimal) record.getValue( index );
        sum = sum.add( value );
      }
      else if( object instanceof Double )
      {
        final Double value = (Double) record.getValue( index );
        sum = sum.add( BigDecimal.valueOf( value ) );
      }
    }
    return sum;
  }

  private static double calculateAverageDamageValue( final Map<Double, RiskStatisticTableValues> periodSortedMap )
  {
    double averageSum = 0.0;

    final Set<Double> keySet = periodSortedMap.keySet();

    final Double[] periods = keySet.toArray( new Double[keySet.size()] );

    for( int i = 0; i < periods.length - 1; i++ )
    {
      if( periods[i] == null || periods[i] == 0 )
        continue;

      /* get the probability for each return period */
      final double p1 = 1 / periods[i];
      final double p2 = 1 / periods[i + 1];

      /* calculate the difference */
      final double d_pi = p1 - p2;

      /*
       * get the specific damage summation value for this and the next return period an calculate the difference
       * (divided by 2). This means nothing else than to calculate the area for trapezoid with ha=specific value 1 and
       * hb= specific value 2. The width of the trapezoid is the difference of the probabilities that belong to both
       * specific damages values.
       */
      final RiskStatisticTableValues statEntry1 = periodSortedMap.get( periods[i] );
      final RiskStatisticTableValues statEntry2 = periodSortedMap.get( periods[i + 1] );

      // final BigDecimal sumStat = statEntry2.getDamageSum().add( statEntry1.getDamageSum() );
      final BigDecimal sumStat = statEntry2.getAverageDamageValue().add( statEntry1.getAverageDamageValue() );
      final double value = sumStat.doubleValue() / 2;
      final BigDecimal si = new BigDecimal( value ).setScale( 2, BigDecimal.ROUND_HALF_UP );

      /* calculate the average damage and add it */
      averageSum = averageSum + si.doubleValue() * d_pi;
    }
    return averageSum;
  }
}
