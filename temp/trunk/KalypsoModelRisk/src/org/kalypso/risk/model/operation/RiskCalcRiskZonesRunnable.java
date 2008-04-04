/**
 * 
 */
package org.kalypso.risk.model.operation;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.risk.model.actions.riskZonesCalculation.Messages;
import org.kalypso.risk.model.actions.riskZonesCalculation.RiskZonesGrid;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public final class RiskCalcRiskZonesRunnable implements ICoreRunnableWithProgress
{
  private static final String DICT_URN = "urn:ogc:gml:dict:kalypso:risk:model:riskresultstat";

  private static final String DICT_LANDUSE = DICT_URN + "#ROW_TITLE";

  private static final String DICT_EVENT = DICT_URN + "#EVENT";

  private static final String DICT_ANNUAL = DICT_URN + "#ANNUAL";

  private final IRasterDataModel m_rasterModel;

  private final IVectorDataModel m_vectorModel;

  private final IRasterizationControlModel m_controlModel;

  private final IFolder m_scenarioFolder;

  public RiskCalcRiskZonesRunnable( final IRasterDataModel rasterModel, final IVectorDataModel vectorModel, final IRasterizationControlModel controlModel, final IFolder scenarioFolder )
  {
    m_rasterModel = rasterModel;
    m_controlModel = controlModel;
    m_scenarioFolder = scenarioFolder;
    m_vectorModel = vectorModel;
  }

  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "RiskZonesCalculationHandler.7" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$

    if( m_rasterModel.getSpecificDamageCoverageCollection().size() < 2 )
      return StatusUtilities.createErrorStatus( Messages.getString( "RiskZonesCalculationHandler.6" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    try
    {
      /* remove existing (invalid) coverages from the model and clean statistic */
      m_rasterModel.getRiskZonesCoverage().clear();
      m_controlModel.resetStatistics();

      final ICoverageCollection outputCoverages = m_rasterModel.getRiskZonesCoverage();

      final IAnnualCoverageCollection maxCoveragesCollection = RiskModelHelper.getMaxReturnPeriodCollection( m_rasterModel.getSpecificDamageCoverageCollection() );
      final ICoverageCollection baseCoverages = maxCoveragesCollection;

      for( int i = 0; i < baseCoverages.size(); i++ )
      {
        final ICoverage srcSpecificDamageCoverage = baseCoverages.get( i );

        final IGeoGrid inputGrid = GeoGridUtilities.toGrid( srcSpecificDamageCoverage );
        final IGeoGrid outputGrid = new RiskZonesGrid( inputGrid, m_rasterModel.getSpecificDamageCoverageCollection(), m_vectorModel.getLandusePolygonCollection(), m_controlModel.getLanduseClassesList(), m_controlModel.getRiskZoneDefinitionsList() );

        // TODO: change name: better: use input name
        final String outputFilePath = "raster/output/RiskZonesCoverage" + i + ".dat"; //$NON-NLS-1$ //$NON-NLS-2$
        final IFile iFile = m_scenarioFolder.getFile( new Path( "models/" + outputFilePath ) ); //$NON-NLS-1$
        final ICoverage coverage = GeoGridUtilities.addCoverage( outputCoverages, outputGrid, iFile.getLocation().toFile(), outputFilePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$

        outputGrid.dispose();
        inputGrid.dispose();

        coverage.setName( "Risikozonen [" + i + "]" );
        coverage.setDescription( Messages.getString( "RiskZonesCalculationHandler.9" ) + new Date().toString() ); //$NON-NLS-1$

        /* fireModellEvent to redraw a map */
        final GMLWorkspace workspace = m_rasterModel.getFeature().getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_rasterModel.getFeature(), new Feature[] { outputCoverages.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

      // statistics...
      // calculate average annual damage value for each landuse class
      RiskModelHelper.calcLanduseAnnualAverageDamage( m_controlModel );

      createResultObsTable( m_controlModel );

      // TODO: what gets fixed here? the data should be valid!
      // if not, then there is a general error in the calculation of the values!
      m_controlModel.fixStatisticsForShowingToUser();

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, "Fehler bei Berechnung der Risikozonen" );
    }
  }

  private static void createResultObsTable( final IRasterizationControlModel controlModel ) throws Exception
  {
    Assert.isNotNull( controlModel );

    final List<ILanduseClass> landuseClassesList = controlModel.getLanduseClassesList();

    /* task: create an observation */
    final Feature controlModelFeature = controlModel.getFeature();
    final CommandableWorkspace workspace = new CommandableWorkspace( controlModelFeature.getWorkspace() );

    final IPropertyType property = controlModelFeature.getFeatureType().getProperty( IRasterizationControlModel.PROPERTY_STATISTIC_OBS );
    final IRelationType relation = (IRelationType) property;

    final Feature fObs = workspace.createFeature( controlModelFeature, relation, relation.getTargetFeatureType() );

    workspace.setFeatureAsComposition( controlModelFeature, relation, fObs, true );

    // new observation
    final TupleResult result = new TupleResult();

    final List<IRiskLanduseStatistic> eventList = landuseClassesList.get( 0 ).getLanduseStatisticList();

    addComponentsToObs( fObs, result, eventList );

    /* fill TupleResult with data */
    fillResultWithData( result, landuseClassesList );

    /* add observation to workspace */
    final IObservation<TupleResult> obs = new Observation<TupleResult>( "name", "description", result, new ArrayList<MetadataObject>() );
    // maybe set phenomenon?
    ObservationFeatureFactory.toFeature( obs, fObs );
  }

  private static void addComponentsToObs( final Feature fObs, final TupleResult result, final List<IRiskLanduseStatistic> eventList )
  {
    /* add the landuse class name component */
    result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, DICT_LANDUSE ) );

    final int numOfDataColumns = eventList.size();

    /* for each dataColumn add an attr_member */
    for( int i = 0; i < numOfDataColumns; i++ )
    {
      final String eventName = eventList.get( i ).getName();
      final IComponent valueComponent = new Component( eventName, eventName, eventName, "none", "", new QName( NS.XSD_SCHEMA, "decimal" ), "", new DictionaryPhenomenon( DICT_EVENT, eventName, eventName ) );
      result.addComponent( valueComponent );
    }

    /* add the average annual damage component */
    result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, DICT_ANNUAL ) );
  }

  private static void fillResultWithData( final TupleResult result, final List<ILanduseClass> landuseClassesList )
  {
    /* get the size of the table */
    // number of columns is the one for the landuse names + number of flood events + last column
    final int numOfEvents = landuseClassesList.get( 0 ).getLanduseStatisticList().size();

    // number of rows is number of landuse classes + last row
    final int numOfLanduses = landuseClassesList.size();

    /* fill for every landuse class */
    for( final ILanduseClass landuseClass : landuseClassesList )
    {
      /* add the data to the observation */
      final IRecord newRecord = result.createRecord();

      // landuse class name
      final String landuseName = landuseClass.getName();
      newRecord.setValue( 0, landuseName );

      // specific damage values for each event
      final List<IRiskLanduseStatistic> landuseStatisticList = landuseClass.getLanduseStatisticList();

      for( int i = 0; i < numOfEvents; i++ )
      {
        if( landuseStatisticList.size() == 0 )
        {
          // fill with zeros, if the landuse was not getting flooded
          newRecord.setValue( i + 1, new BigDecimal( 0.0 ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
        }
        else
        {
          final IRiskLanduseStatistic riskLanduseStatistics = landuseStatisticList.get( i );
          final BigDecimal damageSum = riskLanduseStatistics.getDamageSum();

          newRecord.setValue( i + 1, damageSum );
        }
      }

      // average annual damage value for the whole landuse class
      newRecord.setValue( numOfEvents + 1, landuseClass.getAverageAnnualDamage() );
      result.add( newRecord );
    }

    /* specific damage value for each event */
    final IRecord lastRecord = result.createRecord();

    // name
    lastRecord.setValue( 0, "Summation" );

    // specific damage values per event
    for( int i = 0; i < numOfEvents; i++ )
    {
      BigDecimal eventSummation = new BigDecimal( 0.0 ).setScale( 2, BigDecimal.ROUND_HALF_UP );
      for( int j = 0; j < numOfLanduses; j++ )
      {
        final List<IRiskLanduseStatistic> statisticList = landuseClassesList.get( j ).getLanduseStatisticList();
        if( statisticList.size() == 0 )
        {
          lastRecord.setValue( i + 1, 0.0 );
        }
        else
        {
          final IRiskLanduseStatistic riskLanduseStatistic = statisticList.get( i );
          if( riskLanduseStatistic != null )
          {
            final BigDecimal damageSum = riskLanduseStatistic.getDamageSum();
            eventSummation = eventSummation.add( damageSum );
          }
        }
        lastRecord.setValue( i + 1, eventSummation );
      }
    }

    // calculate overall average annual damage value
    double overAllAverageDamage = 0.0;
    for( final ILanduseClass landuseClass : landuseClassesList )
    {
      final double averageAnnualDamage = landuseClass.getAverageAnnualDamage();
      overAllAverageDamage = overAllAverageDamage + averageAnnualDamage;
    }

    lastRecord.setValue( numOfEvents + 1, overAllAverageDamage );

    result.add( lastRecord );
  }
}