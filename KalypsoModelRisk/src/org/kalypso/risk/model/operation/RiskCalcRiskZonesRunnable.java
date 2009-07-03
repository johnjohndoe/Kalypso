/**
 *
 */
package org.kalypso.risk.model.operation;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.FeatureComponent;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.actions.riskZonesCalculation.RiskZonesGrid;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskLanduseHelper;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public final class RiskCalcRiskZonesRunnable implements ICoreRunnableWithProgress
{
  private static final String DICT_URN = "urn:ogc:gml:dict:kalypso:risk:model:riskresultstat"; //$NON-NLS-1$

  private static final String DICT_ANNUAL = DICT_URN + "#ANNUAL"; //$NON-NLS-1$

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

        coverage.setName( org.kalypso.risk.Messages.getString( "RiskCalcRiskZonesRunnable.4" ) + i + org.kalypso.risk.Messages.getString( "RiskCalcRiskZonesRunnable.5" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        coverage.setDescription( Messages.getString( "RiskZonesCalculationHandler.9" ) + new Date().toString() ); //$NON-NLS-1$

        /* fireModellEvent to redraw a map */
        final GMLWorkspace workspace = m_rasterModel.getFeature().getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_rasterModel.getFeature(), new Feature[] { outputCoverages.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

      // statistics...
      // calculate average annual damage value for each landuse class
      RiskModelHelper.calcLanduseAnnualAverageDamage( m_controlModel );

      createResultObsTable( m_controlModel, m_vectorModel );

      // TODO: what gets fixed here? the data should be valid!
      // if not, then there is a general error in the calculation of the values!
      m_controlModel.fixStatisticsForShowingToUser();

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, org.kalypso.risk.Messages.getString( "RiskCalcRiskZonesRunnable.6" ) ); //$NON-NLS-1$
    }
  }

  private static void createResultObsTable( final IRasterizationControlModel controlModel, final IVectorDataModel vectorModel ) throws Exception
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

    // iterate over landuses and get set of events
    final IRiskLanduseStatistic[] events = getEvents( landuseClassesList.toArray( new ILanduseClass[] {} ) );

    addComponentsToObs( fObs, result, events );

    /* fill TupleResult with data */
    fillResultWithData( result, controlModel, vectorModel );

    /* add observation to workspace */
    final IObservation<TupleResult> obs = new Observation<TupleResult>( "name", "description", result, new ArrayList<MetadataObject>() ); //$NON-NLS-1$ //$NON-NLS-2$
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
    final Component componentLanduse = new Component( "", landuseHeader, landuseHeader, "", "", IWspmConstants.Q_STRING, "null", new Phenomenon( "", landuseHeader, landuseHeader ) );
    result.addComponent( componentLanduse );

    final int numOfDataColumns = riskLanduseStatistics.length;

    /* for each dataColumn add an attr_member */
    for( int i = 0; i < numOfDataColumns; i++ )
    {
      final String eventName = riskLanduseStatistics[i].getName();

      final String headerNameTotalDamage = "TotalDamage_" + eventName; //$NON-NLS-1$
      final String headerNameFloodedArea = "FloodedArea_" + eventName;//$NON-NLS-1$
      final String headerNameAveragedDamage = "AverageDamage_" + eventName;//$NON-NLS-1$

      final IComponent valueComponentTotalDamage = new Component( "", headerNameTotalDamage, headerNameTotalDamage, "none", "", IWspmConstants.Q_DECIMAL, BigDecimal.valueOf( 0.0 ), new Phenomenon( "", "TotalDamage", headerNameTotalDamage ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      result.addComponent( valueComponentTotalDamage );

      final IComponent valueComponentFloodedArea = new Component( "", headerNameFloodedArea, headerNameFloodedArea, "none", "", IWspmConstants.Q_DECIMAL, BigDecimal.valueOf( 0.0 ), new Phenomenon( "", "FloodedArea", headerNameFloodedArea ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      result.addComponent( valueComponentFloodedArea );

      final IComponent valueComponentAveragedDamage = new Component( "", headerNameAveragedDamage, headerNameAveragedDamage, "none", "", IWspmConstants.Q_DECIMAL, BigDecimal.valueOf( 0.0 ), new Phenomenon( "", "AveragedDamage", headerNameAveragedDamage ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
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

          final String[] split = compName.split( "_" );

          final String eventType = split[0];
          if( split.length > 1 )
          {
            final String eventName = split[1];

            if( eventName.equals( statistic.getName() ) )
            {
              if( eventType.equals( "TotalDamage" ) )
                newRecord.setValue( i, statistic.getTotalDamage() );

              if( eventType.equals( "FloodedArea" ) )
                newRecord.setValue( i, statistic.getFloodedArea() );

              if( eventType.equals( "AverageDamage" ) )
                newRecord.setValue( i, statistic.getAverageDamage() );
            }
          }
        }
      }

      final int recordSize = newRecord.getOwner().getComponents().length;
      for( int i = 1; i < recordSize - 1; i++ )
      {
        final Object value = newRecord.getValue( i );
        if( value == null )
        {
          newRecord.setValue( i, new BigDecimal( 0.0 ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
        }
      }

      // average annual damage value for the whole landuse class
      newRecord.setValue( recordSize - 1, landuseClass.getAverageAnnualDamage() );

      result.add( newRecord );
    }

    /* specific damage value for each event */
    final IRecord lastRecord = result.createRecord();
    final int columnSize = result.getComponents().length;

    // name
    lastRecord.setValue( 0, "Summation" ); //$NON-NLS-1$

    // specific damage values per event
    for( int index = 1; index < columnSize; index++ )
    {
      BigDecimal sum = new BigDecimal( 0.0 );

      for( int rowIndex = 0; rowIndex < result.size(); rowIndex++ )
      {
        final IRecord record = result.get( rowIndex );

        // @hack last value (annual) is type of double
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

      // @hack last value (annual) is type of double
      if( index == columnSize - 1 )
        lastRecord.setValue( index, sum.doubleValue() );
      else
        lastRecord.setValue( index, sum );
    }

    result.add( lastRecord );
  }

}