package org.kalypso.risk.model.operation;

import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
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
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.simulation.ISimulationSpecKalypsoRisk;
import org.kalypso.risk.model.simulation.RiskZonesGrid;
import org.kalypso.risk.model.utils.RiskLanduseHelper;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.model.utils.RiskStatisticTableValues;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
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

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final int importantDigits = KalypsoRiskPreferencePage.MAX_RISKTHEMEINFO_PRECISION;
    final SubMonitor subMonitor = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.risk.model.operation.RiskZonesCalculationHandler.7" ), 100 ); //$NON-NLS-1$
    if( m_rasterModel.getSpecificDamageCoverageCollection().size() < 2 )
      return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.risk.model.operation.RiskZonesCalculationHandler.6" ) ); //$NON-NLS-1$

    try
    {
      /* remove existing (invalid) coverages from the model and clean statistic */
      m_rasterModel.getRiskZonesCoverage().getCoverages().clear();
      m_controlModel.resetStatistics();

      final ICoverageCollection outputCoverages = m_rasterModel.getRiskZonesCoverage();

      final IAnnualCoverageCollection maxCoveragesCollection = RiskModelHelper.getMaxReturnPeriodCollection( m_rasterModel.getSpecificDamageCoverageCollection() );
      final ICoverageCollection baseCoverages = maxCoveragesCollection;
      IFeatureBindingCollection<ICoverage> baseCoveragesList = baseCoverages.getCoverages();
      for( int i = 0; i < baseCoveragesList.size(); i++ )
      {
        final ICoverage srcSpecificDamageCoverage = baseCoveragesList.get( i );

        final IGeoGrid inputGrid = GeoGridUtilities.toGrid( srcSpecificDamageCoverage );
        final IGeoGrid outputGrid = new RiskZonesGrid( inputGrid, m_rasterModel.getSpecificDamageCoverageCollection(), m_vectorModel.getLandusePolygonCollection(), m_controlModel.getLanduseClassesList(), m_controlModel.getRiskZoneDefinitionsList() );

        final String outputCoverageFileName = String.format( "%s_%02d.bin", outputCoverages.getId(), i ); //$NON-NLS-1$
        // final String outputCoverageFileName = "RiskZonesCoverage_" + i + ".bin"; //$NON-NLS-1$ //$NON-NLS-2$
        final String outputCoverageFileRelativePath = ISimulationSpecKalypsoRisk.CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + outputCoverageFileName;
        final IFile outputCoverageFile = m_scenarioFolder.getFile( new Path( "models/" + outputCoverageFileRelativePath ) ); //$NON-NLS-1$
        final ICoverage coverage = GeoGridUtilities.addCoverage( outputCoverages, outputGrid, importantDigits, outputCoverageFile.getLocation().toFile(), outputCoverageFileRelativePath, "image/bin", subMonitor ); //$NON-NLS-1$

        outputGrid.dispose();
        inputGrid.dispose();

        coverage.setName( Messages.getString( "org.kalypso.risk.model.operation.RiskCalcRiskZonesRunnable.4" ) + i + Messages.getString( "org.kalypso.risk.model.operation.RiskCalcRiskZonesRunnable.5" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        coverage.setDescription( Messages.getString( "org.kalypso.risk.model.operation.RiskZonesCalculationHandler.9" ) + new Date().toString() ); //$NON-NLS-1$

        /* fireModellEvent to redraw a map */
        final GMLWorkspace workspace = m_rasterModel.getFeature().getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_rasterModel.getFeature(), new Feature[] { outputCoverages }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
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
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.risk.model.operation.RiskCalcRiskZonesRunnable.6" ) ); //$NON-NLS-1$
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
        {
          newRecord.setValue( i, new BigDecimal( 0.0 ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
        }
      }

      // average annual damage value for the whole landuse class
      newRecord.setValue( recordSize - 1, landuseClass.getAverageAnnualDamage() );

      result.add( newRecord );
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
          System.out.println( org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.operation.RiskCalcRiskZonesRunnable.21" ) ); //$NON-NLS-1$

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

}