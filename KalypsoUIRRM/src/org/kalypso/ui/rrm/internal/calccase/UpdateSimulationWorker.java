/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ui.rrm.internal.calccase;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.formula.eval.NotImplementedException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.joda.time.DateTime;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.util.logging.SystemOutLogger;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.hydrology.project.ScenarioAccessor;
import org.kalypso.model.rcm.IRainfallModelProvider;
import org.kalypso.model.rcm.RainfallGenerationOperation;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IRainfallCatchmentModel;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.binding.ITarget;
import org.kalypso.model.rcm.util.PlainRainfallModelProvider;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.timeseries.merged.Source;
import org.kalypso.simulation.core.ant.copyobservation.CopyObservationFeatureVisitor;
import org.kalypso.simulation.core.ant.copyobservation.ICopyObservationSource;
import org.kalypso.simulation.core.ant.copyobservation.source.FeatureCopyObservationSource;
import org.kalypso.simulation.core.ant.copyobservation.target.CopyObservationTargetFactory;
import org.kalypso.simulation.core.ant.copyobservation.target.ICopyObservationTarget;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.visitors.MonitorFeatureVisitor;

import com.google.common.base.Charsets;

/**
 * The worker that actually updates a simulation.
 * 
 * @author Gernot Belger
 */
public class UpdateSimulationWorker
{
  public static final QName MAPPING_MEMBER = new QName( "http://org.kalypso.updateObservationMapping", "mappingMember" ); //$NON-NLS-1$ //$NON-NLS-2$

  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final RrmSimulation m_simulation;

  public UpdateSimulationWorker( final IFolder simulationFolder )
  {
    m_simulation = new RrmSimulation( simulationFolder );
  }

  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString( "UpdateSimulationWorker_2" ), 120 ); //$NON-NLS-1$

    /* Read models */
    monitor.subTask( Messages.getString( "UpdateSimulationWorker_3" ) ); //$NON-NLS-1$
    final NAControl control = readControlFile();
    final NaModell model = readModelFile();
    ProgressUtilities.worked( monitor, 20 );

    /* Copy observations for pegel and zufluss */
    copyMappingTimeseries( control, "ObsQMapping.gml", "Pegel", new SubProgressMonitor( monitor, 20 ) ); //$NON-NLS-1$
    copyMappingTimeseries( control, "ObsQZuMapping.gml", "Zufluss", new SubProgressMonitor( monitor, 20 ) ); //$NON-NLS-1$
    copyMappingTimeseries( control, "ObsEMapping.gml", "Klima", new SubProgressMonitor( monitor, 20 ) ); //$NON-NLS-1$

    /* Execute catchment models */
    executeCatchmentModel( control, model, control.getGeneratorN(), Catchment.PROP_PRECIPITATION_LINK, ITimeseriesConstants.TYPE_RAINFALL, new SubProgressMonitor( monitor, 20 ) );
    executeCatchmentModel( control, model, control.getGeneratorT(), Catchment.PROP_TEMPERATURE_LINK, ITimeseriesConstants.TYPE_TEMPERATURE, new SubProgressMonitor( monitor, 20 ) );
    executeCatchmentModel( control, model, control.getGeneratorE(), Catchment.PROP_EVAPORATION_LINK, ITimeseriesConstants.TYPE_EVAPORATION, new SubProgressMonitor( monitor, 20 ) );

    /* Copy initial condition from long term simulation */
    copyInitialCondition( control );

    return m_log.asMultiStatusOrOK( m_simulation.getName() );
  }

  private NAControl readControlFile( ) throws CoreException
  {
    try
    {
      final IFile controlFile = m_simulation.getCalculationGml();
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlFile );
      return (NAControl) controlWorkspace.getRootFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "UpdateSimulationWorker_10" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private NaModell readModelFile( ) throws CoreException
  {
    try
    {
      final IFile modelFile = m_simulation.getModelGml();
      final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( modelFile );
      return (NaModell) modelWorkspace.getRootFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "UpdateSimulationWorker_11" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private void copyMappingTimeseries( final NAControl control, final String mappingFilename, final String outputFoldername, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      /* Read mapping */
      final FeatureList mappingFeatures = readMapping( mappingFilename, MAPPING_MEMBER );

      /* Prepare visitor */
      final CopyObservationFeatureVisitor visitor = prepareVisitor( outputFoldername, "inObservationLink", control ); //$NON-NLS-1$

      /* Execute visitor */
      final int count = mappingFeatures.size();
      final MonitorFeatureVisitor wrappedVisitor = new MonitorFeatureVisitor( monitor, count, visitor );
      mappingFeatures.accept( wrappedVisitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final String message = String.format( Messages.getString( "UpdateSimulationWorker_12" ), outputFoldername ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), message, e );
      throw new CoreException( status );
    }
  }

  private FeatureList readMapping( final String filename, final QName memberProperty ) throws Exception
  {
    final RrmProject rrmProject = m_simulation.getProject();

    final IFolder observationConfFolder = rrmProject.getObservationConfFolder();

    final IFile mappingFile = observationConfFolder.getFile( filename );
    final GMLWorkspace mappingWorkspace = GmlSerializer.createGMLWorkspace( mappingFile );
    final Feature rootFeature = mappingWorkspace.getRootFeature();
    return (FeatureList) rootFeature.getProperty( memberProperty );
  }

  private CopyObservationFeatureVisitor prepareVisitor( final String outputDir, final String sourceLinkName, final NAControl control )
  {
    final DateRange forecastRange = null;

    /* *Calculate range */
    final Integer timestepMinutes = control.getMinutesOfTimestep();
    final Period timestep = Period.minutes( timestepMinutes ).normalizedStandard();
    final DateRange targetRange = getRange( control, timestep, null );

    final Source source = new Source();
    source.setProperty( sourceLinkName );
    source.setFrom( Long.toString( targetRange.getFrom().getTime() ) );
    source.setTo( Long.toString( targetRange.getTo().getTime() ) );

    // REMARK: must be null, so the special NA-ObservationTarget is created
    final GMLXPath targetPath = null;

    final IFolder calcCaseFolder = m_simulation.getSimulationFolder();
    final File calcDir = calcCaseFolder.getLocation().toFile();
    final File targetObservationDir = new File( calcDir, outputDir );

    final URL context = ResourceUtilities.createQuietURL( calcCaseFolder );
    final String tokens = StringUtils.EMPTY;

    final ICopyObservationTarget obsTarget = CopyObservationTargetFactory.getLink( context, targetPath, targetObservationDir, targetRange, forecastRange );
    final ICopyObservationSource obsSource = new FeatureCopyObservationSource( context, new Source[] { source }, tokens );

    final MetadataList metadata = new MetadataList();

    final SystemOutLogger logger = new SystemOutLogger();

    return new CopyObservationFeatureVisitor( obsSource, obsTarget, metadata, logger );
  }

  private void executeCatchmentModel( final NAControl control, final NaModell model, final IRainfallGenerator generator, final QName targetLink, final String parameterType, final IProgressMonitor monitor ) throws CoreException
  {
    /* The timestep is only defined in linear sum generators for now. */
    if( !(generator instanceof ILinearSumGenerator) )
      throw new NotImplementedException( "Only ILinearSumGenerator's are supported at the moment..." ); //$NON-NLS-1$

    /* Cast. */
    final ILinearSumGenerator linearGenerator = (ILinearSumGenerator) generator;

    try
    {
      monitor.beginTask( Messages.getString( "UpdateSimulationWorker_14" ), 100 ); //$NON-NLS-1$
      final String name = TimeseriesUtils.getName( parameterType );
      monitor.subTask( name );

      /* Calculate date range for filter. */
      final int timestepMinutes = getTimestepMinutes( linearGenerator, control );
      final LocalTime time = linearGenerator.getTimeStamp();
      final Period timestep = Period.minutes( timestepMinutes ).normalizedStandard();
      // REMARK: TODO: check: we use the timestep of each generator to adjust the range,
      // this is not exactly what happened before
      final DateRange range = getRange( control, timestep, time );

      /* Create the rainfall generation operation. */
      final IRainfallCatchmentModel rainfallModel = createRainfallModel( model, linearGenerator, targetLink, range );

      /* Initialize the generator. */
      final ILinearSumGenerator clonedGenerator = (ILinearSumGenerator) rainfallModel.getGenerators().get( 0 );
      initGenerator( clonedGenerator, range, timestep, parameterType );

      /* Initialize the catchment target links. */
      initTargetLinks( clonedGenerator, targetLink, parameterType );

      /* Execute the rainfall generation operation. */
      final IRainfallModelProvider modelProvider = new PlainRainfallModelProvider( rainfallModel );
      final RainfallGenerationOperation operation = new RainfallGenerationOperation( modelProvider, null );

      /* Execute the operation. */
      operation.execute( new SubProgressMonitor( monitor, 100 ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = StatusUtilities.statusFromThrowable( e, Messages.getString( "UpdateSimulationWorker_15" ) ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      monitor.done();

      try
      {
        m_simulation.getSimulationFolder().refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
      }
      catch( final CoreException e )
      {
        // REAMRK: give priority to other exception, so we just system out'it
        e.printStackTrace();
      }
    }
  }

  private int getTimestepMinutes( final ILinearSumGenerator generator, final NAControl control )
  {
    /* If a timestep in the generator is set, this one is used. */
    final Integer timestep = generator.getTimestep();
    if( timestep == null )
      return control.getMinutesOfTimestep();

    return timestep;
  }

  private void initGenerator( final ILinearSumGenerator generator, final DateRange range, final Period timestep, final String parameterType )
  {
    /* Set the period. */
    generator.setPeriod( range );

    /* Get the calendar field and the amount. */
    final String calendarField = PeriodUtils.findCalendarField( timestep ).name();
    final int amount = PeriodUtils.findCalendarAmount( timestep );

    /* Add a source filter. */
    if( ITimeseriesConstants.TYPE_TEMPERATURE.equals( parameterType ) )
      generator.addInterpolationFilter( calendarField, amount, true, "0.0", 0 ); //$NON-NLS-1$
    else
      generator.addIntervalFilter( calendarField, amount, 0.0, 0 );
  }

  private void initTargetLinks( final ILinearSumGenerator generator, final QName targetLink, final String parameterType ) throws Exception
  {
    /* Hash for the already created links. */
    final Map<String, String> linkHash = new HashMap<String, String>();

    /* The workspace to save. */
    GMLWorkspace workspaceToSave = null;

    /* Get the catchments. */
    final List<ICatchment> generatorCatchments = generator.getCatchments();
    for( final ICatchment generatorCatchment : generatorCatchments )
    {
      /* Get the area. */
      final IXLinkedFeature areaLink = (IXLinkedFeature) generatorCatchment.getAreaLink();
      final Catchment catchment = (Catchment) areaLink.getFeature();

      /* Find the workspace to save. */
      if( workspaceToSave == null )
        workspaceToSave = catchment.getWorkspace();

      /* Build the hash. */
      final String hash = RainfallGeneratorUtilities.buildHash( generatorCatchment );

      /* If the link hash contains this hash code, the corresponding link will be used. */
      if( linkHash.containsKey( hash ) )
      {
        /* Get the link. */
        final String link = linkHash.get( hash );

        /* Set the link. */
        setLink( catchment, targetLink, link );
      }
      else
      {
        /* Otherwise create a new link. */
        final String link = buildLink( parameterType, catchment );

        /* Set the link. */
        setLink( catchment, targetLink, link );

        /* Store the hash code. */
        linkHash.put( hash, link );
      }
    }

    /* Save the workspace, because it is reloaded in the rainfall operation. */
    /* HINT: This is the linked workspace of the modell.gml, not the loaded one here. */
    final IFile modelFile = m_simulation.getModelGml();
    GmlSerializer.saveWorkspace( workspaceToSave, modelFile );
  }

  private String buildLink( final String parameterType, final Catchment catchment ) throws UnsupportedEncodingException
  {
    final String folderName = getTargetLinkFolderName( parameterType );

    return String.format( "../%s/%s_%s.zml", folderName, parameterType, URLEncoder.encode( catchment.getName(), Charsets.UTF_8.name() ) ); //$NON-NLS-1$
  }

  private void setLink( final Catchment catchment, final QName targetLink, final String link )
  {
    /* Create the timeseries link type. */
    final TimeseriesLinkType tsLink = new TimeseriesLinkType();
    tsLink.setHref( link );

    /* Set the property. */
    catchment.setProperty( targetLink, tsLink );
  }

  // FIXME: use CalcCaseAccessor for that?
  private String getTargetLinkFolderName( final String parameterType )
  {
    switch( parameterType )
    {
      case ITimeseriesConstants.TYPE_RAINFALL:
        return "Niederschlag"; // TODO i18n; en = 'Precipitation'

      case ITimeseriesConstants.TYPE_TEMPERATURE:
      case ITimeseriesConstants.TYPE_EVAPORATION:
        return "Klima"; // TODO i18n; en = 'Climate'
    }

    throw new IllegalArgumentException();
  }

  private IRainfallCatchmentModel createRainfallModel( final NaModell model, final IRainfallGenerator generator, final QName targetLink, final DateRange targetRange ) throws Exception
  {
    /* Rainfall model. */
    final IFolder modelsFolder = m_simulation.getModelsFolder();
    final URL context = ResourceUtilities.createQuietURL( modelsFolder );
    final GMLWorkspace modelWorkspace = FeatureFactory.createGMLWorkspace( IRainfallCatchmentModel.FEATURE_FAINFALL_CATCHMENT_MODEL, context, GmlSerializer.DEFAULT_FACTORY );
    final IRainfallCatchmentModel rainfallModel = (IRainfallCatchmentModel) modelWorkspace.getRootFeature();

    /* Add a COPY of the generator into the model, because we are going to change it later */
    final IRelationType generatorsRelation = (IRelationType) rainfallModel.getFeatureType().getProperty( IRainfallCatchmentModel.MEMBER_GENERATOR );
    FeatureHelper.cloneFeature( rainfallModel, generatorsRelation, generator );

    /* Create a target. */
    final IRelationType parentRelation = (IRelationType) rainfallModel.getFeatureType().getProperty( IRainfallCatchmentModel.MEMBER_TARGET );
    final IFeatureType type = GMLSchemaUtilities.getFeatureTypeQuiet( ITarget.FEATURE_TARGET );
    final ITarget target = (ITarget) modelWorkspace.createFeature( rainfallModel, parentRelation, type );
    target.setCatchmentPath( "CatchmentCollectionMember/catchmentMember" ); //$NON-NLS-1$

    /* Set the target. */
    rainfallModel.setTarget( target );

    /* Create the link to the catchment. */
    final String catchmentLinkRef = "modell.gml#" + model.getId(); //$NON-NLS-1$
    target.setCatchmentFeature( catchmentLinkRef );

    /* Target range. */
    target.setPeriod( targetRange );

    /* Observation path. */
    target.setObservationPath( targetLink.getLocalPart() );

    return rainfallModel;
  }

  /**
   * Calculates the range for the timeseries to be generated. The range equals the range defined in the simulation
   * adjusted as follows:
   * <ul>
   * <li>1 timestep earlier</li>
   * <li>3 timesteps later</li>
   * </ul>
   */
  private DateRange getRange( final NAControl control, final Period timestep, final LocalTime timestamp )
  {
    final Date simulationStart = control.getSimulationStart();
    final Date simulationEnd = control.getSimulationEnd();

    final DateTime start = new DateTime( simulationStart );
    final DateTime end = new DateTime( simulationEnd );

    final DateTime adjustedStart = start.minus( timestep );
    final DateTime adjustedEnd = end.plus( timestep ).plus( timestep ).plus( timestep );

    if( timestep.getDays() == 0 || timestamp == null )
      return new DateRange( adjustedStart.toDate(), adjustedEnd.toDate() );

    /* Further adjust range by predefined time */
    final DateTime startWithTime = adjustedStart.withTime( timestamp.getHourOfDay(), timestamp.getMinuteOfHour(), timestamp.getSecondOfMinute(), timestamp.getMillisOfSecond() );
    final DateTime endWithTime = adjustedEnd.withTime( timestamp.getHourOfDay(), timestamp.getMinuteOfHour(), timestamp.getSecondOfMinute(), timestamp.getMillisOfSecond() );

    /* New start must always be before unadjusted start, fix, if this is not the case */
    DateTime startWithTimeFixed;
    if( startWithTime.isAfter( adjustedStart ) )
      startWithTimeFixed = startWithTime.minus( timestep );
    else
      startWithTimeFixed = startWithTime;

    /* New end must always be after unadjusted end, fix, if this is not the case */
    DateTime endWithTimeFixed;
    if( endWithTime.isBefore( adjustedEnd ) )
      endWithTimeFixed = endWithTime.plus( timestep );
    else
      endWithTimeFixed = endWithTime;

    return new DateRange( startWithTimeFixed.toDate(), endWithTimeFixed.toDate() );
  }

  private void copyInitialCondition( final NAControl control )
  {
    /* build source file name */
    final String calcCaseNameSource = control.getInitialValueSource();

    /* if not set, nothing to do */
    if( StringUtils.isBlank( calcCaseNameSource ) )
      return;

    /* Does the source simulation exist? */
    final ScenarioAccessor scenario = m_simulation.getScenario();
    final IFolder folderCalcCases = scenario.getSimulationsFolder();

    final RrmSimulation sourceCalcCase = new RrmSimulation( folderCalcCases.getFolder( new Path( calcCaseNameSource ) ) );

    if( !sourceCalcCase.exists() )
    {
      m_log.add( IStatus.WARNING, Messages.getString( "UpdateSimulationWorker_21" ), null, sourceCalcCase.getName() ); //$NON-NLS-1$
      return;
    }

    final IFolder currentSourceFolder = sourceCalcCase.getCurrentResultsFolder();
    if( !currentSourceFolder.exists() )
    {
      m_log.add( IStatus.WARNING, Messages.getString( "UpdateSimulationWorker_22" ), null, sourceCalcCase.getName() ); //$NON-NLS-1$
      return;
    }

    final IFolder initialValuesSourceFolder = sourceCalcCase.getCurrentLzimResultFolder();
    final Date startDate = control.getSimulationStart();
    final String initialValuesSourceFilename = new SimpleDateFormat( "yyyyMMdd'.gml'" ).format( startDate ); //$NON-NLS-1$
    final IFile initialValuesSourceFile = initialValuesSourceFolder.getFile( initialValuesSourceFilename );
    if( !initialValuesSourceFile.exists() )
    {
      m_log.add( IStatus.WARNING, Messages.getString( "UpdateSimulationWorker_24" ), null, sourceCalcCase.getName(), initialValuesSourceFilename ); //$NON-NLS-1$
      return;
    }

    /* target file */
    final IFile initialValuesTargetFile = m_simulation.getLzsimGml();

    /* Copy it ! */
    try
    {
      FileUtils.copyFile( initialValuesSourceFile.getLocation().toFile(), initialValuesTargetFile.getLocation().toFile() );
      initialValuesSourceFile.getParent().refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      m_log.add( IStatus.ERROR, Messages.getString( "UpdateSimulationWorker_25" ), e, sourceCalcCase.getName() ); //$NON-NLS-1$
    }
  }
}