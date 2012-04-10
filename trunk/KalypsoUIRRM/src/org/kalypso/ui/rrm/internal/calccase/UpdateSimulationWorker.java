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
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
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
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.util.logging.SystemOutLogger;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.timeseries.merged.Source;
import org.kalypso.simulation.core.ant.copyobservation.CopyObservationFeatureVisitor;
import org.kalypso.simulation.core.ant.copyobservation.ICopyObservationSource;
import org.kalypso.simulation.core.ant.copyobservation.source.FeatureCopyObservationSource;
import org.kalypso.simulation.core.ant.copyobservation.target.CopyObservationTargetFactory;
import org.kalypso.simulation.core.ant.copyobservation.target.ICopyObservationTarget;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.visitors.MonitorFeatureVisitor;

/**
 * The worker that actually updates a simulation.
 * 
 * @author Gernot Belger
 */
public class UpdateSimulationWorker
{
  /**
   * The qname of the mapping member.
   */
  public static final QName MAPPING_MEMBER = new QName( "http://org.kalypso.updateObservationMapping", "mappingMember" ); //$NON-NLS-1$ //$NON-NLS-2$

  /**
   * The simulation.
   */
  private final RrmSimulation m_simulation;

  /**
   * The log.
   */
  private final IStatusCollector m_log;

  /**
   * The constructor.
   * 
   * @param simulationFolder
   *          The simulation folder.
   */
  public UpdateSimulationWorker( final IFolder simulationFolder )
  {
    m_simulation = new RrmSimulation( simulationFolder );
    m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );
  }

  /**
   * This function updates the simulation.
   * 
   * @param monitor
   *          A progress monitor.
   */
  public IStatus execute( IProgressMonitor monitor ) throws CoreException
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString( "UpdateSimulationWorker_2" ), 120 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "UpdateSimulationWorker_3" ) ); //$NON-NLS-1$

      /* Read models. */
      final NAControl control = readControlFile();
      final NaModell model = readModelFile();

      /* Monitor. */
      ProgressUtilities.worked( monitor, 20 );

      /* Copy observations for pegel and zufluss. */
      copyMappingTimeseries( control, "ObsQMapping.gml", Messages.getString( "UpdateSimulationWorker.0" ), new SubProgressMonitor( monitor, 20 ) ); //$NON-NLS-1$ //$NON-NLS-2$
      copyMappingTimeseries( control, "ObsQZuMapping.gml", Messages.getString( "UpdateSimulationWorker.1" ), new SubProgressMonitor( monitor, 20 ) ); //$NON-NLS-1$ //$NON-NLS-2$
      copyMappingTimeseries( control, "ObsEMapping.gml", Messages.getString( "UpdateSimulationWorker.2" ), new SubProgressMonitor( monitor, 20 ) ); //$NON-NLS-1$ //$NON-NLS-2$

      /* Execute catchment models. */
      CatchmentModelHelper.executeCatchmentModel( m_simulation, control, model, control.getGeneratorN(), Catchment.PROP_PRECIPITATION_LINK, ITimeseriesConstants.TYPE_RAINFALL, new SubProgressMonitor( monitor, 20 ) );
      CatchmentModelHelper.executeCatchmentModel( m_simulation, control, model, control.getGeneratorT(), Catchment.PROP_TEMPERATURE_LINK, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE, new SubProgressMonitor( monitor, 20 ) );
      CatchmentModelHelper.executeCatchmentModel( m_simulation, control, model, control.getGeneratorE(), Catchment.PROP_EVAPORATION_LINK, ITimeseriesConstants.TYPE_MEAN_EVAPORATION_LAND_BASED, new SubProgressMonitor( monitor, 20 ) );

      /* Copy initial condition from long term simulation. */
      copyInitialCondition( control );

      return m_log.asMultiStatusOrOK( m_simulation.getName() );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
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
      /* Read mapping. */
      final FeatureList mappingFeatures = readMapping( mappingFilename, MAPPING_MEMBER );

      /* Prepare visitor. */
      final CopyObservationFeatureVisitor visitor = prepareVisitor( outputFoldername, "inObservationLink", control ); //$NON-NLS-1$

      /* Execute visitor. */
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

    /* Calculate range. */
    final Integer timestepMinutes = control.getMinutesOfTimestep();
    final Period timestep = Period.minutes( timestepMinutes ).normalizedStandard();
    final DateRange targetRange = CatchmentModelHelper.getRange( control, timestep, null );

    final Source source = new Source();
    source.setProperty( sourceLinkName );
    source.setFrom( Long.toString( targetRange.getFrom().getTime() ) );
    source.setTo( Long.toString( targetRange.getTo().getTime() ) );

    /* REMARK: Must be null, so the special NA-ObservationTarget is created. */
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

  private void copyInitialCondition( final NAControl control )
  {
    /* Build source file name. */
    final String calcCaseNameSource = control.getInitialValueSource();

    /* If not set, nothing to do. */
    if( StringUtils.isBlank( calcCaseNameSource ) )
      return;

    /* Does the source simulation exist? */
    final RrmScenario scenario = m_simulation.getScenario();
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

    /* Target file. */
    final IFile initialValuesTargetFile = m_simulation.getLzsimGml();

    /* Copy it! */
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