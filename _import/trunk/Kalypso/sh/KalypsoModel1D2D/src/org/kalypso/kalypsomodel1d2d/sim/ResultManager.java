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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.io.filter.PrefixSuffixFilter;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.ICommandPoster;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * This runnable will be called while running the 2d-exe and will check for new .2d result files.<br>
 * Every new 2d result file we be processed in order to return it to the kalypso client.
 * 
 * @author Gernot Belger
 */
public class ResultManager
{
  public static final Date STEADY_DATE = new Date( 0 );

  public static final Date MAXI_DATE = new Date( 1 );

  private static final FilenameFilter FILTER_2D = new PrefixSuffixFilter( "", ".2d" );

  /**
   * Time step id for non-unsteady calculation
   */
  private static final int PSEUDO_TIME_STEP_NR = -1;

  private final NodeResultMinMaxCatcher m_minMaxCatcher = new NodeResultMinMaxCatcher();

  private final File m_outputDir;

  private final File m_inputDir;

  private final Pattern m_resultFilePattern;

  /* just for test purposes TODO: still? */
  private final List<ResultType.TYPE> m_parameters = new ArrayList<ResultType.TYPE>();

  private final Date m_startTime;

  private final IGeoLog m_geoLog;

  private final ICaseDataProvider<IModel> m_caseDataProvider;

  private final IFolder m_resultFolder;

  public ResultManager( final File inputDir, final File outputDir, final String resultFilePattern, final Date startTime, final ICaseDataProvider<IModel> caseDataProvider, final IFolder resultFolder, final IGeoLog geoLog )
  {
    m_caseDataProvider = caseDataProvider;
    m_resultFolder = resultFolder;
    m_inputDir = inputDir;
    m_outputDir = outputDir;

    m_startTime = startTime;
    m_geoLog = geoLog;
    m_resultFilePattern = Pattern.compile( resultFilePattern + "(\\d+)" );

    m_parameters.add( ResultType.TYPE.DEPTH );
    m_parameters.add( ResultType.TYPE.WATERLEVEL );
    m_parameters.add( ResultType.TYPE.VELOCITY );
    m_parameters.add( ResultType.TYPE.TERRAIN );
  }

  public IStatus process( final boolean deleteAll, final boolean deleteFollowers, final Date[] steps, final IProgressMonitor monitor ) throws CoreException
  {
    final IControlModel1D2D controlModel = m_caseDataProvider.getModel( IControlModelGroup.class ).getModel1D2DCollection().getActiveControlModel();
    final IFlowRelationshipModel flowModel = m_caseDataProvider.getModel( IFlowRelationshipModel.class );
    final IScenarioResultMeta scenarioResultMeta = m_caseDataProvider.getModel( IScenarioResultMeta.class );

    final SubMonitor progress = SubMonitor.convert( monitor, "Ergebnisauswertung: " + controlModel.getName(), 100 );

    /* Process Results */
    final ICalculationUnit calculationUnit = controlModel.getCalculationUnit();

    // Step 1: Delete existing results and save result-DB (in case of problems while processing)
    deleteExistingResults( calculationUnit, steps, deleteAll, deleteFollowers, progress.newChild( 5 ) );

    // Step 2: Process results and add new entries to result-DB
    final IStatus processResultsStatus = processResults( controlModel, flowModel, scenarioResultMeta, progress.newChild( 90 ) );
    m_geoLog.log( processResultsStatus );

    if( processResultsStatus.matches( IStatus.ERROR | IStatus.CANCEL ) )
      return processResultsStatus;

    // TODO: handle the processResultStatus?
    // show error message and ask user if result should be kept anyway?

    // Step 3: Move results into workspace and save result-DB
    return moveResults( m_outputDir, progress.newChild( 5 ) );
  }

  private IStatus moveResults( final File outputDir, final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );
    progress.subTask( "Ergebnisdaten werden in Arbeitsbereich verschoben..." );

    try
    {
      final File unitWorkspaceDir = m_resultFolder.getLocation().toFile();
      FileUtils.forceMkdir( unitWorkspaceDir );
      FileUtilities.moveContents( outputDir, unitWorkspaceDir );
      ProgressUtilities.worked( progress, 70 );

      m_resultFolder.refreshLocal( IResource.DEPTH_INFINITE, progress.newChild( 20 ) );

      ((ICommandPoster) m_caseDataProvider).postCommand( IScenarioResultMeta.class, new EmptyCommand( "", false ) );
      m_caseDataProvider.saveModel( IScenarioResultMeta.class, progress.newChild( 10 ) );

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, "Ergebnisdateien konnten nicht in den Arbeitsbereich verschoben werden", e );
    }
    catch( final InvocationTargetException e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, "Ergebnisdateien konnten nicht in den Arbeitsbereich verschoben werden", e.getTargetException() );
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
  }

  private IStatus deleteExistingResults( final ICalculationUnit calcUnit, final Date[] calculatedSteps, final boolean deleteAll, final boolean deleteFollowers, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );
    progress.subTask( "Bestehende Ergebnisse werden gelöscht..." );

    final IScenarioResultMeta scenarioResultMeta = m_caseDataProvider.getModel( IScenarioResultMeta.class );
    final ICalcUnitResultMeta calcUnitMeta = scenarioResultMeta.findCalcUnitMetaResult( calcUnit.getGmlID() );

    /* If no results available yet, nothing to do. */
    if( calcUnitMeta == null )
      return Status.OK_STATUS;

    final Date[] stepsToDelete = findStepsToDelete( calcUnitMeta, calculatedSteps, deleteAll, deleteFollowers );
    ProgressUtilities.worked( progress, 5 );

    final IStatus result = ResultMeta1d2dHelper.deleteResults( calcUnitMeta, stepsToDelete, progress.newChild( 90 ) );
    if( !result.isOK() )
      throw new CoreException( result );

    /* Save result db */
    try
    {
      ((ICommandPoster) m_caseDataProvider).postCommand( IScenarioResultMeta.class, new EmptyCommand( "", false ) );
    }
    catch( final InvocationTargetException e )
    {
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Fehler beim Speichern der Ergebnisdatenbank", e.getTargetException() ) );
    }

    m_caseDataProvider.saveModel( IScenarioResultMeta.class, progress.newChild( 5 ) );

    return result;
  }

  private Date[] findStepsToDelete( final ICalcUnitResultMeta calcUnitMeta, final Date[] calculatedSteps, final boolean deleteAll, final boolean deleteFollowers )
  {
    final Date[] existingSteps = ResultMeta1d2dHelper.getStepDates( calcUnitMeta );

    if( deleteAll )
      return existingSteps;

    final SortedSet<Date> dates = new TreeSet<Date>();

    /* Always delete all calculated steps */
    dates.addAll( Arrays.asList( calculatedSteps ) );

    if( deleteFollowers )
    {
      /* Delete all steps later than the first calculated */
      final Date firstCalculated = dates.first();
      for( final Date date : existingSteps )
      {
        if( date.after( firstCalculated ) )
          dates.add( date );
      }
    }

    return dates.toArray( new Date[dates.size()] );
  }

  /* check for already processed files */
  // TODO: care for status of each processed file -> step result status!
  private IStatus processResultFile( final File file, final IControlModel1D2D controlModel, final IFlowRelationshipModel flowModel, final ICalcUnitResultMeta calcUnitResultMeta, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final String filename = file.getName();

      if( RMA10Calculation.MODEL_2D.equals( filename ) )
        return Status.OK_STATUS;

      final String resultFileName = FileUtilities.nameWithoutExtension( filename );

      final Date stepDate = findStepDate( controlModel, resultFileName );
      if( stepDate == null )
        return Status.OK_STATUS;

      m_geoLog.formatLog( IStatus.INFO, "Ergebnisauswertung - %s", resultFileName );

      // start a job for each unknown 2d file.
      final String outDirName;

      if( stepDate == STEADY_DATE )
        outDirName = "steady";
      else if( stepDate == MAXI_DATE )
        outDirName = "maxi";
      else
        outDirName = String.format( "timestep-%1$te.%1$tm.%1$tY_%1$tH#%1$tM", stepDate );

      final File resultOutputDir = new File( m_outputDir, outDirName );
      resultOutputDir.mkdirs();
      final ProcessResultsJob processResultsJob = new ProcessResultsJob( file, resultOutputDir, flowModel, controlModel, m_parameters, stepDate, calcUnitResultMeta );
      final IStatus result = processResultsJob.run( monitor );

      m_minMaxCatcher.addNodeResultMinMaxCatcher( processResultsJob.getMinMaxData() );

      // TODO: set this status as step result status?

      return result;
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  private Date findStepDate( final IControlModel1D2D controlModel, final String resultFileName )
  {
    // start a job for each unknown 2d file.
    final Matcher matcher = m_resultFilePattern.matcher( resultFileName );
    if( matcher.matches() )
    {
      final String countStr = matcher.group( 1 );
      final int step = Integer.parseInt( countStr );

      // find TIME
      // final boolean isRestart = controlModel.getRestart();
      // final Integer restartStep = controlModel.getIaccyc();
      final IObservation<TupleResult> obs = controlModel.getTimeSteps();
      final TupleResult timeSteps = obs.getResult();
      final IComponent componentTime = ComponentUtilities.findComponentByID( timeSteps.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
      final XMLGregorianCalendar stepCal = (XMLGregorianCalendar) timeSteps.get( step ).getValue( componentTime );
      return DateUtilities.toDate( stepCal );
    }

    if( resultFileName.startsWith( "steady" ) )
      return STEADY_DATE;

    if( resultFileName.startsWith( "maxi" ) )
      return MAXI_DATE;

    return null;
  }

  private IStatus processResults( final IControlModel1D2D controlModel, final IFlowRelationshipModel flowModel, final IScenarioResultMeta scenarioMeta, final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Ergebnisse werden ausgewertet...", 1000 );
    progress.subTask( "Ergebnisse werden ausgewertet..." );

    try
    {
      /* Create meta result workspace */
      final ICalculationUnit calculationUnit = controlModel.getCalculationUnit();
      final ICalcUnitResultMeta existingCalcUnitMeta = scenarioMeta.findCalcUnitMetaResult( calculationUnit.getGmlID() );
      final ICalcUnitResultMeta calcUnitResultMeta;
      if( existingCalcUnitMeta == null )
        calcUnitResultMeta = scenarioMeta.getChildren().addNew( ICalcUnitResultMeta.QNAME, ICalcUnitResultMeta.class );
      else
        calcUnitResultMeta = existingCalcUnitMeta;

      calcUnitResultMeta.setCalcStartTime( m_startTime );
      calcUnitResultMeta.setCalcUnit( calculationUnit.getGmlID() );
      calcUnitResultMeta.setName( calculationUnit.getName() );
      calcUnitResultMeta.setDescription( calculationUnit.getDescription() );
      calcUnitResultMeta.setPath( new Path( m_resultFolder.getName() ) );

      ProgressUtilities.worked( monitor, 100 );

      /* Process all remaining .2d files. Now including min and max files */
      final File[] existing2dFiles = m_inputDir.listFiles( FILTER_2D );
      progress.setWorkRemaining( existing2dFiles.length );

      final IStatus[] fileStati = new IStatus[existing2dFiles.length];

      for( int i = 0; i < fileStati.length; i++ )
      {
        final File file = existing2dFiles[i];
        fileStati[i] = processResultFile( file, controlModel, flowModel, calcUnitResultMeta, progress.newChild( 1 ) );
      }

      // TODO: this calcUnitStatus is quite important: we should see to it, that it contains some meaningful message to
      // the user Probably it should only be set after everything has finished?
      // TODO: also consider calculation problems
      final MultiStatus multiStatus = new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), -1, "", null );
      // TODO: distinguish between problems during calculation and problems during processing
      final IStatus calcUnitStatus = StatusUtilities.createStatus( multiStatus.getSeverity(), "", null );

      calcUnitResultMeta.setCalcEndTime( new Date() );
      calcUnitResultMeta.setStatus( calcUnitStatus );

      return multiStatus;
    }
    catch( final OperationCanceledException e )
    {
      return StatusUtilities.createStatus( IStatus.CANCEL, "Abbruch durch den Benutzer", e );
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
    catch( final Throwable e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, "Unbekannter Fehler", e );
    }
  }

  public Date[] getCalculatedSteps( ) throws CoreException
  {
    final IControlModel1D2D controlModel = m_caseDataProvider.getModel( IControlModelGroup.class ).getModel1D2DCollection().getActiveControlModel();

    final Set<Date> dates = new TreeSet<Date>();
    final File[] existing2dFiles = m_inputDir.listFiles( FILTER_2D );
    for( final File file : existing2dFiles )
    {
      final Date stepDate = findStepDate( controlModel, file.getName() );
      if( stepDate != null )
        dates.add( stepDate );
    }

    return dates.toArray( new Date[dates.size()] );
  }
}
