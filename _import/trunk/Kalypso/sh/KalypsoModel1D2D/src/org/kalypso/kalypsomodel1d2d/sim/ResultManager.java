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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.java.io.filter.PrefixSuffixFilter;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * This runnable will be called while running the 2d-exe and will check for new .2d result files.
 * <p>
 * Every new 2d result file we be processed in order to return it to the kalypso client.
 * </p>
 * TODO: - write System.out in simulation-log
 * 
 * @author Gernot Belger
 */
public class ResultManager implements Runnable
{
  private final static MutexRule m_mutex = new MutexRule();

  private static final FilenameFilter FILTER_2D = new PrefixSuffixFilter( "", ".2d" );

  private static final FilenameFilter FILTER_GMT = new PrefixSuffixFilter( "", ".gmt" );

  /**
   * Time step id for non-unsteady calculation
   */
  private static final int PSEUDO_TIME_STEP_NR = -1;

  private final List<File> m_found2dFiles = new ArrayList<File>();

  private final List<Job> m_resultJobs = new ArrayList<Job>();

  private final File m_outputDir;

  private final File m_inputDir;

  private final Pattern m_resultFilePattern;

  private final ISimulationDataProvider m_dataProvider;

  private final RMA10Calculation m_calculation;

  private final NodeResultMinMaxCatcher m_minMaxCatcher = new NodeResultMinMaxCatcher();

  private final ICalcUnitResultMeta m_calcUnitResultMeta;

  /* just for test purposes */
  private final List<ResultType.TYPE> m_parameters = new ArrayList<ResultType.TYPE>();
  {
    m_parameters.add( ResultType.TYPE.DEPTH );
    m_parameters.add( ResultType.TYPE.WATERLEVEL );
    m_parameters.add( ResultType.TYPE.VELOCITY );
    m_parameters.add( ResultType.TYPE.TERRAIN );
  }

  private final IJobChangeListener m_finishListener = new JobChangeAdapter()
  {
    /**
     * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void done( final IJobChangeEvent event )
    {
      resultJobDone( event );
    }
  };

  private boolean m_init = false;

  @SuppressWarnings("unchecked")
  public ResultManager( final File inputDir, final File outputDir, final String resultFilePattern, final ISimulationDataProvider dataProvider, final RMA10Calculation calculation, final Date startTime ) throws InvocationTargetException
  {
    m_inputDir = inputDir;
    m_outputDir = outputDir;
    m_dataProvider = dataProvider;
    m_calculation = calculation;
    m_resultFilePattern = Pattern.compile( resultFilePattern + "(\\d+)" );

    /* GMLWorkspace für Ergebnisse anlegen */
    final GMLWorkspace resultMetaWorkspace = FeatureFactory.createGMLWorkspace( ICalcUnitResultMeta.QNAME, null, null );
    m_calcUnitResultMeta = (ICalcUnitResultMeta) resultMetaWorkspace.getRootFeature().getAdapter( ICalcUnitResultMeta.class );

    final ICalculationUnit calculationUnit = calculation.getCalculationUnit();

    m_calcUnitResultMeta.setCalcStartTime( startTime );
    m_calcUnitResultMeta.setCalcUnit( calculationUnit.getGmlID() );
    m_calcUnitResultMeta.setName( calculationUnit.getName() );
    m_calcUnitResultMeta.setDescription( calculationUnit.getDescription() );
    m_calcUnitResultMeta.setPath( new Path( outputDir.getName() ) );
  }

  /**
   * Call this immediately before calculation starts.
   */
  public void calculationAboutToStart( )
  {
    m_init = true;

    /* Filter existing .2d files at that point (filters out model.2d) */
    final File[] existing2dFiles = m_inputDir.listFiles( FILTER_2D );
    m_found2dFiles.addAll( Arrays.asList( existing2dFiles ) );
  }

  /**
   * This method should be run often during the real calculation (i.e. execution of the RMA10S.exe).
   * <p>
   * Each time, it is checked if new result files are present and if this is the case they are processed.
   * </p>
   * 
   * @see java.lang.Runnable#run()
   */
  public void run( )
  {
    if( !m_init )
      return;

    final File[] existing2dFiles = m_inputDir.listFiles( FILTER_2D );
    for( final File file : existing2dFiles )
    {
      if( !m_found2dFiles.contains( file ) )
        addResultFile( file );
    }
  }

  private void addResultFile( final File file )
  {
    final String resultFileName = FileUtilities.nameWithoutExtension( file.getName() );

    System.out.println( "Found new 2d-file: " + resultFileName );
    // check for 2d files

    // start a job for each unknown 2d file.
    final Matcher matcher = m_resultFilePattern.matcher( resultFileName );
    final String outDirName;
    final int timeStepNum;
    if( matcher.matches() )
    {
      final String countStr = matcher.group( 1 );
      final int count = Integer.parseInt( countStr );
      // TODO Please explain why x-1? In that case, we have timestep-0 instead to start from 1

      outDirName = "timestep-" + countStr;
      timeStepNum = count;
    }
    else
    {
      outDirName = resultFileName;
      timeStepNum = PSEUDO_TIME_STEP_NR;
    }

    final File resultOutputDir = new File( m_outputDir, outDirName );
    resultOutputDir.mkdirs();
    final ProcessResultsJob processResultsJob = new ProcessResultsJob( file, resultOutputDir, m_dataProvider, m_calculation, m_parameters, timeStepNum, m_calcUnitResultMeta );
    processResultsJob.addJobChangeListener( m_finishListener );

    m_resultJobs.add( processResultsJob );

    /* Schedule job: wait some time in order to make sure file was written to disk. */
    processResultsJob.setRule( m_mutex );
    processResultsJob.schedule( 1000 );

    m_found2dFiles.add( file );
  }

  public Job[] getResultJobs( )
  {
    return m_resultJobs.toArray( new Job[m_resultJobs.size()] );
  }

  /** After calculation, wait until all result process jobs have finished. */
  private IStatus waitForResultProcessing( )
  {
    // TODO: timeout
    for( int i = 0; i < 1000; i++ )
    {
      try
      {
        final IStatus status = jobsFinished();
        if( status != null )
          return status;

        Thread.sleep( 1000 );
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();
      }
    }

    // Timeout reached, produce error status
    return StatusUtilities.createWarningStatus( "Zeitüberschreitung beim Prozessieren der Ergebnisdateien, möglicherweise können nicht lale Ergebnisdaten übertragen werden." );
  }

  /**
   * Check if all jobs have finished. If this is thre case, return a multi status composed of all job-results.
   * <p>
   * If one or more jobs are not finished, returns <code>null</code>.
   * </p>
   */
  private IStatus jobsFinished( )
  {
    final MultiStatus status = new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), -1, "Ergebnisse der Ergebnisauswertung:", null );
    for( final Job job : m_resultJobs )
    {
      final IStatus jobResult = job.getResult();
      if( jobResult == null )
        return null;

      status.add( jobResult );
    }

    return status;
  }

  protected void resultJobDone( final IJobChangeEvent event )
  {
    final ProcessResultsJob job = (ProcessResultsJob) event.getJob();
    m_minMaxCatcher.addNodeResultMinMaxCatcher( job.getMinMaxData() );
  }

  public void finish( ) throws SimulationException
  {
    if( !m_init )
      return;

    /* Process all remaining .2d files. */
    run();

    /* We need to wait until all result process jobs have finished. */
    final IStatus resultProcessingStatus = waitForResultProcessing();
    if( resultProcessingStatus != null )
      System.out.println( resultProcessingStatus );
    // TODO: evaluate status

    /* other, general post-processing stuff. */
    try
    {
      /* Write template sld into result folder */
      // final URL resultStyleURL = (URL) m_dataProvider.getInputForID( "ResultTemplate" );
      // ZipUtilities.unzip( resultStyleURL, m_outputDir );
      // TODO: error handling! handle stati everywhere....
      writeResultMeta( resultProcessingStatus );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler bei der Ergebnisauswertung", e );
    }
  }

  private void writeResultMeta( final IStatus resultStatus ) throws IOException, GmlSerializeException
  {
    m_calcUnitResultMeta.setCalcEndTime( new Date() );
    m_calcUnitResultMeta.setStatus( resultStatus );

    final GMLWorkspace workspace = m_calcUnitResultMeta.getWrappedFeature().getWorkspace();
    final File metaFile = new File( m_outputDir, "resultMeta.gml" );
    GmlSerializer.serializeWorkspace( metaFile, workspace, "UTF-8" );
  }
}
