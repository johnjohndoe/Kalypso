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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.io.filter.PrefixSuffixFilter;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

/**
 * This runnable will be called while running the 2d-exe and will check for new .2d result files.<br>
 * Every new 2d result file we be processed in order to return it to the kalypso client.
 * 
 * @author Gernot Belger
 */
public class ResultManager implements ISimulation1D2DConstants
{
  public static final Date STEADY_DATE = new Date( 0 );

  public static final Date MAXI_DATE = new Date( 1 );

  private static final FilenameFilter FILTER_2D = new PrefixSuffixFilter( "", ".2d" );

  private final NodeResultMinMaxCatcher m_minMaxCatcher = new NodeResultMinMaxCatcher();

  private final File m_outputDir;

  private final File m_inputDir;

  private final Pattern m_resultFilePattern;

  /* just for test purposes TODO: still? */
  private final List<ResultType.TYPE> m_parameters = new ArrayList<ResultType.TYPE>();

  private final IGeoLog m_geoLog;

  private final IControlModel1D2D m_controlModel;

  private final IFlowRelationshipModel m_flowModel;

  private final IScenarioResultMeta m_scenarioMeta;

  public ResultManager( final File inputDir, final File outputDir, final String resultFilePattern, final IControlModel1D2D controlModel, final IFlowRelationshipModel flowModel, final IScenarioResultMeta scenarioResultMeta, final IGeoLog geoLog )
  {
    m_controlModel = controlModel;
    m_flowModel = flowModel;
    m_scenarioMeta = scenarioResultMeta;
    m_inputDir = inputDir;
    m_outputDir = outputDir;

    m_geoLog = geoLog;
    m_resultFilePattern = Pattern.compile( resultFilePattern + "(\\d+)" );

    m_parameters.add( ResultType.TYPE.DEPTH );
    m_parameters.add( ResultType.TYPE.WATERLEVEL );
    m_parameters.add( ResultType.TYPE.VELOCITY );
    m_parameters.add( ResultType.TYPE.TERRAIN );
  }

  public IStatus processResults( final ICalcUnitResultMeta calcUnitMeta, final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Ergebnisse werden ausgewertet...", 1000 );
    progress.subTask( "Ergebnisse werden ausgewertet..." );

    try
    {
      /* Process all remaining .2d files. Now including min and max files */
      final File[] existing2dFiles = m_inputDir.listFiles( FILTER_2D );
      progress.setWorkRemaining( existing2dFiles.length );

      final IStatus[] fileStati = new IStatus[existing2dFiles.length];

      for( int i = 0; i < fileStati.length; i++ )
      {
        final File file = existing2dFiles[i];
        fileStati[i] = processResultFile( file, m_controlModel, m_flowModel, calcUnitMeta, progress.newChild( 1 ) );
      }

      final MultiStatus multiStatus = new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, "", null );
      if( multiStatus.isOK() )
        return StatusUtilities.createStatus( IStatus.OK, CODE_POST, "Alle Ergebnisse wurden erfolgreich ausgewertet.", null );

      if( multiStatus.matches( IStatus.CANCEL ) )
        return StatusUtilities.createStatus( IStatus.WARNING, CODE_POST, "Abbruch duch den Benutzer.", null );

      if( multiStatus.matches( IStatus.WARNING ) )
        return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, "Warnmeldungen beim Auswerten der Ergebnisse.", null );

      if( multiStatus.matches( IStatus.ERROR ) )
        return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, "Fehler beim Auswerten Ergebnisse.", null );

      return new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), CODE_POST, fileStati, "Unbekanntes Problem bei der Ergebnisauswertung.", null );
    }
    catch( final OperationCanceledException e )
    {
      return StatusUtilities.createStatus( IStatus.CANCEL, CODE_POST, "Abbruch durch den Benutzer", e );
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
    catch( final Throwable e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, CODE_POST, "Unbekannter Fehler", e );
    }
  }

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

      m_geoLog.formatLog( IStatus.INFO, CODE_RUNNING_FINE, "Ergebnisauswertung - %s", resultFileName );

      // start a job for each unknown 2d file.
      final String outDirName;

      if( stepDate == STEADY_DATE )
        outDirName = "steady";
      else if( stepDate == MAXI_DATE )
        outDirName = "maxi";
      else
        outDirName = String.format( "timestep-%1$te.%1$tm.%1$tY_%1$tH_%1$tM_%1$tZ", stepDate );

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

  public Date[] findCalculatedSteps( )
  {
    final Set<Date> dates = new TreeSet<Date>();
    final File[] existing2dFiles = m_inputDir.listFiles( FILTER_2D );
    for( final File file : existing2dFiles )
    {
      final Date stepDate = findStepDate( m_controlModel, file.getName() );
      if( stepDate != null )
        dates.add( stepDate );
    }

    return dates.toArray( new Date[dates.size()] );
  }

  public IControlModel1D2D getControlModel( )
  {
    return m_controlModel;
  }

  public IFlowRelationshipModel getFlowModel( )
  {
    return m_flowModel;
  }

  public IScenarioResultMeta getScenarioMeta( )
  {
    return m_scenarioMeta;
  }

  public File getOutputDir( )
  {
    return m_outputDir;
  }
}
