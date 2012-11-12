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
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import net.opengeospatial.ows.ExceptionReport;
import net.opengeospatial.ows.ExceptionType;
import net.opengeospatial.wps.ExecuteResponseType;
import net.opengeospatial.wps.ProcessFailedType;
import net.opengeospatial.wps.StatusType;

import org.apache.commons.vfs2.FileObject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypso.service.wps.client.exceptions.WPSException;
import org.kalypso.service.wps.refactoring.DefaultWpsObserver;
import org.kalypso.service.wps.refactoring.IWPSProcess;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Represents a calculation with a rma10s.exe. Helps generation of the ASCII input files and to start the process.
 */
public class RMAKalypsoSimulationRunner extends DefaultWpsObserver implements ISimulation1D2DConstants, IKalypsoSimulationRunnerComposite
{
  private static final String HOCHWERT_NAME = "Hochwert:"; //$NON-NLS-1$

  private static final String RECHTSWERT_NAME = "Rechtswert:"; //$NON-NLS-1$

  private final IControlModel1D2D m_controlModel;

  private final IGeoLog m_log;

  private IStatus m_simulationStatus;

  private IIterationInfo m_iterationInfo;

  private IterationInfoJob m_iterationJob;

  private FileObject m_resultsDirRMA = null;

  private final String m_serviceEndpoint;

  private ExecuteRMAKalypsoSimulation m_executeRMAKalypsoSimulation;

  private WPSRequest m_wpsRequest = null;

  private IWPSProcess m_wpsProcess = null;

  private boolean m_boolFirstDone = false;

  public RMAKalypsoSimulationRunner( final IGeoLog geoLog, final IControlModel1D2D controlModel, final String serviceEndpoint )
  {
    m_log = geoLog;
    m_controlModel = controlModel;
    m_serviceEndpoint = serviceEndpoint;
  }

  public IControlModel1D2D getControlModel( )
  {
    return m_controlModel;
  }

  /**
   * Runs < rma10s calculation. The following steps are processed:
   * <ul>
   * <li>write rma10s ASCII files to temporary directory according to provided gml-models</li>
   * <li>write .exe to temporary directory</li>
   * <li>execute the .exe</li>
   * </ul>
   */
  public IStatus runCalculation( final IProgressMonitor monitor )
  {
    m_log.formatLog( IStatus.INFO, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.0" ) ); //$NON-NLS-1$

    final IStatus simulationStatus = doRunCalculation( monitor );
    m_simulationStatus = evaluateSimulationResult( simulationStatus );

    // check if status is already in collection
    final IStatusCollection statusCollection = m_log.getStatusCollection();
    if( !statusCollection.contains( simulationStatus ) )
      m_log.log( m_simulationStatus );

    return m_simulationStatus;
  }

  private IStatus doRunCalculation( final IProgressMonitor progressMonitor )
  {
    final SubMonitor progress = SubMonitor.convert( progressMonitor, 1000 );

    final IStatusCollector log = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

    try
    {
      final List<IRestartInfo> restartInfos;
      if( m_controlModel.getRestart() )
        restartInfos = m_controlModel.getRestartInfos();
      else
        restartInfos = Collections.emptyList();

      // generate input files
      final ExecutePreRMAKalypso executePreRMAKalypso = new ExecutePreRMAKalypso( m_serviceEndpoint, restartInfos );
      m_wpsRequest = executePreRMAKalypso.getWpsRequest();
      final IStatus preStatus = executePreRMAKalypso.run( progress.newChild( 100, SubMonitor.SUPPRESS_NONE ) );

      if( preStatus.isMultiStatus() )
        log.addAll( Arrays.asList( preStatus.getChildren() ) );
      else
        log.add( preStatus );

      // abort on error
      if( preStatus.matches( IStatus.ERROR ) )
        return preStatus;

      m_boolFirstDone = true;

      // gather inputs for simulation
      final String rmaVersion = m_controlModel.getVersion();
      final URI modelFile = new URI( executePreRMAKalypso.getModelFileUrl() );
      final URI controlFile = new URI( executePreRMAKalypso.getControlFileUrl() );
      final URI buildingFile = new URI( executePreRMAKalypso.getBuildingFileUrl() );
      final URI bcwqFile = new URI( executePreRMAKalypso.getBcwqFileUrl() );
      final URI windFile = new URI( executePreRMAKalypso.getWindUrl() );
      final URI windCoordFile = new URI( executePreRMAKalypso.getWindCoordUrl() );

      m_executeRMAKalypsoSimulation = new ExecuteRMAKalypsoSimulation( m_serviceEndpoint, this, rmaVersion, modelFile, controlFile, buildingFile, bcwqFile, windFile, windCoordFile );
      final IStatus simulationStatus = m_executeRMAKalypsoSimulation.run( progress.newChild( 900, SubMonitor.SUPPRESS_NONE ) );
      m_wpsProcess = m_executeRMAKalypsoSimulation.getWpsRequest();

      log.add( simulationStatus );

      // at least once update
      handleStarted( progress, null );

      return log.asMultiStatus( "Kalypso1D2D Simulation" );
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.22" ) ); //$NON-NLS-1$
    }
    finally
    {
      // clean up iteration job
      if( m_iterationJob != null )
      {
        try
        {
          m_iterationJob.finish();
        }
        catch( final IOException e )
        {
          // gobble
        }
      }
    }
  }

  private IStatus evaluateSimulationResult( final IStatus simulationStatus )
  {
    // TODO: distinguish different type of problems:
    // - exe/data errors: calculation could not be started at all
    // - no results
    // - some results

    // TODO: show dialog to user where he can
    // - choose, which steps to process
    // - choose, which results to be deleted

    if( simulationStatus.isOK() )
      return new Status( IStatus.OK, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.1" ), null ); //$NON-NLS-1$

    if( simulationStatus.matches( IStatus.CANCEL ) )
      return new Status( IStatus.CANCEL, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.2" ), null ); //$NON-NLS-1$

    if( simulationStatus.matches( IStatus.ERROR ) )
      return new MultiStatus( KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_RUNNING, simulationStatus.getChildren(), "Simulation mit Fehler beendet.", null );

    if( simulationStatus.matches( IStatus.WARNING ) )
      return new MultiStatus( KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_RUNNING, simulationStatus.getChildren(), "Simulation mit Warnung beendet.", null );

    return simulationStatus;
  }

  @Override
  public IIterationInfo getIterationInfo( )
  {
    return m_iterationInfo;
  }

  @Override
  public IStatus getSimulationStatus( )
  {
    return m_simulationStatus;
  }

  public FileObject getTempDir( )
  {
    return m_resultsDirRMA;
  }

  @Override
  public void handleStarted( final IProgressMonitor monitor, final ExecuteResponseType exState ) throws WPSException
  {
    if( m_iterationJob != null )
      return;

    try
    {
      m_resultsDirRMA = m_executeRMAKalypsoSimulation.getResultsDir();
      if( m_resultsDirRMA == null )
        return;

      // The iteration job (and its info) monitor the content of the "Output.itr" file
      // and inform the user about the current progress of the process.
      // watch iteration observation directly in temp dir (only possible for local simulation)
      final FileObject iterObsFile = m_resultsDirRMA.resolveFile( OUTPUT_ITR_RMA );
      // and put processed iterations GML into folder "iterObs" in result temp dir
      //      final File iterObsDir = new File( m_resultTmpDir, "iterObs" ); //$NON-NLS-1$
      final File iterObsDir = FileUtilities.TMP_DIR;
      iterObsDir.mkdirs();

      m_iterationInfo = new IterationInfo( iterObsFile, iterObsDir, m_controlModel.getTimeSteps() );
      m_iterationJob = new IterationInfoJob( m_iterationInfo, m_controlModel.getNCYC(), monitor );
      m_iterationJob.setSystem( true );
      m_iterationJob.schedule();
    }
    catch( final Exception e )
    {
      throw new WPSException( e );
    }
  }

  @Override
  public IStatus handleFailed( final ExecuteResponseType exState )
  {
    final StatusType exStatus = exState.getStatus();
    final ProcessFailedType processFailed = exStatus.getProcessFailed();
    final ExceptionReport exceptionReport = processFailed.getExceptionReport();
    final List<ExceptionType> exceptions = exceptionReport.getException();
    String messages = ""; //$NON-NLS-1$
    for( final ExceptionType exception : exceptions )
    {
      final List<String> exceptionList = exception.getExceptionText();
      final String exceptionText = org.kalypso.contribs.java.util.Arrays.toString( exceptionList.toArray( new String[exceptionList.size()] ), "\n" ); //$NON-NLS-1$
      messages = messages + exceptionText;
    }
    return errorToStatus( messages );
  }

  private IStatus errorToStatus( final String errorMessage )
  {
    // TODO: error or warning depends, if any steps where calculated; the rma10s should determine if result processing
    // makes sense

    final GM_Object location = getLocationFromStatusMsg( errorMessage );
    if( location == null )
    {
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_RMA10S, errorMessage, null );
    }

    final String[] lines = errorMessage.split( "\n" ); //$NON-NLS-1$
    final String message = lines[0];

    return m_log.log( IStatus.WARNING, CODE_RMA10S, message, location, null );
  }

  private GM_Object getLocationFromStatusMsg( final String errorMessage )
  {
    GM_Object location = null;
    int posR = errorMessage.indexOf( RECHTSWERT_NAME );
    int posH = errorMessage.indexOf( HOCHWERT_NAME );
    if( posR >= 0 || posH >= 0 )
    {
      final int lenR = RECHTSWERT_NAME.length();
      final int lenH = HOCHWERT_NAME.length();
      final String[] lines = errorMessage.split( "\n" ); //$NON-NLS-1$
      for( final String line : lines )
      {
        posR = line.indexOf( RECHTSWERT_NAME );
        if( posR >= 0 )
        {
          posH = line.indexOf( HOCHWERT_NAME );

          final BigDecimal rw = toBigDecimal( line.substring( posR + lenR, posH ) );
          final BigDecimal hw = toBigDecimal( line.substring( posH + lenH ) );
          try
          {
            location = GeometryFactory.createGM_Point( rw.doubleValue(), hw.doubleValue(), KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
          }
          catch( final Exception e )
          {
            location = null;
          }
          break;
        }
      }
    }
    return location;
  }

  private BigDecimal toBigDecimal( final String msg )
  {
    BigDecimal newBd = null;
    try
    {
      newBd = new BigDecimal( msg.trim() );
    }
    catch( final Exception e )
    {
      final String msgNew = msg.trim().replace( ";", "" ).replace( ",", "." ); //$NON-NLS-1$  //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      newBd = new BigDecimal( msgNew );
    }
    return newBd;
  }

  public IStatus cancelJob( )
  {
    final MultiStatus lResStatus = StatusUtilities.createMultiStatusFromMessage( IStatus.OK, KalypsoModel1D2DPlugin.getDefault().toString(), CODE_NONE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.1" ), " ", null ); //$NON-NLS-1$; //$NON-NLS-2$;
    if( m_wpsRequest != null && !m_boolFirstDone )
    {
      lResStatus.add( m_wpsRequest.cancelJob() );
      m_wpsRequest = null;
    }
    if( m_wpsProcess != null )
    {
      lResStatus.add( m_wpsProcess.cancelJob() );
      m_wpsProcess = null;
    }
    return lResStatus;
  }

  @Override
  public String getCalculationTypeName( )
  {
    return "RMA Simulation"; //$NON-NLS-1$
  }
}