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
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
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
 * Represents a calculation with a swan.exe. Helps generation of the ASCII input files and to start the process.
 */
public class SWANKalypsoSimulationRunner extends DefaultWpsObserver implements ISimulation1D2DConstants, IKalypsoSimulationRunnerComposite
{
  private final IControlModel1D2D m_controlModel;

  private final IGeoLog m_log;

  private IStatus m_simulationStatus;

  private IIterationInfo m_iterationInfo;

  private IterationInfoJob m_iterationJob;

  private FileObject m_resultsDirSWAN = null;

  private final String m_serviceEndpoint;

  private ExecuteSWANKalypsoSimulation m_executeSWANKalypsoSimulation;

  private WPSRequest m_wpsRequest = null;

  private IWPSProcess m_wpsProcess = null;

  private boolean m_boolFirstDone = false;

  private URI m_uriRMACalcPath;

  public SWANKalypsoSimulationRunner( final IGeoLog geoLog, final IControlModel1D2D controlModel, final String serviceEndpoint )
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
   * Runs swan calculation. The following steps are processed:
   * <ul>
   * <li>write SWAN input ASCII files to temporary directory according to provided gml-models</li>
   * <li>write .exe to temporary directory</li>
   * <li>execute the .exe</li>
   * </ul>
   */
  public IStatus runCalculation( final IProgressMonitor monitor )
  {
    m_log.formatLog( IStatus.INFO, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.0" ) ); //$NON-NLS-1$

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
    progress.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.0" ), 1000 ); //$NON-NLS-1$
    ProgressUtilities.worked( progress, 1 );
    try
    {
      // final List<IRestartInfo> restartInfos;
      // if( m_controlModel.getRestart() )
      // restartInfos = m_controlModel.getRestartInfos();
      // else
      // restartInfos = Collections.emptyList();

      // generate input files
      final ExecutePreSWANKalypso executePreSWANKalypso = new ExecutePreSWANKalypso( m_serviceEndpoint, m_controlModel.getCalculationUnit().getId(), m_uriRMACalcPath );

      m_wpsRequest = executePreSWANKalypso.getWpsRequest();
      final IStatus preStatus = executePreSWANKalypso.run( progress.newChild( 100, SubMonitor.SUPPRESS_NONE ) );

      // abort on error
      if( !preStatus.isOK() )
        return preStatus;

      m_boolFirstDone = true;

      // gather inputs for simulation
      final String lSWANVersion = m_controlModel.getVersionSWAN();
      final URI lSWANModelPath = new URI( executePreSWANKalypso.getSWANModelPath() );

      m_executeSWANKalypsoSimulation = new ExecuteSWANKalypsoSimulation( m_serviceEndpoint, this, lSWANVersion, lSWANModelPath );
      final IStatus simulationStatus = m_executeSWANKalypsoSimulation.run( progress.newChild( 900, SubMonitor.SUPPRESS_NONE ) );
      m_wpsProcess = m_executeSWANKalypsoSimulation.getWpsRequest();

      // at least once update
      handleStarted( progress, null );
      return simulationStatus;
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.22" ) ); //$NON-NLS-1$
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
      return simulationStatus;

    // if( simulationStatus.matches( IStatus.WARNING ) )
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
    return m_resultsDirSWAN;
  }

  /**
   * @see org.kalypso.service.wps.refactoring.DefaultWpsObserver#handleStarted(org.eclipse.core.runtime.IProgressMonitor,
   *      net.opengeospatial.wps.ExecuteResponseType)
   */
  @Override
  public void handleStarted( final IProgressMonitor monitor, final ExecuteResponseType exState ) throws WPSException
  {
    if( m_iterationJob != null )
    {
      return;
    }

    try
    {
      m_resultsDirSWAN = m_executeSWANKalypsoSimulation.getResultsDir();
      if( m_resultsDirSWAN == null )
        return;

      // The iteration job (and its info) monitor the content of the "Output.itr" file
      // and inform the user about the current progress of the process.
      // watch iteration observation directly in temp dir (only possible for local simulation)
      final FileObject iterObsFile = m_resultsDirSWAN.resolveFile( OUTPUT_ITR_SWAN );
      // and put processed iterations GML into folder "iterObs" in result temp dir
      final File iterObsDir = FileUtilities.TMP_DIR;
      iterObsDir.mkdirs();

      m_iterationInfo = new IterationInfoSWAN( iterObsFile, iterObsDir, m_controlModel.getTimeSteps() );
      m_iterationJob = new IterationInfoJob( m_iterationInfo, m_controlModel.getNCYC(), monitor );
      m_iterationJob.setSystem( true );
      m_iterationJob.schedule();
    }
    catch( final Exception e )
    {
      throw new WPSException( e );
    }
  }

  /**
   * @see org.kalypso.service.wps.client.WPSRequest#doProcessFailed(net.opengeospatial.wps.ExecuteResponseType)
   */
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

    final String[] lines = errorMessage.split( "\n" ); //$NON-NLS-1$
    if( lines.length != 7 )
    {
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, CODE_RMA10S, errorMessage, null );
    }

    final int severity = IStatus.WARNING;
    final String message = lines[0];

    final GM_Object location;
    if( lines[5].length() < 51 )
    {
      location = null;
    }
    else
    {
      final BigDecimal rw = new BigDecimal( lines[5].substring( 13, 26 ).trim() );
      final BigDecimal hw = new BigDecimal( lines[5].substring( 38, 51 ).trim() );

      location = GeometryFactory.createGM_Point( rw.doubleValue(), hw.doubleValue(), KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    }

    return m_log.log( severity, CODE_RMA10S, message, location, null );
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

  public void setRMACalculationOutputPath( final URI rmaCalcPath )
  {
    m_uriRMACalcPath = rmaCalcPath;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.sim.IKalypsoSimulationRunnerComposite#getCalculationTypeName()
   */
  @Override
  public String getCalculationTypeName( )
  {
    return "SWAN Simulation"; //$NON-NLS-1$
  }
}
