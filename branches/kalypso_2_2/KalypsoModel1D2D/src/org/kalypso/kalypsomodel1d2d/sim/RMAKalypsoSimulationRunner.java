/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.opengeospatial.ows.ExceptionReport;
import net.opengeospatial.ows.ExceptionType;
import net.opengeospatial.wps.ExecuteResponseType;
import net.opengeospatial.wps.ProcessFailedType;
import net.opengeospatial.wps.StatusType;
import net.opengeospatial.wps.IOValueType.ComplexValueReference;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypso.service.wps.client.exceptions.WPSException;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Represents a calculation with a rma10s.exe. Helps generation of the ASCII input files and to start the process.
 */
public class RMAKalypsoSimulationRunner extends WPSRequest implements ISimulation1D2DConstants
{
  private final IControlModel1D2D m_controlModel;

  private final IGeoLog m_log;

  private IStatus m_simulationStatus;

  private IterationInfo m_iterationInfo;

  // private IFolder m_scenarioFolder;

  private IterationInfoJob m_iterationJob;

  private FileObject m_resultsDir;

  private FileSystemManagerWrapper m_manager;

  public RMAKalypsoSimulationRunner( final IGeoLog geoLog, final SzenarioDataProvider caseDataProvider ) throws CoreException
  {
    super( RMAKalypsoSimulation.ID, WPSRequest.SERVICE_LOCAL, -1 );
    m_log = geoLog;
    final IControlModelGroup controlModelGroup = caseDataProvider.getModel( IControlModelGroup.class );
    m_controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();
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
   * <li>read .2d files and process them to the output directory</li>
   * </ul>
   */
  public IStatus runCalculation( final IProgressMonitor monitor )
  {
    m_log.formatLog( IStatus.INFO, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.0" ) ); //$NON-NLS-1$

    final IStatus simulationStatus = doRunCalculation( monitor );
    m_simulationStatus = evaluateSimulationResult( simulationStatus );

    // check if status is allready in collection
    final IStatusCollection statusCollection = m_log.getStatusCollection();
    if( !statusCollection.contains( simulationStatus ) )
      m_log.log( m_simulationStatus );

    return m_simulationStatus;
  }

  private IStatus doRunCalculation( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "", 1000 ); //$NON-NLS-1$

    try
    {
      // final ICalculationUnit calculationUnit = m_controlModel.getCalculationUnit();
      // final String calcUnitID = calculationUnit.getGmlID();
      // result temp dir is "[scenarioFolder]/results/[calcUnitID]/temp"
      // final IFolder scenarioResultsFolder = m_scenarioFolder.getFolder( ISimulation1D2DConstants.OUTPUT_DIR_NAME );
      //      final IFolder resultTempFolder = scenarioResultsFolder.getFolder( calcUnitID ).getFolder( "temp" ); //$NON-NLS-1$
      //
      // // remember result temp dir for later result processing (absolute path)
      // m_resultTmpDir = new File( resultTempFolder.getLocationURI() );

      final Map<String, Object> inputs = new HashMap<String, Object>();
      final List<String> outputs = new ArrayList<String>();
      final String resultsID = "results"; //$NON-NLS-1$
      outputs.add( resultsID );

      return this.run( inputs, outputs, progress.newChild( 1000, SubMonitor.SUPPRESS_NONE ) );
      // TODO: read other outputs
      // - error-log
      // - border conditions-log
    }
    catch( final Throwable e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, CODE_RMA10S, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.22" ), e ); //$NON-NLS-1$
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

  /**
   * @see org.kalypso.service.wps.client.WPSRequest#doProcessStarted(org.eclipse.core.runtime.IProgressMonitor,
   *      net.opengeospatial.wps.ExecuteResponseType)
   */
  @Override
  protected void doProcessStarted( final IProgressMonitor monitor, final ExecuteResponseType exState ) throws WPSException
  {
    super.doProcessStarted( monitor, exState );

    if( m_iterationJob != null )
    {
      return;
    }

    final Map<String, ComplexValueReference> references = getReferences();
    final ComplexValueReference complexValueReference = references.get( "results" ); //$NON-NLS-1$
    if( complexValueReference == null )
    {
      return;
    }

    final String statusLocation = complexValueReference.getReference();
    // final String statusLocation = wpsRequest.getStatusLocation();
    try
    {
      // this manager is closed when the results dir is not needed anymore
      m_manager = VFSUtilities.getNewManager();
      m_resultsDir = m_manager.resolveFile( statusLocation );

      // The iteration job (and its info) monitor the content of the "Output.itr" file
      // and inform the user about the current progress of the process.
      // watch iteration observation directly in temp dir (only possible for local simulation)
      final FileObject iterObsFile = m_resultsDir.resolveFile( OUTPUT_ITR );
      // and put processed iterations GML into folder "iterObs" in result temp dir
      //      final File iterObsDir = new File( m_resultTmpDir, "iterObs" ); //$NON-NLS-1$
      final File iterObsDir = FileUtilities.TMP_DIR;
      iterObsDir.mkdirs();

      m_iterationInfo = new IterationInfo( iterObsFile, iterObsDir, m_controlModel );
      m_iterationJob = new IterationInfoJob( m_iterationInfo, m_controlModel, monitor );
      m_iterationJob.setSystem( true );
      m_iterationJob.schedule();
    }
    catch( final IOException e )
    {
      throw new WPSException( e );
    }
  }

  /**
   * @see org.kalypso.service.wps.client.WPSRequest#doProcessFailed(net.opengeospatial.wps.ExecuteResponseType)
   */
  @Override
  protected IStatus doProcessFailed( final ExecuteResponseType exState )
  {

    final StatusType exStatus = exState.getStatus();
    final ProcessFailedType processFailed = exStatus.getProcessFailed();
    final ExceptionReport exceptionReport = processFailed.getExceptionReport();
    final List<ExceptionType> exceptions = exceptionReport.getException();
    String messages = ""; //$NON-NLS-1$
    for( final ExceptionType exception : exceptions )
    {
      final List<String> exceptionList = exception.getExceptionText();
      final String exceptionText = Arrays.toString( exceptionList.toArray( new String[exceptionList.size()] ), "\n" ); //$NON-NLS-1$
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
      return StatusUtilities.createStatus( IStatus.WARNING, CODE_RMA10S, errorMessage, null );
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
      return StatusUtilities.createStatus( IStatus.OK, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.1" ), null ); //$NON-NLS-1$

    if( simulationStatus.matches( IStatus.CANCEL ) )
      return StatusUtilities.createStatus( IStatus.CANCEL, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.2" ), null ); //$NON-NLS-1$

    if( simulationStatus.matches( IStatus.ERROR ) )
      return simulationStatus;

    // if( simulationStatus.matches( IStatus.WARNING ) )
    return simulationStatus; // StatusUtilities.createStatus( IStatus.WARNING, CODE_RUNNING, "Simulation mit Warnung
    // beendet.",
    // null );
  }

  public IterationInfo getIterationInfo( )
  {
    return m_iterationInfo;
  }

  public IStatus getSimulationStatus( )
  {
    return m_simulationStatus;
  }

  public FileObject getTempDir( )
  {
    return m_resultsDir;
  }
}
