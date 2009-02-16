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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.osgi.service.datalocation.Location;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.KalypsoCommonsExtensions;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.IProcessFactory;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.ICancelable;
import org.kalypso.contribs.java.lang.ProgressCancelable;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.Building1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.BuildingIDProvider;
import org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.conv.WQboundaryConditions1D2DConverter;
import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author kurzbach
 * 
 */
public class RMAKalypsoSimulation implements ISimulation, ISimulation1D2DConstants
{
  public static final String ID = "org.kalypso.model1d2d"; //$NON-NLS-1$

  private IFEDiscretisationModel1d2d m_discretisationModel;

  private IFlowRelationshipModel m_flowRelationshipModel;

  private IRoughnessClsCollection m_roughnessModel;

  private IControlModel1D2D m_controlModel;

  private IGeoLog m_log;

  private IContainer m_scenarioFolder;

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resource/kalypso1d2dspec.xml" ); //$NON-NLS-1$
  }

  /**
   * Runs < rma10s calculation. The following steps are processed:
   * <ul>
   * <li>write rma10s ASCII files to temporary directory according to provided gml-models</li>
   * <li>write .exe to temporary directory</li>
   * <li>execute the .exe</li>
   * <li>read .2d files and process them to the output directory</li>
   * </ul>
   * 
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final SimulationMonitorAdaptor progressMonitor = new SimulationMonitorAdaptor( monitor );

    try
    {
      m_log = new GeoLog( KalypsoModel1D2DPlugin.getDefault().getLog() );
    }
    catch( final InvocationTargetException e )
    {
      throw new SimulationException( "Could not initialize GeoLog", e ); //$NON-NLS-1$
    }

    final SzenarioDataProvider caseDataProvider = ScenarioHelper.getScenarioDataProvider();

    OutputStream logOS = null;
    OutputStream errorOS = null;
    try
    {
      m_scenarioFolder = caseDataProvider.getScenarioFolder();
      m_discretisationModel = caseDataProvider.getModel( IFEDiscretisationModel1d2d.class );
      m_flowRelationshipModel = caseDataProvider.getModel( IFlowRelationshipModel.class );
      m_roughnessModel = caseDataProvider.getModel( IRoughnessClsCollection.class );
      final IControlModelGroup controlModelGroup = caseDataProvider.getModel( IControlModelGroup.class );
      m_controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();

      writeRma10Files( tmpdir, progressMonitor );

      final File stdoutFile = new File( tmpdir, "exe.log" ); //$NON-NLS-1$
      final File stderrFile = new File( tmpdir, "exe.err" ); //$NON-NLS-1$

      logOS = new BufferedOutputStream( new FileOutputStream( stdoutFile ) );
      errorOS = new BufferedOutputStream( new FileOutputStream( stderrFile ) );

      startCalcCore( tmpdir, logOS, errorOS, progressMonitor );

      resultEater.addResult( "results", new File( tmpdir, Control1D2DConverter.RESULT_DIR_NAME ) ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      throw new SimulationException( String.format( "Simulation error in %s", m_scenarioFolder.getFullPath() ), e ); //$NON-NLS-1$
    }
    catch( final FileNotFoundException e )
    {
      throw new SimulationException( String.format( "Simulation error in %s", m_scenarioFolder.getFullPath() ), e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );
      progressMonitor.done();
    }
  }

  private void writeRma10Files( final File tmpDir, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    try
    {
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.3" ) ); //$NON-NLS-1$

      /* Read restart data */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.4" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.5" ) ); //$NON-NLS-1$

      RestartNodes m_restartNodes;

      if( m_controlModel.getRestart() )
        m_restartNodes = RestartNodes.createRestartNodes( m_scenarioFolder, m_controlModel );
      else
        m_restartNodes = null;

      ProgressUtilities.worked( progress, 20 );

      /* .2d File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.6" ) ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.7" ) ); //$NON-NLS-1$
      final File modelFile = new File( tmpDir, MODEL_2D );
      final ICalculationUnit calculationUnit = m_controlModel.getCalculationUnit();
      final Gml2RMA10SConv converter2D = new Gml2RMA10SConv( m_discretisationModel, m_flowRelationshipModel, calculationUnit, m_roughnessModel, m_restartNodes, false, true, m_log );
      converter2D.writeRMA10sModel( modelFile );
      ProgressUtilities.worked( progress, 20 );

      /* Control File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.8" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.9" ) ); //$NON-NLS-1$
      final File r10file = new File( tmpDir, ISimulation1D2DConstants.R10_File );
      final BuildingIDProvider buildingProvider = converter2D.getBuildingProvider();
      final Control1D2DConverter controlConverter = new Control1D2DConverter( m_controlModel, m_flowRelationshipModel, m_roughnessModel, converter2D, buildingProvider, m_log );
      controlConverter.writeR10File( r10file );
      ProgressUtilities.worked( progress, 20 );

      /* Building File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.10" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.11" ) ); //$NON-NLS-1$
      final File buildingFile = new File( tmpDir, ISimulation1D2DConstants.BUILDING_File );
      final Building1D2DConverter buildingConverter = new Building1D2DConverter( buildingProvider );
      buildingConverter.writeBuildingFile( buildingFile );
      ProgressUtilities.worked( progress, 20 );

      /* W/Q BC File */
      m_log.formatLog( IStatus.INFO, CODE_RUNNING_FINE, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.12" ) ); //$NON-NLS-1$
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.13" ) ); //$NON-NLS-1$
      final File bcWQFile = new File( tmpDir, ISimulation1D2DConstants.BC_WQ_File );
      final WQboundaryConditions1D2DConverter bc1D2DConverter = new WQboundaryConditions1D2DConverter( controlConverter.getBoundaryConditionsIDProvider() );
      bc1D2DConverter.writeWQbcFile( bcWQFile );
      ProgressUtilities.worked( progress, 20 );
    }
    catch( final IOException e )
    {
      final String msg = String.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.14" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, CODE_PRE, msg, e ) );
    }
    finally
    {
      progress.done();
    }
  }

  /**
   * Runs the rma10s simulation, i.e. starts the rma10s.exe in the temp-dir and waits until the process returns.
   */
  private void startCalcCore( final File tmpDir, final OutputStream errorOS, final OutputStream logOS, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, m_controlModel.getNCYC() );
    progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.15" ) ); //$NON-NLS-1$

    // Generate the process-factory-id
    // TODO: at the moment, this is hard wired.... later we should get it from System.properties and/or from our own
    // simulation-id (as we are no simulation, this does not work yet).
    // Example1: org.kalypso.simulation.process.factory.<simulation-id>=<factory-id>
    // For the moment, we could also provide it directly from outside or from a system-property
    // (fall-back should always be the default factory)
    final String processFactoryId = IProcessFactory.DEFAULT_PROCESS_FACTORY_ID;

    try
    {
      final File exeFile = findRma10skExe();

      final String commandString = exeFile.getAbsolutePath();

      // Run the Calculation
      m_log.formatLog( IStatus.INFO, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.0" ) + ": " + commandString ); //$NON-NLS-1$ //$NON-NLS-2$

      final ICancelable progressCancelable = new ProgressCancelable( progress );

      // TODO: specific error message if exe was not found
      final URL exeURL = exeFile.toURI().toURL();
      final IProcess process = KalypsoCommonsExtensions.createProcess( processFactoryId, tmpDir, exeURL, null );
      process.setProgressMonitor( progress );
      process.startProcess( logOS, errorOS, null, progressCancelable );
    }
    catch( final OperationCanceledException e )
    {
      throw new CoreException( StatusUtilities.createStatus( IStatus.CANCEL, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.2" ), e ) ); //$NON-NLS-1$
    }
    catch( final CoreException ce )
    {
      throw ce;
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, CODE_RMA10S, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.22" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      ProgressUtilities.done( progress );
    }
  }

  private File findRma10skExe( ) throws CoreException
  {
    // Determine exe filename
    final String version = m_controlModel.getVersion();
    if( version == null || version.length() == 0 )
      // REMARK: maybe could instead use a default or the one with the biggest version number?
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.23" ) ) ); //$NON-NLS-1$

    // REMARK: This is OS dependent; we use should use a pattern according to OS
    final String exeName = ISimulation1D2DConstants.SIM_EXE_FILE_PREFIX + version + ".exe"; //$NON-NLS-1$

    final Location installLocation = Platform.getInstallLocation();
    final File installDir = FileUtils.toFile( installLocation.getURL() );
    final File exeDir = new File( installDir, "bin" ); //$NON-NLS-1$
    final File exeFile = new File( exeDir, exeName );
    if( exeFile.exists() )
      return exeFile;

    final String exeMissingMsg = String.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.26" ), exeFile.getAbsolutePath() ); //$NON-NLS-1$
    throw new CoreException( StatusUtilities.createErrorStatus( exeMissingMsg ) );
  }
}
