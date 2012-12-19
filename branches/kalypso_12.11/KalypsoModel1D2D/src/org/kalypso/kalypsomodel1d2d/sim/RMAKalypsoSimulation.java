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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileUtil;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.osgi.service.datalocation.Location;
import org.kalypso.commons.KalypsoCommonsExtensions;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.IProcessFactory;
import org.kalypso.commons.process.ProcessTimeoutException;
import org.kalypso.commons.vfs.FileSystemManagerWrapper;
import org.kalypso.contribs.java.lang.ICancelable;
import org.kalypso.contribs.java.lang.ProgressCancelable;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;

/**
 * @author kurzbach
 */
public class RMAKalypsoSimulation implements ISimulation
{
  public static final String ID = "org.kalypso.model1d2d"; //$NON-NLS-1$

  public static final String INPUT_RMA_VERSION = IRMAPreprocessing.OUTPUT_RMA_VERSION;

  public static final String INPUT_MESH = IRMAPreprocessing.OUTPUT_MESH;

  public static final String INPUT_BC_WQ = IRMAPreprocessing.OUTPUT_BC_WQ;

  public static final String INPUT_BUILDINGS = IRMAPreprocessing.OUTPUT_BUILDINGS;

  public static final String INPUT_CONTROL = IRMAPreprocessing.OUTPUT_CONTROL;

  public static final String INPUT_WORKING_DIR = "workingDirectory"; //$NON-NLS-1$

  public static final String OUTPUT_RESULTS = "results"; //$NON-NLS-1$

  public static final String INPUT_WIND = IRMAPreprocessing.OUTPUT_WIND;

  public static final String INPUT_WIND_COORD = IRMAPreprocessing.OUTPUT_WIND_COORD;

  private IGeoLog m_log;

  private FileObject workingDir;

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
   * </ul>
   * 
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider, org.kalypso.simulation.core.ISimulationResultEater,
   *      org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final SimulationMonitorAdaptor progressMonitor = new SimulationMonitorAdaptor( monitor );
    final ICancelable progressCancelable = new ProgressCancelable( progressMonitor );

    try
    {
      m_log = new GeoLog( KalypsoModel1D2DPlugin.getDefault().getLog() );
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Could not initialize GeoLog", e ); //$NON-NLS-1$
    }

    OutputStream logOS = null;
    OutputStream errorOS = null;
    FileSystemManagerWrapper manager = null;
    try
    {
      // TODO: use URI instead of URL
      final String version = (String)inputProvider.getInputForID( INPUT_RMA_VERSION );
      final URL modelFileUrl = (URL)inputProvider.getInputForID( INPUT_MESH );
      final URL controlFileUrl = (URL)inputProvider.getInputForID( INPUT_CONTROL );
      final URL buildingFileUrl = (URL)inputProvider.getInputForID( INPUT_BUILDINGS );
      final URL bcwqFileUrl = (URL)inputProvider.getInputForID( INPUT_BC_WQ );
      final URL windFileUrl = (URL)inputProvider.getInputForID( INPUT_WIND );
      final URL windCoordFileUrl = (URL)inputProvider.getInputForID( INPUT_WIND_COORD );

      manager = VFSUtilities.getNewManager();
      final FileObject modelFile = manager.resolveFile( modelFileUrl.toString() );
      final FileObject controlFile = manager.resolveFile( controlFileUrl.toString() );
      final FileObject buildingFile = manager.resolveFile( buildingFileUrl.toString() );
      final FileObject bcwqFile = manager.resolveFile( bcwqFileUrl.toString() );
      final FileObject windFile = manager.resolveFile( windFileUrl.toString() );
      final FileObject windCoordFile = manager.resolveFile( windCoordFileUrl.toString() );

      // find executable for version
      final File exeFile = findRma10skExe( version );
      final FileObject executableFile = manager.toFileObject( exeFile );
      final String executableName = exeFile.getName();

      // Generate the process-factory-id
      // TODO: at the moment, this is hard wired.... later we should get it from System.properties and/or from our own
      // simulation-id (as we are no simulation, this does not work yet).
      // Example1: org.kalypso.simulation.process.factory.<simulation-id>=<factory-id>
      // For the moment, we could also provide it directly from outside or from a system-property
      // (fall-back should always be the default factory)

      final String processFactoryId = IProcessFactory.DEFAULT_PROCESS_FACTORY_ID;
      // simply switch here and we run in the grid :)
      //      final String processFactoryId = "org.kalypso.simulation.gridprocess"; //$NON-NLS-1$

      final String tempDirName;
      if( inputProvider.hasID( INPUT_WORKING_DIR ) )
        tempDirName = (String)inputProvider.getInputForID( INPUT_WORKING_DIR );
      else
        tempDirName = tmpdir.getName();

      final IProcess process = KalypsoCommonsExtensions.createProcess( processFactoryId, tempDirName, executableName );

      // add sandbox dir to results for monitoring (empty at this time)
      final String sandboxDirectory = process.getSandboxDirectory();

      try
      {
        final URI resultURI = new URI( sandboxDirectory );
        if( resultURI.getScheme().equals( "file" ) ) //$NON-NLS-1$
          resultEater.addResult( OUTPUT_RESULTS, new File( resultURI ) );
        else
          resultEater.addResult( OUTPUT_RESULTS, resultURI );
      }
      catch( final URISyntaxException e )
      {
        e.printStackTrace();
      }

      // check if user cancelled
      if( progressMonitor.isCanceled() )
      {
        throw new OperationCanceledException();
      }

      // copy executable and write input files
      // final FileObject
      workingDir = manager.resolveFile( sandboxDirectory );
      VFSUtilities.copyFileTo( executableFile, workingDir );
      VFSUtilities.copyFileTo( modelFile, workingDir );
      VFSUtilities.copyFileTo( controlFile, workingDir );
      VFSUtilities.copyFileTo( buildingFile, workingDir );
      VFSUtilities.copyFileTo( bcwqFile, workingDir );
      VFSUtilities.copyFileTo( windFile, workingDir );
      VFSUtilities.copyFileTo( windCoordFile, workingDir );

      final File stdoutFile = new File( tmpdir, "exe.log" ); //$NON-NLS-1$
      final File stderrFile = new File( tmpdir, "exe.err" ); //$NON-NLS-1$

      logOS = new BufferedOutputStream( new FileOutputStream( stdoutFile ) );
      errorOS = new BufferedOutputStream( new FileOutputStream( stderrFile ) );

      // check if user cancelled
      if( monitor.isCanceled() )
      {
        throw new OperationCanceledException();
      }
      System.gc();
      // Run the Calculation
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.0" ) + ": " + executableName ); //$NON-NLS-1$ //$NON-NLS-2$
      process.startProcess( logOS, errorOS, null, progressCancelable );

      // decide based on ERROR.OUT if simulation was successful
      final FileObject errorFile = workingDir.resolveFile( "ERROR.OUT" ); //$NON-NLS-1$
      if( errorFile == null || !errorFile.exists() || errorFile.getContent().getSize() == 0 )
      {
        /* Successfully finished simulation */
        progressMonitor.done( new Status( IStatus.OK, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.20" ) ) ); //$NON-NLS-1$
      }
      else
      {
        /* ERROR: return contents of error file as error message */
        final byte[] content = FileUtil.getContent( errorFile );
        final String charset = Charset.defaultCharset().name();
        final String errorMessage = new String( content, charset );
        final IStatus status = new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, errorMessage );
        progressMonitor.done( status );
        throw new CoreException( status );
      }
    }
    catch( final ProcessTimeoutException e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMAKalypsoSimulation.0" ), e ); //$NON-NLS-1$
    }
    catch( final OperationCanceledException e )
    {
      // do not throw an exception if cancelled
      monitor.setFinishInfo( IStatus.CANCEL, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMAKalypsoSimulation.1" ) );//$NON-NLS-1$
      monitor.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMAKalypsoSimulation.1" ) );//$NON-NLS-1$
      return;
    }
    catch( final CoreException e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMAKalypsoSimulation.2" ), e ); //$NON-NLS-1$
    }
    catch( final IOException e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMAKalypsoSimulation.3" ), e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );

      if( manager != null )
        manager.close();
    }
  }

  private File findRma10skExe( final String version ) throws CoreException
  {
    if( version == null || version.length() == 0 )
      // REMARK: maybe could instead use a default or the one with the biggest version number?
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.23" ) ) ); //$NON-NLS-1$

    // REMARK: This is OS dependent; we use should use a pattern according to OS
    final String exeName = ISimulation1D2DConstants.SIM_RMA10_EXE_FILE_PREFIX + version + ".exe"; //$NON-NLS-1$

    final Location installLocation = Platform.getInstallLocation();
    final File installDir = FileUtils.toFile( installLocation.getURL() );
    final File exeDir = new File( installDir, "bin" ); //$NON-NLS-1$
    final File exeFile = new File( exeDir, exeName );
    if( exeFile.exists() )
      return exeFile;

    final String exeMissingMsg = String.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.26" ), exeFile.getAbsolutePath() ); //$NON-NLS-1$
    throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, exeMissingMsg ) );
  }

  public final FileObject getWorkingDir( )
  {
    return workingDir;
  }

}
