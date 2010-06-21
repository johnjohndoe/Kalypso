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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.apache.commons.vfs.FileUtil;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.service.datalocation.Location;
import org.kalypso.commons.KalypsoCommonsExtensions;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.IProcessFactory;
import org.kalypso.commons.process.ProcessTimeoutException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
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
 * @author ig
 * 
 */
public class SWANKalypsoSimulation implements ISimulation, ISimulation1D2DConstants
{
  private static final String EXECUTE_RESPONSE_XML = "executeResponse.xml"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.model1d2d.swan"; //$NON-NLS-1$

  public static final String OUTPUT_RESULTS = "resultsSWAN"; //$NON-NLS-1$

  public static final String INPUT_SWAN_VERSION = "calcCoreExeSWAN"; //$NON-NLS-1$

  private IGeoLog m_log;

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resource/kalypso1d2dspec_swan.xml" ); //$NON-NLS-1$
  }

  /**
   * Runs SWAN calculation. The following steps are processed:
   * <ul>
   * <li>write swan.exe to temporary directory</li>
   * <li>execute the swan.exe</li>
   * <li>read results files and process them to the output directory</li>
   * </ul>
   * 
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
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
      manager = VFSUtilities.getNewManager();

      // TODO: specific error message if exe was not found
      final String version = (String) inputProvider.getInputForID( INPUT_SWAN_VERSION );
      final File exeFile = findSWANExe( version );
      final FileObject executableFile = manager.toFileObject( exeFile );
      final String executableName = exeFile.getName();

      final String processFactoryId = IProcessFactory.DEFAULT_PROCESS_FACTORY_ID;
      // simply switch here and we run in the grid :)
      // Remark: it would be good also for swan :)
      //      final String processFactoryId = "org.kalypso.simulation.gridprocess"; //$NON-NLS-1$

      final String tempDirName = tmpdir.getName();
      final IProcess process = KalypsoCommonsExtensions.createProcess( processFactoryId, tempDirName, executableName );
      // process.setProgressMonitor( progress );

      // add sandbox dir to results for monitoring (empty at this time)
      final String sandboxDirectory = process.getSandboxDirectory();
      try
      {
        resultEater.addResult( SWANKalypsoSimulation.OUTPUT_RESULTS, new URI( sandboxDirectory ) ); //$NON-NLS-1$
      }
      catch( final URISyntaxException e )
      {
        e.printStackTrace();
      }

      // copy executable and write input files
      final FileObject lFileObjWorkingDir = manager.resolveFile( sandboxDirectory );
      VFSUtilities.copyFileTo( executableFile, lFileObjWorkingDir );

      final String lStrPreSWANURL = (String) inputProvider.getInputForID( PreSWANKalypso.OUTPUT_PATH_SWAN );
      final FileObject lFileObjPreResultsDir = manager.resolveFile( lStrPreSWANURL );
      copyFilesToWorkDir( lFileObjPreResultsDir, lFileObjWorkingDir );

      final File stdoutFile = new File( tmpdir, "exe.log" ); //$NON-NLS-1$
      final File stderrFile = new File( tmpdir, "exe.err" ); //$NON-NLS-1$

      logOS = new BufferedOutputStream( new FileOutputStream( stdoutFile ) );
      errorOS = new BufferedOutputStream( new FileOutputStream( stderrFile ) );

      // Run the Calculation
      // final SubMonitor progress = SubMonitor.convert( progressMonitor, m_controlModel.getNCYC() );
      m_log.formatLog( IStatus.INFO, CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.0" ) + ": " + executableName ); //$NON-NLS-1$ //$NON-NLS-2$
      progressMonitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.15" ) ); //$NON-NLS-1$
      process.startProcess( logOS, errorOS, null, progressCancelable );

      // decide based on ERROR.OUT if simulation was successful
      final FileObject errorFile = lFileObjWorkingDir.resolveFile( "ERROR.OUT" ); //$NON-NLS-1$
      if( errorFile == null || !errorFile.exists() || errorFile.getContent().getSize() == 0 )
      {
        /* Successfully finished simulation */
        progressMonitor.done( StatusUtilities.createOkStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.20" ) ) ); //$NON-NLS-1$
      }
      else
      {
        /* ERROR: return contents of error file as error message */
        final byte[] content = FileUtil.getContent( errorFile );
        final String charset = Charset.defaultCharset().name();
        final String errorMessage = new String( content, charset );
        final IStatus status = StatusUtilities.createErrorStatus( errorMessage );
        progressMonitor.done( status );
      }
      // TODO: implement the results eater
    }
    catch( final ProcessTimeoutException e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMAKalypsoSimulation.0" ), e ); //$NON-NLS-1$
    }
    catch( final OperationCanceledException e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMAKalypsoSimulation.1" ), new CoreException( StatusUtilities.createStatus( IStatus.CANCEL, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.2" ), e ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
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

  private void copyFilesToWorkDir( final FileObject tmpDir, final FileObject targetDir ) throws FileSystemException, IOException
  {
    List<FileObject> lListFilesToRemove = new ArrayList<FileObject>();
    Set<String> exclusionFileNamesToMove = new HashSet<String>();
    exclusionFileNamesToMove.add( EXECUTE_RESPONSE_XML );
    // copy input files
    for( int i = 0; i < tmpDir.getChildren().length; i++ )
    {
      FileObject actFile = tmpDir.getChildren()[i];
      if( !exclusionFileNamesToMove.contains( actFile.getName().getBaseName().toLowerCase().trim() ) )
      {
        VFSUtilities.copyFileTo( actFile, targetDir );
        lListFilesToRemove.add( actFile );
      }
    }
    for( Iterator<FileObject> iterator = lListFilesToRemove.iterator(); iterator.hasNext(); )
    {
      FileObject actFile = iterator.next();
      actFile.delete();
    }

  }

  private File findSWANExe( final String exeVersionName ) throws CoreException
  {
    // REMARK: This is OS dependent; we use should use a pattern according to OS
    String exeName = exeVersionName;
    if( exeVersionName == null || "".equals( exeVersionName.trim() ) )
    {
      exeName = ISimulation1D2DConstants.SIM_SWAN_EXE_FILE_PREFIX + ".exe"; //$NON-NLS-1$
    }
    else{
      exeName = ISimulation1D2DConstants.SIM_SWAN_EXE_FILE_PREFIX + exeVersionName + ".exe"; //$NON-NLS-1$

    }

    final Location installLocation = Platform.getInstallLocation();
    final File installDir = FileUtils.toFile( installLocation.getURL() );
    final File exeDir = new File( installDir, "bin" ); //$NON-NLS-1$
    final File exeFile = new File( exeDir, exeName );
    if( exeFile.exists() )
      return exeFile;

    final String exeMissingMsg = String.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.26" ), exeFile.getAbsolutePath() ); //$NON-NLS-1$
    throw new CoreException( StatusUtilities.createErrorStatus( exeMissingMsg ) );
  }

}
