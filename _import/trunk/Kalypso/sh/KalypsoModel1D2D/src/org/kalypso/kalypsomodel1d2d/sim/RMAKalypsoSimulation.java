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
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.opengeospatial.wps.IOValueType.ComplexValueReference;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.apache.commons.vfs.FileUtil;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
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
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kurzbach
 * 
 */
public class RMAKalypsoSimulation implements ISimulation
{
  public static final String INPUT_WORKING_DIR = "workingDirectory"; //$NON-NLS-1$

  public static final String OUTPUT_RESULTS = "results"; //$NON-NLS-1$

  public static final String ID = "org.kalypso.model1d2d"; //$NON-NLS-1$

  private IGeoLog m_log;

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
    final ICancelable progressCancelable = new ProgressCancelable( progressMonitor );

    try
    {
      m_log = new GeoLog( KalypsoModel1D2DPlugin.getDefault().getLog() );
    }
    catch( final InvocationTargetException e )
    {
      throw new SimulationException( "Could not initialize GeoLog", e ); //$NON-NLS-1$
    }

    OutputStream logOS = null;
    OutputStream errorOS = null;
    FileSystemManagerWrapper manager = null;
    try
    {
      final Map<String, Object> inputs = new HashMap<String, Object>();

      final URL controlUrl = (URL) inputProvider.getInputForID( PreRMAKalypso.INPUT_CONTROL );
      inputs.put( PreRMAKalypso.INPUT_CONTROL, controlUrl.toURI() );

      final URL meshUrl = (URL) inputProvider.getInputForID( PreRMAKalypso.INPUT_MESH );
      inputs.put( PreRMAKalypso.INPUT_MESH, meshUrl.toURI() );

      if( inputProvider.hasID( PreRMAKalypso.INPUT_CALCULATION_UNIT_ID ) )
      {
        final String calcUnitID = (String) inputProvider.getInputForID( PreRMAKalypso.INPUT_CALCULATION_UNIT_ID );
        inputs.put( PreRMAKalypso.INPUT_CALCULATION_UNIT_ID, calcUnitID );
      }

      final URL flowRelURL = (URL) inputProvider.getInputForID( PreRMAKalypso.INPUT_FLOW_RELATIONSHIPS );
      inputs.put( PreRMAKalypso.INPUT_FLOW_RELATIONSHIPS, flowRelURL.toURI() );

      final URL roughnessURL = (URL) inputProvider.getInputForID( PreRMAKalypso.INPUT_ROUGHNESS );
      inputs.put( PreRMAKalypso.INPUT_ROUGHNESS, roughnessURL.toURI() );

      // deserialize control model for version checking
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlUrl, null );
      final IControlModelGroup controlModelGroup = (IControlModelGroup) controlWorkspace.getRootFeature().getAdapter( IControlModelGroup.class );
      final IControlModel1D2D controlModel = controlModelGroup.getModel1D2DCollection().getActiveControlModel();

      if( controlModel.getRestart() )
      {
        for( int i = 0; i < 3; i++ )
        {
          final String restartFileInputName = PreRMAKalypso.INPUT_RESTART_FILE_PREFIX + i;
          if( inputProvider.hasID( restartFileInputName ) )
          {
            final URL restartURL = (URL) inputProvider.getInputForID( restartFileInputName );
            inputs.put( restartFileInputName, restartURL.toURI() );
          }
        }
      }

      final List<String> outputs = new ArrayList<String>();
      outputs.add( ISimulation1D2DConstants.MODEL_2D );
      outputs.add( ISimulation1D2DConstants.R10_File );
      outputs.add( ISimulation1D2DConstants.BUILDING_File );
      outputs.add( ISimulation1D2DConstants.BC_WQ_File );

      final WPSRequest wpsRequest = new WPSRequest( PreRMAKalypso.ID, WPSRequest.SERVICE_LOCAL, 60 * 60 * 1000 );
      final IStatus statusPreRMAKalypso = wpsRequest.run( inputs, outputs, progressMonitor );
      if( !statusPreRMAKalypso.isOK() )
      {
        throw new CoreException( statusPreRMAKalypso );
      }

      manager = VFSUtilities.getNewManager();

      final Map<String, ComplexValueReference> references = wpsRequest.getReferences();
      final ComplexValueReference modelFileReference = references.get( ISimulation1D2DConstants.MODEL_2D );
      final String modelFileUrl = modelFileReference.getReference();
      final FileObject modelFile = manager.resolveFile( modelFileUrl );

      final ComplexValueReference controlFileReference = references.get( ISimulation1D2DConstants.R10_File );
      final String controlFileUrl = controlFileReference.getReference();
      final FileObject controlFile = manager.resolveFile( controlFileUrl );

      final ComplexValueReference buildingFileReference = references.get( ISimulation1D2DConstants.BUILDING_File );
      final String buildingFileUrl = buildingFileReference.getReference();
      final FileObject buildingFile = manager.resolveFile( buildingFileUrl );

      final ComplexValueReference bcwqFileReference = references.get( ISimulation1D2DConstants.BC_WQ_File );
      final String bcwqFileUrl = bcwqFileReference.getReference();
      final FileObject bcwqFile = manager.resolveFile( bcwqFileUrl );

      // TODO: specific error message if exe was not found
      final File exeFile = findRma10skExe( controlModel );
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
        tempDirName = (String) inputProvider.getInputForID( INPUT_WORKING_DIR );
      else
        tempDirName = tmpdir.getName();

      final IProcess process = KalypsoCommonsExtensions.createProcess( processFactoryId, tempDirName, executableName );

      // add sandbox dir to results for monitoring (empty at this time)
      final String sandboxDirectory = process.getSandboxDirectory();

      try
      {
        resultEater.addResult( OUTPUT_RESULTS, new URI( sandboxDirectory ) ); //$NON-NLS-1$
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
      final FileObject workingDir = manager.resolveFile( sandboxDirectory );
      VFSUtilities.copyFileTo( executableFile, workingDir );
      VFSUtilities.copyFileTo( modelFile, workingDir );
      VFSUtilities.copyFileTo( controlFile, workingDir );
      VFSUtilities.copyFileTo( buildingFile, workingDir );
      VFSUtilities.copyFileTo( bcwqFile, workingDir );

      final File stdoutFile = new File( tmpdir, "exe.log" ); //$NON-NLS-1$
      final File stderrFile = new File( tmpdir, "exe.err" ); //$NON-NLS-1$

      logOS = new BufferedOutputStream( new FileOutputStream( stdoutFile ) );
      errorOS = new BufferedOutputStream( new FileOutputStream( stderrFile ) );

      // check if user cancelled
      if( monitor.isCanceled() )
      {
        throw new OperationCanceledException();
      }

      // Run the Calculation
      final int ncyc = controlModel.getNCYC();
      final SubMonitor progress = SubMonitor.convert( progressMonitor, ncyc );
      progress.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.15" ) ); //$NON-NLS-1$
      m_log.formatLog( IStatus.INFO, ISimulation1D2DConstants.CODE_RUNNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.0" ) + ": " + executableName ); //$NON-NLS-1$ //$NON-NLS-2$
      process.startProcess( logOS, errorOS, null, progressCancelable );

      // decide based on ERROR.OUT if simulation was successful
      final FileObject errorFile = workingDir.resolveFile( "ERROR.OUT" ); //$NON-NLS-1$
      if( errorFile == null || !errorFile.exists() || errorFile.getContent().getSize() == 0 )
      {
        /* Successfully finished simulation */
        progressMonitor.done( StatusUtilities.createOkStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.20" ) ) ); //$NON-NLS-1$
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
    catch( final IOException e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMAKalypsoSimulation.3" ), e ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMAKalypsoSimulation.2" ), e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );

      if( manager != null )
        manager.close();
    }
  }

  private File findRma10skExe( final IControlModel1D2D controlModel ) throws CoreException
  {
    final String version = controlModel.getVersion();
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

    final String exeMissingMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation.26", exeFile.getAbsolutePath() ); //$NON-NLS-1$
    throw new CoreException( StatusUtilities.createErrorStatus( exeMissingMsg ) );
  }
}
