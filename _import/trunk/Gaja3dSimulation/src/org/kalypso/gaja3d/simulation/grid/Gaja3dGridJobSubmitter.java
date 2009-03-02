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
package org.kalypso.gaja3d.simulation.grid;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystem;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.impl.StandardFileSystemManager;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.KalypsoCommonsExtensions;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.ProcessTimeoutException;
import org.kalypso.commons.xml.NS;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.NullSimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.simspec.DataType;
import org.kalypso.simulation.core.simspec.Modelspec;

import uk.ac.dl.escience.vfs.util.VFSUtil;

/**
 * Submits a Gaja3d job to the grid using ISimulation inputs/outputs
 * 
 * @author skurzbach
 */
public class Gaja3dGridJobSubmitter
{

  /**
   * AnyURI QName.
   */
  public static QName QNAME_ANY_URI = new QName( NS.XSD_SCHEMA, "anyURI" );

  /**
   * Name of the service. Will determine the name of the executable package (zip) and script (sh).
   */
  private static final URL EXEC_ZIP_URL = Gaja3dGridJobSubmitter.class.getResource( "Gaja3dService_linux64.zip" );

  private static final URL EXEC_SCRIPT_URL = Gaja3dGridJobSubmitter.class.getResource( "Gaja3dService_linux64.sh" );

  /**
   * The working directory of Gaja3d
   */
  static final String WORKING_DIR = ".";

  private List<String> m_outputs = new ArrayList<String>();

  public void submitJob( final Modelspec modelSpec, final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, ISimulationMonitor monitor, List<String> arguments ) throws SimulationException
  {
    // check if arguments are really available
    if( arguments == null )
    {
      arguments = new ArrayList<String>();
    }

    if( monitor == null )
    {
      monitor = new NullSimulationMonitor();
    }

    // always add workingDir argument
    arguments.add( "workingDir" );
    arguments.add( WORKING_DIR );

    // keep track of output names that were mentioned in the inputs
    // map from output id to output name
    final Map<String, String> outputNames = new HashMap<String, String>();

    // create list of inputs
    final List<URI> externalInputs = new ArrayList<URI>();
    try
    {
      externalInputs.add( EXEC_ZIP_URL.toURI() );
    }
    catch( final Exception e )
    {
      // could be null or not a valid uri
      throw new SimulationException( "Problem with executable.", e );
    }

    // convert inputs to command line arguments
    final List<DataType> input = modelSpec.getInput();
    for( final DataType data : input )
    {
      final String id = data.getId();
      if( !inputProvider.hasID( id ) )
      {
        if( data.isOptional() )
        {
          // ignore missing optional inputs
          continue;
        }
        else
        {
          throw new SimulationException( "Unexpected input with id " + id );
        }

      }
      final Object inputForID = inputProvider.getInputForID( id );
      if( id.startsWith( "_" ) )
      {
        // this input corresponds to an output and specifies the file
        // name of the output, do not include as input argument
        outputNames.put( id.substring( 1 ), (String) inputForID );
        continue;
      }

      // add key-value argument pair
      // id of input is the key
      arguments.add( id );

      // value depends on type of input
      if( inputForID instanceof URI )
      {
        // if it is a URI, stage in a file for given URI
        final URI inputURL = (URI) inputForID;
        externalInputs.add( inputURL );

        final String inputURLString = inputURL.toString();
        final String inputBaseName = inputURLString.substring( inputURLString.lastIndexOf( '/' ) + 1 );
        // add local file (on grid node) as argument
        arguments.add( inputBaseName );
      }
      else if( inputForID instanceof URL )
      {
        // if it is a URL, stage in a file for given URL
        final URL inputURL = (URL) inputForID;
        try
        {
          externalInputs.add( inputURL.toURI() );
        }
        catch( final URISyntaxException e )
        {
          throw new SimulationException( "The input URL is not a valid URI for staging.", e );
        }

        final String inputURLString = inputURL.toExternalForm();
        final String inputBaseName = inputURLString.substring( inputURLString.lastIndexOf( '/' ) + 1 );
        // add local file (on grid node) as argument
        arguments.add( inputBaseName );
      }
      else
      {
        // regular file or input
        final String inputString = inputForID.toString();
        // add the string representation of the input as an argument
        // this will at least work for strings and numeric types
        arguments.add( inputString );
      }
    }

    // stream stdout and stderr to files
    final File stdoutFile = new File( tmpdir, "stdout" );
    final File stderrFile = new File( tmpdir, "stderr" );

    // prepare streams
    OutputStream stdOut = null;
    OutputStream stdErr = null;

    int returnCode = IStatus.ERROR;
    StandardFileSystemManager manager = null;
    FileObject workingDir = null;
    try
    {
      // create working dir (sandbox)
      manager = VFSUtilities.getNewManager();
      final String sandboxRoot = tmpdir.getName();
      final String serverRoot = "gridftp://gramd1.gridlab.uni-hannover.de";

      final FileObject remoteRoot = manager.resolveFile( serverRoot );
      final FileSystem fileSystem = remoteRoot.getFileSystem();
      final String homeDirString = (String) fileSystem.getAttribute( "HOME_DIRECTORY" );
      final FileObject homeDir = remoteRoot.resolveFile( homeDirString );

      // create working dir if non-existent
      workingDir = homeDir.resolveFile( sandboxRoot );
      workingDir.createFolder();

      // create process handle
      final String processFactoryId = "org.kalypso.simulation.gridprocess";
      final IProcess process = KalypsoCommonsExtensions.createProcess( processFactoryId, workingDir, EXEC_SCRIPT_URL, arguments.toArray( new String[arguments.size()] ) );
      process.setProgressMonitor( new SimulationMonitorAdaptor( monitor ) );
      process.environment().put( "OMP_NUM_THREADS", "4" );

      // add required output files
      final List<DataType> output = modelSpec.getOutput();
      for( final DataType data : output )
      {
        final String id = data.getId();
        // only URI is supported for outputs at the time
        if( data.getType().equals( QNAME_ANY_URI ) )
        {
          // stage out file
          final String outputName = outputNames.get( id );
          final String destName;
          if( outputName != null )
          {
            destName = outputName;
          }
          else
          {
            destName = id;
          }
          m_outputs.add( destName );
          // final File outputLocation = new File( tmpdir, source );
          final FileObject destFile = workingDir.resolveFile( destName );
          final FileName destFileName = destFile.getName();
          try
          {
            final URI outputLocation = new URI( destFileName.getURI() );
            resultEater.addResult( id, outputLocation );
          }
          catch( final URISyntaxException e )
          {
            throw new SimulationException( destFileName + "is not a valid URI." );
          }
        }
        // TODO: support literal outputs
      }

      stdOut = new BufferedOutputStream( new FileOutputStream( stdoutFile ) );
      stdErr = new BufferedOutputStream( new FileOutputStream( stderrFile ) );

      // stage-in files
      for( final URI einput : externalInputs )
      {
        final FileObject inputFile = manager.resolveFile( getUriAsString( einput ) );
        VFSUtil.copy( inputFile, workingDir, null, true );
      }

      // start process
      returnCode = process.startProcess( stdOut, stdErr, null, null );
    }
    catch( final CoreException e )
    {
      // when process cannot be created
      throw new SimulationException( "Could not create process.", e );
    }
    catch( final ProcessTimeoutException e )
    {
      e.printStackTrace();
    }
    catch( final FileNotFoundException e )
    {
      // can only happen when files cannot be created in tmpdir
      throw new SimulationException( "Could not create temporary files for stdout and stderr.", e );
    }
    catch( final IOException e )
    {
      throw new SimulationException( "Process I/O error.", e );
    }
    finally
    {
      // close streams
      IOUtils.closeQuietly( stdOut );
      IOUtils.closeQuietly( stdErr );
      if( workingDir != null )
      {
        try
        {
          workingDir.close();
        }
        catch( final FileSystemException e )
        {
          // gobble
        }
      }

      if( manager != null )
      {
        manager.close();
      }

    }

    // process failure handling
    if( returnCode != IStatus.OK )
    {
      String errString = "Process failed.";
      try
      {
        final FileReader input2 = new FileReader( stderrFile );
        errString = errString + "\n" + IOUtils.toString( input2 );
        input2.close();
      }
      catch( final IOException e )
      {
        // ignore
      }
      monitor.setFinishInfo( returnCode, errString );
      throw new SimulationException( errString );
    }
    else
    {
      monitor.setFinishInfo( IStatus.OK, "Process finished successfully." );
    }
  }

// private void stageOut( final FileObject fromDir, final FileObject toDir ) throws IOException
// {
// final FileObject[] children = fromDir.getChildren();
// nextChild: for( FileObject child : children )
// {
// final FileName childName = child.getName();
// final String baseName = childName.getBaseName();
// for( final String output : m_outputs )
// {
// if( FilenameUtils.wildcardMatch( baseName, output ) )
// {
// final FileObject destination = toDir.resolveFile( baseName );
// if( FileType.FILE.equals( child.getType() ) )
// {
// /* Copy ... */
// VFSUtilities.copyFileTo( child, destination, true );
// }
// else if( FileType.FOLDER.equals( child.getType() ) )
// {
// /* Copy ... */
// Debug.println( "Copy directory " + childName + " to " + destination.getName() + " ..." );
// VFSUtilities.copyDirectoryToDirectory( child, destination, true );
// }
// else
// {
// Debug.println( "Could not determine the file type ..." );
// }
// continue nextChild;
// }
// }
// }
// }

  private String getUriAsString( final URI uri )
  {
    try
    {
      final URL url = FileLocator.toFileURL( uri.toURL() );
      return url.toExternalForm();
    }
    catch( final IOException e )
    {
      return uri.toString();
    }
  }
}
