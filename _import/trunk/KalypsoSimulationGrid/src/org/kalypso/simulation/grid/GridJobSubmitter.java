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
package org.kalypso.simulation.grid;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileContent;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManager;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.KalypsoCommonsExtensions;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.ProcessTimeoutException;
import org.kalypso.commons.xml.NS;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.NullSimulationMonitor;
import org.kalypso.simulation.core.SimulationException;

import uk.ac.dl.escience.vfs.util.VFSUtil;

/**
 * Submits a Gaja3d job to the grid using ISimulation inputs/outputs
 * 
 * @author skurzbach
 */
public class GridJobSubmitter
{

  public static QName QNAME_ANY_URI = new QName( NS.XSD_SCHEMA, "anyURI" );

  private Map<URI, String> m_externalInputs = new HashMap<URI, String>();

  public void submitJob( final FileObject workingDir, final String executable, ISimulationMonitor monitor, String... arguments ) throws SimulationException
  {
    if( monitor == null )
    {
      monitor = new NullSimulationMonitor();
    }

    // prepare streams
    FileObject stdoutFile = null;
    FileObject stderrFile = null;
    int returnCode = IStatus.ERROR;
    try
    {
      // stream stdout and stderr to files
      stdoutFile = workingDir.resolveFile( "stdout" );
      stderrFile = workingDir.resolveFile( "stderr" );

      // create process handle
      final String processFactoryId = "org.kalypso.simulation.gridprocess";
      // TODO: refactor so tempdir is created inside process
      final String tempDirName = workingDir.getName().getBaseName();
      final IProcess process = KalypsoCommonsExtensions.createProcess( processFactoryId, tempDirName, executable, arguments );
      // process.setProgressMonitor( new SimulationMonitorAdaptor( monitor ) );
      process.environment().put( "OMP_NUM_THREADS", "4" );

      final FileContent stdOutContent = stdoutFile.getContent();
      final OutputStream stdOut = stdOutContent.getOutputStream();
      final FileContent stdErrContent = stderrFile.getContent();
      final OutputStream stdErr = stdErrContent.getOutputStream();

      // stage-in files
      final FileSystemManager manager = workingDir.getFileSystem().getFileSystemManager();
      for( final URI einput : m_externalInputs.keySet() )
      {
        final FileObject inputFile = manager.resolveFile( getUriAsString( einput ) );
        final String destName = m_externalInputs.get( einput );
        if( destName != null )
        {
          final FileObject destFile = workingDir.resolveFile( destName );
          VFSUtil.copy( inputFile, destFile, null, true );
        }
        else
        {
          VFSUtil.copy( inputFile, workingDir, null, true );
        }
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
      // close files
      if( stdoutFile != null )
      {
        try
        {
          stdoutFile.getContent().close();
        }
        catch( final FileSystemException e )
        {
          // gobble
        }
      }
      if( stderrFile != null )
      {
        try
        {
          stderrFile.getContent().close();
        }
        catch( final FileSystemException e )
        {
          // gobble
        }
      }
    }

    // process failure handling
    if( returnCode != IStatus.OK )
    {
      String errString = "Process failed.";
      try
      {
        final FileContent content = stderrFile.getContent();
        final InputStream input2 = content.getInputStream();
        errString = errString + "\n" + IOUtils.toString( input2 );
        content.close();
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

  public void addExternalInput( URI input, String destName )
  {
    m_externalInputs.put( input, destName );
  }
}
