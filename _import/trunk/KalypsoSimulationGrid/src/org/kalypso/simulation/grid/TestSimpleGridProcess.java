/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.simulation.grid;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManager;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.kalypso.commons.KalypsoCommonsExtensions;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.process.IProcess;
import org.osgi.framework.Bundle;

public class TestSimpleGridProcess extends TestCase
{

  public static final String GRID_SERVER_ROOT = "gridftp://gramd1.gridlab.uni-hannover.de";

  @Override
  protected void setUp( ) throws Exception
  {
    super.setUp();
  }

  @Override
  protected void tearDown( ) throws Exception
  {
    super.tearDown();
  }

  public void testGridProcess( ) throws Exception
  {
    final String processFactoryId = "org.kalypso.simulation.gridprocess";
    System.setProperty( "GLOBUS_LOCATION", "d:/workspace3.4/org.globus.ws.core" );
    final String sandboxRoot = "testRma";

    final FileSystemManagerWrapper manager = VFSUtilities.getNewManager();

    final String rmaName = "RMA10Sk_35";

    final IProcess process = KalypsoCommonsExtensions.createProcess( processFactoryId, sandboxRoot, rmaName );
    process.environment().put( "OMP_NUM_THREADS", "4" );

    final String sandboxDirectory = process.getSandboxDirectory();
    final FileObject workingDir = manager.resolveFile( sandboxDirectory );
    copyFileToWorking( manager, workingDir, rmaName );
    copyFileToWorking( manager, workingDir, "model.2d" );
    copyFileToWorking( manager, workingDir, "control.r10" );

    process.startProcess( System.out, System.err, null, null );

    final FileObject resultFile = workingDir.resolveFile( "A0001.2d" );
    if( !resultFile.exists() )
      fail( "Result was not created!" );

    manager.close();
  }

  private void copyFileToWorking( final FileSystemManager manager, final FileObject workingDir, final String rmaName ) throws IOException, FileSystemException, URISyntaxException
  {
    final Path path = new Path( rmaName );
    final Bundle bundle = Activator.getDefault().getBundle();
    final URL exeURL = FileLocator.toFileURL( FileLocator.find( bundle, path, null ) );
    final FileObject exeFile = manager.toFileObject( new File( exeURL.toURI() ) );
    VFSUtilities.copy( exeFile, workingDir );
  }

}
