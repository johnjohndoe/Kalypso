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
package org.kalypso.kalypsomodel1d2d.conv.results.test;

import java.io.File;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.apache.commons.vfs.FileObject;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.Test;
import org.kalypso.commons.io.FileSystemManagerWrapper;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;

/**
 * @author Thomas Jung
 * 
 */
public class NodeResultsHandler2DTest
{
  @Test
  public void testLoadResults( ) throws Exception
  {
    final File tempDir = FileUtilities.createNewTempDir( "result2dtest" );
    tempDir.mkdirs();

    final FileSystemManagerWrapper manager = VFSUtilities.getNewManager();
    try
    {
      final ILog log = KalypsoModel1D2DPlugin.getDefault().getLog();
      log.log( StatusUtilities.createStatus( IStatus.INFO, "Start Result Processing Test (2D only)", null ) );

      final URL zipLocation = getClass().getResource( "resources/original.2d.zip" );
      ZipUtilities.unzip( zipLocation, tempDir );

      // get 2d-file from resources
      final File result2dFile = new File( tempDir, "A0001.2d" );
      final FileObject resultFileObject = manager.toFileObject( result2dFile );
      final File outputDir = new File( tempDir, "output" );
      outputDir.mkdir();

      log.log( StatusUtilities.createStatus( IStatus.INFO, "calling ProcessResultsJob", null ) );
      final ProcessResultsJob job = new ProcessResultsJob( resultFileObject, outputDir, null, null, null, null, ResultManager.STEADY_DATE, null );
      final IStatus result = job.run( new NullProgressMonitor() );
      log.log( result );
    }
    finally
    {
      FileUtils.forceDelete( tempDir );
      manager.close();
      System.gc();
      System.gc();
      System.gc();
      System.out.println( "Total memory" + Runtime.getRuntime().totalMemory() );
    }
  }
}
