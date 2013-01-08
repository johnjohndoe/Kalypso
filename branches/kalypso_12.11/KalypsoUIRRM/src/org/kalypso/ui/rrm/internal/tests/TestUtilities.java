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
package org.kalypso.ui.rrm.internal.tests;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.utils.log.GeoStatusLog;

/**
 * This class contains functions for the {@link MultiCatchmentModelTest} and {@link LinearSumCatchmentModelTest}.
 * 
 * @author Holger Albert
 */
public final class TestUtilities
{
  private TestUtilities( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * This function creates and opens the temporary project in the workspace of the unittest. If it already exists it
   * will be returned.
   * 
   * @param projectName
   *          The project name.
   * @return The temporary project.
   */
  public static IProject createProject( final String projectName ) throws CoreException
  {
    /* Get the workspace root. */
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    /* Get the project. */
    final IProject project = root.getProject( projectName );
    if( project.exists() )
      return project;

    /* Create the project. */
    project.create( new NullProgressMonitor() );

    /* Open the project. */
    project.open( new NullProgressMonitor() );

    return project;
  }

  /**
   * This function unzips the resources into the project.
   * 
   * @param path
   *          The path of the resources.
   * @param project
   *          The target project.
   */
  public static void unzipResources( final String path, final IProject project ) throws IOException, CoreException
  {
    /* The input stream. */
    InputStream inputStream = null;

    try
    {
      /* Get the input stream of the resources. */
      inputStream = TestUtilities.class.getResourceAsStream( path );

      /* Unzip them into the project. */
      ZipUtilities.unzip( inputStream, project.getLocation().toFile() );

      project.refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    finally
    {
      /* Close the input stream. */
      IOUtils.closeQuietly( inputStream );
    }
  }

  public static void saveLogQuietly( final IStatus status, final File targetDir )
  {
    try
    {
      /* Save the log as text file. */
      final GeoStatusLog log = new GeoStatusLog( new File( targetDir, "log.gml" ), KalypsoUIRRMPlugin.getID() ); //$NON-NLS-1$
      log.log( status );
      log.serialize();
    }
    catch( final Exception ex )
    {
      /* Ignore exception. */
      ex.printStackTrace();
    }
  }
}