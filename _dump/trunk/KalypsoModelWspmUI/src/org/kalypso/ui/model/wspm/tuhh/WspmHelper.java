/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.model.wspm.tuhh;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.java.util.zip.ZipUtilities;

/**
 * @author thuel2
 */
public class WspmHelper
{
  private WspmHelper( )
  {
    // will not be instantiated
  }

  /**
   * Ensures that the given container holds a valid wspm-tuhh modell.
   * <p>
   * If it is not the case, it creates the structure
   * </p>
   */
  public static void ensureValidWspmTuhhStructure( final IContainer wspmContainer, final IProgressMonitor monitor ) throws IOException, CoreException
  {
    monitor.beginTask( "Validiere Modellstruktur", 1000 );

    InputStream zipInputStream = null;
    try
    {
      if( !wspmContainer.exists() )
      {
        if( wspmContainer instanceof IFolder )
        {
          monitor.subTask( "Verzeichnis wird erzeugt" );
          ((IFolder) wspmContainer).create( false, true, new SubProgressMonitor( monitor, 100 ) );
        }
        else if( wspmContainer instanceof IProject )
        {
          monitor.subTask( "Projekt wird angelegt" );
          final IProject project = (IProject) wspmContainer;
          (project).create( new SubProgressMonitor( monitor, 50 ) );
          monitor.subTask( "Projekt wird geöffnet" );
          project.open( new SubProgressMonitor( monitor, 50 ) );
        }
      }

      monitor.subTask( "Validiere Modellstruktur" );
      final File containerDir = wspmContainer.getLocation().toFile();
      zipInputStream = WspmHelper.class.getResourceAsStream( "resources/wspmTuhh_template.zip" );
      ZipUtilities.unzip( zipInputStream, containerDir, false );
      monitor.worked( 650 );
      wspmContainer.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 250 ) );

      // TODO: check if model.gml is valid xml with right namespace
    }
    finally
    {
      IOUtils.closeQuietly( zipInputStream );
      monitor.done();
    }

  }

}
