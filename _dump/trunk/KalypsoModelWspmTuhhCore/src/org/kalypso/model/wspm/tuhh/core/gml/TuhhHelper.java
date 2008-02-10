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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializer;

public class TuhhHelper implements IWspmConstants, IWspmTuhhConstants
{
  private TuhhHelper( )
  {
    // never instantiate
  }

  /**
   * TODO: just name it createTuhh-Project
   * <p>
   * Ensures that the given container holds a valid wspm-tuhh modell.
   * </p>
   * <p>
   * If it is not the case, it creates the structure
   * </p>
   * 
   * @return the freshly created modell file
   */
  public static IFile ensureValidWspmTuhhStructure( final IContainer wspmContainer, final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( "Validierung der Modellstruktur", 2000 );

    InputStream zipInputStream = null;
    try
    {
      if( !wspmContainer.exists() )
      {
        if( wspmContainer instanceof IFolder )
        {
          monitor.subTask( " - erzeuge Verzeichnis " + wspmContainer.getName() );
          ((IFolder) wspmContainer).create( false, true, new SubProgressMonitor( monitor, 100 ) );
        }
        else
          monitor.worked( 100 );
        // else if( wspmContainer instanceof IProject )
        // {
        // monitor.subTask( "Projekt wird angelegt" );
        // final IProject project = (IProject) wspmContainer;
        // (project).create( new SubProgressMonitor( monitor, 50 ) );
        // monitor.subTask( "Projekt wird geöffnet" );
        // project.open( new SubProgressMonitor( monitor, 50 ) );
        // }
      }

      monitor.subTask( " - erzeuge Datenmodell" );

      final IFile targetFile = wspmContainer.getFile( new Path( "modell.gml" ) );
      // Add the tuhh namespace to the known namespaces, so it is later known for adding new feature
      final String[] additionalNamespaces = new String[] { IWspmTuhhConstants.NS_WSPM_TUHH };
      GmlSerializer.createGmlFile( new QName( IWspmConstants.NS_WSPMPROJ, "WspmProject" ), additionalNamespaces, targetFile, new SubProgressMonitor( monitor, 900 ), null );

      monitor.subTask( " - kopiere Modellstruktur" );
      final File containerDir = wspmContainer.getLocation().toFile();

      zipInputStream = TuhhHelper.class.getResourceAsStream( "resources/wspmTuhh_template.zip" );

      // REMARK: unzipping files with non UTF-8 filename encoding is really awesome in java.
      // We do use the apache tools, which let us at least set the encoding.
      // Try and error led to use the encoding: "IBM850" for WinZippes .zip's.

      // TODO: It still doesn´t work on each machine!!! @gernot: This is a test - I will remove it, if it doesn´t work
      // Jessica: The test also doesn´t work?!?

      // REMARK: make sure that the .zip was created with WinZIP/PKZIP instead of Eclipse-ZIP?
      ZipUtilities.unzip( zipInputStream, containerDir );
// ZipUtilities.unzipApache( zipInputStream, containerDir, false, "IBM850" );
      monitor.worked( 500 );
      wspmContainer.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 500 ) );

      return targetFile;
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      IOUtils.closeQuietly( zipInputStream );
      monitor.done();
    }

  }
}
