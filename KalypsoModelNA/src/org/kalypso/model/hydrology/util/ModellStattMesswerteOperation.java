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
package org.kalypso.model.hydrology.util;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.ogc.sensor.util.ZmlLink;

/**
 * @author Gernot Belger
 *
 */
public class ModellStattMesswerteOperation implements ICoreRunnableWithProgress
{
  private final Node m_node;

  private final URL m_context;

  public ModellStattMesswerteOperation( final Node node, final URL context )
  {
    m_node = node;
    m_context = context;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    try
    {
      if( !m_node.isUseResultAsInflow() )
        return Status.OK_STATUS;

      final ZmlLink resultLink = new ZmlLink( m_node.getResultLink(), m_context );
      final ZmlLink resultAsInflowLink = new ZmlLink( m_node.getResultAsInflowLink(), m_context );

      // FIXME: for the moment, we just copy the result...
      final URL resultLocation = resultLink.getExistingLocation();
      final URL inflowLocation = resultAsInflowLink.getLocation();
      if( resultLocation == null || inflowLocation == null )
        return Status.OK_STATUS;

      final File resultJavaFile = asFile( resultLocation );
      final File inflowJavaFile = asFile( inflowLocation );

      if( resultJavaFile == null || inflowJavaFile == null )
        return Status.OK_STATUS;

      FileUtils.copyFile( resultJavaFile, inflowJavaFile );

      final IFile inflowFile = ResourceUtilities.findFileFromURL( inflowLocation );
      if( inflowFile == null )
        return Status.OK_STATUS;

      inflowFile.refreshLocal( IResource.DEPTH_ONE, monitor );

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
  }

  private File asFile( final URL location )
  {
    final File file = FileUtils.toFile( location );
    if( file != null )
      return file;

    final IFile resultFile = ResourceUtilities.findFileFromURL( location );
    if( resultFile == null )
      return null;

    final IPath path = resultFile.getLocation();
    if( path == null )
      return null;

    return path.toFile();
  }
}

