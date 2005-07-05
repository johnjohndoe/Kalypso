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
package org.kalypso.commons.java.util.zip;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.ZipOutputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;

/**
 * A {@link org.eclipse.core.resources.IResourceVisitor}which puts all visited resources in a single zip file.
 * 
 * @author belger
 */
public class ZipResourceVisitor implements IResourceVisitor
{
  private final ZipOutputStream m_zos;

  private final Set m_entries = new HashSet();

  public ZipResourceVisitor( final File zipfile ) throws FileNotFoundException
  {
    m_zos = new ZipOutputStream( new BufferedOutputStream( new FileOutputStream( zipfile ) ) );
  }

  /**
   * Must be called, to close Zip-File
   * 
   * @throws IOException
   */
  public void close() throws IOException
  {
    m_zos.close();
  }

  /**
   * @see org.eclipse.core.resources.IResourceVisitor#visit(org.eclipse.core.resources.IResource)
   */
  public boolean visit( final IResource resource ) throws CoreException
  {
    if( resource.getType() == IResource.FILE )
    {
      final IFile file = (IFile)resource;
      final String relativePathTo = file.getProjectRelativePath().toString();

      try
      {
        if( !m_entries.contains( relativePathTo ) )
        {
          m_entries.add( relativePathTo );
          ZipUtilities.writeZipEntry( m_zos, file.getLocation().toFile(), relativePathTo );
        }
      }
      catch( final IOException e )
      {
        final IStatus status = new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, "Datei wurde nicht in das ZIP Archiv geschrieben: " + relativePathTo, e );
        throw new CoreException( status );
      }
    }

    return true;
  }
}