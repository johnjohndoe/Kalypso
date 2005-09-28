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
package org.kalypso.contribs.eclipse.core.resources;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.IPath;

/**
 * die Visitor sammelt alle IFile-Objekte, deren Location auf einen festgelegten FileFilter passen.
 * 
 * @author belger
 */
public class FileFilterVisitor implements IResourceVisitor
{
  private final FileFilter m_filter;

  private final Collection m_foundFiles = new ArrayList();

  public FileFilterVisitor( final FileFilter filter )
  {
    m_filter = filter;
  }

  public IFile[] getFiles()
  {
    return (IFile[])m_foundFiles.toArray( new IFile[m_foundFiles.size()] );
  }

  /**
   * @see org.eclipse.core.resources.IResourceVisitor#visit(org.eclipse.core.resources.IResource)
   */
  public boolean visit( final IResource resource )
  {
    if( resource instanceof IFile )
    {
      final IFile file = (IFile)resource;
      final IPath location = file.getLocation();
      final File fileFile = location.toFile();
      if( m_filter.accept( fileFile ) )
        m_foundFiles.add( file );
    }

    return true;
  }

}
