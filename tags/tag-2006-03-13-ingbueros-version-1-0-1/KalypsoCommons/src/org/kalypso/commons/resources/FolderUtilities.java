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
package org.kalypso.commons.resources;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;

/**
 * @author belger
 */
public class FolderUtilities
{
  /** Do not instantiate */
  private FolderUtilities()
  {
  // 
  }

  public static void mkdirs( final IContainer folder ) throws CoreException
  {
    if( folder == null || folder.exists() )
      return;

    if( !( folder instanceof IFolder ) )
      throw new CoreException( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, "Cannot mkdirs project or workspace", null ) );

    // create parents
    mkdirs( folder.getParent() );

    ( (IFolder)folder ).create( false, true, new NullProgressMonitor() );
  }

  public static IFolder createUnusedFolder( final IFolder parentFolder, final String prefix )
  {
    int i = 0;
    while( true )
    {
      final IFolder f = parentFolder.getFolder( prefix + i );
      if( !f.exists() )
        return f;

      i++;
    }
  }
}
