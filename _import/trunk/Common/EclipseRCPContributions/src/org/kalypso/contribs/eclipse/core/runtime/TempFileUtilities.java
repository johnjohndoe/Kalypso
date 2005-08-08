/*--------------- Kalypso-Header ------------------------------------------

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

--------------------------------------------------------------------------*/

package org.kalypso.contribs.eclipse.core.runtime;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Plugin;

/**
 * Utilities for temp files stored within work directories of plugins
 *
 * @author schlienger
 */
public class TempFileUtilities
{
  private TempFileUtilities()
  {
    // not intended to be instanciated
  }

  /**
   * Create a temp file in the subDirName of the plugin's state location (where files can be created, deleted, etc.).
   * Uses File.createTempFile() so as written in the File javadoc, you should call .deleteOnExit() on the returned file
   * instance to make it a real 'temp' file.
   */
  public static File createTempFile( final Plugin plugin, final String subDirName, String prefix, final String suffix ) throws IOException
  {
    if( prefix.length() < 3 )
      prefix += "___";
    
    final IPath path = plugin.getStateLocation();
    final File dir = new File( path.toFile(), subDirName );
    if( !dir.exists() )
      dir.mkdir();

    final File file = File.createTempFile( prefix, suffix, dir );
    return file;
  }

  /**
   * Deletes the given subDir of this plugin's state location. This method can be called when the plugin starts for
   * instance, in order to clear non-deleted temp files.
   */
  public static void deleteTempDir( final Plugin plugin, final String subDirName )
  {
    final IPath path = plugin.getStateLocation();
    final File dir = new File( path.toFile(), subDirName );
    if( dir.exists() )
      cleanDirectory( dir );
  }
  
  private static void cleanDirectory( final File dir )
  {
    final File[] files = dir.listFiles();
    for( int i = 0; i < files.length; i++ )
    {
      if( files[i].isDirectory() )
        cleanDirectory( files[i] );
      else
        files[i].delete();
    }
    
    dir.delete();
  }
}
