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
package org.kalypso.commons.java.io;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.kalypso.contribs.java.io.FileVisitor;

/**
 * Copiert eine Datei und zwar relativ zu den angegebenen 'fromDir' und 'toDir'
 * 
 * @author belger
 */
public class FileCopyVisitor implements FileVisitor
{
  private final File m_toDir;

  private final File m_fromDir;

  /** if true, target file is always replaced by source file */
  private final boolean m_overwrite;

  private final String m_excludeDirWithFile;

  public FileCopyVisitor( final File fromDir, final File toDir, final boolean overwrite )
  {
    this( fromDir, toDir, overwrite, null );
  }

  /**
   * @param overwrite Zieldatei immer überschreiben, auch wenn sie schon existiert
   */
  public FileCopyVisitor( final File fromDir, final File toDir, final boolean overwrite,
      final String excludeDirWithFile )
  {
    m_fromDir = fromDir;
    m_toDir = toDir;
    m_overwrite = overwrite;
    m_excludeDirWithFile = excludeDirWithFile;
  }

  /**
   * @see org.kalypso.contribs.java.io.FileVisitor#visit(java.io.File)
   */
  public boolean visit( final File file ) throws IOException
  {
    final String relativePathTo = FileUtilities.getRelativePathTo( m_fromDir, file );
    if( relativePathTo != null )
    {
      final File targetFile = new File( m_toDir, relativePathTo );

      final boolean isDir = file.isDirectory();
      
      // falls es ein Verzeichnis ist und das Auschlussfile enthält, hier abbrechen
      if( m_excludeDirWithFile != null && isDir )
      {
        final File excludeFile = new File( file, m_excludeDirWithFile );
        if( excludeFile.exists() )
          return false;
      }

      if( isDir )
        targetFile.mkdir();
      else
      {
        // falls die Zieldatei neuer ist und das überschreiben neuerer verboten wurde
        // einfach abbrechen
//        if( !m_overwrite && targetFile.exists() )
        if( targetFile.exists() )
        {
          // falls neuer überschreiben oder nicht?
          final long targetLastModified = targetFile.lastModified();
          final long lastModified = file.lastModified();
          if( !m_overwrite && targetLastModified > lastModified )
            return false;

          // falls die Dateien wirklich gleich sind, nichts tun
          if( targetLastModified == lastModified /*&& targetFile.length() == file.length()*/ )
            return false;
        }

        // sonst kopieren
        FileUtils.copyFile( file, targetFile );

        return false;
      }
    }

    return true;
  }
}
