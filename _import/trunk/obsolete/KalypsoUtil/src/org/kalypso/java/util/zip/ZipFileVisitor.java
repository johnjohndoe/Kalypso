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
package org.kalypso.java.util.zip;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipOutputStream;

import org.kalypso.java.io.FileVisitor;

/**
 * <p>
 * Zips all visited files into an archive.
 * </p>
 * 
 * <p>
 * Die Namen der Zip entries (=Pfade im ZIP) können durch ein Pattern-Replace
 * aus den Dateinamen erzeugt werden
 * </p>
 * 
 * @author belger
 */
public class ZipFileVisitor implements FileVisitor
{
  private final ZipOutputStream m_zos;

  private String m_basePattern;

  private String m_baseReplace;

  public ZipFileVisitor( final File zipFile ) throws FileNotFoundException
  {
    m_zos = new ZipOutputStream( new BufferedOutputStream( new FileOutputStream( zipFile ) ) );
  }

  public ZipFileVisitor( final ZipOutputStream zos )
  {
    m_zos = zos;
  }

  public void close() throws IOException
  {
    m_zos.close();
  }

  public void setBasePattern( final String basePattern )
  {
    // Sonderfall abfangen, dass ein \E im basePattern ist
    final String hackpattern = basePattern.replaceAll( "\\\\E", "\\\\E\\\\\\\\E\\\\Q" );

    // quote the regexp
    m_basePattern = "\\Q" + hackpattern + "\\E";
  }

  public void setBaseReplace( final String baseReplace )
  {
    m_baseReplace = baseReplace;
  }

  /**
   * @throws IOException
   * 
   * @see org.kalypso.java.io.FileVisitor#visit(java.io.File)
   */
  public boolean visit( final File file ) throws IOException
  {
    if( file.isFile() )
    {
      final String absolutePath = file.getAbsolutePath();

      final String relPath = absolutePath.replaceFirst( m_basePattern, m_baseReplace );

      ZipUtilities.writeZipEntry( m_zos, file, relPath );
    }

    return true;
  }
}
