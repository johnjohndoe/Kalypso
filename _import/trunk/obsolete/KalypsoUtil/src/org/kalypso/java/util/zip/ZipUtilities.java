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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipInputStream;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;

/**
 * @author belger
 */
public class ZipUtilities
{
  private ZipUtilities()
  {
  // wird nicht instantiiert
  }

  public static void unzip( final File zip, final File targetdir ) throws ZipException, IOException
  {
    FileInputStream zipIS = null;
    try
    {
      zipIS = new FileInputStream( zip );
      unzip( zipIS, targetdir );
    }
    finally
    {
      IOUtils.closeQuietly( zipIS );
    }
  }

  public static void unzip( final InputStream inputStream, final File targetdir )
      throws IOException
  {
    final ZipInputStream zis = new ZipInputStream( inputStream );
    while( true )
    {
      final ZipEntry entry = zis.getNextEntry();
      if( entry == null )
        break;
      
      final File newfile = new File( targetdir, entry.getName() );
      if( entry.isDirectory() )
        newfile.mkdirs();
      else
      {
        if( !newfile.getParentFile().exists() )
          newfile.getParentFile().mkdirs();

        FileOutputStream os = null;
        try
        {
          os = new FileOutputStream( newfile );
          CopyUtils.copy( zis, os );
        }
        finally
        {
          IOUtils.closeQuietly( os );
          zis.closeEntry();
        }
      }
    }
  }
}