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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * FileUtilities
 * 
 * @author schlienger
 */
public class FileUtilities
{
  private FileUtilities()
  {
  // not intended to be instanciated
  }

  /**
   * Sets the contents of the dest file using the source file.
   */
  public static void copyFile( final String sourceCharset, final File source, final IFile dest,
      final IProgressMonitor monitor ) throws CoreException
  {
    final SetContentHelper helper = new SetContentHelper()
    {
      @Override
      protected void write( final OutputStreamWriter writer ) throws Throwable
      {
        final PrintWriter pwr = new PrintWriter( writer );
        final BufferedReader reader = new BufferedReader( new InputStreamReader( new FileInputStream( source ),
            sourceCharset ) );

        try
        {
          String strLine = reader.readLine();
          while( strLine != null )
          {
            pwr.println( strLine );

            strLine = reader.readLine();
          }
        }
        finally
        {
          reader.close();
          pwr.close();
        }
      }
    };

    helper.setFileContents( dest, false, false, monitor );
  }
}