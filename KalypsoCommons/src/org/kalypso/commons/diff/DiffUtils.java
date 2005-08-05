/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.commons.diff;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.zip.ZipException;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class DiffUtils
{

  /**
   *  
   */

  public DiffUtils()
  {
    super();
    // TODO Auto-generated constructor stub
  }

  /**
   * 
   * @param zip1
   * @param zip2
   * @return log
   * @throws IOException
   * @throws ZipException
   * @throws ZipException
   * @throws IOException
   */
  public static boolean diffZips( PrintStream writer, File zip1, File zip2, String[] ignorePath ) throws ZipException,
      IOException
  {
    final List ignores;
    if( ignorePath != null )
      ignores = Arrays.asList( ignorePath );
    else
      ignores = new ArrayList();
    final IDiffObject a = new ZipDiffObject( zip1 );
    final IDiffObject b = new ZipDiffObject( zip2 );
    final IDiffVisitor visitor = new DiffVisitor( a );
    final String[] pathesA = a.getPathes();
    for( int i = 0; i < pathesA.length; i++ )
    {
      final String path = pathesA[i];
      try
      {
        if( !ignores.contains( path ) )
          visitor.diff( path, b );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }
    final String[] pathesB = b.getPathes();
    for( int i = 0; i < pathesB.length; i++ )
    {
      final String path = pathesB[i];
      if( ( !ignores.contains( path ) ) && !a.exists( path ) )
        visitor.addLog( DiffVisitor.DIFF_ADDED, path );
    }

    if( writer != null )
      visitor.printLog( writer );
    return !visitor.hasDiffs();
  }
}
