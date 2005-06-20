/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.tools;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class ZipUtil
{

  private StringBuffer details = null;

  /**
   * packs the passed files into a zip-archive, deletes the files if desired and returns the name of the archive
   * 
   * @param dirName
   *          name of the directory where to store the archive
   * @param archiveName
   *          desired archive name
   * @param fileNames
   *          names of the files to be packed into the zip archive
   */
  public String doZip( String dirName, String archiveName, String[] fileNames ) throws FileNotFoundException,
      IOException
  {
    Debug.debugMethodBegin();

    byte[] b = new byte[512];

    String archive = dirName + "/" + archiveName;
    ZipOutputStream zout = new ZipOutputStream( new FileOutputStream( archive ) );

    details = new StringBuffer();

    for( int i = 0; i < fileNames.length; i++ )
    {
      InputStream in = new FileInputStream( dirName + "/" + fileNames[i] );
      ZipEntry e = new ZipEntry( fileNames[i] );
      zout.putNextEntry( e );

      int len = 0;

      while( ( len = in.read( b ) ) != -1 )
      {
        zout.write( b, 0, len );
      }

      in.close();
      zout.closeEntry();
      details.append( createZipDetails( e ) + "\n" );
    }

    for( int i = 0; i < fileNames.length; i++ )
    {
      File file = new File( dirName + "/" + fileNames[i] );
      Debug.debugSimpleMessage( dirName + "/" + fileNames[i] + " deleted: " + file.delete() );
    }

    zout.close();

    Debug.debugMethodEnd();
    return archive;
  }

  /**
   * returns details about the zipping
   */
  public String getZipDetails()
  {
    return details.toString();
  }

  /**
   * returns some information about the zip process of the current <code>ZipEntry</code>.
   */
  private StringBuffer createZipDetails( ZipEntry e )
  {

    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer();

    sb.append( "added " + e.getName() );

    if( e.getMethod() == ZipEntry.DEFLATED )
    {
      long size = e.getSize();

      if( size > 0 )
      {
        long csize = e.getCompressedSize();
        long ratio = ( ( size - csize ) * 100 ) / size;
        sb.append( " (deflated " + ratio + "%)" );
      }
      else
      {
        sb.append( " (deflated 0%)" );
      }
    }
    else
    {
      sb.append( " (stored 0%)" );
    }

    Debug.debugMethodEnd();
    return sb;
  }

}