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
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

/**
 * 
 * @author doemming
 */
public class ZipDiffObject extends AbstractDiffObject
{

  private final ZipFile m_zipFile;

  private final Hashtable m_pathes = new Hashtable();

  /**
   * @throws IOException
   * @throws ZipException
   *  
   */
  public ZipDiffObject( File zipFile ) throws ZipException, IOException
  {
    m_zipFile = new ZipFile( zipFile );
    final Enumeration enumeration = m_zipFile.entries();
    while( enumeration.hasMoreElements() )
    {
      final ZipEntry entry = (ZipEntry)enumeration.nextElement();
      String key = entry.getName().replaceAll( "\\\\", "/" ).replaceFirst( "^/", "" );
      m_pathes.put( key, entry );
    }
  }

  /**
   * 
   * @see org.kalypso.commons.diff.IDiffObject#exists(java.lang.String)
   */
  public boolean exists( String path )
  {
    return m_pathes.containsKey( path );
  }

  /**
   * 
   * @see org.kalypso.commons.diff.IDiffObject#getDiffComparator(java.lang.String)
   */
  public IDiffComparator getDiffComparator( final String path )
  {
    final ZipEntry entry = getEntry( path );

    if( entry.isDirectory() )
      return new IDiffComparator()
      {
        /**
         * 
         * @see org.kalypso.commons.diff.IDiffComparator#diff(org.kalypso.commons.diff.IDiffLogger, java.lang.Object,
         *      java.lang.Object)
         */
        public boolean diff( IDiffLogger logger, Object content, Object content2 )
        {
          logger.log( IDiffComparator.DIFF_OK, path );
          return false;
        }
      };
    return super.getDiffComparator( path );
  }

  /**
   * 
   * @param path
   * @return entry from path
   */
  private ZipEntry getEntry( String path )
  {
    return (ZipEntry)m_pathes.get( path );
  }

  /**
   * 
   * @see org.kalypso.commons.diff.IDiffObject#getContent(java.lang.String)
   */
  public Object getContent( String path ) throws IOException
  {
    final ZipEntry entry = getEntry( path );
    if( entry.isDirectory() )
      return entry;
    return m_zipFile.getInputStream( entry );
  }

  /**
   * @see org.kalypso.commons.diff.IDiffObject#getPathes()
   */
  public String[] getPathes()
  {
    return (String[])m_pathes.keySet().toArray( new String[m_pathes.size()] );
  }

}
