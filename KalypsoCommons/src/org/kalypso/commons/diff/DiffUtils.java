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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.zip.ZipException;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.io.FileUtilities;
import org.kalypso.contribs.java.io.StreamUtilities;
import org.kalypso.contribs.java.util.logging.ILogger;

/**
 * @author doemming
 */
public class DiffUtils
{
  /** Compares to single urls */
  public static boolean diffUrls( final ILogger logger, final URL file1, final URL file2 ) throws Exception
  {
    final IDiffLogger diffLogger = new DiffLogger( logger );

    final String extension = "." + FileUtilities.getSuffix( file1.getFile() );
    
    final IDiffComparator diffComp = getDiffComparatorFor( extension, file1.toString() );

    BufferedInputStream bis1 = null;
    BufferedInputStream bis2 = null;

    try
    {
      bis1 = new BufferedInputStream( file1.openStream() );
      bis2 = new BufferedInputStream( file2.openStream() );

      return diffComp.diff( diffLogger, bis1, bis2 );
    }
    finally
    {
      IOUtils.closeQuietly( bis1 );
      IOUtils.closeQuietly( bis2 );
    }
  }
  /** Compares to single files */
  public static boolean diffFiles( final ILogger logger, final File file1, final File file2 ) throws Exception
  {
    final IDiffLogger diffLogger = new DiffLogger( logger );

    final String extension = FileUtilities.getSuffix( "." + file1 );
    final IDiffComparator diffComp = getDiffComparatorFor( extension, file1.getName() );

    BufferedInputStream bis1 = null;
    BufferedInputStream bis2 = null;

    try
    {
      bis1 = new BufferedInputStream( new FileInputStream( file1 ) );
      bis2 = new BufferedInputStream( new FileInputStream( file2 ) );

      return diffComp.diff( diffLogger, bis1, bis2 );
    }
    finally
    {
      IOUtils.closeQuietly( bis1 );
      IOUtils.closeQuietly( bis2 );
    }
  }

  public static boolean diffZips( final ILogger logger, final File zip1, final File zip2, final String[] ignorePath )
      throws ZipException, IOException
  {
    boolean result = false;
    final IDiffLogger diffLogger = new DiffLogger( logger );

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

        if( !ignore( ignores, path ) )
          result |= visitor.diff( diffLogger, path, b );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    final String[] pathesB = b.getPathes();
    for( int i = 0; i < pathesB.length; i++ )
    {
      final String path = pathesB[i];
      if( !ignore( ignores, path ) && !a.exists( path ) )
      {
        diffLogger.log( IDiffComparator.DIFF_ADDED, path );
        result = true;
      }
    }
    return result;
  }

  /**
   * @param ignores
   * @param path
   * @return true if path should be ignored
   */
  private static boolean ignore( List ignores, String path )
  {
    if( ignores.contains( path ) )
      return true;
    // check for jokers '*'
    for( Iterator iter = ignores.iterator(); iter.hasNext(); )
    {
      final String ignorePath = (String)iter.next();
      if( ignorePath.startsWith( "*" ) ) // *foo
      {
        if( path.endsWith( ignorePath.substring( 1 ) ) )
          return true;
      }
      if( ignorePath.endsWith( "*" ) )
      {
        if( path.startsWith( ignorePath.substring( 0, ignorePath.length() - 1 ) ) )
          return true;
      }
    }
    return false;
  }

  /**
   * 
   * @param logger
   * @param list1
   * @param list2
   * @param infoMessage
   * @return boolean
   */
  public static boolean diffIgnoreOrder( IDiffLogger logger, List list1, List list2, String infoMessage )
  {
    logger.block();
    logger.log( IDiffComparator.DIFF_INFO, infoMessage );

    boolean result = false;
    for( Iterator iter = list1.iterator(); iter.hasNext(); )
    {
      final String element = (String)iter.next();
      if( list2.contains( element ) )
      {
        logger.log( IDiffComparator.DIFF_OK, element );
        list2.remove( element );
      }
      else
      {
        logger.log( IDiffComparator.DIFF_ADDED, element );
        result = true;
      }
    }
    for( Iterator iter = list2.iterator(); iter.hasNext(); )
    {
      final String element = (String)iter.next();
      logger.log( IDiffComparator.DIFF_REMOVED, element );
      result = true;
    }
    logger.unblock( result );
    return result;
  }

  /**
   * Search for a diff comparator with the given suffix. If the suffix is not registere yet, we return a diffcomparator
   * which just compares the streams bytewise.
   */
  final public static IDiffComparator getDiffComparatorFor( final String suffix, final String logMsg )
  {
    final DiffComparatorRegistry instance = DiffComparatorRegistry.getInstance();
    if( instance.hasComparator( suffix ) )
      return instance.getDiffComparator( suffix );
    return new IDiffComparator()
    {
      /**
       * @see org.kalypso.commons.diff.IDiffComparator#diff(org.kalypso.commons.diff.IDiffLogger, java.lang.Object,
       *      java.lang.Object)
       */
      public boolean diff( final IDiffLogger logger, final Object content, final Object content2 ) throws Exception
      {
        final InputStream c1 = (InputStream)content;
        final InputStream c2 = (InputStream)content2;
        final boolean hasDiff = !StreamUtilities.isEqual( c1, c2 );
        if( hasDiff )
          logger.log( IDiffComparator.DIFF_CONTENT, logMsg );
        else
          logger.log( IDiffComparator.DIFF_OK, logMsg );
        return hasDiff;
      }
    };
  }
}
