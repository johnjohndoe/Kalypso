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
import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.kalypso.contribs.java.io.StreamUtilities;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class DiffVisitor implements IDiffVisitor
{

  private final IDiffObject m_base;

  private Hashtable m_log = new Hashtable();

  public static final Integer DIFF_REMOVED = new Integer( 1 );

  public static final Integer DIFF_UNCOMPAREABLE = new Integer( 2 );

  public static final Integer DIFF_CONTENT = new Integer( 3 );

  public static final Integer DIFF_OK = new Integer( 5 );

  public static final Integer DIFF_ADDED = new Integer( 6 );

  /**
   *  
   */
  public DiffVisitor( final IDiffObject base )
  {
    m_base = base;
  }

  public void addLog( Integer status, String path )
  {
    if( !m_log.containsKey( status ) )
      m_log.put( status, new ArrayList() );
    ( (List)m_log.get( status ) ).add( path );
  }

  /**
   * 
   * @see org.kalypso.commons.diff.IDiffVisitor#diff(java.lang.String, org.kalypso.commons.diff.IDiffObject)
   */
  public boolean diff( final String path, final IDiffObject other ) throws IOException
  {
    if( !other.exists( path ) )
    {
      addLog( DIFF_REMOVED, path );
      return false;
    }
    // diff Content
    final Class contentClass = m_base.getContentClass( path );
    if( !contentClass.equals( other.getContentClass( path ) ) )
    {
      addLog( DIFF_UNCOMPAREABLE, path );
      return false;
    }
    final IDiffComparator differ = getDiffComparator( contentClass );
    final Integer status = differ.diff( m_base.getContent( path ), other.getContent( path ) );
    addLog( status, path );
    return status == DIFF_OK;
  }

  /**
   * 
   * @param contentClass
   */
  private IDiffComparator getDiffComparator( Class contentClass )
  {
    if( contentClass.equals( File.class ) )
      return new IDiffComparator()
      {
        /**
         * @see org.kalypso.commons.diff.IDiffComparator#diff(java.lang.Object, java.lang.Object)
         */
        public Integer diff( Object content, Object content2 )
        {
          return DIFF_OK;
        }
      };
    if( contentClass.equals( InputStream.class ) )
      return new IDiffComparator()
      {
        /**
         * @throws IOException
         * @see org.kalypso.commons.diff.IDiffComparator#diff(java.lang.Object, java.lang.Object)
         */
        public Integer diff( Object content, Object content2 ) throws IOException
        {
          final InputStream c1 = (InputStream)content;
          final InputStream c2 = (InputStream)content2;
          if( !StreamUtilities.isEqual( c1, c2 ) )
            return DIFF_CONTENT;
          return DIFF_OK;
        }
      };
    return null;
  }

  /**
   * 
   * @see org.kalypso.commons.diff.IDiffVisitor#printLog(java.io.PrintStream, java.lang.String, java.lang.Integer)
   */
  public void printLog( PrintStream out, String prefix, Integer status )
  {
    if( !m_log.containsKey( status ) )
      return;
    final List list = (List)m_log.get( status );
    for( Iterator iter = list.iterator(); iter.hasNext(); )
    {
      String path = (String)iter.next();
      out.print( prefix );
      out.println( path );
    }
  }

  /**
   * @see org.kalypso.commons.diff.IDiffVisitor#printLog(java.io.PrintStream)
   */
  public void printLog( PrintStream out )
  {
    printLog( System.out, "<-> ", DiffVisitor.DIFF_CONTENT );
    printLog( System.out, "--- ", DiffVisitor.DIFF_REMOVED );
    printLog( System.out, "<!> ", DiffVisitor.DIFF_UNCOMPAREABLE );
    //    printLog( System.out, "=== ", DiffVisitor.DIFF_OK );
    printLog( System.out, "+++ ", DiffVisitor.DIFF_ADDED );
  }

  /**
   * @see org.kalypso.commons.diff.IDiffVisitor#hasDiffs()
   */
  public boolean hasDiffs()
  {
    return m_log.containsKey( DIFF_CONTENT ) || m_log.containsKey( DIFF_ADDED ) || m_log.containsKey( DIFF_REMOVED )
        || m_log.containsKey( DIFF_UNCOMPAREABLE );
  }
}
