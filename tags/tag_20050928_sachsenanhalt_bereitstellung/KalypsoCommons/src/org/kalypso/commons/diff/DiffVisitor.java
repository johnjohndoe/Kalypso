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

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * 
 * 
 * @author doemming
 */
public class DiffVisitor implements IDiffVisitor
{

  private final IDiffObject m_base;

  private Hashtable m_log = new Hashtable();

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
   * @see org.kalypso.commons.diff.IDiffVisitor#diff(org.kalypso.commons.diff.IDiffLogger, java.lang.String,
   *      org.kalypso.commons.diff.IDiffObject)
   */
  public boolean diff( final IDiffLogger logger, final String path, final IDiffObject other ) throws Exception
  {
    if( !other.exists( path ) )
    {
      logger.log( IDiffComparator.DIFF_REMOVED, path );
      return false;
    }
    // diff Content
    final IDiffComparator differ = m_base.getDiffComparator( path );
    final Object content1 = m_base.getContent( path );
    final Object content2 = other.getContent( path );
    if( !content1.getClass().equals( content2.getClass() ) )
    {
      logger.log( IDiffComparator.DIFF_UNCOMPAREABLE, path );
      return false;
    }
    logger.block();
    logger.log( IDiffComparator.DIFF_INFO, path );
    boolean result = differ.diff( logger, content1, content2 );
    logger.unblock( result );
    return result;
  }
}
