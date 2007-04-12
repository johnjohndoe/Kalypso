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

import java.util.Stack;
import java.util.logging.Level;

import org.kalypso.contribs.java.util.logging.ILogger;

/**
 * @author doemming
 */
public class DiffLogger implements IDiffLogger
{

  private final ILogger m_logger;

  private Stack m_buffers = new Stack();

  public DiffLogger( ILogger logger )
  {
    m_logger = logger;
  }

  /**
   * @see org.kalypso.commons.diff.IDiffLogger#log(int, java.lang.String)
   */
  public void log( int status, String message )
  {
    final String diff;
    switch( status )
    {
      case IDiffComparator.DIFF_CONTENT:
        diff = "<-> ";
        break;
      case IDiffComparator.DIFF_REMOVED:
        diff = "--- ";
        break;
      case IDiffComparator.DIFF_ADDED:
        diff = "+++ ";
        break;
      case IDiffComparator.DIFF_UNCOMPAREABLE:
        diff = "??? ";
        break;
      //    case IDiffComparator.DIFF_INFO:
      case IDiffComparator.DIFF_OK:
        return;
      //        case IDiffComparator.DIFF_OK:
      //          diff = "=== ";
      //          break;
      case IDiffComparator.DIFF_INFO:
        diff = "# ";
        break;
      default:
        diff = "unknown ";
        break;
    }
    int offset = m_buffers.size();
    StringBuffer tab = new StringBuffer();
    for( int i = 0; i < offset; i++ )
      tab.append( " " );
    innerLog( Level.INFO, -1, tab.toString() + diff + message );
  }

  public void innerLog( final Level level, final int msgCode, final String message )
  {
    if( m_buffers.isEmpty() )
      m_logger.log( level, msgCode, message );
    else
    {
      final StringBuffer buffer = (StringBuffer)m_buffers.peek();
      if( buffer.length() > 0 )
        buffer.append( "\n" );
      buffer.append( message );
    }
  }

  /**
   * @see org.kalypso.commons.diff.IDiffLogger#block()
   */
  public void block()
  {
    StringBuffer stringBuffer = new StringBuffer();
    m_buffers.push( stringBuffer );
  }

  /**
   * @see org.kalypso.commons.diff.IDiffLogger#unblock(boolean)
   */
  public void unblock( boolean keepLastLog )
  {
    final StringBuffer buffer = (StringBuffer)m_buffers.pop();
    if( keepLastLog )
      innerLog( Level.INFO, -1, buffer.toString() );
  }
}