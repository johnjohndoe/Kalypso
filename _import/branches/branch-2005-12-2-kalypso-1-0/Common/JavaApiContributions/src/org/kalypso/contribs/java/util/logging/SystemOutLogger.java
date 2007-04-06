/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.contribs.java.util.logging;

import java.util.logging.Level;

/**
 * Simple implementation printing everything to System.out in the log-file-style (LEVEL: warning).
 * 
 * @author Belger
 */
public class SystemOutLogger implements ILogger
{
  /**
   * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, boolean, java.lang.String)
   */
  public void log( final Level level, final boolean mainMsg, final String message )
  {
    final String outString = formatLogStylish( level, mainMsg, message );
    System.out.print( outString );
  }

  /**
   * Formats the log-message like java.util.logging.Logger's do it normally.
   */
  public static String formatLogStylish( final Level level, final boolean mainMsg, final String message )
  {
    final StringBuffer sb = new StringBuffer( level.toString() );
    sb.append( ": " );
    // TODO: change this to a real number?
    if( mainMsg )
      sb.append( '*' );
    sb.append( message );

    final String outString = sb.toString();
    return outString;
  }

}
