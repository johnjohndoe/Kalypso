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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.java.JavaApiContributionsPlugin;

/**
 * Utility class for {@link org.kalypso.contribs.java.util.logging.ILogger}stuff.
 * 
 * @author Gernot Belger
 */
public class LoggerUtilities
{
  public static final String PLUGIN_ID = JavaApiContributionsPlugin.getDefault().getBundle().getSymbolicName();

  //  private final static String LEVEL = "(" +
  //  Level.SEVERE + "|" +
  //  Level.WARNING + "|" +
  //  Level.INFO + "|" +
  //  Level.CONFIG + "|" +
  //  Level.FINE + "|" +
  //  Level.FINER + "|" +
  //  Level.FINEST +
  //  ")";
  //
  //  private final static String CODE = "\\[(\\d+)\\]|()";
  //
  //  private final static String MSG = "(.+)";
  //
  //  private final static Pattern LOG_LINE_PATTERN = Pattern.compile( LEVEL + CODE + ": " + MSG );

  // Constants for the msgCode parameter. Will get interpretated by the LogfileAnalyser

  /** Non-specific message code. */
  public static final int CODE_NONE = -1;

  /** This message will be shown toplevel inside the message box. */
  public static final int CODE_SHOW_MSGBOX = 100;

  /** This message will be shown inside the details part of the message box. */
  public static final int CODE_SHOW_DETAILS = 101;

  /** This message will be shown inside the details part of the message box. */
  public static final int CODE_NEW_MSGBOX = 102;

  private LoggerUtilities()
  {
    throw new UnsupportedOperationException( "Utility class" );
  }

  /**
   * Formats a log-message similar to java.util.logging.Logger's do it.
   * <p>
   * Example:
   * <p>
   * WARNING[19]: My mesage
   * 
   * @param msgCode
   *          An arbitrary message code (interpretation similar to status-code in eclipses IStatus).
   *          <p>
   *          If -1, no message code will be formatted into the message.
   */
  public static String formatLogStylish( final Level level, final int msgCode, final String message )
  {
    final StringBuffer sb = new StringBuffer( level.toString() );
    if( msgCode != -1 )
    {
      sb.append( "[" );
      sb.append( msgCode );
      sb.append( "]" );
    }

    sb.append( ": " );
    sb.append( message );

    final String outString = sb.toString();
    return outString;
  }

  /**
   * Converts a previously with {@link #formatLogStylish(Level, int, String)}formatted line into a {@link IStatus}
   * object.
   */
  public static IStatus lineToStatus( final String line )
  {
    final String LEVEL = "(" + Level.SEVERE + "|" + Level.WARNING + "|" + Level.INFO + "|" + Level.CONFIG + "|"
        + Level.FINE + "|" + Level.FINER + "|" + Level.FINEST + ")";

    final String CODE = "((\\[\\d+\\])|())";

    final String MSG = "(.+)";

    final Pattern LOG_LINE_PATTERN = Pattern.compile( LEVEL + CODE + ": " + MSG );

    final Matcher matcher = LOG_LINE_PATTERN.matcher( line );

    if( matcher.matches() )
    {
      final Level level = Level.parse( matcher.group( 1 ) );
      final String codeString = matcher.group( 2 );
      final int code;
      if( codeString == null || codeString.length() < 2 )
        code = -1;
      else
      {
        final String intString = codeString.substring( 1, codeString.length() - 1 );
        code = Integer.parseInt( intString );
      }

      final String msg = matcher.group( 5 );
      final int severity = severityFromLevel( level );

      return new Status( severity, PLUGIN_ID, code, msg, null );
    }

    return null;
  }

  /**
   * Converts a Level into a IStatus-severity.
   */
  private static int severityFromLevel( final Level level )
  {
    if( level == Level.SEVERE )
      return IStatus.ERROR;

    if( level == Level.WARNING )
      return IStatus.WARNING;

    if( level == Level.INFO )
      return IStatus.INFO;

    if( level == Level.CONFIG )
      return IStatus.INFO;

    if( level == Level.FINE )
      return IStatus.INFO;

    if( level == Level.FINER )
      return IStatus.INFO;
    
    if( level == Level.FINEST )
      return IStatus.INFO;

    return IStatus.OK;
  }
}
