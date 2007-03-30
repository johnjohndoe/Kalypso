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
package org.kalypso.commons.runtime;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

/**
 * This is the new version of the former (now deprecated) LogStatusWrapper.
 * <p>
 * The log files do now contain log-entries of the form 'LEVEL: message' which are read as IStatus object into a
 * multistatus.
 * 
 * @author Marc Schlienger
 */
public class LogAnalyzer
{
  private final static String LEVEL = "(WARNING|INFO): ";

  private final static String CODE = "(\\**)";

  private final static String MSG = "(.+)";

  private final static Pattern LOG_LINE_PATTERN = Pattern.compile( LEVEL + CODE + MSG );

  /**
   * Reads a log file into a (multi-)status.
   * <p>
   * Each line of the file is added to the status as a new child if it starts with a log-level (e.g. WARNING: ...).
   * </p>
   * <p>
   * The rest of the lines is read as the message.
   * </p>
   * <p>
   * 
   * @param status
   *          A status object used as template for the returned status.
   * @return The result status. If problems are encountered while reading the file, instead an error status describing
   *         the io-problems is returned.
   */
  public static IStatus logfileToStatus( final File file, final String charset, final MultiStatus status )
  {
    InputStream inputStream = null;
    LineNumberReader lnr = null;
    try
    {
      inputStream = new FileInputStream( file );
      lnr = new LineNumberReader( new InputStreamReader( inputStream, charset ) );

      final String summary = readerToStatus( lnr, status );

      lnr.close();

      return new MultiStatus( status.getPlugin(), status.getCode(), status.getChildren(), summary, null );
    }
    catch( final Throwable t )
    {
      final StringBuffer msg = new StringBuffer( "Fehler beim Lesen" );
      if( lnr != null )
      {
        msg.append( " von Zeile" );
        msg.append( lnr.getLineNumber() );
      }

      msg.append( " der Logdatei: " );
      msg.append( file.getAbsolutePath() );

      return StatusUtilities.statusFromThrowable( t, msg.toString() );
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
    }
  }

  /**
   * Reads a reader into a (multi-)status.
   * <p>
   * The reader will not be closed.
   * 
   * @return The summary: all msg lines starting with a '*' concatenated.
   * @throws IOException
   */
  public static String readerToStatus( final BufferedReader br, final MultiStatus status ) throws IOException
  {
    final StringWriter sw = new StringWriter();
    final PrintWriter summary = new PrintWriter( sw );

    while( br.ready() )
    {
      final String line = br.readLine();
      if( line == null )
        break;

      final Matcher matcher = LOG_LINE_PATTERN.matcher( line );
      if( matcher.matches() )
      {
        final Level level = Level.parse( matcher.group( 1 ) );
        final int code = matcher.group( 2 ).length();
        final String msg = level.getLocalizedName() + ": " + matcher.group( 3 );

        final int severity = severityFromLevel( level );
        final IStatus lineStatus = new Status( severity, KalypsoCommonsPlugin.getID(), code, msg, null );
        if( !lineStatus.isOK() )
        {
          if( code > 0 )
            summary.println( msg );

          status.add( lineStatus );
        }
      }
    }

    summary.close();
    return shortenMessage( sw.toString() );
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

    return IStatus.OK;
  }
  
  /**
   * The ErrorDialog cannot show a message which is too long: the dialog's height would be bigger than the user's
   * monitor. That's why we truncate the summary string here. If the user wants to see more, he can have a look at the
   * log file using the 'details' button in the ErrorDialog
   */
  public static String shortenMessage( final String message )
  {
    /* Do not shorten if we are already short enough */
    if( message.length() < 512 )
      return message;
    
    final String truncatedSummary = StringUtils.left( message, 512 );
    // '\r' verschwinden lassen, da sonst der Status-Dialog zuviele Umbrüche generiert
    final String msg = truncatedSummary.replace( '\r', '\t' );
    return msg + "...";
  }

}
