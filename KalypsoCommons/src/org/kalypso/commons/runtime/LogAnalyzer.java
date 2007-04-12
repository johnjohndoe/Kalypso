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
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.contribs.eclipse.core.runtime.DialogMultiStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;

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
  /**
   * Reads a log file into an array of (multi-)statuses.
   * <p>
   * The log file is parsed as follows:
   * <ul>
   * <li>All lines formatted as a log-message are parsed into a list of status object. (See {@link LoggerUtilities})
   * </li>
   * <li>All statuses between two messages with code {@link LoggerUtilities#CODE_NEW_MSGBOX}go into a separate
   * MultiStatus.</li>
   * <li>Only messages with a code unequal to {@link LoggerUtilities#CODE_NONE}are considered.</li>
   * <li>The message text of each multi-status will be composed of all its statuses with code
   * {@link LoggerUtilities#CODE_SHOW_MSGBOX}</li>
   * </ul>
   * 
   * @return The resulting stati. If problems are encountered while reading the file, instead an error status describing
   *         the io-problems is returned.
   */
  public static IStatus[] logfileToStatus( final File file, final String charset )
  {
    InputStream inputStream = null;
    LineNumberReader lnr = null;
    try
    {
      inputStream = new FileInputStream( file );
      lnr = new LineNumberReader( new InputStreamReader( inputStream, charset ) );

      final IStatus[] result = readerToStatus( lnr );

      lnr.close();

      return result;
    }
    catch( final Throwable t )
    {
      final StringBuffer msg = new StringBuffer( "Fehler beim Lesen" );
      if( lnr != null )
      {
        msg.append( " von Zeile " );
        msg.append( lnr.getLineNumber() );
      }

      msg.append( " der Logdatei: " );
      msg.append( file.getAbsolutePath() );

      final IStatus status = new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), LoggerUtilities.CODE_SHOW_MSGBOX,
          msg.toString(), t );

      return new IStatus[]
      { status };
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
    }
  }

  /**
   * Reads a reader into an array of statuses.
   * <p>
   * The reader will not be closed.
   * 
   * @throws IOException
   */
  public static IStatus[] readerToStatus( final BufferedReader br ) throws IOException
  {
    final List stati = new ArrayList();

    while( br.ready() )
    {
      final String line = br.readLine();
      if( line == null )
        break;

      final IStatus lineStatus = LoggerUtilities.lineToStatus( line );
      if( lineStatus != null )
        stati.add( lineStatus );
    }

    return (IStatus[])stati.toArray( new IStatus[stati.size()] );
  }

  /** sorts an array of stati according to its {@link LoggerUtilities}-codes. */
  public static IStatus[] groupStati( final IStatus[] stati )
  {
    final GroupStatusStrategry collector = new GroupStatusStrategry();
    for( int i = 0; i < stati.length; i++ )
      collector.add( stati[i] );

    return collector.getResult();
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

  /**
   * @see LogAnalyzer#groupStati(IStatus[])
   * 
   * @author Belger
   */
  private static final class GroupStatusStrategry
  {
    private final List m_children = new ArrayList();

    private final StringBuffer m_message = new StringBuffer();

    private String m_dialogMessage = null;

    private MultiStatus m_result;

    public GroupStatusStrategry()
    {
      m_result = new MultiStatus( KalypsoCommonsPlugin.getID(), -1, "", null );
      createNewSub();
    }

    public void add( final IStatus status )
    {
      if( m_result == null )
        throw new IllegalStateException( "Result can only be retrieved once." );

      switch( status.getCode() )
      {
      case LoggerUtilities.CODE_NEW_MSGBOX:
        createNewSub();
        m_dialogMessage = status.getMessage();
        break;

      case LoggerUtilities.CODE_SHOW_DETAILS:
      case LoggerUtilities.CODE_SHOW_MSGBOX:
        addToCurrent( status );
        break;

      case LoggerUtilities.CODE_NONE:
      // fall through
      default:
        // Stati with unknown codes dont get grouped.
        break;
      }
    }

    private void createNewSub()
    {
      if( !m_children.isEmpty() )
      {
        final IStatus[] children = (IStatus[])m_children.toArray( new IStatus[m_children.size()] );
        final String message = m_message.length() == 0 ? "siehe Details" : m_message.toString();

        final int code = m_result.getCode();
        final String plugin = m_result.getPlugin();

        final MultiStatus status;
        if( m_dialogMessage == null )
          status = new MultiStatus( plugin, code, children, message, null );
        else
        {
          status = new DialogMultiStatus( plugin, code, children, message, null );
          ( (DialogMultiStatus)status ).setDialogMessage( m_dialogMessage );
        }

        m_result.add( status );
      }

      m_children.clear();
      m_message.setLength( 0 );
      m_dialogMessage = null;
    }

    private void addToCurrent( final IStatus status )
    {
      final int code = status.getCode();
      final String message = status.getMessage();
      final int severity = status.getSeverity();
      final String plugin = status.getPlugin();

      final String msg = StatusUtilities.getLocalizedSeverity( status ) + ": " + message;
      final IStatus clonedStatus = new Status( severity, plugin, code, msg, status.getException() );
      m_children.add( clonedStatus );

      if( code == LoggerUtilities.CODE_SHOW_MSGBOX )
      {
        if( m_message.length() > 0 )
          m_message.append( '\n' );
        m_message.append( message );
      }

    }

    public IStatus[] getResult()
    {
      if( m_result == null )
        throw new IllegalStateException( "Result can only be retrieved once." );

      createNewSub();

      final IStatus[] children = m_result.getChildren();
      m_result = null; // this method should not get called more than once

      //      /** Create nice messages for all children. */
      //      final List result = new ArrayList();
      //
      //      for( int i = 0; i < children.length; i++ )
      //      {
      //        /** We do know its a multi status. */
      //        final MultiStatus status = (MultiStatus)children[i];
      //
      //        final IStatus[] subChildren = status.getChildren();
      //
      //        /* Skip empty children */
      //        if( subChildren.length == 0 )
      //          continue;
      //
      //        final StringWriter sw = new StringWriter();
      //        final PrintWriter pw = new PrintWriter( sw );
      //        for( int j = 0; j < subChildren.length; j++ )
      //        {
      //          final IStatus subChild = subChildren[j];
      //          if( subChild.getCode() == LoggerUtilities.CODE_SHOW_MSGBOX )
      //            pw.println( subChild.getMessage() );
      //        }
      //        pw.close();
      //
      //        final String message = shortenMessage( sw.toString() );
      //        final String msg = message.length() == 0 ? "siehe Details" : message;
      //        result.add( new MultiStatus( KalypsoCommonsPlugin.getID(), -1, subChildren, msg, null ) );
      //      }

      //      return (IStatus[])result.toArray( new IStatus[result.size()] );
      return children;
    }
  }

}
